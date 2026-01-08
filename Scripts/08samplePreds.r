################################################################################
########### Sample predictions from model to match with each participant ##########
#################################################################################

library(tidyverse)
library(here)

set.seed(12)

# This script forces the model to give an actual answer to pair with each participant's observation.
# This cannot just be the same probability each time,
# but must have the chance to show up as a differnet node, proportional to its probability. It must be SAMPLED.

# This needs the participant data by row, and then the model data too.

load(here('Data', 'modelData', 'fitforplot25.rda')) # model df, 288 obs of 43
load(here('Data', 'Data.rdata')) # participant data, 2580 obs of 22

# Reduced participant data to just what they selected per trial, with numbered rows
# Sometimes participants effectively ended up with the 'same' trialtype more than once
# Due to the counterbalancing issue
datfortest <- data |>
  arrange(subject_id, trial_id) |>
  select(subject_id, trial_id, node3) |>
  group_by(subject_id) |>
  mutate(row_num = row_number())

# Likewise reduce the model df to just what we need
justmp <- df |>
  select(trial_id, node3, full_known) # [[[[[[[[[CHANGE MODEL HERE]]]]]]]]]] then rerun everything after this

# 39x9 - just the FULL model, in wide form
model_predictions_wide <- justmp |>
  pivot_wider(
    names_from = node3,
    values_from = full_known # [[[[[[[AND HERE]]]]]]]
  )

# Next section samples a variable as explanation for each participant trial,
# for that world condition, to match against the participant's answer

# The eight possible answers are always the same
variable_names <- setdiff(names(model_predictions_wide), "trial_id")

# Function to sample one of the eight answers, according to its probabilities on that trial
sample_variable_name <- function(probabilities, variable_names) {
  sampled_name <- sample(
    x = variable_names,
    size = 1,
    prob = as.numeric(probabilities)
  )
  return(as.character(sampled_name))
}

# Join the probabilities for all node values on to the participant data
# Not strictly needed but good to see what is going on
joined_df <- datfortest |>
  left_join(model_predictions_wide, by = "trial_id")

# Prepare a vector to store sampled variable names
sampled_variable <- vector("character", nrow(joined_df))

# Loop over each participant trial
for (i in seq_len(nrow(joined_df))) {
  # Extract the probabilities for this row (participant)
  probs <- as.numeric(joined_df[i, variable_names])

  # Sample one variable name for this participant, using its probabilities
  sampled_variable[i] <- sample_variable_name(probs, variable_names)
}

# Assign sampled variables as a new column
joined_df$sampled_variable <- sampled_variable

# Select the relevant columns to return
sampled_predictions <- joined_df |>
  arrange(subject_id, trial_id) |>
  group_by(subject_id) |>
  mutate(row_num = row_number()) |>
  ungroup() |>
  select(subject_id, trial_id, row_num, node3, sampled_variable)

# How many times did the model sample the same answer as the participant?
# Add a new column to sampled_predictions that is TRUE if the sampled variable matches the participant's node3
sampled_predictions <- sampled_predictions |>
  mutate(match = (sampled_variable == node3))

# Count the number of matches
num_matches <- sum(sampled_predictions$match, na.rm = TRUE) # 647 out of 2580 = 25.1%

# Now merge back in to real participant data. This is for binomial logistic regression
# so needs certain vars like UNOBSERVED to be 1/0.

modAndDat <- merge(
  datfortest,
  sampled_predictions,
  by = c("subject_id", "trial_id", "row_num", "node3"),
  all.x = TRUE
) |>
  select(-row_num, -match)

# Pivot longer - 5160
fortest <- pivot_longer(
  modAndDat,
  cols = -c(subject_id, trial_id),
  names_to = "Respondent",
  values_to = "Response"
)

# Now we need to put back in all those important variables we had before...

# fortest <- fortest |>
#   mutate(Observed = factor(!Response%in%c('Au=0','Au=1','Bu=0', 'Bu=1'), levels = c(TRUE, FALSE)))

fortest$subject_id <- as.factor(fortest$subject_id)
fortest$trial_id <- as.factor(fortest$trial_id)
fortest$Respondent <- factor(
  fortest$Respondent,
  levels = c('sampled_variable', 'node3')
)
fortest$Response <- as.factor(fortest$Response)

# ----------- Merge back in to get the full data for plotting -------------

# Same length as fortest, 5160, with both model and ppt responses, then adding in all the important condition tags
# Trial_id and Response are important merging vars on the left
# So this is boy row participant, and doesn't have the aggregated vars like n and prop
merged <- fortest |>
  left_join(
    df |>
      select(
        trial_id,
        Response,
        pgroup,
        Actual,
        Include,
        Observed,
        Known,
        structure,
        trial_type,
        trial_structure_type,
        Variable,
        SE
      ),
    by = c("trial_id", "Response")
  )


# save
save(merged, file = here('Data', 'modelData', 'matchedBypptf_known.rda')) # Now go to script 9
