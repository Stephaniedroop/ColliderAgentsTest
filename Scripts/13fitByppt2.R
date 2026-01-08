#####################################################
########### REAL Model fit by participant ############
#####################################################

library(tidyverse)
library(here)

set.seed(12)

# --------- Load what we need ---------
source(here('Scripts', 'optimUtilsNNN3.R')) # functions to optimise
# Unfitted model scores
load(here('Data', 'modelData', 'modelAndDataUnfitig.rda')) # df, 288 of 23
# Participant data
load(here('Data', 'Data.Rdata')) # This is one big data, 2580, of 215 ppts

byppt <- data |>
  select(subject_id, trial_id, node3)

# Participant data
data_list <- split(byppt, byppt$subject_id) # 215 elements each of 12x14

# Test what one of the elements looks like
test <- data_list[[23]] # 12 obs of 3: subjid, trialid, node (what they answered in each trial)

# Merges the model1 series into each ppt, so now sum$n=12 for each df in the list
ppt_merges <- lapply(data_list, function(ppt_data) {
  merged <- df |>
    left_join(ppt_data, by = c('trial_id', 'node3')) |>
    mutate(n = ifelse(!is.na(subject_id), 1, 0)) |> # now always uses 'n'
    select(-subject_id)
  merged
})

# ----------- A separate section to save similar data sparsely for a later regression to predict answer from all conditions ----------

# Need the same again but with subject_id for something else later (big regression): just do it again in it throws something out later in this script:
ppt_merges2 <- lapply(data_list, function(ppt_data) {
  merged <- df |>
    left_join(ppt_data, by = c('trial_id', 'node3')) |>
    mutate(n = ifelse(!is.na(subject_id), 1, 0)) #|> # now always uses 'n'
  #select(-subject_id)
  merged
})

# For each element in the ppt_merges list, the column subject_id is mostly NAs but has some content. Fill up or down to fill the whole column with the content
ppt_merges2 <- lapply(ppt_merges2, function(dat) {
  subid <- unique(na.omit(dat$subject_id))
  dat$subject_id <- subid
  dat
})

# Now for each element in the list, take columns 1:5 and 24, and unlist into a big df - 61968 of 5 - 215 ppts x 288 rows each - should be 61920
forregress <- do.call(
  rbind,
  lapply(ppt_merges2, function(dat) {
    dat[, c(1:5, 24)]
  })
)

# save
save(forregress, file = here('Data', 'modelData', 'forregress.rda'))

check <- forregress |> group_by(subject_id) |> summarise(n = n()) # 288 each - good
check2 <- check |> filter(n != 288) # 43 obs - these are the ones who saw a condition twice - due to counterbalancing error - it's fine

# ----------

test2 <- ppt_merges[[23]] #
test3 <- ppt_merges[[124]]
test4 <- ppt_merges[[215]]


# -------- Back to optimisation ------------

# Test on one ppt (one element of the list)
get_optimisation(model_names, test4, operative = WITH_KAPPA)
get_optimisation('full', test4, operative = WITH_KAPPA)

# Call optimize_models(model_names, df) for every element of data_list
results_3par <- lapply(ppt_merges, function(dat) {
  get_optimisation(model_names, dat, operative = WITH_KAPPA)
})

print(results_3par)

# ---------- The two par version ---------

# Rename columns 8:16 of every df in the ppt_merges list
ppt_merges <- lapply(ppt_merges, function(dat) {
  colnames(dat)[8:16] <- model_names2
  dat
})

results_3par <- lapply(ppt_merges, function(dat) {
  get_optimisation(model_names2, dat, operative = NO_KAPPA)
})

# Backing up - actually the baseline model needs a less stringent BIC because it has no parameters
# So access the last element of the BIC elements and change it to just -2 x the logl
results_3par <- lapply(results_3par, function(res) {
  # access the first element, which contains logl and BIC vectors
  res[[1]]$BIC[9] <- -2 * as.numeric(res[[1]]$logl[9])
  res
})

# Get some random ones and test
test5 <- results_3par[[60]][[1]]$BIC[9] # -24.95 = 49.9 - it will be the same for everyone

#print(results_2par)

# Save results of both series
save(
  results_3par,
  results_4par,
  file = here('Data', 'modelData', 'individFitsNNN4.rda')
)
