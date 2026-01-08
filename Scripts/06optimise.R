##################################################################
######## Optimise params and NLL ################
##################################################################

library(tidyverse)
library(here)
library(xtable)
source(here('Scripts', 'optimUtils.R')) # functions to optimise

set.seed(12)

load(here('Data', 'modelData', 'modelAndDataUnfitig.rda')) # df, 288 of 23

# ------------- An extra section for reporting noisy answers -------------
# nons <- sum(df$n[df$Include == FALSE]) #132 / 2580.   389???!!
#
# ppl <- df |>
#   group_by(trial_id) |>
#   summarise(n=sum(n))
#
# sum(ppl$n) # 2580

# For old experiment it is 50/3408 = .0147
# For new experiment it is 132/2580 = .0511 now it is 15.1 %???!!

# -------------- Run the functions ------------------

# Was in two versions, one for 3 pars and one for 2. Then made a new comparison table for the 3-par versions.

# Initial values for testing:
#pars <- c(1, 1, 1) # This now in 'operatives list' in the optimUtils file
mod_name <- 'full'
i <- 1

# Usage:
model_names <- c(
  'full',
  'noAct',
  'noInf',
  'noSelect',
  'noActnoInf',
  'noActnoSelect',
  'noInfnoSelect',
  'noActnoInfnoSelect'
)

results1 <- optimize_models(model_names, df, operatives_list)

print(results1)

# ---------- Format and save the results, series run in several blocks ----------

#---------- Model 1: -------------

# Useful names for tables
title1 <- "kappa acting on ig vs Known" # Becomes caption in latex
filename1 <- "3par_series_igk" # title for html

# Format results1$model_fits in latex code in a table for publication in a paper
results1$model_fits[, -c(1, 2)] <- apply(
  results1$model_fits[, -c(1, 2)],
  2,
  trim_zeros
)
rownames(results1$model_fits) <- NULL

xtab <- xtable(results1$model_fits, digits = 3, caption = title1) # caption = c('Full title', 'Short title') if you wwant to include long tags

# Then print to file in both tex and html
print(xtab, file = here('Data', 'fitTables', paste0(filename1, '.tex'))) # LaTeX output
print(
  xtab,
  type = 'html',
  file = here('Data', 'fitTables', paste0(filename1, '.html'))
)


# And also .rds with long descriptive name and short name for easy reference later
xtablabel <- list(
  data = xtab,
  info = "Full name"
)
saveRDS(xtablabel, file = here('Data', 'fitTables', paste0(filename1, '.rds')))

#------------- SMALL HACKY SECTION TO PLOT `IG` VERSION IN SERIES WITH LESIONED MODELS ------------

# Filter for operatives==ig_only - we're not using known any more, not enough theoretical backing
results1$model_fits2 <- results1$model_fits |>
  filter(operatives == 'ig') # CHANGE HERE

# Now remove the column called operatives
results1$model_fits2 <- results1$model_fits2 |>
  select(-operatives)

# Keep only the predictions for known_only too
results1$predictions <- results1$predictions |>
  filter(operatives == 'ig') # CHANGE HERE

# Remove the operatives column from predictions too
results1$predictions <- results1$predictions |>
  select(-operatives)

# -------------- The two par version FOR ALL THE NOKIND SERIES --------------

# Initial values for testing:
pars <- c(1, 1)
mod_name <- 'full' # change back to noKind if the 3-way doesn't work
i <- 1

# Usage:
model_names2 <- c(
  'noKind',
  'noActnoKind',
  'noInfnoKind',
  'noKindnoSelect',
  'noActnoInfnoKind',
  'noActnoKindnoSelect',
  'noInfnoKindnoSelect',
  'noActnoInfnoKindnoSelect',
  'baseline'
)

# model_names2 <- c(
#   model_names,
#   'baseline'
# )

# Replace the predictions with the model_names2 (this is fine because the presence or absence of the other modules remains same)
df <- df |>
  relocate(baseline, .before = Actual)

colnames(df)[8:16] <- model_names2

results2 <- optimize_models2(model_names2, df)

print(results2)

# Format results1$model_fits in latex code in a table for publication in a paper
results2$model_fits[, -1] <- apply(results2$model_fits[, -1], 2, trim_zeros)
xtable(results2$model_fits[, -1], digits = 3)

# Add column to results2$predictions with name 'operatives' and value 'none'
# results2$predictions <- results2$predictions |>
#   mutate(operatives = 'none')

#saveRDS(xtablabel, file = here('Data', 'fitTables', paste0(filename1, '.rds')))

# ------------- Combine and process --------------------

allpredictions <- rbind(results1$predictions, results2$predictions) # 4896: 36tt x 8 nodevals x 17 models
# 7200 if we have 36 tt x 8 nodevals x 25 models (8 k=ig, 8 k=known, 8 k=none, baseline)

# Pivot wider on both model and operatives to get one column per model
# allpredictions <- allpredictions |>
#   unite("model", c("model", "operatives"), sep = "_") # combine model and operatives

## ---------------------------------------------------------------------------------------------------------------
df_wide <- allpredictions |>
  pivot_wider(
    id_cols = c(trial_id, node3),
    names_from = model,
    values_from = predicted_prob
  )

## ---------------------------------------------------------------------------------------------------------------
justppt <- df |>
  select(
    trial_id,
    node3,
    n,
    prop,
    pgroup,
    Actual,
    A,
    B,
    E,
    Include,
    Observed,
    Known
  )

fitforplot <- merge(df_wide, justppt, by = c('trial_id', 'node3')) # 288 of 37

# Change the name of the column 'baseline_none' to just baseline
# colnames(fitforplot)[which(
#   colnames(fitforplot) == 'baseline_none'
# )] <- 'baseline'

# Set the NAs in Actual to False
#fitforplot[, 23][is.na(fitforplot[, 23])] <- FALSE

## ---------------------------------------------------------------------------------------------------------------

save(fitforplot, file = here('Data', 'modelData', 'fit16ig.rda')) #
