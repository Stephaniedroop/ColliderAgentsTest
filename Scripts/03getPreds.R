####################################################
###### Collider - get model predictions  #####
####################################################
# Script to set up probability vectors of each variable, then run a series of functions from source file `cesmUtils`
# to implement the cesm and save the model predictions for each run

library(tidyverse)
library(here)
source(here('Scripts', 'cesmUtils.R')) # Functions for running the cesm model

# Other values set outside for now
N_cf <- 100000L # How many counterfactual samples to draw
modelruns <- 10
s <- 0.7
causes1 <- c('A', 'Au', 'B', 'Bu')

load(here('Data', 'modelData', 'params.rdata')) # defined in script `set_params.r`

set.seed(12)

# -------------- Full CESM ----------

# Empty df to put everything in
all <- data.frame(matrix(ncol = 27, nrow = 0)) # needs to be 10 longer than df

# For each setting of possible probability parameters we want to:
# 1) generate worlds, 2) get CESM model predictions
for (i in 1:length(poss_params)) {
  # 1) Get possible world combos of two observed variables in both dis and conj structures
  dfd <- world_combos(params = poss_params[[i]], structure = 'disjunctive')
  dfd$pgroup <- i
  dfc <- world_combos(params = poss_params[[i]], structure = 'conjunctive')
  dfc$pgroup <- i
  # Empty df to put model predictions in
  mp1 <- data.frame(matrix(ncol = 27, nrow = 0)) # was 27
  # 3) Get predictions of the counterfactual effect size model for these worlds
  # We also want to calculate 10 versions to get the variance of model predictions
  for (m in 1:modelruns) {
    # If you want to test the Quillien and Lucas 2023 'General version' of CESM, replace 'get_cfs' with 'get_cfs_ql'
    mpd <- get_cfs(
      params = poss_params[[i]],
      structure = 'disjunctive',
      df = dfd
    )
    mpd$run <- m
    mpc <- get_cfs(
      params = poss_params[[i]],
      structure = 'conjunctive',
      df = dfc
    )
    mpc$run <- m
    mp1 <- rbind(mp1, mpd, mpc)
  }
  mp1$pgroup <- i
  all <- rbind(all, mp1) # 1440 of 26
}
# It takes a minute or two but not terrible.

save(all, file = here('Data', 'modelData', 'all.rda')) # 1440 of 27
