####################################################
###### Collider - get N/S model predictions  #####
####################################################
#
# Script to generate Necessity/Sufficiency model predictions
# for all 36 world/parameter combinations (3 pgroups × 12 trialtypes)
#
# Based on Icard, Kominsky & Knobe (2017)
#
####################################################

library(tidyverse)
library(here)

# Source the N/S utility functions
source(here('Scripts', 'nsUtils.R'))

# Load parameters
load(here('Data', 'modelData', 'params.rdata'))  # loads poss_params

set.seed(12)

# -------------- Define trial types --------------

trial_types <- data.frame(
  trialtype = c('c1', 'c2', 'c3', 'c4', 'c5', 'd1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7'),
  structure = c(rep('conjunctive', 5), rep('disjunctive', 7)),
  A = c(0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1),
  B = c(0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1),
  E = c(0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1)
)

# -------------- Generate predictions for all combinations --------------

# Empty dataframe to collect all predictions
all_ns <- data.frame()

# Loop over parameter groups
for (pgroup in 1:length(poss_params)) {
  params <- poss_params[[pgroup]]

  # Loop over trial types
  for (i in 1:nrow(trial_types)) {
    trial <- trial_types[i, ]

    # Get NS predictions for this world
    scores <- get_ns_predictions(
      params = params,
      structure = trial$structure,
      trial_info = list(A = trial$A, B = trial$B, E = trial$E)
    )

    # Add identifiers
    scores$pgroup <- pgroup
    scores$trialtype <- trial$trialtype

    # Append to results
    all_ns <- rbind(all_ns, scores)
  }

  cat("Completed parameter group", pgroup, "\n")
}

# -------------- Process and format --------------

# Create node2 column (just the variable name without value)
all_ns <- all_ns |>
  mutate(node2 = cause)

# Add trial_id
all_ns <- all_ns |>
  unite('trial_id', pgroup, trialtype, sep = "_", remove = FALSE)

# Reorder columns to match existing format
all_ns <- all_ns |>
  select(
    pgroup, trialtype, trial_id, structure,
    A, B, E,
    node2, node3, cause_value,
    kappa, necessity, sufficiency
  )

# -------------- Create alternative combination measures --------------

all_ns <- all_ns |>
  mutate(
    # Icard's kappa (already computed, but rename for clarity)
    ns_kappa = kappa,

    # Alternative combinations
    ns_product = necessity * sufficiency,
    ns_average = (necessity + sufficiency) / 2,
    ns_necessity_only = necessity,
    ns_sufficiency_only = sufficiency
  )

# -------------- Summary statistics --------------

cat("\n--- N/S Predictions Summary ---\n")
cat("Total predictions:", nrow(all_ns), "\n")
cat("Parameter groups:", length(unique(all_ns$pgroup)), "\n")
cat("Trial types:", length(unique(all_ns$trialtype)), "\n")
cat("Response options per trial:", length(unique(all_ns$node3)), "\n")

# Check for any NA values
na_counts <- colSums(is.na(all_ns))
if (any(na_counts > 0)) {
  cat("\nWarning: NA values found:\n")
  print(na_counts[na_counts > 0])
}

# -------------- Save predictions --------------

save(all_ns, file = here('Data', 'modelData', 'nsPreds.rda'))
cat("\nSaved predictions to Data/modelData/nsPreds.rda\n")

# Also save as CSV for inspection
write.csv(all_ns, file = here('Data', 'modelData', 'nsPreds.csv'), row.names = FALSE)
cat("Saved CSV to Data/modelData/nsPreds.csv\n")

# -------------- Quick validation --------------

# Check a few known cases
cat("\n--- Validation Checks ---\n")

# In conjunctive c5 (A=1, B=1, E=1), both A and B should have high scores
c5_check <- all_ns |>
  filter(trialtype == 'c5', pgroup == 1, node3 %in% c('A=1', 'B=1'))
cat("\nc5 (conjunctive, E=1): A=1 and B=1 scores:\n")
print(c5_check |> select(node3, kappa, necessity, sufficiency))

# In disjunctive d7 (A=1, B=1, E=1), scores should differ based on priors
d7_check <- all_ns |>
  filter(trialtype == 'd7', pgroup == 1, node3 %in% c('A=1', 'B=1'))
cat("\nd7 (disjunctive, E=1): A=1 and B=1 scores:\n")
print(d7_check |> select(node3, kappa, necessity, sufficiency))
