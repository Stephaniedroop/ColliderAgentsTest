####################################################
###### Collider - Process N/S predictions  #####
####################################################
#
# Script to process N/S model predictions and merge with participant data
# Creates format compatible with likelihood calculation
#
####################################################

library(tidyverse)
library(here)

# Load N/S predictions
load(here('Data', 'modelData', 'nsPreds.rda'))  # loads all_ns (288 rows)

# Load participant data
load(here('Data', 'Data.rdata'))  # loads data (2580 obs)

# Load information gain from existing processing (needed for compatibility)
load(here('Data', 'modelData', 'modelproc.rda'))  # loads mp

cat("=== N/S Model Processing ===\n")
cat("N/S predictions rows:", nrow(all_ns), "\n")
cat("Participant data rows:", nrow(data), "\n")

# -------------- 1. Process N/S predictions --------------

# The N/S predictions already have the right structure:
# - 36 trials (3 pgroups × 12 trialtypes)
# - 8 response options per trial (A=0, A=1, B=0, B=1, Au=0, Au=1, Bu=0, Bu=1)
# = 288 rows total

# Ensure proper factor types
ns_preds <- all_ns |>
  mutate(
    pgroup = as.factor(pgroup),
    trialtype = as.factor(trialtype),
    structure = as.factor(structure),
    node3 = as.factor(node3),
    node2 = as.factor(node2)
  )

# -------------- 2. Compute Actual causation module for N/S --------------
# Same logic as CESM: a cause is "actual" when it matches the effect

ns_preds <- ns_preds |>
  mutate(
    Actual = case_when(
      node2 == 'A' & cause_value == E ~ TRUE,
      node2 == 'B' & cause_value == E ~ TRUE,
      node2 == 'Au' & cause_value == E ~ TRUE,
      node2 == 'Bu' & cause_value == E ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Additional condition: unobserved variable can only follow main variable
ns_preds$Actual[ns_preds$A == 0 & ns_preds$node3 == 'Au=1'] <- FALSE
ns_preds$Actual[ns_preds$B == 0 & ns_preds$node3 == 'Bu=1'] <- FALSE

# -------------- 3. Create model variants with Actual treatment --------------

# Full N/S models (with Actual causation applied)
ns_preds <- ns_preds |>
  mutate(
    # Kappa with Actual
    ns_kappa_actual = ns_kappa * Actual,

    # Alternative combinations with Actual
    ns_product_actual = ns_product * Actual,
    ns_average_actual = ns_average * Actual,
    ns_necessity_actual = ns_necessity_only * Actual,
    ns_sufficiency_actual = ns_sufficiency_only * Actual
  )

# -------------- 4. Create lesioned versions --------------
# For N/S, the key lesion is removing Actual constraint

# Full models (Actual applied)
ns_full <- ns_preds |>
  group_by(pgroup, trialtype, node3, .drop = FALSE) |>
  summarise(
    ns_kappa_full = first(ns_kappa_actual),
    ns_product_full = first(ns_product_actual),
    ns_average_full = first(ns_average_actual),
    ns_necessity_full = first(ns_necessity_actual),
    ns_sufficiency_full = first(ns_sufficiency_actual),
    .groups = "drop"
  )

# NoActual lesion (no Actual constraint)
ns_noAct <- ns_preds |>
  group_by(pgroup, trialtype, node3, .drop = FALSE) |>
  summarise(
    ns_kappa_noAct = first(ns_kappa),
    ns_product_noAct = first(ns_product),
    ns_average_noAct = first(ns_average),
    ns_necessity_noAct = first(ns_necessity_only),
    ns_sufficiency_noAct = first(ns_sufficiency_only),
    .groups = "drop"
  )

# Get Actual values for reference
ns_actual <- ns_preds |>
  group_by(pgroup, trialtype, node3) |>
  summarise(Actual_ns = first(Actual), .groups = "drop")

# Merge all N/S model variants
ns_models <- ns_full |>
  left_join(ns_noAct, by = c('pgroup', 'trialtype', 'node3')) |>
  left_join(ns_actual, by = c('pgroup', 'trialtype', 'node3'))

# -------------- 5. Summarise participant data --------------

dataNorm <- data |>
  group_by(pgroup, trialtype, node3, .drop = FALSE) |>
  tally() |>
  mutate(prop = n / sum(n))

# Get information gain from CESM processing (for compatibility)
# Extract unique ig values from the existing modelproc data
getpost_cesm <- mp |>
  filter(!node2 %in% c('A', 'B')) |>
  group_by(pgroup, trialtype, node3, .drop = FALSE) |>
  summarise(post = sum(posterior), prior = sum(PrUn), .groups = "drop")

unobs_ig <- getpost_cesm |>
  group_by(pgroup, trialtype, node3) |>
  summarise(
    prior_entropy = round(-sum(prior * log2(prior + 1e-10)), 3),
    post_entropy = round(-sum(post * log2(post + 1e-10)), 3),
    ig = round(prior_entropy - post_entropy, 3),
    .groups = "drop"
  )

ig <- unobs_ig |>
  select(pgroup, trialtype, node3, ig)

# Merge ig with participant data
dataNorm <- merge(x = dataNorm, y = ig, by = c('pgroup', 'trialtype', 'node3'))

# -------------- 6. Merge N/S models with participant data --------------

ns_modelAndData <- merge(x = dataNorm, y = ns_models, by = c('pgroup', 'trialtype', 'node3'))

# Add trial_id
ns_modelAndData <- ns_modelAndData |>
  unite('trial_id', pgroup, trialtype, sep = "_", remove = FALSE)

ns_modelAndData$trial_id <- as.factor(ns_modelAndData$trial_id)

# Add baseline (uniform probability)
ns_modelAndData <- ns_modelAndData |>
  group_by(trial_id) |>
  mutate(baseline = 1 / n()) |>
  ungroup()

# -------------- 7. Add trial mapping and Include/Known flags --------------

df.map <- data.frame(
  condition = c('c1', 'c2', 'c3', 'c4', 'c5', 'd1', 'd2', 'd3', 'd4', 'd5', 'd6', 'd7'),
  A = c(0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1),
  B = c(0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1),
  E = c(0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1)
)

ns_modelAndData <- ns_modelAndData |>
  left_join(df.map, by = c("trialtype" = "condition"))

# Include flag: FALSE for responses that contradict observations
ns_modelAndData <- ns_modelAndData |>
  mutate(
    Include = !((node3 == 'B=0' & B == 1) |
                  (node3 == 'B=1' & B == 0) |
                  (node3 == 'A=0' & A == 1) |
                  (node3 == 'A=1' & A == 0))
  )

# Observed flag
ns_modelAndData <- ns_modelAndData |>
  mutate(
    Observed = if_else(node3 %in% c('A=0', 'A=1', 'B=0', 'B=1'), TRUE, FALSE)
  )

# Known flag (logical deduction possible)
ns_modelAndData <- ns_modelAndData |>
  mutate(
    Known = case_when(
      node3 %in% c('A=0', 'A=1', 'B=0', 'B=1') & Actual_ns == TRUE ~ TRUE,
      trialtype == 'c5' & node3 %in% c('Au=1', 'Bu=1') ~ TRUE,
      trialtype == 'd2' & node3 == 'Bu=0' ~ TRUE,
      trialtype == 'd3' & node3 == 'Bu=1' ~ TRUE,
      trialtype == 'd4' & node3 == 'Au=0' ~ TRUE,
      trialtype == 'd5' & node3 == 'Au=1' ~ TRUE,
      trialtype == 'd6' & node3 %in% c('Au=0', 'Bu=0') ~ TRUE,
      TRUE ~ FALSE
    )
  )

# -------------- 8. Apply -Inf for impossible responses --------------
# (For softmax, -Inf ensures these get near-zero probability)

# Get column indices for N/S model columns
ns_model_cols <- grep("^ns_", names(ns_modelAndData), value = TRUE)

# Set -Inf for responses that contradict observations
for (col in ns_model_cols) {
  ns_modelAndData[[col]][ns_modelAndData$Include == FALSE] <- -Inf
}

# Set -Inf for non-actual responses in "_full" models (already done via multiplication by 0/FALSE)
# But need to convert 0 to -Inf for softmax
full_cols <- grep("_full$", ns_model_cols, value = TRUE)
for (col in full_cols) {
  ns_modelAndData[[col]][ns_modelAndData$Actual_ns == FALSE] <- -Inf
}

# Handle NAs in Actual
ns_modelAndData$Actual_ns[is.na(ns_modelAndData$Actual_ns)] <- FALSE

# -------------- 9. Final cleanup and save --------------

# Rename for consistency
df_ns <- ns_modelAndData

# Reorder columns
df_ns <- df_ns |>
  relocate(baseline, .before = ns_kappa_full)

# Summary
cat("\n=== N/S Model + Data Summary ===\n")
cat("Total rows:", nrow(df_ns), "\n")
cat("Unique trials:", length(unique(df_ns$trial_id)), "\n")
cat("Response options per trial:", nrow(df_ns) / length(unique(df_ns$trial_id)), "\n")
cat("\nModel columns:\n")
print(ns_model_cols)

# Check for any issues
na_counts <- colSums(is.na(df_ns[, ns_model_cols]))
if (any(na_counts > 0)) {
  cat("\nWarning: NA values in model columns:\n")
  print(na_counts[na_counts > 0])
}

# Save
save(df_ns, file = here('Data', 'modelData', 'nsModelAndData.rda'))
cat("\nSaved to Data/modelData/nsModelAndData.rda\n")

# Also save as CSV for inspection
write.csv(df_ns, file = here('Data', 'modelData', 'nsModelAndData.csv'), row.names = FALSE)
cat("Saved CSV to Data/modelData/nsModelAndData.csv\n")

# -------------- 10. Quick validation --------------

cat("\n=== Validation Checks ===\n")

# Check c5 (conjunctive, E=1) - should have high scores for A=1, B=1
c5_check <- df_ns |>
  filter(trialtype == 'c5', pgroup == 1) |>
  select(node3, n, prop, ns_kappa_full, ns_kappa_noAct, Actual_ns)
cat("\nc5 (conjunctive, E=1) - pgroup 1:\n")
print(c5_check)

# Check d7 (disjunctive, E=1, overdetermination)
d7_check <- df_ns |>
  filter(trialtype == 'd7', pgroup == 1) |>
  select(node3, n, prop, ns_kappa_full, ns_kappa_noAct, Actual_ns)
cat("\nd7 (disjunctive, E=1) - pgroup 1:\n")
print(d7_check)
