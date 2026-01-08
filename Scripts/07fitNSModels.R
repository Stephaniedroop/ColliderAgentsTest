####################################################
###### Collider - Fit N/S models  #####
####################################################
#
# Script to fit N/S model variants and compare likelihoods
# Uses same optimization approach as CESM fitting
#
####################################################

library(tidyverse)
library(here)

set.seed(12)

# Load N/S model+data
load(here('Data', 'modelData', 'nsModelAndData.rda'))  # loads df_ns

# Load participant data
load(here('Data', 'Data.Rdata'))  # loads data (2580 obs)

cat("=== N/S Model Fitting ===\n")
cat("N/S model+data rows:", nrow(df_ns), "\n")
cat("Participants:", length(unique(data$subject_id)), "\n")

# -------------- Define N/S model names --------------

ns_model_names <- c(
  'ns_kappa_full',      # Full model with Actual
  'ns_kappa_noAct',     # No Actual constraint
  'ns_product_full',
  'ns_product_noAct',
  'ns_average_full',
  'ns_average_noAct',
  'ns_necessity_full',
  'ns_necessity_noAct',
  'ns_sufficiency_full',
  'ns_sufficiency_noAct'
)

# -------------- Core functions (adapted from optimUtilsNNN3.R) --------------

# Parameter transformation (2-parameter: tau, epsilon)
NS_TRANSFORM <- list(
  name = "ns_basic",
  n_params = 2,
  transform = function(pars) {
    list(
      tau = exp(pars[1]),           # Temperature for softmax
      epsilon = plogis(pars[2])     # Noise mixing (0-1)
    )
  }
)

# Compute model prediction for a single trial
compute_ns_mpred <- function(tr, mod_name, params) {
  # Get raw model scores (already -Inf for impossible responses)
  base_scores <- tr[[mod_name]]

  # Apply temperature scaling
  logits <- base_scores / params$tau

  # Softmax (handles -Inf correctly)
  max_logit <- max(logits[is.finite(logits)], na.rm = TRUE)
  exp_logits <- exp(logits - max_logit)  # Numerical stability
  soft <- exp_logits / sum(exp_logits)

  # Epsilon mixing (uniform noise)
  params$epsilon * (1 / 8) + (1 - params$epsilon) * soft
}

# Negative log-likelihood function
ns_likelihood <- function(pars, df, mod_name, operative) {
  params <- operative$transform(pars)
  trials <- split(df, df$trial_id)

  nlls <- vapply(
    trials,
    function(tr) {
      mpred <- compute_ns_mpred(tr, mod_name, params)
      # n is the count for each response option
      -sum(log(pmax(mpred, 1e-10)) * tr$n)  # pmax for numerical stability
    },
    numeric(1)
  )
  sum(nlls)
}

# Get predictions for a fitted model
ns_prediction <- function(pars, df, mod_name, operative) {
  params <- operative$transform(pars)
  trials <- split(df, df$trial_id)

  preds <- lapply(
    names(trials),
    function(tr_id) {
      tr <- trials[[tr_id]]
      mpred <- compute_ns_mpred(tr, mod_name, params)

      data.frame(
        model = mod_name,
        trial_id = tr_id,
        node3 = tr$node3,
        predicted_prob = mpred
      )
    }
  )
  do.call(rbind, preds)
}

# Optimization function
ns_optimize <- function(model_names, df, operative, initial_values = rep(0, operative$n_params)) {
  # Optimize each model
  fits <- lapply(model_names, function(mod_name) {
    tryCatch(
      optim(
        par = initial_values,
        fn = ns_likelihood,
        df = df,
        mod_name = mod_name,
        operative = operative
      ),
      error = function(e) {
        message("Error optimizing ", mod_name, ": ", e$message)
        list(par = rep(NA, operative$n_params), value = NA)
      }
    )
  })
  names(fits) <- model_names

  # Build results table
  tau <- sapply(fits, function(x) exp(x$par[1]))
  epsilon <- sapply(fits, function(x) plogis(x$par[2]))
  logl <- -sapply(fits, function(x) x$value)

  n_obs <- sum(df$n)
  n_par <- operative$n_params

  model_fits <- data.frame(
    model = model_names,
    tau = tau,
    epsilon = epsilon,
    logl = logl,
    BIC = -2 * logl + n_par * log(n_obs),
    AIC = -2 * logl + 2 * n_par
  )

  # Build predictions
  predictions <- do.call(
    rbind,
    lapply(model_names, function(mod_name) {
      pars <- fits[[mod_name]]$par
      if (!any(is.na(pars))) {
        ns_prediction(pars, df, mod_name, operative)
      }
    })
  )

  list(
    model_fits = model_fits,
    predictions = predictions,
    raw_fits = fits
  )
}

# -------------- 1. Aggregate-level fitting --------------

cat("\n--- Aggregate Model Fitting ---\n")

# Fit all N/S models on aggregate data
agg_results <- ns_optimize(ns_model_names, df_ns, NS_TRANSFORM)

cat("\nModel fits (aggregate):\n")
print(agg_results$model_fits |>
        arrange(BIC) |>
        mutate(
          tau = round(tau, 3),
          epsilon = round(epsilon, 3),
          logl = round(logl, 2),
          BIC = round(BIC, 2),
          AIC = round(AIC, 2)
        ))

# -------------- 2. Add baseline model for comparison --------------

# Baseline: uniform random selection (no parameters)
baseline_logl <- sum(log(1/8) * df_ns$n)
n_obs <- sum(df_ns$n)

baseline_fit <- data.frame(
  model = "baseline",
  tau = NA,
  epsilon = NA,
  logl = baseline_logl,
  BIC = -2 * baseline_logl,  # 0 parameters
  AIC = -2 * baseline_logl
)

# Combine with N/S results
all_fits <- rbind(agg_results$model_fits, baseline_fit)

cat("\n--- All Models (including baseline) ---\n")
print(all_fits |>
        arrange(BIC) |>
        mutate(
          tau = round(tau, 3),
          epsilon = round(epsilon, 3),
          logl = round(logl, 2),
          BIC = round(BIC, 2),
          AIC = round(AIC, 2)
        ))

# -------------- 3. Individual participant fitting --------------

cat("\n--- Individual Participant Fitting ---\n")

# Prepare participant data for individual fitting
byppt <- data |>
  select(subject_id, trial_id, node3)

data_list <- split(byppt, byppt$subject_id)

# Merge N/S model data with each participant's responses
ppt_merges_ns <- lapply(data_list, function(ppt_data) {
  merged <- df_ns |>
    left_join(ppt_data, by = c('trial_id', 'node3')) |>
    mutate(n = ifelse(!is.na(subject_id), 1, 0)) |>
    select(-subject_id)
  merged
})

# Fit models for each participant (this may take a while)
cat("Fitting models for", length(ppt_merges_ns), "participants...\n")

ppt_results_ns <- lapply(seq_along(ppt_merges_ns), function(i) {
  if (i %% 50 == 0) cat("  Participant", i, "\n")
  ns_optimize(ns_model_names, ppt_merges_ns[[i]], NS_TRANSFORM)
})
names(ppt_results_ns) <- names(ppt_merges_ns)

cat("Done fitting individual participants.\n")

# -------------- 4. Summarize individual fits --------------

# Extract best model for each participant
best_models <- sapply(ppt_results_ns, function(res) {
  fits <- res$model_fits
  fits$model[which.min(fits$BIC)]
})

cat("\n--- Best Model Distribution (Individual BIC) ---\n")
print(table(best_models))

# Extract BIC for each model and participant
all_bics <- do.call(rbind, lapply(names(ppt_results_ns), function(subj) {
  fits <- ppt_results_ns[[subj]]$model_fits
  data.frame(
    subject_id = subj,
    model = fits$model,
    BIC = fits$BIC,
    logl = fits$logl
  )
}))

# Summary statistics
bic_summary <- all_bics |>
  group_by(model) |>
  summarise(
    mean_BIC = mean(BIC),
    sd_BIC = sd(BIC),
    mean_logl = mean(logl),
    n_best = sum(BIC == min(BIC)),
    .groups = "drop"
  ) |>
  arrange(mean_BIC)

cat("\n--- BIC Summary Across Participants ---\n")
print(bic_summary |>
        mutate(
          mean_BIC = round(mean_BIC, 2),
          sd_BIC = round(sd_BIC, 2),
          mean_logl = round(mean_logl, 2)
        ))

# -------------- 5. Save results --------------

save(
  agg_results,
  all_fits,
  ppt_results_ns,
  all_bics,
  bic_summary,
  file = here('Data', 'modelData', 'nsFits.rda')
)
cat("\nSaved results to Data/modelData/nsFits.rda\n")

# Save summary as CSV
write.csv(all_fits, file = here('Data', 'modelData', 'nsAggregateFits.csv'), row.names = FALSE)
write.csv(bic_summary, file = here('Data', 'modelData', 'nsBICSummary.csv'), row.names = FALSE)
cat("Saved CSV summaries.\n")

# -------------- 6. Quick model comparison visualization --------------

cat("\n=== Final Summary ===\n")
cat("N/S models fitted successfully.\n")
cat("Best aggregate model:", all_fits$model[which.min(all_fits$BIC)], "\n")
cat("Best individual model (most wins):", names(which.max(table(best_models))), "\n")
