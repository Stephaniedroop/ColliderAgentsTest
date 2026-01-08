###########################################################################
################### Necessity/Sufficiency Model Functions #################
###########################################################################
#
# Implementation of Icard, Kominsky & Knobe (2017) causal strength measure
# Based on: "Normality and actual causal strength", Cognition, 161, 80-93
#
# Key formula (Equation 1, p.15):
#   κ_P(X,Y) = P(X=0) · P^ν_{X=0}(Y=0) + P(X=1) · P^σ_{X=1}(Y=1)
#
# Where:
#   P^ν_{X=0}(Y=0) = Necessity strength (would E not occur if C didn't?)
#   P^σ_{X=1}(Y=1) = Sufficiency strength (would E occur if C did?)
#
###########################################################################

library(tidyverse)

# ------------- Compute Necessity Strength -----------------
# For a given cause C, compute: P(E=0 | do(C=0), other variables)
# In our collider structure, this depends on the causal structure and
# the values/distributions of other variables

compute_necessity <- function(cause, cause_value, A, Au, B, Bu, E, structure, params) {

  # Necessity asks: If C hadn't taken its actual value, would E not have occurred?
  # For C=1 causing E=1: Would E=0 if C had been 0?
  # For C=0 causing E=0: Would E=1 if C had been 1? (then necessity = 1 - that prob)

  # Get the counterfactual cause value
  cf_cause_value <- 1 - cause_value

  # Set up the counterfactual world
  cf_A <- A
  cf_Au <- Au
  cf_B <- B
  cf_Bu <- Bu

  # Intervene on the cause

  if (cause == "A") cf_A <- cf_cause_value
  if (cause == "Au") cf_Au <- cf_cause_value
  if (cause == "B") cf_B <- cf_cause_value
  if (cause == "Bu") cf_Bu <- cf_cause_value

  # For unobserved variables that are NOT the cause, we need to consider their distribution
  # But in the actual world, we may have evidence about them from E

  # Compute the counterfactual effect
  if (structure == "conjunctive") {
    cf_E <- as.numeric((cf_A & cf_Au) & (cf_B & cf_Bu))
  } else {
    cf_E <- as.numeric((cf_A & cf_Au) | (cf_B & cf_Bu))
  }

  # Necessity strength: did the effect change?
  # If actual E=1 and cf_E=0, the cause was necessary (return 1)
  # If actual E=0 and cf_E=1, the absence was necessary (return 1)
  # Otherwise return 0

  if (E == 1 && cause_value == 1) {
    # Cause occurred and effect occurred - was cause necessary?
    # Necessity = P(E'=0 | do(C=0))
    return(1 - cf_E)  # Returns 1 if cf_E=0 (cause was necessary)
  } else if (E == 0 && cause_value == 0) {
    # Cause didn't occur and effect didn't occur - was absence necessary?
    # Necessity = P(E'=1 | do(C=1))
    return(cf_E)  # Returns 1 if cf_E=1 (absence was necessary)
  } else {
    # Cause value doesn't match effect - this cause isn't "actual"
    return(0)
  }
}

# ------------- Compute Sufficiency Strength -----------------
# For a given cause C, compute: P(E=1 | do(C=1), other variables vary)
# Sufficiency asks: would C reliably produce E across different backgrounds?

compute_sufficiency <- function(cause, cause_value, A, Au, B, Bu, E, structure, params) {
  # Sufficiency asks: Given C took its value, would E have occurred

  # even if background conditions were different?

  # For sufficiency, we intervene to SET the cause to its actual value
  # and ask if the effect would occur

  # In a deterministic structure, sufficiency depends on the other variables
  # For the collider: E = f(A, Au, B, Bu)

  if (structure == "conjunctive") {
    # E = (A & Au) & (B & Bu)
    # Sufficiency of A=1: Does E=1? Only if Au=1 AND B=1 AND Bu=1
    # Sufficiency of Au=1: Does E=1? Only if A=1 AND B=1 AND Bu=1
    # etc.

    if (cause == "A" && cause_value == 1) {
      return(as.numeric(Au == 1 & B == 1 & Bu == 1))
    } else if (cause == "A" && cause_value == 0) {
      return(1)  # A=0 is always "sufficient" to prevent E in conjunctive
    } else if (cause == "Au" && cause_value == 1) {
      return(as.numeric(A == 1 & B == 1 & Bu == 1))
    } else if (cause == "Au" && cause_value == 0) {
      return(1)
    } else if (cause == "B" && cause_value == 1) {
      return(as.numeric(A == 1 & Au == 1 & Bu == 1))
    } else if (cause == "B" && cause_value == 0) {
      return(1)
    } else if (cause == "Bu" && cause_value == 1) {
      return(as.numeric(A == 1 & Au == 1 & B == 1))
    } else if (cause == "Bu" && cause_value == 0) {
      return(1)
    }
  } else {
    # Disjunctive: E = (A & Au) | (B & Bu)
    # Sufficiency of A=1: Does E=1? Yes if Au=1 (regardless of B, Bu)
    # Sufficiency of B=1: Does E=1? Yes if Bu=1 (regardless of A, Au)

    if (cause == "A" && cause_value == 1) {
      return(as.numeric(Au == 1))  # A=1 sufficient if Au=1
    } else if (cause == "A" && cause_value == 0) {
      return(as.numeric(!(B == 1 & Bu == 1)))  # A=0 sufficient to prevent if other path inactive
    } else if (cause == "Au" && cause_value == 1) {
      return(as.numeric(A == 1))
    } else if (cause == "Au" && cause_value == 0) {
      return(as.numeric(!(B == 1 & Bu == 1)))
    } else if (cause == "B" && cause_value == 1) {
      return(as.numeric(Bu == 1))
    } else if (cause == "B" && cause_value == 0) {
      return(as.numeric(!(A == 1 & Au == 1)))
    } else if (cause == "Bu" && cause_value == 1) {
      return(as.numeric(B == 1))
    } else if (cause == "Bu" && cause_value == 0) {
      return(as.numeric(!(A == 1 & Au == 1)))
    }
  }

  return(0)
}

# ------------- Compute Icard's κ measure -----------------
# κ_P(C,E) = P(C=0) · Necessity + P(C=1) · Sufficiency
# This is computed for a specific cause explaining a specific effect

compute_kappa <- function(cause, cause_value, A, Au, B, Bu, E, structure, params) {
  # Get the prior probability of the cause
  if (cause == "A") {
    p_cause_1 <- params["pA", "1"]
  } else if (cause == "Au") {
    p_cause_1 <- params["peA", "1"]
  } else if (cause == "B") {
    p_cause_1 <- params["pB", "1"]
  } else if (cause == "Bu") {
    p_cause_1 <- params["peB", "1"]
  }
  p_cause_0 <- 1 - p_cause_1

  # Compute necessity and sufficiency
  nec <- compute_necessity(cause, cause_value, A, Au, B, Bu, E, structure, params)
  suf <- compute_sufficiency(cause, cause_value, A, Au, B, Bu, E, structure, params)

  # Icard's formula: weight by prior probabilities
  # For cause_value=1: κ = P(C=0)·Necessity + P(C=1)·Sufficiency
  # For cause_value=0: κ = P(C=1)·Necessity + P(C=0)·Sufficiency

  if (cause_value == 1) {
    kappa <- p_cause_0 * nec + p_cause_1 * suf
  } else {
    kappa <- p_cause_1 * nec + p_cause_0 * suf
  }

  return(kappa)
}

# ------------- Compute NS scores for all response options in a world -----------------
# Given a world state (A, B, E, structure, params), compute scores for all 8 explanations

compute_ns_scores <- function(A, B, E, structure, params, Au_values = c(0, 1), Bu_values = c(0, 1)) {
  # For unobserved variables, we need to marginalize over their possible values
  # weighted by their posterior probability given the observed world

  # First compute the posterior over (Au, Bu) given (A, B, E)
  posteriors <- compute_posterior(A, B, E, structure, params)

  # Initialize scores for each response option
  scores <- data.frame(
    node3 = c("A=0", "A=1", "B=0", "B=1", "Au=0", "Au=1", "Bu=0", "Bu=1"),
    cause = c("A", "A", "B", "B", "Au", "Au", "Bu", "Bu"),
    cause_value = c(0, 1, 0, 1, 0, 1, 0, 1),
    kappa = NA_real_,
    necessity = NA_real_,
    sufficiency = NA_real_
  )

  # For each response option, compute expected kappa over posterior
  for (i in 1:nrow(scores)) {
    cause <- scores$cause[i]
    cause_value <- scores$cause_value[i]

    # Marginalize over unobserved variables
    total_kappa <- 0
    total_nec <- 0
    total_suf <- 0

    for (au in Au_values) {
      for (bu in Bu_values) {
        post_prob <- posteriors[posteriors$Au == au & posteriors$Bu == bu, "posterior"]
        if (length(post_prob) == 0 || is.na(post_prob)) post_prob <- 0

        k <- compute_kappa(cause, cause_value, A, au, B, bu, E, structure, params)
        n <- compute_necessity(cause, cause_value, A, au, B, bu, E, structure, params)
        s <- compute_sufficiency(cause, cause_value, A, au, B, bu, E, structure, params)

        total_kappa <- total_kappa + post_prob * k
        total_nec <- total_nec + post_prob * n
        total_suf <- total_suf + post_prob * s
      }
    }

    scores$kappa[i] <- total_kappa
    scores$necessity[i] <- total_nec
    scores$sufficiency[i] <- total_suf
  }

  return(scores)
}

# ------------- Compute posterior over unobserved variables -----------------
# P(Au, Bu | A, B, E, structure)

compute_posterior <- function(A, B, E, structure, params) {
  # Get priors
  p_Au_1 <- params["peA", "1"]
  p_Bu_1 <- params["peB", "1"]

  # Create all combinations
  grid <- expand.grid(Au = c(0, 1), Bu = c(0, 1))
  grid$prior <- NA
  grid$likelihood <- NA
  grid$posterior <- NA

  for (i in 1:nrow(grid)) {
    au <- grid$Au[i]
    bu <- grid$Bu[i]

    # Prior P(Au, Bu)
    p_au <- ifelse(au == 1, p_Au_1, 1 - p_Au_1)
    p_bu <- ifelse(bu == 1, p_Bu_1, 1 - p_Bu_1)
    grid$prior[i] <- p_au * p_bu

    # Likelihood P(E | A, Au, B, Bu, structure)
    if (structure == "conjunctive") {
      predicted_E <- as.numeric((A & au) & (B & bu))
    } else {
      predicted_E <- as.numeric((A & au) | (B & bu))
    }

    # Deterministic: likelihood is 1 if predicted matches actual, 0 otherwise
    grid$likelihood[i] <- as.numeric(predicted_E == E)
  }

  # Posterior = prior * likelihood / normalizing constant
  grid$unnorm_post <- grid$prior * grid$likelihood
  total <- sum(grid$unnorm_post)

  if (total > 0) {
    grid$posterior <- grid$unnorm_post / total
  } else {
    # This shouldn't happen if the world is consistent
    grid$posterior <- 0
  }

  return(grid)
}

# ------------- Main function to get NS predictions for all worlds -----------------

get_ns_predictions <- function(params, structure, trial_info) {
  # trial_info should contain: A, B, E values for this trial
  A <- trial_info$A
  B <- trial_info$B
  E <- trial_info$E

  # Compute scores
  scores <- compute_ns_scores(A, B, E, structure, params)

  # Add trial info
  scores$A <- A
  scores$B <- B
  scores$E <- E
  scores$structure <- structure

  return(scores)
}

# ------------- Alternative combination methods -----------------
# These allow testing different ways of combining N and S

compute_ns_product <- function(necessity, sufficiency) {
  return(necessity * sufficiency)
}

compute_ns_average <- function(necessity, sufficiency) {
  return((necessity + sufficiency) / 2)
}

compute_ns_weighted <- function(necessity, sufficiency, w_n = 0.5, w_s = 0.5) {
  return(w_n * necessity + w_s * sufficiency)
}
