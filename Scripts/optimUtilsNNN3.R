#=========================================================
#  Optimisation functions
# =========================================================

# Static functions used by the optimisation script

# TWO SOFTMAX VERSION

# A small function to strip the trailing 0s for presentation. Needs as.character as input. Likely to use elsewhere?
trim_zeros <- function(x) sub("^(-?)0\\.", ".", sprintf("%s", as.numeric(x)))

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

# =======================================
# OPERATIVE DEFINITIONS of 'operative part' (what differs between optimisation functions for kindness v no kindness - what used to be done using different series of functions)
# =======================================

# WITHOUT kappa for models with noKind
NO_KAPPA <- list(
  name = "no_kappa",
  n_params = 2,
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]), # constrained to between 0,1
      kappa = NULL
    )
  }
  # logits = function(df, mod_name) {
  #   # Just the model column
  #   df[[mod_name]]
  # }
)

# WITH kappa for kindness - full model
WITH_KAPPA <- list(
  name = "with_kappa",
  n_params = 3,
  transform = function(pars) {
    list(
      tau1 = exp(pars[1]),
      epsilon = plogis(pars[2]),
      kappa = exp(pars[3])
    )
  }
  # logits = function(df, mod_name) {
  #   # Model column plus kappa * ig - this is the part that's extra from the 2-par version and which models the contribution of Kindness
  #   df[[mod_name]] + df$ig
  # }
)

# ==========================
# MATHS CORE OF LIKELIHOOD AND PREDICTION
# ==========================

compute_mpred <- function(tr, mod_name, params) {
  # Get which raw model numbers are meaningful for this trial and model (not -Inf)
  base_logits <- tr[[mod_name]]

  # Bring kappa in if it's there
  if (!is.null(params$kappa)) {
    idx <- is.finite(base_logits)
    base_logits[idx] <- base_logits[idx] + params$kappa * tr$ig[idx]
  }

  logits <- base_logits / params$tau1
  soft <- exp(logits) / sum(exp(logits))

  # Epsilon mixing
  params$epsilon * (1 / 8) + (1 - params$epsilon) * soft
}

# =======================================
#  VECTORIZED LIKELIHOOD FUNCTION
# =======================================

get_likelihood <- function(pars, df, mod_name, operative) {
  # -----------------------------
  # 1. Get pars and data in right format
  # -----------------------------

  # Transform raw parameters into usable values
  params <- operative$transform(pars)
  # Split into named list of trials using split (keeps names)
  trials <- split(df, df$trial_id)

  # -----------------------------
  # 2. Compute negative log-likelihood
  # -----------------------------

  nlls <- vapply(
    # like sapply but better for number vectors
    trials,
    function(tr) {
      # Call the function above to transform with softmaxes and apply kappa and choice
      mpred <- compute_mpred(tr, mod_name, params)
      # Get NLL for this trial. n is number of people who chose each option
      -sum(log(mpred) * tr$n)
    },
    numeric(1) # vapply needs specified output and will error if output is not this
  )
  sum(nlls)
}


# =======================================
# PREDICTION FUNCTION - follows same format as likelihood, maybe later we can repeat a module across both
# =======================================

get_prediction <- function(pars, df, mod_name, operative) {
  # Transform raw parameters into usable values
  params <- operative$transform(pars)
  # Split df into trials
  trials <- split(df, df$trial_id)

  # Although NLL used vapply, here lapply is better to give
  preds <- lapply(
    names(trials),
    function(tr_id) {
      tr <- trials[[tr_id]]
      # Call softmax etc from above like in NLLs
      mpred <- compute_mpred(tr, mod_name, params)

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

# =======================================
#  MODEL FIT
# =======================================

get_optimisation <- function(
  model_names,
  df,
  operative,
  initial_values = rep(1, operative$n_params)
) {
  # ---------------------------
  # A. Optimize each model
  # ---------------------------
  fits <- lapply(model_names, function(mod_name) {
    tryCatch(
      optim(
        par = initial_values,
        fn = get_likelihood,
        df = df,
        mod_name = mod_name,
        operative = operative
      ),
      error = function(e) {
        message("Error in optimization for model ", mod_name, ": ", e$message)
        list(par = rep(NA, operative$n_params), value = NA)
      }
    )
  })
  names(fits) <- model_names

  # ---------------------------
  # B. Build model_fits table (perhaps this is not most efficient structure but let's keep it to fit later analysis)
  # ---------------------------
  tau1 <- sapply(fits, function(x) exp(x$par[1]))
  epsilon <- sapply(fits, function(x) plogis(x$par[2]))
  #tau2 <- sapply(fits, function(x) exp(x$par[3]))
  logl <- -sapply(fits, function(x) x$value)

  model_fits <- data.frame(
    model = model_names,
    tau1 = tau1,
    epsilon = epsilon,
    #tau2 = tau2,
    logl = logl
  )

  # Add kappa only if 4-parameter model
  if (operative$n_params == 3) {
    model_fits$kappa <- sapply(fits, function(x) exp(x$par[3]))
    n_par <- 3
  } else {
    n_par <- 2
  }

  # Calculate BIC
  model_fits$BIC <- -2 * model_fits$logl + n_par * log(sum(df$n))

  # Format model_fits
  model_fits <- model_fits |>
    mutate(
      tau1 = format(tau1, digits = 3),
      epsilon = format(epsilon, digits = 3),
      #tau2 = format(tau2, digits = 3),
      logl = format(logl, digits = 4),
      BIC = format(BIC, digits = 4)
    )

  if (operative$n_params == 3) {
    model_fits <- model_fits |>
      mutate(kappa = format(kappa, digits = 3))
  }

  # ---------------------------
  # C. Build predictions table: a prediction for each variable in each condition for each model
  # ---------------------------
  predictions <- do.call(
    rbind,
    lapply(model_names, function(mod_name) {
      pars <- fits[[mod_name]]$par
      if (!any(is.na(pars))) {
        get_prediction(
          pars = pars,
          df = df,
          mod_name = mod_name,
          operative = operative
        )
      }
    })
  )

  # ---------------------------
  # D. Return
  # ---------------------------

  list(
    model_fits = model_fits,
    predictions = predictions
  )
}
