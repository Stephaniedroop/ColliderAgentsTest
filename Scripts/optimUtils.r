##################################################################
######## Optimisation functions ##################################

# Static functions used by the optimisation script

# A small function to strip the trailing 0s for presentation. Needs as.character as input. Likely to use elsewhere?
trim_zeros <- function(x) sub("^(-?)0\\.", ".", sprintf("%s", as.numeric(x)))


# This page has several series of 3 functions. Each series is for a different version of the model fitting/optimization.
# Each is long and complex but follows the same structure of: 1) optimise models, 2) likelihood, 3) generate predictions.
# The first series, for kappa, has a list first to vary what kappa acts on (ig vs Known). Then the structure goes as normal.
# Then a version for the 2-par models without kappa.

# -------------- KAPPA series -----------------
# All the different ways of combining information gain and Known variables into a distance measure for kappa to scale
operatives_list <- list(
  "ig" = list(
    dist = function(df, tt, i) {
      df$ig[df$trial_id == tt[i]]
    },
    kappa_contrib = function(kappa, dist) {
      kappa * dist
    }
  ),
  "known" = list(
    dist = function(df, tt, i) {
      df$Known[df$trial_id == tt[i]]
    },
    kappa_contrib = function(kappa, dist) {
      kappa * dist
    }
  )
)

optimize_models <- function(
  model_names,
  df,
  operatives_list,
  initial_values = c(1, 1, 1)
) {
  # Get all combinations
  combo_grid <- expand.grid(
    mod_name = model_names,
    operatives_name = names(operatives_list),
    stringsAsFactors = FALSE
  )

  # Optimization for each combo
  out <- apply(combo_grid, 1, function(row) {
    mod_name <- row["mod_name"]
    operatives_name <- row["operatives_name"]
    operatives <- operatives_list[[operatives_name]]

    result <- tryCatch(
      {
        optim(
          par = initial_values,
          fn = model_likelihood,
          df = df,
          mod_name = mod_name,
          operatives = operatives
        )
      },
      error = function(e) {
        message(
          "Error in optimization for model ",
          mod_name,
          " + ",
          operatives_name,
          ": ",
          e$message
        )
        return(list(par = c(NA, NA, NA), value = NA))
      }
    )

    # Return results with identifiers
    list(
      mod_name = mod_name,
      operatives_name = operatives_name,
      result = result
    )
  })

  # Extract parameters into dataframe
  mfs <- do.call(
    rbind,
    lapply(out, function(x) {
      par <- x$result$par
      logl <- -x$result$value
      data.frame(
        model = x$mod_name,
        operatives = x$operatives_name,
        epsilon = plogis(par[1]),
        tau = exp(par[2]),
        kappa = exp(par[3]),
        logl = logl,
        BIC = -2 * logl + 3 * log(sum(df$n))
      )
    })
  )

  # Predictions
  predictions <- do.call(
    rbind,
    lapply(out, function(x) {
      if (!any(is.na(x$result$par))) {
        generate_predictions(
          mod_name = x$mod_name,
          df = df,
          pars = x$result$par,
          operatives = operatives_list[[x$operatives_name]]
        ) |>
          dplyr::mutate(operatives = x$operatives_name)
      }
    })
  )

  # Format and return
  list(
    model_fits = mfs |>
      dplyr::mutate(
        epsilon = format(epsilon, digits = 3),
        tau = format(tau, digits = 3),
        kappa = format(kappa, digits = 3),
        logl = format(logl, digits = 4),
        BIC = format(BIC, digits = 4)
      ),
    predictions = predictions
  )
}


# Function to get the model likelihood. The NEW, OPERATIVES, THREE PAR version
model_likelihood <- function(pars, df, mod_name, operatives) {
  epsilon <- plogis(pars[1])
  tau <- exp(pars[2])
  kappa <- exp(pars[3])
  tt <- unique(df$trial_id)

  nlls <- rep(NA, length(tt))
  for (i in 1:length(tt)) {
    n <- df$n[df$trial_id == tt[i]]
    mod_raw <- df[[mod_name]][df$trial_id == tt[i]]

    # Dynamically compute dist and kappa_contrib
    dist <- operatives$dist(df, tt, i)
    kappa_contrib <- operatives$kappa_contrib(kappa, dist)

    # Model predictions
    mpred <- epsilon *
      (1 / 8) +
      (1 - epsilon) *
        (exp((mod_raw + kappa_contrib) / tau) /
          sum(exp((mod_raw + kappa_contrib) / tau)))
    nlls[i] <- -sum(log(mpred) * n) # Get likelihood for this trial
  }

  sum(nlls) # Return the total likelihood
}

generate_predictions <- function(mod_name, df, pars, operatives) {
  epsilon <- plogis(pars[1]) # exp(pars[1])/(1+exp(pars[1])) #
  tau <- exp(pars[2]) #exp(pars[2])/(1+exp(pars[2])) #
  kappa <- exp(pars[3])
  tt <- unique(df$trial_id)

  do.call(
    rbind,
    lapply(tt, function(t_id) {
      trial_rows <- df$trial_id == t_id
      mod_raw <- df[[mod_name]][trial_rows]

      # Dynamically compute dist and kappa_contrib
      dist <- operatives$dist(df, tt = tt, i = which(tt == t_id)) # Ensure consistent indexing
      kappa_contrib <- operatives$kappa_contrib(kappa, dist)

      # Model predictions
      mpred <- epsilon *
        (1 / 8) +
        (1 - epsilon) *
          (exp((mod_raw + kappa_contrib) / tau) /
            sum(exp((mod_raw + kappa_contrib) / tau)))

      data.frame(
        model = mod_name,
        trial_id = t_id,
        node3 = df$node3[trial_rows],
        predicted_prob = mpred
      )
    })
  )
}

# ------------------ 2. The two-par version ----------------------------
# Still in use for lesioned models without kappa

# Function to get the model likelihood.
model_likelihood2 <- function(pars, df, mod_name) {
  #
  epsilon <- plogis(pars[1]) # exp(pars[1])/(1+exp(pars[1])) #
  tau <- exp(pars[2]) # exp(pars[2])/(1+exp(pars[2])) #

  tt <- unique(df$trial_id)

  nlls <- rep(NA, length(tt))
  for (i in 1:length(tt)) {
    n <- df$n[df$trial_id == tt[i]]
    mod_raw <- df[[mod_name]][df$trial_id == tt[i]]

    mpred <- epsilon *
      (1 / 8) +
      (1 - epsilon) * (exp((mod_raw) / tau) / sum(exp((mod_raw) / tau)))

    nlls[i] <- -sum(log(mpred) * n) # Get likelihood for this trial
  }

  sum(nlls) # Return the total likelihood
}


generate_predictions2 <- function(mod_name, df, pars) {
  epsilon <- plogis(pars[1]) #exp(pars[1])/(1+exp(pars[1])) #
  tau <- exp(pars[2]) #exp(pars[2])/(1+exp(pars[2])) #
  #kappa <- exp(pars[3])
  tt <- unique(df$trial_id)

  do.call(
    rbind,
    lapply(tt, function(t_id) {
      trial_rows <- df$trial_id == t_id
      mod_raw <- df[[mod_name]][trial_rows]

      mpred <- epsilon *
        (1 / 8) +
        (1 - epsilon) * (exp((mod_raw) / tau) / sum(exp((mod_raw) / tau)))

      data.frame(
        model = mod_name,
        trial_id = t_id,
        node3 = df$node3[trial_rows],
        predicted_prob = mpred
      )
    })
  )
}


optimize_models2 <- function(model_names, df, initial_values = c(1, 1)) {
  optimize_single <- function(mod_name) {
    result <- tryCatch(
      {
        optim(
          par = initial_values,
          fn = model_likelihood2,
          df = df,
          mod_name = mod_name
        )
      },
      error = function(e) {
        message("Error in optimization for model ", mod_name, ": ", e$message)
        return(list(par = c(NA, NA), value = NA))
      }
    )
    return(result)
  }

  out <- lapply(model_names, optimize_single)
  names(out) <- model_names

  mfs <- data.frame(
    model = names(out),
    epsilon = plogis(sapply(out, function(x) x$par[1])), #plogis(logodd) gives prob
    tau = exp(sapply(out, function(x) x$par[2])),
    #kappa = exp(sapply(out, function(x) x$par[3])),
    logl = -sapply(out, function(x) x$value)
  ) |>
    mutate(BIC = -2 * logl + 2 * log(sum(df$n)))

  # To actually generate predictions
  predictions <- do.call(
    rbind,
    lapply(names(out), function(mod_name) {
      if (!any(is.na(out[[mod_name]]$par))) {
        generate_predictions2(
          mod_name = mod_name,
          df = df,
          pars = out[[mod_name]]$par
          # epsilon = plogis(sapply(out, function(x) x$par[1])),
          # tau = plogis(sapply(out, function(x) x$par[2])),
          # kappa = exp(sapply(out, function(x) x$par[3])),
        )
      }
    })
  )

  list(
    model_fits = mfs |>
      mutate(
        epsilon = format(epsilon, digits = 3),
        tau = format(tau, digits = 3),
        logl = format(logl, digits = 4),
        BIC = format(BIC, digits = 4)
      ),
    predictions = predictions
  )
}
