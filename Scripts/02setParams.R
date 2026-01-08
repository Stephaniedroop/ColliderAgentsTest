###############################################################
###### Collider - save parameters as probability vectors  #####
###############################################################

library(here)

# All the params we want, put into a list of 4x2 dfs
params1 <- data.frame("0" = c(0.9, 0.5, 0.2, 0.5), "1" = c(0.1, 0.5, 0.8, 0.5))
params2 <- data.frame("0" = c(0.5, 0.9, 0.5, 0.2), "1" = c(0.5, 0.1, 0.5, 0.8))
params3 <- data.frame("0" = c(0.9, 0.3, 0.2, 0.5), "1" = c(0.1, 0.7, 0.8, 0.5))
row.names(params1) <- row.names(params2) <- row.names(params3) <- c(
  "pA",
  "peA",
  "pB",
  "peB"
)
names(params1) <- names(params2) <- names(params3) <- c('0', '1')
poss_params <- list(params1, params2, params3)
save(poss_params, file = here('Data', 'modelData', 'params.rdata'))
