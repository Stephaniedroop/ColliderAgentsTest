# ==================================
# Test for abnormal inflation
# ==================================

library(tidyverse)
library(here)
library(glmer)
library(lmerTest)

set.seed(12)

load(here('Data', 'Data.Rdata')) # This is one big df, 'data', 2580, of 215 ppts

# Filter this down to the trialtypes d7 and c5 (which are the disjunctive and conjunctive ones where 'everything happened', 111).
# Tag this with whether or not they selected the lower-probability variable (which is A in each case).

dfab <- data |> # 92 obs of 22
  filter(
    pgroup == 1,
    trialtype %in% c('c5', 'd7'),
    node3 %in% c('A=1', 'B=1')
  ) |>
  ungroup()

# Variable P: allocate 1 when they choose the more ABnormal variable (same in both pgroup 1 and 3)
dfab <- dfab |>
  mutate(
    P = case_when(
      node3 == 'B=1' ~ 0,
      node3 == 'A=1' ~ 1
    )
  )

dfab <- dfab |>
  mutate(P = as.factor(P))

dfab <- dfab |>
  select(subject_id, structure, trial_id, P)


# Try a test

predP <- glmer(
  P ~ 1 + (1 | trial_id) + (1 | subject_id),
  data = dfab,
  family = binomial(link = 'logit')
) # Also tried with + (1|subject_id) but no difference in fit, has convergence error and looks overparametrised?
summary(predP)

coef <- fixef(predP) # .445

est <- exp(coef) # 1.56 - exp converts logodds to odds.
prob <- plogis(coef) # .61 - plogis is exp/(1+exp) and converts logodds to probs

se_intercept <- sqrt(diag(vcov(predP)))["(Intercept)"]

lower_logodds <- coef - (1.96 * se_intercept)
upper_logodds <- coef + (1.96 * se_intercept)

lower_or <- exp(lower_logodds) # .916
upper_or <- exp(upper_logodds) # 2.66

# z 1.64, p .102

# Actually as there is only one conjunctive trial type, and one disjunctive, those count as the fixed effect

predP2 <- glmer(
  P ~ structure + (1 | trial_id) + (1 | subject_id),
  data = dfab,
  family = binomial(link = 'logit')
) # Also tried with + (1|subject_id) but no difference in fit, has convergence error and looks overparametrised?
summary(predP2)

coef <- fixef(predP2) # -.00218

est <- exp(coef) # .998
prob <- plogis(coef) #

#se_intercept <- sqrt(diag(vcov(predP)))["(Intercept)"]
se <- sqrt(diag(vcov(predP2)))

lower_logodds <- coef - (1.96 * se)
upper_logodds <- coef + (1.96 * se)

lower_or <- exp(lower_logodds) # .337
upper_or <- exp(upper_logodds) # 2.95

# z -.004, p .901

## A new version

# See plot 111abf in reportingFigs: instead of just comparing A and B, we compound whether the answer was associated with A or B.
# That puts the observed or unobserved together.
#We already made the plot for that, so now we need to analyse the data in the same way.

#So we need a 'normalised probability pair'? Or can we just add eg. .5 and .1, .5 and .8

dab <- data |> # 430 obs of 22
  filter(trialtype %in% c('c5', 'd7'), pgroup == 1)


# Sum and normalise, then A and Au together is the low probability option in every pgroup:

# First make the probs numeric not character
dab[, c("prob0", "prob1", "prob2", "prob3")] <- lapply(
  dab[, c("prob0", "prob1", "prob2", "prob3")],
  function(x) {
    as.numeric(sub("%", "", x)) / 100
  }
)

dab$probA <- (dab$prob0 + dab$prob1) /
  (dab$prob0 + dab$prob1 + dab$prob2 + dab$prob3)
dab$probB <- (dab$prob2 + dab$prob3) /
  (dab$prob0 + dab$prob1 + dab$prob2 + dab$prob3)


# Then give each a variable called P, like above:
dab$node3 <- as.character(dab$node3)

# Variable P: allocate 1 when they choose the more normal variable (same in both pgroup 1 and 3)
dab <- dab |>
  mutate(
    P = case_when(
      startsWith(node3, "B") ~ 1,
      startsWith(node3, "A") ~ 0
    )
  )

dab$P <- as.factor(dab$P) # where 0 is picks, the abnormal var, and 1 is picks B, the normal var

dab <- dab |>
  select(subject_id, structure, P)


predPa <- glmer(
  P ~ structure + (1 | subject_id),
  data = dab,
  family = binomial(link = 'logit')
) # Also tried with + (1|subject_id) but no difference in fit, has convergence error and looks overparametrised?
summary(predPa)

coef <- fixef(predPa) # 0.972

est <- exp(coef) # 2.643 exp converts logodds to odds.

se <- sqrt(diag(vcov(predPa))) # ["(Intercept)"] .371

lower_logodds <- coef - (1.96 * se)
upper_logodds <- coef + (1.96 * se)

lower_or <- exp(lower_logodds) # 1.22

upper_or <- exp(upper_logodds) # 1.70

# z2.619 , p .0088
