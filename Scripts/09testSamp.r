############################################################################################
########### Statistical tests on forced model samples matched to participant rows ##########
###############################################################################################

library(tidyverse)
library(here)
library(lme4)
library(lmerTest)

# Test the matched sampled explanations against participants for our theory metrics:
# - OBSERVED
# - ACTUAL
# - KNOWN
# - VARIABLE?

# Load
load(here('Data', 'modelData', 'matchedBypptf_ig.rda')) # loads merged 5160 of 14

# Reference level is the MODEL and participants are compared to that.
# For each measure, the reference level is FALSE and TRUE is compared to that.

# In ig, participants select Observed more often than model, and Known more often than model

# The tag 'Known' should no longer apply to Observed vars.
# If node3 starts with A= or B= then put FALSE in the column Known
vals <- c("A=0", "A=1", "B=0", "B=1")

# If node3 %in% vals, then Known = FALSE
merged$Known[merged$Response %in% vals] <- FALSE


# -------------- ig -----------------

# chisq for just ppt answer proportions, are they different from expected? Total 5160 NO - THIS IS THE PPT AND MODEL MERGED SO YOU CANT DO THIS

countObs <- merged |> # F: 2995, T: 2165, answer F 58.0% of the time
  filter(Respondent == "node3") |>
  group_by(Observed) |>
  summarise(n = n())

chisq.test(countObs$n, p = c(0.5, 0.5)) # X-squared 133.51, df 1, p < 2.2e-16 ***
#
# countAct <- merged |> # F: 1186, T: 3974, answer T 77.0% of the time
#   group_by(Actual) |>
#   summarise(n = n())
#
# chisq.test(countAct$n, p = c(180 / 288, 108 / 288)) # 3437.7, 1, p < 2.2e-16 ***
#
# countKnown <- merged |> # F: 2247, T: 2913, answer T 56.5% of the time
#   filter(Respondent == "node3") |>
#   group_by(Known) |>
#   summarise(n = n())
#
# chisq.test(countKnown$n, p = ???????) # 3285.3, df = 1, p-value < 2.2e-16 *** c(222 / 288, 66 / 288)

# ------------- Test for OBSERVED ig  --------------
# It shows participants select OBSERVED more often than predicted by the model.
# This is reported in the section `Unobserved vs observed variables` section.

# Respondent is model 0; ppt 1. Observed is F/T
predObs <- glmer(
  Observed ~ Respondent + (1 | subject_id) + (1 | trial_id),
  data = merged,
  family = binomial(link = 'logit')
)

summary(predObs)

coef <- fixef(predObs) # .251

est <- exp(coef) # 1.285 - exp converts logodds to odds.

se <- sqrt(diag(vcov(predObs)))

lower_logodds <- coef - (1.96 * se)
upper_logodds <- coef + (1.96 * se)

lower_or <- exp(lower_logodds) # 1.12
upper_or <- exp(upper_logodds) # 1.446

# z 4.179 p 2.93e-05 ***

# What if we split by group?
# Actually unlikely to matter

# ------------- Test for ACTUAL ig  --------------
# Participants select Actual causes more often than the model

predAct <- glmer(
  Actual ~ Respondent + (1 | subject_id) + (1 | trial_id),
  data = merged,
  family = binomial(link = 'logit')
)

summary(predAct)

coef <- fixef(predAct) # -.0714

est <- exp(coef) # .931 exp converts logodds to odds.

se <- sqrt(diag(vcov(predAct))) # .068

lower_logodds <- coef - (1.96 * se)
upper_logodds <- coef + (1.96 * se)

lower_or <- exp(lower_logodds) # .814
upper_or <- exp(upper_logodds) # 1.06

# z -1.045 p .296 ns

# ------------ Test for KNOWN ig -------------

# Participants select Known causes more often than the model in the ig one, because known is not modelled

predKn <- glmer(
  Known ~ Respondent + (1 | subject_id) + (1 | trial_id),
  data = merged,
  family = binomial(link = 'logit')
)

summary(predKn)

coef <- fixef(predKn) # .292

est <- exp(coef) # 1.34 - exp converts logodds to odds.

se <- sqrt(diag(vcov(predKn)))

lower_logodds <- coef - (1.96 * se)
upper_logodds <- coef + (1.96 * se)

lower_or <- exp(lower_logodds) # 1.182
upper_or <- exp(upper_logodds) # 1.52

# z 4.579  p 4.68e-06 ***
