# ================================================================
# Do people answer in a patterned way - Do the conditions predict their answer?
# ================================================================

library(tidyverse)
library(here)
library(lme4)

# (THE MODELS ARENT USED HERE, JUST FOR HANDY GROUPED PPT VALUES SO DOESNT VARY WITH MODELS, DOESNT NEED OT BE THE LATEST)
load(here('Data', 'modelData', 'forregress.rda')) # 61968 of 5


# A big regression for Select (called n because I made this df from sparse model fitting data)
m1 <- glmer(
  n ~ pgroup + trialtype + node3 + (1 | subject_id),
  data = forregress,
  family = binomial
)
summary(m1) # basically for  pgroup3 (diff from 2)    estimate:  9.154e-02, se: 4.943e-02, Z value:  1.852, p 0.064044 .

# Neil says now do m2 without the factor and then compare with aov(m1,m2) to see if it needs the factor

m2 <- glmer(
  n ~ trialtype + node3 + (1 | subject_id),
  data = forregress,
  family = binomial
)
summary(m2)

aov(m1, m2) # doesnt like it

anova(m1, m2, test = "Chisq")
