##################################################################
######## Get lesions and combine with data ############

library(tidyverse)
library(here)

# Script takes the processed data from the collider ppt expt (`DATA.RDATA`)
# and combines it with the pre-processed model predictions to get all the other model modules and lesions

load(here('Data', 'modelData', 'modelproc.rda')) # loads mp 576 obs of 23
load(here('Data', 'Data.rdata')) # loads data 2580 obs of 23

# Models with the Actual causation module
# Allocate a cause as Actual when it fulfils either of two conditions:
#  1) it equals the Effect
#  2) unobserved variable can only follow main variable

# Condition 1
mp <- mp |> #
  mutate(
    Actual = case_when(
      node2 == 'A' ~ A == E.x,
      node2 == 'B' ~ B == E.x,
      node2 == 'Au' ~ Au == E.x,
      node2 == 'Bu' ~ Bu == E.x
    )
  )

# Condition 2 - many of these are already caught but just to catch the extras
mp$Actual[mp$A == '0' & mp$node3 == 'Au=1'] <- FALSE
mp$Actual[mp$B == '0' & mp$node3 == 'Bu=1'] <- FALSE

mp <- mp |>
  mutate(cesmActual = cesm * Actual)

# --------- Full model -------------

# Posterior here means for the two unobserved vars, which is shorthand for posterior(Au=1|Bu=0) + posterior(Au=1|Bu=1) * Pr_Au=1

# [CES_{Au=1|Bu=1}*P(Bu=1|Au=1) + CES_{Au=1|Bu=0}P(Bu=0|Au=1)] * P(Au=1)
# or [CES_{Au=1|Bu=1}*P(Bu=1 & Au=1) + CES_{Au=1|Bu=0}P(Bu=0 & Au=1)]
full <- mp |>
  group_by(pgroup, trialtype, node3, .drop = F) |>
  summarise(full = sum(cesmActual * posterior))

noAct <- mp |> #2
  group_by(pgroup, trialtype, node3, .drop = F) |>
  summarise(noAct = sum(cesm * posterior))

# Test noSelect
noSelect_test <- mp |>
  group_by(pgroup, trialtype, node3, .drop = F) |>
  summarise(noSelect_test = sum(Actual * posterior))


# -------- Next, lesion Inference  ------------

# Uses the cesm after treatment for Actuality, but then uses prior of unobserved variables rather than posterior
noInf <- mp |> #3
  group_by(pgroup, trialtype, node3, .drop = F) |>
  summarise(noInf = sum(cesmActual * PrUn))

# Uses plain cesm before treatment for Actual, and prior of unobserved variables rather than posterior
noActnoInf <- mp |> #6
  group_by(pgroup, trialtype, node3, .drop = F) |>
  summarise(noActnoInf = sum(cesm * PrUn))

#--------- Lesioning causal selection - noSelect -------------

# ------- Brief diversion to get the individual posteriors for info gain -----------
# Get the individual posterior, for example:
# For Au, keep Au fixed and sum the joint posterior for each possible value of Bu.

getpost <- mp |>
  filter(!node2 %in% c('A', 'B')) |>
  group_by(pgroup, trialtype, node3, .drop = F) |> # if we need Eig then go back and group by node2
  summarise(post = sum(posterior), prior = sum(PrUn))

# Simple ig of each pair of unobserved vars
unobs_ig <- getpost |>
  group_by(pgroup, trialtype, node3) |> # if we need eig then go back and group by node2
  summarise(
    prior_entropy = round(-sum(prior * log2(prior + 1e-10)), 3),
    post_entropy = round(-sum(post * log2(post + 1e-10)), 3),
    ig = round(prior_entropy - post_entropy, 3)
  ) |>
  ungroup()

# Merge only column ig of unobs_ig with mp to get postig
ig <- unobs_ig |>
  select(pgroup, trialtype, node3, ig)

postigi <- merge(
  mp,
  getpost,
  by = c('pgroup', 'trialtype', 'node3'),
  all.x = TRUE
)

postig <- merge(
  postigi,
  ig,
  by = c('pgroup', 'trialtype', 'node3'),
  all.x = TRUE
)


# An intermediate measure: For A and B, gives 1 when E matches, 0 if not. This sets B to 1 for actual cause, eg. if B=0 when E=0
postig <- postig |>
  mutate(
    Act1 = case_when(
      node2 == 'A' ~ Actual,
      node2 == 'B' ~ Actual,
      node2 == 'Au' ~ post,
      node2 == 'Bu' ~ post
    )
  )

postig <- postig |>
  mutate(
    noSelect = case_when(
      node2 == 'A' ~ Actual,
      node2 == 'B' ~ Actual,
      node2 == 'Au' ~ post * Actual,
      node2 == 'Bu' ~ post * Actual
    )
  )


noSelect <- postig |>
  group_by(pgroup, trialtype, node3, .drop = F) |>
  summarise(noSelect = mean(noSelect)) # These are meant to be mean, even though full is sum, because the same value is listed several times, one for each combination of unobs vars

noActnoSelect <- postig |>
  group_by(pgroup, trialtype, node3, .drop = F) |>
  summarise(noActnoSelect = mean(Act1))

# Get full and noAct using individual posteriors

# postig <- postig |>
#   mutate(
#     full = case_when(
#       node2 == 'A' ~ cesmActual * posterior, # when it is observed you do definitely need to sum for the full model, because it is made of weighted averages
#       node2 == 'B' ~ cesmActual * posterior,
#       node2 == 'Au' ~ cesmActual * post,
#       node2 == 'Bu' ~ cesmActual * post
#     )
#   )

# full <- postig |>
#   group_by(pgroup, trialtype, node3, .drop = F) |>
#   summarise(full = sum(full)) # Yes this should be sum because the different u values have different cesm values and they all combine

# postig <- postig |>
#   mutate(
#     noAct = case_when(
#       node2 == 'A' ~ cesm * posterior, # also sum
#       node2 == 'B' ~ cesm * posterior,
#       node2 == 'Au' ~ cesm * post,
#       node2 == 'Bu' ~ cesm * post
#     )
#   )

# noAct <- postig |> #2
#   group_by(pgroup, trialtype, node3, .drop = F) |>
#   summarise(noAct = sum(noAct)) # Likewise should be sum yes because the different u values have different cesm values and they all combine

#------- Lesion both inference and selection ---------
# As before, but it's the prior instead of posterior

mp <- mp |>
  mutate(
    Act3 = case_when(
      node2 == 'A' ~ Actual,
      node2 == 'B' ~ Actual,
      node2 == 'Au' ~ peA * Actual,
      node2 == 'Bu' ~ peB * Actual
    )
  )

noInfnoSelect <- mp |>
  group_by(pgroup, trialtype, node3, .drop = F) |>
  summarise(noInfnoSelect = mean(Act3))

#-------- Lesion everything ----------
# Assign causal score of C=1 to observed variables, and prior to unobserved variables.

mp <- mp |> #
  mutate(
    Act2 = case_when(
      node2 == 'A' ~ Actual,
      node2 == 'B' ~ Actual,
      node2 == 'Au' ~ peA,
      node2 == 'Bu' ~ peB
    )
  )

noActnoInfnoSelect <- mp |> # no act here means for the unobserved variables
  group_by(pgroup, trialtype, node3, .drop = F) |>
  summarise(noActnoInfnoSelect = mean(Act2))


# ---------- Merge models back together --------------------

df_list <- list(
  full,
  noAct,
  noInf,
  noSelect,
  noActnoInf,
  noActnoSelect,
  noInfnoSelect,
  noActnoInfnoSelect
)

models <- df_list |>
  reduce(full_join, by = c('pgroup', 'trialtype', 'node3'))

# Get values of Actual for each combination of pgroup, trialtype, node3
Actual <- mp |>
  group_by(pgroup, trialtype, node3) |>
  summarise(Actual = first(Actual), .groups = "drop")

models2 <- merge(models, Actual, all.x = TRUE)

# ------------- 2. Summarise participant data in same format ---------------------

dataNorm <- data |>
  group_by(pgroup, trialtype, node3, .drop = FALSE) |>
  tally() |>
  mutate(prop = n / sum(n))

# Merge with data just because it's needed across all models so best do it once here
dataNorm <- merge(x = dataNorm, y = ig, by = c('pgroup', 'trialtype', 'node3')) #all.x = T)

# ----------- 3. The actual merge! ------------

modelAndData <- merge(x = dataNorm, y = models2)

modelAndData <- modelAndData |>
  unite('trial_id', pgroup, trialtype, sep = "_", remove = FALSE)

modelAndData$trial_id <- as.factor(modelAndData$trial_id)

modelAndData <- modelAndData |>
  group_by(trial_id) |>
  mutate(baseline = 1 / n())

# Actually let's call it df
df <- modelAndData

#-------- Create variables coding the actual observation -------------
# We had this before in data, but it wasn't right because of the incomplete counterbalancing issue
# We know trialtype is right, so let's recode
df.map <- data.frame(
  condition = c(
    'c1',
    'c2',
    'c3',
    'c4',
    'c5',
    'd1',
    'd2',
    'd3',
    'd4',
    'd5',
    'd6',
    'd7'
  ),
  A = c(0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1),
  B = c(0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1),
  E = c(0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1)
)

df <- df |>
  left_join(df.map, by = c("trialtype" = "condition"))

# Although we don't filter by Include because now we have the epsilon par, still nice to know how many ppts choose noise
df <- df |>
  mutate(
    Include = !((node3 == 'B=0' & B == 1) |
      (node3 == 'B=1' & B == 0) |
      (node3 == 'A=0' & A == 1) |
      (node3 == 'A=1' & A == 0))
  )

# Now is a good time to get some other variables
# Add a column called Observed, which is TRUE if they selected an observed variable (A or B) and FALSE if unobserved (Au or Bu)
df <- df |>
  mutate(
    Observed = if_else(node3 %in% c('A=0', 'A=1', 'B=0', 'B=1'), TRUE, FALSE)
  )

# Now change some values of Known from FALSE to TRUE,
# where the unobserved variable can be logically inferred from the situation
# That means:
# -- in c5 both Au=1 and Bu=1
# -- in d2 Bu=0
# -- in d3 Bu=1
# -- in d4 Au=0
# -- in d5 Au=1
# -- in d6 both Au=0 and Bu=0

# A new version of Known, like before except also has TRUE for any times node3 is A or B, plus the conditions before named, and otherwise false
df <- df |>
  mutate(
    Known = case_when(
      node3 %in% c('A=0', 'A=1', 'B=0', 'B=1') & Actual == TRUE ~ TRUE, #
      trialtype == 'c5' & node3 %in% c('Au=1', 'Bu=1') ~ TRUE,
      trialtype == 'd2' & node3 == 'Bu=0' ~ TRUE,
      trialtype == 'd3' & node3 == 'Bu=1' ~ TRUE,
      trialtype == 'd4' & node3 == 'Au=0' ~ TRUE,
      trialtype == 'd5' & node3 == 'Au=1' ~ TRUE,
      trialtype == 'd6' & node3 %in% c('Au=0', 'Bu=0') ~ TRUE,
      TRUE ~ FALSE
    )
  )


# In the softmax, 0 causal score will be given a relatively high score by the softmax. So give -Inf here
# These will then be given some small value by the epsilon noise part of the model prediction in the likelihood function
df[df$Include == FALSE, 8:15] <- -Inf

df$Actual[is.na(df$Actual)] <- FALSE
df[df$Actual == FALSE, c(8, 10, 11, 14)] <- -Inf # Make sure noInf is included
# Put basseline with models to include it in fits later
df <- df |>
  relocate(baseline, .before = Actual)

## ---------------------------------------------------------------------------------------------------------------
save(df, file = here('Data', 'modelData', 'modelAndDataUnfitig.rda'))
