############################################################################################
########### Processing cesm model predictions to be neater and get the other modules ##########
#########################################################################################

library(tidyverse)
library(here)

# Load fit16mp
load(here('Data', 'ModelData', 'fit4par.rda'))

df <- fitforplot # naming consistency sigh

# Wrangling and renaming of some factors and variables to make them individual per trial and useful for plotting
# Some vars may exist already in this new version so check first

df$structure <- ifelse(
  substr(df$trial_id, 3, 3) == "c",
  "conjunctive",
  ifelse(substr(df$trial_id, 3, 3) == "d", "disjunctive", NA)
)

df <- df |>
  mutate(across(where(is.character), as.factor))

df <- df |>
  mutate(across(where(is.logical), as.factor))


df$trial_type <- recode(
  df$trial_id,
  "1_c1" = "A=0,B=0,E=0",
  "1_c2" = "A=0,B=1,E=0",
  "1_c3" = "A=1,B=0,E=0",
  "1_c4" = "A=1,B=1,E=0",
  "1_c5" = "A=1,B=1,E=1",
  "1_d1" = "A=0,B=0,E=0",
  "1_d2" = "A=0,B=1,E=0",
  "1_d3" = "A=0,B=1,E=1",
  "1_d4" = "A=1,B=0,E=0",
  "1_d5" = "A=1,B=0,E=1",
  "1_d6" = "A=1,B=1,E=0",
  "1_d7" = "A=1,B=1,E=1",
  "2_c1" = "A=0,B=0,E=0",
  "2_c2" = "A=0,B=1,E=0",
  "2_c3" = "A=1,B=0,E=0",
  "2_c4" = "A=1,B=1,E=0",
  "2_c5" = "A=1,B=1,E=1",
  "2_d1" = "A=0,B=0,E=0",
  "2_d2" = "A=0,B=1,E=0",
  "2_d3" = "A=0,B=1,E=1",
  "2_d4" = "A=1,B=0,E=0",
  "2_d5" = "A=1,B=0,E=1",
  "2_d6" = "A=1,B=1,E=0",
  "2_d7" = "A=1,B=1,E=1",
  "3_c1" = "A=0,B=0,E=0",
  "3_c2" = "A=0,B=1,E=0",
  "3_c3" = "A=1,B=0,E=0",
  "3_c4" = "A=1,B=1,E=0",
  "3_c5" = "A=1,B=1,E=1",
  "3_d1" = "A=0,B=0,E=0",
  "3_d2" = "A=0,B=1,E=0",
  "3_d3" = "A=0,B=1,E=1",
  "3_d4" = "A=1,B=0,E=0",
  "3_d5" = "A=1,B=0,E=1",
  "3_d6" = "A=1,B=1,E=0",
  "3_d7" = "A=1,B=1,E=1"
)

df$trial_structure_type <- recode(
  df$trial_id,
  "1_c1" = "Conjunctive: A=0,B=0,E=0",
  "1_c2" = "Conjunctive: A=0,B=1,E=0",
  "1_c3" = "Conjunctive: A=1,B=0,E=0",
  "1_c4" = "Conjunctive: A=1,B=1,E=0",
  "1_c5" = "Conjunctive: A=1,B=1,E=1",
  "1_d1" = "Disjunctive: A=0,B=0,E=0",
  "1_d2" = "Disjunctive: A=0,B=1,E=0",
  "1_d3" = "Disjunctive: A=0,B=1,E=1",
  "1_d4" = "Disjunctive: A=1,B=0,E=0",
  "1_d5" = "Disjunctive: A=1,B=0,E=1",
  "1_d6" = "Disjunctive: A=1,B=1,E=0",
  "1_d7" = "Disjunctive: A=1,B=1,E=1",
  "2_c1" = "Conjunctive: A=0,B=0,E=0",
  "2_c2" = "Conjunctive: A=0,B=1,E=0",
  "2_c3" = "Conjunctive: A=1,B=0,E=0",
  "2_c4" = "Conjunctive: A=1,B=1,E=0",
  "2_c5" = "Conjunctive: A=1,B=1,E=1",
  "2_d1" = "Disjunctive: A=0,B=0,E=0",
  "2_d2" = "Disjunctive: A=0,B=1,E=0",
  "2_d3" = "Disjunctive: A=0,B=1,E=1",
  "2_d4" = "Disjunctive: A=1,B=0,E=0",
  "2_d5" = "Disjunctive: A=1,B=0,E=1",
  "2_d6" = "Disjunctive: A=1,B=1,E=0",
  "2_d7" = "Disjunctive: A=1,B=1,E=1",
  "3_c1" = "Conjunctive: A=0,B=0,E=0",
  "3_c2" = "Conjunctive: A=0,B=1,E=0",
  "3_c3" = "Conjunctive: A=1,B=0,E=0",
  "3_c4" = "Conjunctive: A=1,B=1,E=0",
  "3_c5" = "Conjunctive: A=1,B=1,E=1",
  "3_d1" = "Disjunctive: A=0,B=0,E=0",
  "3_d2" = "Disjunctive: A=0,B=1,E=0",
  "3_d3" = "Disjunctive: A=0,B=1,E=1",
  "3_d4" = "Disjunctive: A=1,B=0,E=0",
  "3_d5" = "Disjunctive: A=1,B=0,E=1",
  "3_d6" = "Disjunctive: A=1,B=1,E=0",
  "3_d7" = "Disjunctive: A=1,B=1,E=1"
)

df <- df |>
  mutate(
    trial_type = factor(
      trial_type,
      levels = c(
        "A=0,B=0,E=0",
        "A=0,B=1,E=0",
        "A=0,B=1,E=1",
        "A=1,B=0,E=0",
        "A=1,B=0,E=1",
        "A=1,B=1,E=0",
        "A=1,B=1,E=1"
      )
    ),
    trial_structure_type = factor(
      trial_structure_type,
      levels = c(
        "Conjunctive: A=1,B=1,E=1",
        "Conjunctive: A=1,B=1,E=0",
        "Conjunctive: A=1,B=0,E=0",
        "Conjunctive: A=0,B=1,E=0",
        "Conjunctive: A=0,B=0,E=0",
        "Disjunctive: A=0,B=1,E=0",
        "Disjunctive: A=1,B=1,E=1",
        "Disjunctive: A=1,B=0,E=1",
        "Disjunctive: A=0,B=1,E=1",
        "Disjunctive: A=1,B=1,E=0",
        "Disjunctive: A=1,B=0,E=0",
        "Disjunctive: A=0,B=0,E=0"
      )
    ),
    # NOTE THESE (ABOVE) LOOK IN THE WRONG ORDER BUT ITS JUST FOR THE PLOT
    pgroup = factor(
      pgroup,
      levels = 1:3,
      labels = c(
        'A=.1,Au=.5,B=.8,Bu=.5',
        'A=.5,Au=.1,B=.5,Bu=.8',
        'A=.1,Au=.7,B=.8,Bu=.5'
      )
    ),
    Response = factor(node3),
    #Observed = factor(!Response%in%c('Au=0','Au=1','Bu=0', 'Bu=1'), levels = c(T,F)),
    Variable = factor(substr(node3, 1, 1))
    #State = factor(rep(c(0,1), 144)),
    #Actual = factor(Actual, levels = c(FALSE, TRUE), labels = c('FALSE', 'TRUE')),
  )

#df$trial_id <- as.factor(df$trial_id)
#df$node3 <- as.factor(df$node3)

df$SE <- NA

# Need to add SE
for (i in unique(df$trial_id)) {
  df$SE[df$trial_id == i] <- sqrt(
    (df$prop[df$trial_id == i] * (1 - df$prop[df$trial_id == i])) /
      sum(df$n[df$trial_id == i])
  )
}

#df$Actual[is.na(df$Actual)] <- FALSE

# save
save(df, file = here('Data', 'ModelData', 'fitforplot4par.rda')) # Goes to script 10 and 11 and 12
