################################################################################
########### Master script for collider for new experiment - June 2025 ##########
#################################################################################

library(tidyverse)
library(rjson)
library(here)

# Get individual csvs into a list
csvList <- lapply(
  list.files(here("Data", "new"), full.names = TRUE),
  read.csv,
  stringsAsFactors = F
)

# Reference column names from the first file
colnames_ref <- colnames(csvList[[1]])

# Identify files with mismatched column names
#bad_files <- files[sapply(csvList, function(df) !identical(colnames(df), colnames_ref))]
#print(bad_files) # There are two where the column headings did not copy but the answers are in right structure

csvList <- lapply(csvList, function(df) {
  colnames(df) <- colnames_ref
  return(df)
})

# An initial data file. (It still needs to be filtered for complete ppts and reconciled with prolific account of who was paid, below)
df1 <- do.call(rbind, csvList)

dfq <- df1 |>
  group_by(prolific_id) |>
  summarise(n = n()) |>
  filter(n == 24) # 216

df1 <- df1 |>
  filter(prolific_id %in% dfq$prolific_id)


## --------------------------------------------------------------------------------------------------------------------------------
# Each trial's info is spread across multiple rows, so we need to get everything in one row
# But first we need to replace spaces with NA
df1 <- df1 |>
  mutate(across(c('answer'), ~ na_if(., "")))

# Then can fill upwards to get the text answer in the same place as the trial info
df1 <- df1 |>
  fill(answer, .direction = 'up')

# Remove empty cols and rows
df1 <- df1 |>
  filter(cb != 'NA') # 2592


## --------------------------------------------------------------------------------------------------------------------------------
# Get the list of who got paid according to prolific
demognew <- read.csv(here('Data', 'demognew.csv')) |>
  filter(Status == 'APPROVED') # 240

reconc1 <- demognew |>
  filter((Participant.id %in% df1$prolific_id)) # 215

reconc2 <- df1 |>
  filter(prolific_id %in% demognew$Participant.id) |>
  group_by(prolific_id) |>
  summarise(n = n()) # 215

keep <- filter(reconc2, n == 12) # 215 only!! Prolific messed up
keep2 <- reconc1 |>
  filter(Participant.id %in% keep$prolific_id)

df <- df1 |>
  filter(prolific_id %in% keep$prolific_id) # 2580 = 215 ppts x 12 trials


## --------------------------------------------------------------------------------------------------------------------------------
dem <- keep2
dem$Age <- as.numeric(dem$Age)
print(mean(dem$Age)) # 38.5
min(dem$Age) # 18
max(dem$Age) # 80
sd(dem$Age) # 12.8
sum(dem$Sex == 'Female') # 111
sum(dem$Sex == 'Male') # 103 so other == 1
mean(dem$Time.taken) / 60 # 31.5
sd(dem$Time.taken) / 60 # 13.9


## --------------------------------------------------------------------------------------------------------------------------------
# Put a column with structure - for some reason it doesn't have
df <- df |>
  mutate(
    structure = if_else(grepl("^c", trialtype), 'conjunctive', 'disjunctive')
  )

df <- df |>
  select(-c(2:4, 6, 7, 18, 19))


## --------------------------------------------------------------------------------------------------------------------------------
check <- df |>
  group_by(subject_id, trialtype) |>
  filter(n() > 1) |>
  distinct(subject_id, trialtype)

# It says 0, so that's nice

# --------------- Processing the answers participants gave -----------------

# Now reattach the number of their answer, as atm it has only recorded the text on their radio button
# First here are the arrays of possible buttons from js exp.
jobanswers <- c(
  'The presenter presented Feature A',
  'The presenter did not present Feature A',
  'The files underlying Feature A were fine',
  'The files underlying Feature A were corrupted',
  'The presented presented Feature B',
  'The presented did not present Feature B',
  'The files underlying Feature B were fine',
  'The files underlying Feature B were corrupted'
)

cookanswers <- c(
  'The main dish was on the menu',
  'The main dish was not on the menu',
  'The person wanted to eat the main dish',
  'The person did not want to eat the main dish',
  'The dessert was on the menu',
  'The dessert was not on the menu',
  'The person wanted to eat the dessert',
  'The person did not want to eat the dessert'
)

groupanswers <- c(
  'The first student attended',
  'The first student did not attend',
  'The first student had had a good morning',
  'The first student had not had a good morning',
  'The second student attended',
  'The second student did not attend',
  'The second student had had a good morning',
  'The second student had not had a good morning'
)


## --------------------------------------------------------------------------------------------------------------------------------
# Make a new column with the position in array of their answer
df <- df |>
  mutate(
    ans = if_else(
      scenario == 'job',
      match(df$answer, jobanswers),
      if_else(
        scenario == 'cook',
        match(df$answer, cookanswers),
        match(df$answer, groupanswers)
      )
    )
  )


## --------------------------------------------------------------------------------------------------------------------------------
data0 <- df |> filter(cb == 0)
data1 <- df |> filter(cb == 1)

# I think for c1,c4, c5, d1, d6, d7, we can just switch the answers
# But for the others, for TRIALTYPE, they have to be changed as follows:
# c2 goes to c3
# c3 goes to c2
# d2 goes to d4
# d3 goes to d5
# d4 goes to d2
# d5 goes to d3

# To do this, we'll take an intermediate column and then start replacing values
data1$cbtt <- data1$trialtype
data1$cbtt[data1$trialtype == 'c2'] <- 'c3'
data1$cbtt[data1$trialtype == 'c3'] <- 'c2'
data1$cbtt[data1$trialtype == 'd2'] <- 'd4'
data1$cbtt[data1$trialtype == 'd3'] <- 'd5'
data1$cbtt[data1$trialtype == 'd4'] <- 'd2'
data1$cbtt[data1$trialtype == 'd5'] <- 'd3'


# Now we can flip all the answers
# Store indices of which answers are already a and which are b
aans <- as.vector(1:4)
bans <- as.vector(5:8)

# Create new var where they are flipped
data1$intans <- data1$ans

data1 <- data1 |>
  mutate(anscb = if_else(ans %in% aans, intans + 4, intans - 4))

data1 <- data1 |>
  select(-c(trialtype, ans)) |>
  rename(trialtype = cbtt, ans = anscb)

data1 <- data1 |>
  select(-intans)

df2 <- rbind(data0, data1) # 2640 of 9


## --------------------------------------------------------------------------------------------------------------------------------
# Map the answers they gave to the variables, in three increasing levels of granularity

# 1. Just the 4 variables, irrespective of the value they took
df2 <- df2 |>
  mutate(
    node = if_else(
      ans == 1 | ans == 2,
      'A',
      if_else(
        ans == 3 | ans == 4,
        'Au',
        if_else(ans == 5 | ans == 6, 'B', 'Bu')
      )
    )
  )

# 2. Six values, ie A and B are observed so don't need options, but unobserved Au and Bu can take either value
df2 <- df2 |>
  mutate(
    node2 = if_else(
      ans == 1 | ans == 2,
      'A',
      if_else(
        ans == 3,
        'Au=1',
        if_else(
          ans == 4,
          'Au=0',
          if_else(ans == 5 | ans == 6, 'B', if_else(ans == 7, 'Bu=1', 'Bu=0'))
        )
      )
    )
  )
# 3. Eight values: each var can take either value
df2 <- df2 |>
  mutate(
    node3 = if_else(
      ans == 1,
      'A=1',
      if_else(
        ans == 2,
        'A=0',
        if_else(
          ans == 3,
          'Au=1',
          if_else(
            ans == 4,
            'Au=0',
            if_else(
              ans == 5,
              'B=1',
              if_else(ans == 6, 'B=0', if_else(ans == 7, 'Bu=1', 'Bu=0'))
            )
          )
        )
      )
    )
  )


## --------------------------------------------------------------------------------------------------------------------------------
# We now give a variable for probgroup
df2 <- df2 |>
  mutate(
    pgroup = if_else(
      prob0 == '10%' & prob1 == '50%' | prob2 == '10%' & prob3 == '50%',
      '1',
      if_else(
        prob0 == '50%' & prob1 == '80%' | prob2 == '50%' & prob3 == '80%',
        '2',
        '3'
      )
    )
  )

# And make a unique trial-pgroup identifier for easier modelling and plotting later
df2 <- df2 |>
  unite('trial_id', pgroup, trialtype, sep = "_", remove = FALSE)


# Add a column called Observed, which is TRUE if they selected an observed variable (A or B) and FALSE if unobserved (Au or Bu)
df2 <- df2 |>
  mutate(Observed = if_else(node %in% c('A', 'B'), TRUE, FALSE))

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
df2 <- df2 |>
  mutate(
    Known = case_when(
      node3 %in% c('A=0', 'A=1', 'B=0', 'B=1') ~ TRUE,
      trialtype == 'c5' & node3 %in% c('Au=1', 'Bu=1') ~ TRUE,
      trialtype == 'd2' & node3 == 'Bu=0' ~ TRUE,
      trialtype == 'd3' & node3 == 'Bu=1' ~ TRUE,
      trialtype == 'd4' & node3 == 'Au=0' ~ TRUE,
      trialtype == 'd5' & node3 == 'Au=1' ~ TRUE,
      trialtype == 'd6' & node3 %in% c('Au=0', 'Bu=0') ~ TRUE,
      TRUE ~ FALSE
    )
  )

# NOTE: use trialtype but don't use A,B,E, because of the counterbalancing issue they no longer match

# Now arrange df2 so all subjects are in order, and within each subject all trials are in order
df2 <- df2 |>
  arrange(subject_id, trial_id)

# Make everything a factor for later

df2 <- df2 |>
  mutate(across(where(is.logical), as.factor))

df2 <- df2 |>
  mutate(across(where(is.character), as.factor))


data <- df2

## --------------------------------------------------------------------------------------------------------------------------------
save(data, file = here::here("Data", "Data.Rdata"))
