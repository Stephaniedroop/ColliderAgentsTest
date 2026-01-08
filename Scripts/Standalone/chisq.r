# ================================================================
# Do people answer in a patterned way - Do they answer differently from uniform/random?
# Does 1) world affect their answer, 2) probability manipulations affect their answer?
# ================================================================

library(tidyverse)
library(here)

# (THE MODELS ARENT USED HERE, JUST FOR HANDY GROUPED PPT VALUES SO DOESNT VARY WITH MODELS, DOESNT NEED OT BE THE LATEST)
load(here('Data', 'modelData', 'modelAndDataUnfitig.rda')) # df, 288 of 23


# As a top-level sanity check that people are answering some sort of question: do they answer differently from uniform?
# We check this by first grouping by world, and then run a series of 36 chi-square tests.

# The first section prints a 36 Bonferroni-corrected chisq test results, and finds
# in every one of the 36 worlds, that people answer in a patterned way, ie. different from uniform or at random.

results <- df |>
  group_by(trial_id) |>
  summarise(
    chi_sq_stat = chisq.test(n, p = rep(1 / n(), n()))$statistic,
    p_value = chisq.test(n, p = rep(1 / n(), n()))$p.value
  ) |>
  mutate(
    p_adj = p.adjust(p_value, method = "bonferroni")
  )

print(results)

print(min(results$chi_sq_stat)) # 50.5
print(max(results$p_adj)) # 4e-7


# Then check grouping by theory metrics: Actual, Known, Observed

# Simple 50/50 chisq doesnt actually make sense unless it's for ob/un where the proportions are actually equally split.
# Otherwise you'd have to calculate strange proportions for expected.
# We can get those strange proportions here because the tags are for the variables themselves, not the answers.

countObs <- df |> # 144 144 as expected
  group_by(Observed) |>
  summarise(n = n())

countAct <- df |> # F: 180, T: 108
  group_by(Actual) |>
  summarise(n = n())

countKnown <- df |> # F: 222, T: 66
  group_by(Known) |>
  summarise(n = n())

# -------------- Probability manipulation ---------------

# Are answers different across the three probability groupings? A quick series of chisq, ignoring the grouping by ppt

# Split df into 3, on pgroup
df_p1 <- df |>
  filter(pgroup == 1) |>
  select(pgroup, trialtype, node3, n) |>
  # Concatenate trialtype and node3 into a new variable in a new column
  mutate(trial_node = paste(trialtype, node3, sep = "_"))

df_p2 <- df |>
  filter(pgroup == 2) |>
  select(pgroup, trialtype, node3, n) |>
  # Concatenate trialtype and node3 into a new variable in a new column
  mutate(trial_node = paste(trialtype, node3, sep = "_"))

df_p3 <- df |>
  filter(pgroup == 3) |>
  select(pgroup, trialtype, node3, n) |>
  # Concatenate trialtype and node3 into a new variable in a new column
  mutate(trial_node = paste(trialtype, node3, sep = "_"))

trialnodes <- unique(df_p1$trial_node)

# Merge these three dataframes on trial_node to make a 3-way composite for chisq tests
fortest <- df_p1 |>
  inner_join(df_p2, by = "trial_node", suffix = c("_1", "_2")) |>
  inner_join(df_p3, by = "trial_node") |> #, suffix = ("_3")
  rename(n1 = n_1, n2 = n_2, n3 = n) |>
  select(n1, n2, n3)

fortest_nonzero <- fortest[rowSums(fortest) > 0, ]

omnibus <- chisq.test(fortest_nonzero) # X = 238.8, df = 186, p= .005411
print(omnibus)


# ------- EFFECT SIZE -------

nom <- sum(fortest_nonzero) # 2580

om <- sqrt(omnibus$statistic / (nom * 2)) # .304 medium effect size CRAMERS V INSTEAD OF OMEGA


# ---------- PAIRWISE -------------

# Earlier - before we worked out you can do a three-way chisq
# Now make three new matrices for the chisq
# ---------- 12 ----------
df_12 <- df_p1 |> # 96 rows each time
  inner_join(df_p2, by = "trial_node", suffix = c("_p1", "_p2")) |>
  select(n_p1, n_p2)

df_12 <- as.matrix(df_12)
rownames(df_12) <- trialnodes
colnames(df_12) <- c("p1", "p2")

# Now run chisq on these two columns
#chisq.test(df_12$p1, df_12$p2) #

chisq.test(df_12) # can't do it perhaps because of too many zeroes?
# (It does not like 0 or low counts especially in the 'chisq.test(df_12)$expected' values which it uses to calculate the statistic
# by comparing observed and expected counts.

# So remove columns with only zeroes
df_12_nonzero <- df_12[rowSums(df_12) > 0, ]
n12 <- sum(df_12_nonzero) # 1686

ch12 <- chisq.test(df_12_nonzero) # X = 100.04, df = 92, p = 0.254
print(ch12)

# Now do for other pairs
# --------- 23 --------

df_23 <- df_p2 |>
  inner_join(df_p3, by = "trial_node", suffix = c("_p2", "_p3")) |>
  select(n_p2, n_p3)

df_23 <- as.matrix(df_23) # remove text column
rownames(df_23) <- trialnodes
colnames(df_23) <- c("p2", "p3")

# Remove zero rows
df_23_nonzero <- df_23[rowSums(df_23) > 0, ]
n23 <- sum(df_23_nonzero) # 1761
ch23 <- chisq.test(df_23_nonzero) # X = 139, df = 91, p = .00091

# -------- 13 --------
df_13 <- df_p1 |>
  inner_join(df_p3, by = "trial_node", suffix = c("_p1", "_p3")) |>
  select(n_p1, n_p3)

df_13 <- as.matrix(df_13) # remove text column

rownames(df_13) <- trialnodes
colnames(df_13) <- c("p1", "p3")

# Remove zero rows
df_13_nonzero <- df_13[rowSums(df_13) > 0, ]
n13 <- sum(df_13_nonzero) # 1713

ch13 <- chisq.test(df_13_nonzero) # X = 103.34, df = 91, p = .162

ch13$statistic

# -------- get Cohen's w omega for these comparisons ------------

# w = sqrt(chi-sq / N) where N is the sum of all observations in each matrix. All rather small-medium effect sizes here

om13 <- sqrt((ch13$statistic / n13)) # .246
om12 <- sqrt((ch12$statistic / n12)) # .244
om23 <- sqrt((ch23$statistic / n23)) # .281
