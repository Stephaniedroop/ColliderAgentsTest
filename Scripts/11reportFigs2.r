###########################################################
########### Other plots not using functions  ##############
###########################################################

library(tidyverse)
library(ggnewscale) # Load these if you don't have them
library(here)
library(RColorBrewer)
library(ggplot2)


load(here('Data', 'modelData', 'fitforplot25.rda')) # 288 of 43. Pre 3 Nov: 288 of 35 fitforplot16k.rda
# These are the same models but with an extra series called '_Known' that we didn't end up using. So, if it ends in '_ig' it is equivalent to using KIndnes, and if it ends in '_none' that is the no K series
source(here('Scripts', 'plotUtils.R'))

# The tag 'Known' should no longer apply to Observed vars.
# If node3 starts with A= or B= then put FALSE in the column Known
vals <- c("A=0", "A=1", "B=0", "B=1")

# If node3 %in% vals, then Known = FALSE
df$Known[df$node3 %in% vals] <- FALSE


# ------------  Just 111, for compound decision AvB . Just pgroup 1, noKind -----------------

# FIG 4 IN PAPER
df5 <- df |>
  filter(
    trial_structure_type %in%
      c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"),
    pgroup == "A=.1,Au=.5,B=.8,Bu=.5"
  ) |>
  select(
    trial_id,
    pgroup,
    full_none,
    prop,
    trial_structure_type,
    Variable,
    Actual,
    SE
  )

# Summarize mean proportions by group (A, B)
df_bar <- df5 |>
  group_by(trial_structure_type, Variable) |>
  summarise(Participants = sum(prop), Model = sum(full_none), .groups = 'drop')

# Summarize SE separately for error bars
df_error <- df5 |>
  group_by(trial_structure_type, Variable) |>
  summarise(
    mean_prop = sum(prop),
    se = sqrt(sum(SE^2)), # Example assuming independence; adapt based on your data
    .groups = 'drop'
  )

p111abf <- ggplot() +
  geom_col(
    data = df_bar,
    aes(x = Variable, y = Participants, fill = Variable),
    position = position_dodge()
  ) +
  geom_errorbar(
    data = df_error,
    aes(x = Variable, ymin = mean_prop - se, ymax = mean_prop + se),
    width = 0.2,
    position = position_dodge(.9)
  ) +
  scale_fill_brewer(palette = "Set2", name = "Participants \nand variable") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  geom_point(
    data = df_bar,
    aes(x = Variable, y = Model, shape = "Model", color = "Model"),
    size = 3,
    position = position_dodge(.9),
    show.legend = TRUE
  ) +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_color_manual(name = "", values = c("Model" = "blue")) +
  labs(
    x = 'Response',
    y = 'Proportion/Prediction',
    title = 'A=.1,Au=.5,B=.8,Bu=.5'
  ) + #'
  facet_wrap(~trial_structure_type) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(vjust = 0.5, hjust = 1),
    legend.margin = margin(c(0, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 0, l = 0)),
    legend.text = element_text(size = 18), # here thats 'Full Model'
    legend.title = element_text(size = 10)
  ) # here 'Participants and variable'


print(p111abf)

ggsave(
  filename = "p111abnok.pdf",
  plot = p111abf,
  path = here("Other", "Plots"),
  width = 6,
  height = 6,
  units = "in"
)


# ------------- A version of the previous, Fig.4 in papar, stacked bar chart but not combining A and Au -------------

# ------------  Just 111, for compound decision AvB . Just pgroup 1, noKind -----------------

# Add a column called node2 for whether node3 starts with A, Au, B or Bu
df <- df |>
  mutate(
    node2 = case_when(
      str_starts(node3, "A=") ~ "A",
      str_starts(node3, "Au=") ~ "Au",
      str_starts(node3, "B=") ~ "B",
      str_starts(node3, "Bu=") ~ "Bu",
      TRUE ~ NA_character_
    )
  )

df6 <- df |>
  filter(
    trial_structure_type %in%
      c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1"),
    pgroup == "A=.1,Au=.5,B=.8,Bu=.5"
  ) |>
  select(
    trial_id,
    pgroup,
    full_none,
    prop,
    trial_structure_type,
    Variable,
    node2,
    Actual,
    SE
  )

# Summarize mean proportions by group (A, B)
df_bar6 <- df6 |>
  group_by(trial_structure_type, Variable, node2) |>
  summarise(Participants = sum(prop), Model = sum(full_none), .groups = 'drop')

# Summarize SE separately for error bars
df_error6 <- df6 |>
  group_by(trial_structure_type, Variable, node2) |>
  summarise(
    mean_prop = sum(prop),
    se = sqrt(sum(SE^2)), # Example assuming independence; adapt based on your data
    .groups = 'drop'
  )

p111abf6 <- ggplot() +
  geom_col(
    data = df_bar6,
    aes(x = Variable, y = Participants, fill = node2),
    position = position_stack()
  ) +
  # geom_errorbar(
  #   data = df_error6,
  #   aes(x = Variable, ymin = mean_prop - se, ymax = mean_prop + se),
  #   width = 0.2,
  #   position = position_dodge(.9)
  # ) +
  scale_fill_brewer(palette = "Accent", name = "Participants \nand variable") +
  guides(fill = guide_legend(override.aes = list(shape = NA))) +
  # geom_point(
  #   data = df_bar6,
  #   aes(x = Variable, y = Model, shape = "Model", color = "Model"),
  #   size = 3,
  #   position = position_dodge(.9),
  #   show.legend = TRUE
  # ) +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_color_manual(name = "", values = c("Model" = "blue")) +
  labs(
    x = 'Response',
    y = 'Proportion/Prediction',
    title = 'A=.1,Au=.5,B=.8,Bu=.5'
  ) + #'
  facet_wrap(~trial_structure_type) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(vjust = 0.5, hjust = 1),
    legend.margin = margin(c(0, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 0, l = 0)),
    legend.text = element_text(size = 18), # here thats 'Full Model'
    legend.title = element_text(size = 10)
  ) # here 'Participants and variable'


# axis.text = element_text(size = 14),
#axis.title = element_text(size = 18),

# chat GPT version

# my_colors <- c(
#   "A"  = brewer.pal(9, "Blues")[4],
#   "Au" = brewer.pal(9, "Blues")[7],
#   "B"  = brewer.pal(9, "Purples")[4],
#   "Bu" = brewer.pal(9, "Purples")[7]
# )

my_colors <- c(
  "A" = brewer.pal(9, "Oranges")[4],
  "Au" = brewer.pal(9, "Oranges")[7],
  "B" = brewer.pal(9, "Greens")[4],
  "Bu" = brewer.pal(9, "Greens")[7]
)


# Summaries for stacked bars (unchanged)
df_bar6 <- df6 |>
  group_by(trial_structure_type, Variable, node2) |>
  summarise(Participants = sum(prop), Model = sum(full_none), .groups = 'drop')

# NEW: Total summary for placing error bars
df_error6_total <- df6 |>
  group_by(trial_structure_type, Variable) |>
  summarise(
    mean_prop = sum(prop),
    se = sqrt(sum(SE^2)),
    .groups = "drop"
  )

# NEW: Total summary for model dots
df_model6_total <- df6 |>
  group_by(trial_structure_type, Variable) |>
  summarise(
    Model = sum(full_none),
    .groups = "drop"
  )


p111abf6 <- ggplot() +
  geom_col(
    data = df_bar6,
    aes(x = Variable, y = Participants, fill = node2),
    position = position_stack()
  ) +

  # ---- ERROR BARS (total response) ----
  geom_errorbar(
    data = df_error6_total,
    aes(
      x = Variable,
      ymin = mean_prop - se,
      ymax = mean_prop + se
    ),
    width = 0.25,
    size = 0.8
  ) +

  # ---- MODEL DOTS (total prediction) ----
  geom_point(
    data = df_model6_total,
    aes(
      x = Variable,
      y = Model,
      shape = "Model",
      color = "Model"
    ),
    size = 5
  ) +
  scale_fill_manual(values = my_colors, name = "Participants \nand variable") +
  #scale_fill_brewer(palette = "Spectral", name = "Participants \nand variable") +
  scale_shape_manual(name = "", values = c("Model" = 16)) +
  scale_color_manual(name = "", values = c("Model" = "blue")) +

  labs(
    x = 'Response',
    y = 'Proportion/Prediction',
    title = 'A=.1,Au=.5,B=.8,Bu=.5'
  ) +

  facet_wrap(~trial_structure_type) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    axis.text.x = element_text(vjust = 0.5, hjust = 1),
    legend.margin = margin(c(0, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(t = 1)),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 10)
  )


print(p111abf6)

ggsave(
  filename = "p111abnokst.pdf",
  plot = p111abf6,
  path = here("Other", "Plots"),
  width = 12,
  height = 6,
  units = "in"
)


# -------- THEORY PLOTS FOR FIGURE 8 - OBSERVED, KNOWN, ACTUAL ------

# try scale_fill_manual(values = c("Yes" = "#66c2a5", "No" = "#fc8d62")) + after scale fill brewer line

# --------- ig model, KNOWN, FULL ----------

# 1. Gather and calculate mean and SE per group - 576 of 5
df_long0 <- df |>
  select(trial_id, node3, prop, full_ig, Known) |>
  gather(key, val, prop:full_ig) |>
  filter(!is.na(val)) |>
  mutate(
    key = factor(
      key,
      levels = c("full_ig", "prop"),
      labels = c("Model", "Participants")
    )
  )

summary_df0 <- df_long0 |>
  group_by(Known, key) |>
  summarise(
    val_sum = sum(val),
    se = sd(val) / sqrt(n())
  ) |>
  mutate(val = val_sum / 36) |>
  select(-val_sum) |>
  ungroup()

# 2. Plot with bars and error bars
pknown <- ggplot(summary_df0, aes(x = key, y = val, fill = Known)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = val - se, ymax = val + se),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  labs(x = "Response", y = "Proportion/Prediction", fill = "Known status") +
  scale_fill_brewer(palette = "Set2") + #, labels = c("", "") +
  scale_fill_manual(values = c("TRUE" = "#66c2a5", "FALSE" = "#fc8d62")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

# A version with *** added, although now with new model no use
pknown <- ggplot(summary_df0, aes(x = key, y = val, fill = Known)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = val - se, ymax = val + se),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(summary_df0$val + summary_df0$se) + 0.05,
    label = "***",
    hjust = 0.5,
    vjust = 0.5,
    size = 5
  ) +
  labs(x = "Response", y = "Proportion/Prediction", fill = "Known") +
  scale_fill_brewer(palette = "Set2") +
  scale_fill_manual(values = c("TRUE" = "#66c2a5", "FALSE" = "#fc8d62")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

print(pknown)

ggsave(
  filename = "pknown.pdf",
  plot = pknown,
  path = here("Other", "Plots"),
  width = 3.5,
  height = 4,
  units = "in"
)


# --------- model ig OBSERVED, FULL ----------

# NOT IMPORATNT NOW - NO DIFFERENCE

# Repeat just the observed v unobs for the full model just in case it's needed,
# although it gives the same results as noKind, because k is so small in the best fitting models

# 1. Gather and calculate mean and SE per group
df_longf <- df |>
  select(trial_id, node3, prop, full_ig, Observed) |>
  gather(key, val, prop:full_ig) |>
  filter(!is.na(val)) |>
  mutate(
    key = factor(
      key,
      levels = c("full_ig", "prop"),
      labels = c("Model", "Participants")
    )
  )

summary_dff <- df_longf |>
  group_by(Observed, key) |>
  summarise(
    val_sum = sum(val),
    se = sd(val) / sqrt(n())
  ) |>
  mutate(val = val_sum / 36) |>
  select(-val_sum) |>
  ungroup()


# 2. Plot with bars and error bars
punf <- ggplot(summary_dff, aes(x = key, y = val, fill = Observed)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = val - se, ymax = val + se),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(summary_dff$val + summary_dff$se) + 0.05,
    label = "***",
    hjust = 0.5,
    vjust = 0.5,
    size = 5
  ) +
  labs(x = "Response", y = "Proportion/Prediction", fill = "Observed") +
  scale_fill_brewer(palette = "Set2") + #, labels = c("Unobserved \n(Au|Bu)", "Observed \n(A|B)")
  scale_fill_manual(values = c("TRUE" = "#66c2a5", "FALSE" = "#fc8d62")) +
  #scale_fill_discrete(labels = c("Observed \n(A|B)", "Unobserved \n(Au|Bu)")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

print(punf)

ggsave(
  filename = "punf.pdf",
  plot = punf,
  path = here("Other", "Plots"),
  width = 3.5,
  height = 4,
  units = "in"
)


# --------- ACTUAL, FULL -----------

# 1. Gather and calculate mean and SE per group
df_longA <- df |>
  select(trial_id, node3, prop, full_ig, Actual) |>
  gather(key, val, prop:full_ig) |>
  filter(!is.na(val)) |>
  mutate(
    key = factor(
      key,
      levels = c("full_ig", "prop"),
      labels = c("Model", "Participants")
    )
  )

summary_dfA <- df_longA |>
  group_by(Actual, key) |>
  summarise(
    val_sum = sum(val),
    se = sd(val) / sqrt(n())
  ) |>
  mutate(val = val_sum / 36) |>
  select(-val_sum) |>
  ungroup()


# 2. Plot with bars and error bars
punA <- ggplot(summary_dfA, aes(x = key, y = val, fill = Actual)) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = val - se, ymax = val + se),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  #annotate("text", x = 1.5, y = max(summary_dfA$val + summary_dfA$se) + 0.05, hjust = 0.5, vjust = 0.5, size = 5) + #label = "***"
  labs(x = "Response", y = "Proportion/Prediction", fill = "Actual") +
  scale_fill_brewer(palette = "Set2") + #, labels = c("Unobserved \n(Au|Bu", "Observed \n(A|B)")
  scale_fill_manual(values = c("TRUE" = "#66c2a5", "FALSE" = "#fc8d62")) +
  #scale_fill_discrete(labels = c("Observed \n(A|B)", "Unobserved \n(Au|Bu)")) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

print(punA)

ggsave(
  filename = "punA.pdf",
  plot = punA,
  path = here("Other", "Plots"),
  width = 3.5,
  height = 4,
  units = "in"
)


# Tried hunners more plots but none had good message, so none used, examples below: contact me for others

# ------------- STRUCTURE AND EFFECT, FULL --------------

#### A panel splitting out observed v observed by E=0/1, for the full model

# Now same thing for structure only
df.s <- df |>
  gather(key, val, prop:noAct) |>
  mutate(
    key = factor(
      key,
      levels = c('noAct', 'prop'),
      labels = c("Model", "Participants")
    )
  ) |>
  group_by(Observed, key, structure, E) |>
  summarise(val = sum(val) / 3)

df.s <- df.s |>
  filter(!is.na(key))

df.s$struct_effect <- paste0(df.s$structure, df.s$E)

# Better
df.s$val <- ifelse(
  df.s$struct_effect == "conjunctive0",
  df.s$val / 4,
  ifelse(
    df.s$struct_effect == "disjunctive0",
    df.s$val / 4,
    ifelse(df.s$struct_effect == "disjunctive1", df.s$val / 3, df.s$val)
  )
)


# This is the two-panel plot split for Effect, for the full model, to compare structures where E=0 with E=1:

punobs_f <- ggplot(df.s, aes(y = val, x = key, fill = Observed)) +
  stat_summary(
    fun.y = mean,
    geom = "bar",
    position = position_dodge(),
    colour = 'black'
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(.9),
    width = .2
  ) +
  labs(x = 'Response', y = 'Proportion/Prediction') +
  facet_grid(
    structure ~ E,
    labeller = labeller(E = function(x) paste("Effect =", x))
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.margin = margin(c(0, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 0, l = 0))
  )

print(punobs_f)

ggsave(
  filename = "punobs_f.pdf",
  plot = punobs_f,
  path = here("Other", "Plots"),
  width = 4,
  height = 4,
  units = "in"
)


# Is this reverse effect also seen for the Known variables? Perhaps people only choose known variables when E=0

# --------------- STRUCTURE, KNOWN, FULL --------------

# ------------- STRUCTURE AND EFFECT, KNOWN, FULL --------------

#### A panel splitting out observed v observed by E=0/1, for the full model

# Now same thing for structure only
df.k <- df |>
  gather(key, val, prop:noAct) |>
  mutate(
    key = factor(
      key,
      levels = c('noAct', 'prop'),
      labels = c("Model", "Participants")
    )
  ) |>
  group_by(Known, key, structure, E) |>
  summarise(val = sum(val) / 3)

df.k <- df.k |>
  filter(!is.na(key))

df.k$struct_effect <- paste0(df.k$structure, df.k$E)

# Better
df.k$val <- ifelse(
  df.k$struct_effect == "conjunctive0",
  df.k$val / 4,
  ifelse(
    df.k$struct_effect == "disjunctive0",
    df.k$val / 4,
    ifelse(df.k$struct_effect == "disjunctive1", df.k$val / 3, df.k$val)
  )
)


# This is the two-panel plot split for Effect, for the full model, to compare structures where E=0 with E=1:

pk_f <- ggplot(df.k, aes(y = val, x = key, fill = Known)) +
  stat_summary(
    fun.y = mean,
    geom = "bar",
    position = position_dodge(),
    colour = 'black'
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(.9),
    width = .2
  ) +
  labs(x = 'Response', y = 'Proportion/Prediction') +
  facet_grid(
    structure ~ E,
    labeller = labeller(E = function(x) paste("Effect =", x))
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.margin = margin(c(0, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 0, l = 0))
  )

print(pk_f)

ggsave(
  filename = "pk_f.pdf",
  plot = pk_f,
  path = here("Other", "Plots"),
  width = 4,
  height = 4,
  units = "in"
)


# --------------- STRUCTURE, ACTUAL, FULL --------------

# ------------- STRUCTURE AND EFFECT, ACTUAL, FULL --------------

#### A panel splitting out observed v observed by E=0/1, for the full model

# Now same thing for structure only
df.a <- df |>
  gather(key, val, prop:full) |>
  mutate(
    key = factor(
      key,
      levels = c('full', 'prop'),
      labels = c("Full Model", "Participants")
    )
  ) |>
  group_by(Actual, key, structure, E) |>
  summarise(val = sum(val) / 3)

df.a <- df.a |>
  filter(!is.na(key))

df.a$struct_effect <- paste0(df.a$structure, df.a$E)

# Better
df.a$val <- ifelse(
  df.a$struct_effect == "conjunctive0",
  df.a$val / 4,
  ifelse(
    df.a$struct_effect == "disjunctive0",
    df.a$val / 4,
    ifelse(df.a$struct_effect == "disjunctive1", df.a$val / 3, df.a$val)
  )
)


# This is the two-panel plot split for Effect, for the full model, to compare structures where E=0 with E=1:

pk_a <- ggplot(df.a, aes(y = val, x = key, fill = Actual)) +
  stat_summary(
    fun.y = mean,
    geom = "bar",
    position = position_dodge(),
    colour = 'black'
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(.9),
    width = .2
  ) +
  labs(x = 'Response', y = 'Proportion/Prediction') +
  facet_grid(
    structure ~ E,
    labeller = labeller(E = function(x) paste("Effect =", x))
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.margin = margin(c(0, 0, 0, 0)),
    axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 0, l = 0))
  )

print(pk_a)

ggsave(
  filename = "pk_a.pdf",
  plot = pk_a,
  path = here("Other", "Plots"),
  width = 6,
  height = 6,
  units = "in"
)
