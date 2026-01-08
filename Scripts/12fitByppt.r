#####################################################
########### Model fit by participant ################
#####################################################

library(tidyverse)
library(here)

set.seed(12)

load(here('Data', 'Data.Rdata')) # This is one big df, 2580, of 215 ppts

# Now select just the columns you need
byppt <- data |>
  select(subject_id, trial_id, node3)


# THIS IS JUST THE AGGREGATE FITTED PROBS. NOW REDO WITH BY-PPT FITTED PROBS. GO TO NEW SCRIPT
load(here('Data', 'ModelData', 'fitforplot16ig.rda')) # df, 288 of 35


ppts <- merge(byppt, df, by = c('trial_id', 'node3'))

# Want the model preds fitted to the aggregate group

# So, split df2 into ppts list
# Some ppts did see some conditions more than once - due to a counterbalancing error in the experiment design.

# Participant data
data_list <- split(ppts, ppts$subject_id) # 215 elements each of 12x14

# The models
model_names <- c(
  'full',
  'noAct',
  'noInf',
  'noSelect',
  'noActnoInf',
  'noActnoSelect',
  'noInfnoSelect',
  'noActnoInfnoSelect',
  'noKind',
  'noActnoKind',
  'noInfnoKind',
  'noKindnoSelect',
  'noActnoInfnoKind',
  'noActnoKindnoSelect',
  'noInfnoKindnoSelect',
  'noActnoInfnoKindnoSelect',
  'baseline'
)

# Number of parameters for each model (aligns with model_names)
n_params <- c(rep(3, 8), rep(2, 8), 0) # this gives 'baseline' 1 param; 0 might be better but we don't use it here

# Compute all NLLs once per subject, storing both full NLLs and min info together
results <- lapply(data_list, function(dat) {
  # data_list is the set of models appended 215 times, once to each participant
  nlls <- sapply(model_names, function(mod_col) {
    # we get the nlls of each model for each ppt data
    mpred <- dat[[mod_col]]
    -sum(log(mpred))
  })

  # Get BICs using stored parameter counts
  bics <- n_params * log(12) + 2 * nlls

  list(
    nlls = nlls,
    bics = bics,
    # min_nll = min(nlls),
    # min_model = names(nlls)[which.min(nlls)]
    min_bic = min(bics),
    min_model = names(bics)[which.min(bics)]
  )
})


# Extract min_model vector for later
best_models <- sapply(results, function(x) x$min_model)
best_models_df <- data.frame(model = best_models, stringsAsFactors = FALSE)

# Construct a dataframe of all NLLs per subject
# nll_matrix <- t(sapply(results, function(x) x$nlls))
# nll_df <- as.data.frame(nll_matrix)
# rownames(nll_df) <- names(results)

# Construct a dataframe of all BICs per subject
bic_matrix <- t(sapply(results, function(x) x$bic))
bic_df <- as.data.frame(bic_matrix)
rownames(bic_df) <- names(results)


# Count how many participants have each model as best fit
summary_table <- best_models_df |>
  group_by(model) |>
  summarise(participant_count = n()) |>
  arrange(desc(participant_count))


all_models <- data.frame(model = model_names)

summary_table_full <- all_models |>
  left_join(summary_table, by = "model") |>
  mutate(
    participant_count = ifelse(is.na(participant_count), 0, participant_count)
  )

print(summary_table_full)

sum(summary_table_full$participant_count) # 215


# What if this is no different from chance?
# run a chisq test on this distribution of counts, against an equal distribution
expected_counts <- rep(
  nrow(best_models_df) / length(model_names),
  length(model_names)
) # 12.6
chisq_test <- chisq.test(
  summary_table_full$participant_count,
  p = rep(1 / length(model_names), length(model_names))
)
print(chisq_test)
# X-squared = 216.8, df = 16, p-value = 2.2e-16

# Doesn't actually tell us much. Let's split into whether the model name mentions noAct or not
summary_table_full <- summary_table_full |>
  mutate(noAct = ifelse(grepl("noAct", model), "noAct", "Act"))
summary_table_full <- summary_table_full |>
  mutate(noInf = ifelse(grepl("noInf", model), "noInf", "Inf"))
summary_table_full <- summary_table_full |>
  mutate(noSelect = ifelse(grepl("noSelect", model), "noSelect", "Select"))
summary_table_full <- summary_table_full |>
  mutate(noKind = ifelse(grepl("noKind", model), "noKind", "Kind"))

# Remove the baseline row because no point
summary_table_full <- summary_table_full |>
  filter(model != "baseline")

# Now sum the participant count for each column of whether it is present or not
# Act no Act
noAct_table <- summary_table_full |>
  group_by(noAct) |>
  summarise(participant_count = sum(participant_count))
print(noAct_table)
# 108:84 Act:noAct

chisq_test_noAct <- chisq.test(noAct_table$participant_count, p = c(0.5, 0.5))
print(chisq_test_noAct) # x 3, p .08, no difference

# Inf no Inf
noInf_table <- summary_table_full |>
  group_by(noInf) |>
  summarise(participant_count = sum(participant_count))
print(noInf_table)
# 105:87 Inf:noInf

chisq_test_noInf <- chisq.test(noInf_table$participant_count, p = c(0.5, 0.5))
print(chisq_test_noInf) # x 1.69, p.19, no difference

# select no select
noSelect_table <- summary_table_full |>
  group_by(noSelect) |>
  summarise(participant_count = sum(participant_count))
print(noSelect_table)
# 101:93 Select:noSelect

chisq_test_noSelect <- chisq.test(
  noSelect_table$participant_count,
  p = c(0.5, 0.5)
)
print(chisq_test_noSelect) # x 30.1, p4.14e-8, significant difference

# kind no kind
noKind_table <- summary_table_full |>
  group_by(noKind) |>
  summarise(participant_count = sum(participant_count))
print(noKind_table)
# 110:84 Kind:noKind

chisq_test_noKind <- chisq.test(noKind_table$participant_count, p = c(0.5, 0.5))
print(chisq_test_noKind) # x 13.02, p.00031, significant difference

# a blocky mosaic chart to show the proportions
library(ggplot2)


# Simple df with Yes/No on top, then the four components down the side
combined_table <- data.frame(
  Component = rep(c("Act", "Inf", "Select", "Kind"), each = 2),
  Presence = rep(c("Yes", "No"), times = 4),
  Count = c(
    noAct_table$participant_count,
    noInf_table$participant_count,
    noSelect_table$participant_count,
    noKind_table$participant_count
  )
)
print(combined_table)

# Vertical, stacked, with label
p <- ggplot(data = combined_table) +
  geom_bar(
    aes(x = Component, y = Count, fill = Presence),
    stat = "identity",
    position = "fill"
  ) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  labs(
    title = "Proportion of participants best fit by models with/without each component",
    x = "Component",
    y = "Proportion of Participants"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  #scale_fill_manual(values = c("Yes" = "orange", "No" = "lightgrey")) +
  theme(legend.position = "top")

print(p)

# Horizontal, with label
p <- ggplot(data = combined_table) +
  geom_bar(
    aes(y = Component, x = Count, fill = Presence),
    stat = "identity",
    position = "fill"
  ) +
  geom_vline(xintercept = 0.5, linetype = "dotted") +
  geom_text(
    aes(y = Component, x = Count, label = Component, fill = Presence),
    stat = "identity",
    position = position_fill(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  labs(
    #title = "Proportion of participants best fit by models with/without each component",
    y = "Component",
    x = "Proportion of Participants"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "top", axis.text.y = element_blank())

combined_table$Component <- factor(
  combined_table$Component,
  levels = c("Act", "Inf", "Select", "Kind"),
  labels = c("Actual", "Inference", "Selection", "Kindness")
)

combined_table$label <- ifelse(
  combined_table$Presence == "No",
  paste0("no ", combined_table$Component),
  as.character(combined_table$Component)
)

# combined_table$Component <- factor(combined_table$Component,
#                                    levels = c("Selection", "Kindness",  "Inference", "Actual"))

# Horizontal, no label, but has No
p <- ggplot(data = combined_table) +
  geom_bar(
    aes(y = Component, x = Count, fill = Presence),
    stat = "identity",
    position = "fill"
  ) +
  geom_vline(xintercept = 0.5, linetype = "dotted") +
  geom_text(
    aes(y = Component, x = Count, label = label, fill = Presence),
    stat = "identity",
    position = position_fill(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  labs(y = "Component", x = "Proportion of Participants") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none", axis.text.y = element_blank())

print(p)

ggsave(
  filename = "bestByComponent.pdf",
  plot = p,
  path = here("Other", "Plots"),
  width = 12,
  #height = 8,
  units = "in"
)
