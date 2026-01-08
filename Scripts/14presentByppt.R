#=========================================================
#  Interpret and present fit-by-ppt results
# =========================================================

library(tidyverse)
library(here)
library(ggplot2)


set.seed(12)

# Load results
load(here('Data', 'modelData', 'individFitsNNN.rda'))


# Data from optimUtils script but cba loading that whole thing again so let's copy them in again
model_names <- c(
  'full',
  'noAct',
  'noInf',
  'noSelect',
  'noActnoInf',
  'noActnoSelect',
  'noInfnoSelect',
  'noActnoInfnoSelect'
)

model_names2 <- c(
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

model_names_remove <- c(
  'noInf',
  'noActnoInf',
  'noInfnoSelect',
  'noActnoInfnoSelect'
)

model_names_keep <- c(
  'full',
  'noAct',
  'noSelect',
  'noActnoSelect'
)

# Remove model_names_remove from every item in results_3par
results_3par2 <- lapply(results_3par, function(res_list) {
  res_list[[1]] <- res_list[[1]] |>
    filter(model %in% model_names_keep)
  res_list
})


# Now unlist to access, then format and store the results
best_models <- lapply(seq_along(results_2par), function(i) {
  # extract numeric BICs for this participant
  b2 <- as.numeric(results_2par[[i]][[1]]$BIC)
  b3 <- as.numeric(results_3par2[[i]][[1]]$BIC)

  # extract model names from each fit
  n2 <- results_2par[[i]][[1]]$model
  n3 <- results_3par2[[i]][[1]]$model

  # combine
  all_bics <- c(b2, b3)
  all_names <- c(n2, n3)

  # find best model
  best_idx <- which.min(all_bics)

  list(
    best_model = all_names[best_idx],
    best_bic = all_bics[best_idx],
    all_bics = all_bics,
    all_names = all_names
  )
})

# Store as df
best_df <- data.frame(
  subject_id = names(results_2par),
  best_model = sapply(best_models, `[[`, "best_model"),
  best_bic = sapply(best_models, `[[`, "best_bic"),
  stringsAsFactors = FALSE
)

# Define the order you want: 3-par models first, then 2-par
model_order <- c(model_names_keep, model_names2)

# Convert best_model to factor with custom levels
best_df$model <- factor(best_df$best_model, levels = model_order)

# Now summarise
summary_table <- best_df |>
  group_by(model) |>
  summarise(participant_count = n(), .groups = "drop")

print(sum(summary_table$participant_count)) # 215

# What if this is no different from chance?
# run a chisq test on this distribution of counts, against an equal distribution
expected_counts <- rep(
  nrow(best_df) / length(model_order),
  length(model_order)
) # 12.6
chisq_test <- chisq.test(
  summary_table$participant_count,
  p = rep(1 / length(model_order), length(model_order))
)
print(chisq_test)
# X-squared = 59.09, df = 12, p-value = 3.31e-08

# Doesn't actually tell us much. Let's split into whether the model name mentions noAct or not
summary_table <- summary_table |>
  mutate(noAct = ifelse(grepl("noAct", model), "noAct", "Act"))
summary_table <- summary_table |>
  mutate(noInf = ifelse(grepl("noInf", model), "noInf", "Inf"))
summary_table <- summary_table |>
  mutate(noSelect = ifelse(grepl("noSelect", model), "noSelect", "Select"))
summary_table <- summary_table |>
  mutate(noKind = ifelse(grepl("noKind", model), "noKind", "Kind"))

# Remove the baseline row because no point
summary_table2 <- summary_table |>
  filter(model != "baseline")

# Now sum the participant count for each column of whether it is present or not
# Act no Act
noAct_table <- summary_table2 |>
  group_by(noAct) |>
  summarise(participant_count = sum(participant_count))
print(noAct_table)
#

chisq_test_noAct <- chisq.test(noAct_table$participant_count, p = c(0.5, 0.5))
print(chisq_test_noAct) # x 3, p .08, no difference

# Inf no Inf
noInf_table <- summary_table2 |>
  group_by(noInf) |>
  summarise(participant_count = sum(participant_count))
print(noInf_table)
#

chisq_test_noInf <- chisq.test(
  noInf_table$participant_count,
  p = c(0.667, 0.333)
)
print(chisq_test_noInf) # x 6.23, p.013, no difference

# select no select
noSelect_table <- summary_table2 |>
  group_by(noSelect) |>
  summarise(participant_count = sum(participant_count))
print(noSelect_table)
#

chisq_test_noSelect <- chisq.test(
  noSelect_table$participant_count,
  p = c(0.5, 0.5)
)
print(chisq_test_noSelect) # x 30.1, p4.14e-8, significant difference

# kind no kind
noKind_table <- summary_table2 |>
  group_by(noKind) |>
  summarise(participant_count = sum(participant_count))
print(noKind_table)
# 110:84 Kind:noKind

chisq_test_noKind <- chisq.test(
  noKind_table$participant_count,
  p = c(0.333, 0.667)
)
print(chisq_test_noKind) # x 1.802, p.0180, no significant difference

# a blocky mosaic chart to show the proportions

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

combined_table$Component <- factor(
  combined_table$Component,
  levels = c("Act", "Inf", "Select", "Kind"),
  labels = c("ActualCause", "Inference", "Selection", "Kindness")
)

# combined_table$Presence <- factor(
#   combined_table$Presence,
#   levels = c("No", "Yes") # reversed
# )

combined_table$label <- ifelse(
  combined_table$Presence == "No",
  paste0("no ", combined_table$Component),
  as.character(combined_table$Component)
)

# Custom vlines because expected proportions are not .5 because some modules occur more frequuently:
vlines <- data.frame(
  Component = c("ActualCause", "Inference", "Selection", "Kindness"),
  x = c(0.5, 0.67, 0.5, 0.33) # your custom cutoffs
)

# A hack to get the y positions right for the vlines which indicate chance for the chisq
component_levels <- sort(unique(combined_table$Component))
vlines$y <- match(vlines$Component, component_levels)


# Horizontal, no label, but has No
p <- ggplot(data = combined_table) +
  geom_bar(
    aes(y = Component, x = Count, fill = Presence),
    stat = "identity",
    position = "fill"
  ) +
  geom_segment(
    data = vlines,
    aes(
      x = x,
      xend = x,
      y = y - 0.45,
      yend = y + 0.45
    ),
    linetype = "dotted",
    linewidth = 1
  ) +
  scale_y_discrete(limits = component_levels) +
  geom_text(
    aes(y = Component, x = Count, label = label, fill = Presence),
    stat = "identity",
    position = position_fill(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 5
  ) +
  labs(y = "Component", x = "Proportion of Participants") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  scale_fill_manual(values = c("Yes" = "#66c2a5", "No" = "#fc8d62")) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18)
  ) # This looks like the best way to get size right in saving plots

print(p)


ggsave(
  filename = "bestByComponent.pdf",
  plot = p,
  path = here("Other", "Plots"),
  width = 12,
  #height = 8,
  units = "in"
)
