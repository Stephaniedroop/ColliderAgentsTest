################################################################################
########### Functions to generate plots and other reporting figs ##############
#################################################################################

# -------------- Static variables -----------------
models <- c(
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
  'noActnoInfnoKindnoSelect'
)

row_labeller <- c(
  "A=.1,Au=.5,B=.8,Bu=.5" = ".1,.5,.8,.5",
  "A=.5,Au=.1,B=.5,Bu=.8" = ".5,.1,.5,.8",
  "A=.1,Au=.7,B=.8,Bu=.5" = ".1,.7,.8,.5"
)

column_labeller <- c(
  "Conjunctive: A=1,B=1,E=1" = "Conjunctive",
  "Disjunctive: A=1,B=1,E=1" = "Disjunctive"
)

# ---------------- Functions -----------------

# Main function for plotting any model and any pgroup
plot_model_pgroup <- function(model_colname, pgroup_label, df) {
  df_filtered <- df |>
    filter(pgroup == pgroup_label)
  ggplot(
    df_filtered,
    aes(x = node3, y = prop, fill = Observed, colour = Actual)
  ) +
    geom_bar(stat = 'identity') +
    geom_errorbar(aes(ymin = prop - SE, ymax = prop + SE), width = .2) +
    labs(
      x = 'Response',
      y = 'Proportion/Prediction',
      # title = sprintf("Model %s : %s", model_colname, pgroup_label)
    ) + # I uncommented the title for the paper because APA title in caption
    scale_fill_brewer(palette = "Set2") + # , labels = c("Observed \n(A|B)", "Unobserved \n(Au|Bu)")
    scale_colour_manual(values = c('gray', 'black')) +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +
    geom_point(aes(y = .data[[model_colname]]), colour = 'black') +
    geom_rect(
      data = subset(
        df_filtered,
        trial_structure_type %in%
          c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1")
      ),
      fill = NA,
      colour = "blue",
      xmin = -Inf,
      xmax = Inf,
      linewidth = 2,
      ymin = -Inf,
      ymax = Inf
    ) +
    facet_wrap(~trial_structure_type, ncol = 6) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 18),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.margin = margin(c(0, 0, 0, 0)),
      axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 0, l = 0)),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
}

# +
#   theme(
#     axis.text = element_text(size = 14),
#     axis.title = element_text(size = 18)
#   )

# Specific for Neil stanford talk to plot only model no participants
plot_nomodel_pgroup <- function(pgroup_label, df) {
  df_filtered <- df |>
    filter(pgroup == pgroup_label)
  ggplot(
    df_filtered,
    aes(x = node3, y = prop, fill = Observed, colour = Actual)
  ) +
    geom_bar(stat = 'identity') +
    geom_errorbar(aes(ymin = prop - SE, ymax = prop + SE), width = .2) +
    labs(
      x = 'Response',
      y = 'Proportion',
      title = sprintf("Participant responses at %s", pgroup_label)
    ) +
    scale_fill_brewer(palette = "Set2") + # , labels = c("Observed \n(A|B)", "Unobserved \n(Au|Bu)")
    scale_colour_manual(values = c('gray', 'black')) +
    guides(fill = guide_legend(override.aes = list(shape = NA))) +
    #geom_point(aes(y = .data[[model_colname]]), colour = 'black') +
    geom_rect(
      data = subset(
        df_filtered,
        trial_structure_type %in%
          c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1")
      ),
      fill = NA,
      colour = "blue",
      xmin = -Inf,
      xmax = Inf,
      linewidth = 2,
      ymin = -Inf,
      ymax = Inf
    ) +
    facet_wrap(~trial_structure_type, ncol = 6) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.margin = margin(c(0, 0, 0, 0)),
      axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 0, l = 0)),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
}

# A different function to compare two models on one plot
plot_two_models_pgroup <- function(model1, model2, pgroup_label, df) {
  df_filtered <- df |> filter(pgroup == pgroup_label)
  ggplot(
    df_filtered,
    aes(x = node3, y = prop, fill = Observed, colour = Actual)
  ) +
    geom_bar(stat = 'identity') +
    geom_errorbar(aes(ymin = prop - SE, ymax = prop + SE), width = .2) +
    # Model 1: black circles
    geom_point(
      aes(y = .data[[model1]]),
      colour = 'black',
      shape = 16,
      size = 2,
      alpha = 0.9,
      show.legend = TRUE
    ) +
    # Model 2: red triangles
    geom_point(
      aes(y = .data[[model2]]),
      colour = 'red',
      shape = 17,
      size = 2,
      alpha = 0.9,
      show.legend = TRUE
    ) +
    geom_rect(
      data = subset(
        df_filtered,
        trial_structure_type %in%
          c("Conjunctive: A=1,B=1,E=1", "Disjunctive: A=1,B=1,E=1")
      ),
      fill = NA,
      colour = "blue",
      xmin = -Inf,
      xmax = Inf,
      linewidth = 2,
      ymin = -Inf,
      ymax = Inf
    ) +
    facet_wrap(~trial_structure_type, ncol = 6) +
    labs(
      x = 'Response',
      y = 'Proportion/Prediction'
      # title = sprintf(
      #   "Models %s (black) & %s (red) at  %s",
      #   model1,
      #   model2,
      #   pgroup_label
      # )
    ) +
    scale_fill_brewer(palette = "Set2") + #, labels = c("Observed \n(A|B)", "Unobserved \n(Au|Bu)")) +
    scale_colour_manual(values = c('gray', 'black')) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 14),
      axis.title = element_text(size = 18),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      legend.margin = margin(c(0, 0, 0, 0)),
      axis.title.x = element_text(margin = margin(t = 1, r = 0, b = 0, l = 0)),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
}
