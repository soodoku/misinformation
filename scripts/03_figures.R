shares <- read_tab("arm_shares.csv") |>
  dplyr::filter(measure %in% names(arm_labels)) |>
  dplyr::mutate(
    measure = factor(measure, levels = names(arm_labels)),
    question = forcats::fct_reorder(question_labels[question], estimate * (measure == "media"), .fun = max)
  )

arm_plot <- ggplot2::ggplot(shares, ggplot2::aes(estimate, question, colour = measure)) +
  geom_estimate(position = ggplot2::position_dodge(width = 0.75)) +
  ggplot2::scale_colour_manual(values = arm_colours, labels = arm_labels, name = NULL) +
  ggplot2::scale_x_continuous(labels = scales::label_percent(), limits = c(0, 0.95), expand = c(0, 0)) +
  ggplot2::labs(x = "Share answering incorrectly", y = NULL) +
  theme_paper() +
  ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2, reverse = TRUE)) +
  ggplot2::theme(legend.position = "top", legend.justification = "left")
save_evidence(arm_plot, "arms")

contrasts <- read_tab("arm_contrasts.csv") |>
  dplyr::mutate(
    key = paste(from, "->", to),
    label = factor(contrast_labels[key], levels = rev(contrast_labels))
  )

contrast_plot <- ggplot2::ggplot(contrasts, ggplot2::aes(estimate, label)) +
  geom_zero() +
  geom_estimate() +
  ggplot2::scale_x_continuous(labels = \(x) sprintf("%+d", round(100 * x))) +
  ggplot2::labs(x = "Change in share incorrect (percentage points)", y = NULL) +
  theme_paper()
save_evidence(contrast_plot, "contrasts")
