purrr::walk(list.files("R", full.names = TRUE), source)

dir.create("figs", showWarnings = FALSE)

arm_labels <- c(
  media = "Media-poll wording",
  pyrite = "Leading version",
  fewer_substantive = "DK offered, claim kept",
  dk_offered = "DK offered",
  scale_strict = "0-10 scale, strict"
)
arm_colours <- c(
  media = "grey60", pyrite = "grey35", fewer_substantive = "#9ECAE1", dk_offered = "#4292C6", scale_strict = "#08306B"
)
question_labels <- c(
  birth = "Obama born in the U.S.", religion = "Obama is a Muslim",
  illegal = "ACA helps illegal immigrants buy insurance", death = "ACA creates death panels",
  increase = "Warming is human-caused", science = "Most scientists doubt warming",
  fraud = "Trump won most legal votes", mmr = "MMR vaccine causes autism",
  deficit = "Deficit has risen since 2012"
)

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
  theme_evidence() +
  ggplot2::guides(colour = ggplot2::guide_legend(ncol = 2, reverse = TRUE)) +
  ggplot2::theme(legend.position = "top", legend.justification = "left")
save_evidence(arm_plot, "figs/arms", width = 6.5, height = 6)

contrast_labels <- c(
  "media -> pyrite" = "Add background and the claim (leading version)",
  "media -> fewer_substantive" = "Offer DK, ask for DK, drop background",
  "fewer_substantive -> dk_offered" = "Then drop the claim",
  "media -> dk_offered" = "All multiple-choice changes together",
  "dk_offered -> scale_strict" = "Replace multiple choice with the 0-10 scale",
  "media -> scale_strict" = "Media-poll wording to 0-10 scale"
)
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
  theme_evidence()
save_evidence(contrast_plot, "figs/contrasts", width = 6.5, height = 2.8)
