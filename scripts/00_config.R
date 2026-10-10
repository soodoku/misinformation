project_root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
project_file <- function(...) file.path(project_root, ...)
raw_dir <- project_file("data", "raw")
derived_dir <- project_file("data", "derived")
table_dir <- project_file("tabs")
figure_dir <- project_file("figs")
prepared_data_file <- file.path(derived_dir, "prepared_data.rds")
figure_sizes <- list(arms = c(width = 6.5, height = 6), contrasts = c(width = 6.5, height = 2.8))
figure_dpi <- 180
reference_colour <- "grey60"
table_style <- list(font_size = "small", column_padding = "6pt", row_stretch = 1)

raw_files <- c(
  media_poll_items_2018.csv = "22a61c3847d92d48edb801e52be9a753f43e43f9165a044af6bcb0aa71bc25b3",
  roper_toplines.csv = "8a390f7537c50ab228f3172092cfffbc938577d366c4fc865ec47ad9c0e62bcc",
  mturk_july_2017.csv = "62aa2380baf0fcb4ca7aeb64d552de597c2927d61289d27146072e70f329fa67"
)

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

contrast_labels <- c(
  "media -> pyrite" = "Add background and the claim (leading version)",
  "media -> fewer_substantive" = "Offer DK, ask for DK, drop background",
  "fewer_substantive -> dk_offered" = "Then drop the claim",
  "media -> dk_offered" = "All multiple-choice changes together",
  "dk_offered -> scale_strict" = "Replace multiple choice with the 0-10 scale",
  "media -> scale_strict" = "Media-poll wording to 0-10 scale"
)
theme_paper <- function() {
  ggplot2::theme_minimal(base_size = 11, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot"
    )
}
