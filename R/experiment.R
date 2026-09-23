july_items <- c("birth", "religion", "illegal", "death", "increase", "science", "fraud", "mmr", "deficit")

# Arm names in the data, and the prefix of each arm's item columns.
arms <- tibble::tribble(
  ~question_type, ~arm, ~prefix,
  "RW", "media", "rw_",
  "IPS", "pyrite", "ips_",
  "FSR", "fewer_substantive", "fsr_",
  "14k", "dk_offered", "14k_",
  "24k", "scale", NA
)

normalize_text <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", " ") |>
    stringr::str_squish()
}

# Qualtrics puts two label rows under the header. The sample is U.S. MTurk
# workers who consented, finished, and received a completion code.
read_july <- function() {
  path <- file.path("data", "raw", "mturk_july_2017.csv")
  names <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE))
  readr::read_csv(path, skip = 3, col_names = names, col_types = readr::cols(.default = "c")) |>
    dplyr::filter(Finished == "True", consent == "I agree", ccode == "US", !is.na(mTurkCode)) |>
    dplyr::inner_join(arms, by = "question_type") |>
    assertr::assert(assertr::is_uniq, ResponseId)
}

classify_mc <- function(response, options) {
  text <- normalize_text(response)
  matched <- purrr::map_int(text, \(s) {
    if (is.na(s)) {
      return(NA_integer_)
    }
    hits <- which(stringr::str_detect(s, options$pattern))
    if (length(hits) != 1) stop("Response matches ", length(hits), " options: ", s)
    hits
  })
  dplyr::case_when(
    is.na(text) ~ "no answer",
    is.na(options$correct[matched]) ~ "don't know",
    options$correct[matched] ~ "correct",
    .default = "incorrect"
  )
}

mc_answers <- function(july) {
  options <- read_strict_csv("docs/mc_options.csv")
  asked <- dplyr::filter(july, arm != "scale")
  # Each respondent has answers only in their own arm's columns.
  purrr::map(july_items, \(item) {
    columns <- paste0(stats::na.omit(arms$prefix), item)
    tibble::tibble(
      respondent = asked$ResponseId, arm = asked$arm, pid = asked$pid, question = item,
      response = dplyr::coalesce(!!!unname(as.list(asked[columns])))
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::group_by(question) |>
    dplyr::group_modify(\(d, key) {
      dplyr::mutate(d, outcome = classify_mc(response, dplyr::filter(options, question == key$question)))
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(incorrect = outcome == "incorrect", dont_know = outcome %in% c("don't know", "no answer"))
}

# On the scale, misinformation is a rating at the wrong end (strict: 0 or 10;
# lenient: also 1 or 9). "Not applicable" is the scale's don't know.
scale_answers <- function(july) {
  statements <- read_strict_csv("docs/scale_statements.csv")
  july |>
    dplyr::filter(arm == "scale") |>
    dplyr::select(respondent = ResponseId, arm, pid, dplyr::all_of(statements$column)) |>
    tidyr::pivot_longer(-c(respondent, arm, pid), names_to = "column", values_to = "rating") |>
    dplyr::inner_join(statements, by = "column") |>
    dplyr::mutate(
      rating = as.numeric(rating),
      toward_truth = dplyr::if_else(truth, rating, 10 - rating),
      strict = dplyr::coalesce(toward_truth == 0, FALSE),
      lenient = dplyr::coalesce(toward_truth <= 1, FALSE),
      dont_know = is.na(rating)
    ) |>
    assertr::assert(assertr::within_bounds(0, 10), rating)
}

wilson <- function(successes, n, z = qnorm(0.975)) {
  p <- successes / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  tibble::tibble(estimate = p, lower = centre - half, upper = centre + half)
}

arm_shares <- function(mc, scale) {
  measured <- dplyr::bind_rows(
    dplyr::transmute(mc, respondent, measure = arm, question, value = incorrect),
    dplyr::transmute(scale, respondent, measure = "scale_strict", question, value = strict),
    dplyr::transmute(scale, respondent, measure = "scale_lenient", question, value = lenient)
  )
  measured |>
    dplyr::group_by(measure, question) |>
    dplyr::summarise(n = dplyr::n(), successes = sum(value), .groups = "drop") |>
    dplyr::mutate(wilson(successes, n))
}

dk_shares <- function(mc, scale) {
  dplyr::bind_rows(
    dplyr::transmute(mc, measure = arm, question, dont_know),
    dplyr::transmute(scale, measure = "scale", question, dont_know)
  ) |>
    dplyr::group_by(measure, question) |>
    dplyr::summarise(dont_know = mean(dont_know), n = dplyr::n(), .groups = "drop")
}

# Pooled over the nine questions: the difference between two arms is the
# coefficient on the arm indicator with question fixed effects, clustered by
# respondent (each answered all nine).
pooled_contrast <- function(mc, scale, from, to) {
  stacked <- dplyr::bind_rows(
    dplyr::transmute(mc, respondent, measure = arm, question, value = as.numeric(incorrect)),
    dplyr::transmute(scale, respondent, measure = "scale_strict", question, value = as.numeric(strict))
  ) |>
    dplyr::filter(measure %in% c(from, to)) |>
    dplyr::mutate(treated = as.numeric(measure == to))
  fit <- lm(value ~ treated + question, data = stacked)
  se <- sqrt(sandwich::vcovCL(fit, cluster = stacked$respondent, type = "HC1")["treated", "treated"])
  tibble::tibble(
    from = from, to = to, estimate = coef(fit)[["treated"]], std_error = se,
    lower = estimate - qnorm(0.975) * se, upper = estimate + qnorm(0.975) * se,
    base = mean(stacked$value[stacked$treated == 0])
  )
}
