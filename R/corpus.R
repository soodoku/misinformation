read_strict_csv <- function(path, ...) {
  data <- readr::read_csv(path, show_col_types = FALSE, ...)
  if (nrow(readr::problems(data)) > 0) stop("Parsing problems in ", path)
  data
}

# The 2018 corpus was saved as Windows-1252; question text is read as such.
read_corpus <- function() {
  path <- file.path("data", "raw", "media_poll_items_2018.csv")
  read_strict_csv(path, locale = readr::locale(encoding = "windows-1252"), name_repair = "minimal") |>
    dplyr::rename(id = 1) |>
    dplyr::select(id, question, n_options, mode) |>
    dplyr::inner_join(read_strict_csv("docs/item_decisions.csv"), by = c("id", "mode")) |>
    assertr::assert(assertr::is_uniq, id) |>
    assertr::verify(length(id) == 180)
}

normalize_response <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("\\s*\\((correct|correct answer)\\)|-correct", "") |>
    stringr::str_squish()
}

# Rules apply in file order and the first match wins, so topic-specific
# exceptions sit above the generic don't-know rule.
key_response <- function(id, topic, response, rules, numeric) {
  text <- normalize_response(response)
  purrr::pmap_chr(list(id, topic, text), \(i, t, s) {
    if (i %in% numeric$id) {
      if (stringr::str_detect(s, rules$pattern[rules$topic == "*"])) {
        return("dk")
      }
      if (stringr::str_detect(s, "other \\(vol")) {
        return("dk")
      }
      pattern <- numeric$correct_pattern[numeric$id == i]
      return(if (stringr::str_detect(s, pattern)) "correct" else "incorrect")
    }
    applicable <- rules[rules$topic %in% c(t, "*"), ]
    hit <- purrr::detect_index(applicable$pattern, \(p) stringr::str_detect(s, p))
    if (hit == 0) stop("No key for item ", i, ": ", s)
    applicable$key[hit]
  })
}

score_corpus <- function(corpus) {
  kept <- dplyr::filter(corpus, keep)
  rules <- read_strict_csv("docs/response_keys.csv")
  numeric <- read_strict_csv("docs/numeric_keys.csv")
  toplines <- read_strict_csv(file.path("data", "raw", "roper_toplines.csv"), col_types = readr::cols(RespPct = "c")) |>
    dplyr::semi_join(kept, by = "id") |>
    dplyr::left_join(dplyr::select(kept, id, topic), by = "id") |>
    # Roper prints "*" for shares under half a percent.
    dplyr::mutate(
      RespPct = dplyr::if_else(RespPct == "*", 0.25, suppressWarnings(as.numeric(RespPct))),
      key = key_response(id, topic, RespTxt, rules, numeric)
    ) |>
    assertr::assert(assertr::not_na, RespPct) |>
    dplyr::group_by(id, key) |>
    dplyr::summarise(share = sum(RespPct), .groups = "drop") |>
    tidyr::pivot_wider(names_from = key, values_from = share, values_fill = 0) |>
    dplyr::mutate(basis = "Roper topline")
  manual <- read_strict_csv("docs/manual_shares.csv") |>
    dplyr::select(id, correct, incorrect, dk, confident_correct, confident_incorrect, basis)
  scored <- dplyr::bind_rows(toplines, manual) |>
    dplyr::mutate(total = correct + incorrect + dk)
  missing <- setdiff(kept$id, scored$id)
  if (length(missing) > 0) stop("Kept items without shares: ", paste(missing, collapse = ", "))
  kept |>
    dplyr::inner_join(scored, by = "id") |>
    assertr::assert(assertr::within_bounds(88, 102), total) |>
    dplyr::mutate(dplyr::across(c(correct, incorrect, dk), \(x) x / total))
}

# Opinion-inviting phrases tell respondents a guess will do. "Do you (happen
# to) know" asks for knowledge and invites a don't-know, so it is not counted.
opinion_phrases <- c(
  "do you think", "do you believe", "do you personally (think|believe)", "to the best of your knowledge",
  "as far as you know", "from what you('ve| have)? (read|heard|know)",
  "based on what you('ve| have)? (read|heard|learned|know)",
  "comes? closest to your view", "your personal opinion", "your best guess", "what do you think", "your impression"
)

code_features <- function(scored) {
  pattern <- paste(opinion_phrases, collapse = "|")
  scored |>
    dplyr::mutate(
      text = stringr::str_to_lower(question),
      opinion_wording = stringr::str_detect(text, pattern),
      claim_cue = !is.na(claim_cue) & claim_cue != "",
      graded = !is.na(confident_incorrect),
      format = dplyr::case_when(
        graded ~ "graded",
        n_options == 2 ~ "binary",
        .default = "three or more"
      )
    ) |>
    dplyr::select(-text)
}

corpus_features <- function(items) {
  features <- c(
    dk_shown = "Explicit don't-know option", opinion_wording = "Opinion-inviting wording",
    claim_cue = "Stem repeats the false claim", binary = "Two substantive options"
  )
  items |>
    dplyr::mutate(binary = format == "binary") |>
    tidyr::pivot_longer(dplyr::all_of(names(features)), names_to = "feature", values_to = "present") |>
    dplyr::group_by(feature) |>
    dplyr::summarise(n = dplyr::n(), successes = sum(present), .groups = "drop") |>
    dplyr::mutate(label = features[feature], wilson(successes, n))
}

# Observational: items differ in topic and era as well as design, so the
# comparison holds topic fixed. Repeated wordings make items dependent;
# standard errors are clustered on the question text.
corpus_regression <- function(items) {
  data <- dplyr::mutate(items, wording = stringr::str_squish(stringr::str_to_lower(question)))
  purrr::map(c("incorrect", "dk"), \(outcome) {
    fit <- lm(reformulate(c("dk_shown", "opinion_wording", "claim_cue", "format", "topic"), outcome), data = data)
    vcov <- sandwich::vcovCL(fit, cluster = data$wording, type = "HC1")
    terms <- c("dk_shownTRUE", "opinion_wordingTRUE", "claim_cueTRUE", "formatgraded", "formatthree or more")
    tibble::tibble(
      outcome = outcome, term = terms, estimate = coef(fit)[terms],
      std_error = sqrt(diag(vcov)[terms]), items = nrow(data), wordings = dplyr::n_distinct(data$wording)
    )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(lower = estimate - qnorm(0.975) * std_error, upper = estimate + qnorm(0.975) * std_error)
}

# Items that let respondents say how sure they are show how much of the
# wrong side is confident.
graded_items <- function(items) {
  items |>
    dplyr::filter(graded) |>
    dplyr::transmute(
      id, topic, polling_firm, date, question,
      incorrect,
      confident_incorrect = confident_incorrect / total
    )
}
