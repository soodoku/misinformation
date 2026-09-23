purrr::walk(list.files("R", full.names = TRUE), source)

items <- read_tab("corpus_items.csv")
features <- read_tab("corpus_features.csv")
regression <- read_tab("corpus_regression.csv")
graded <- read_tab("corpus_graded.csv")
exclusions <- read_tab("corpus_exclusions.csv")
shares <- read_tab("arm_shares.csv")
dk <- read_tab("dk_shares.csv")
contrasts <- read_tab("arm_contrasts.csv")
sizes <- read_tab("arm_sizes.csv")

feature <- \(f) features$estimate[features$feature == f]
coef_of <- \(o, t) regression[regression$outcome == o & regression$term == t, ]
contrast <- \(f, t) contrasts[contrasts$from == f & contrasts$to == t, ]
share <- \(m, q) shares$estimate[shares$measure == m & shares$question == q]
mean_share <- \(m) mean(shares$estimate[shares$measure == m])
mean_dk <- \(m) mean(dk$dont_know[dk$measure == m])
n_arm <- \(a) sizes$n[sizes$arm == a]

values <- c(
  nCorpus = sum(exclusions$n), nKept = nrow(items), nDropped = sum(exclusions$n[!exclusions$keep]),
  nWordings = regression$wordings[1], nTopics = dplyr::n_distinct(items$topic),
  nFirms = dplyr::n_distinct(items$polling_firm),
  firstYear = min(as.integer(stringr::str_sub(items$date, -2))) + 2000,
  lastYear = max(as.integer(stringr::str_sub(items$date, -2))) + 2000,
  shareDkShown = pct(feature("dk_shown")), shareOpinion = pct(feature("opinion_wording")),
  shareBinary = pct(feature("binary")), shareCue = pct(feature("claim_cue")),
  nCue = features$successes[features$feature == "claim_cue"],
  dkShownDk = pts(coef_of("dk", "dk_shownTRUE")$estimate),
  dkShownDkLower = pts(coef_of("dk", "dk_shownTRUE")$lower),
  dkShownDkUpper = pts(coef_of("dk", "dk_shownTRUE")$upper),
  dkShownIncorrect = pts(coef_of("incorrect", "dk_shownTRUE")$estimate),
  dkShownIncorrectLower = pts(coef_of("incorrect", "dk_shownTRUE")$lower),
  dkShownIncorrectUpper = pts(coef_of("incorrect", "dk_shownTRUE")$upper),
  opinionDk = pts(-coef_of("dk", "opinion_wordingTRUE")$estimate),
  opinionDkLower = pts(-coef_of("dk", "opinion_wordingTRUE")$upper),
  opinionDkUpper = pts(-coef_of("dk", "opinion_wordingTRUE")$lower),
  nGraded = nrow(graded), gradedIncorrect = pct(mean(graded$incorrect)),
  gradedConfident = pct(mean(graded$confident_incorrect)),
  nMedia = n_arm("media"), nPyrite = n_arm("pyrite"), nFewer = n_arm("fewer_substantive"),
  nDkOffered = n_arm("dk_offered"), nScale = n_arm("scale"), nJuly = count(sum(sizes$n)),
  meanMedia = pct(mean_share("media")), meanPyrite = pct(mean_share("pyrite")),
  meanFewer = pct(mean_share("fewer_substantive")), meanDkOffered = pct(mean_share("dk_offered")),
  meanScale = pct(mean_share("scale_strict")), meanScaleLenient = pct(mean_share("scale_lenient")),
  dkFewer = pct(mean_dk("fewer_substantive")), dkDkOffered = pct(mean_dk("dk_offered")),
  dkScale = pct(mean_dk("scale")), dkMedia = pct(mean_dk("media"), 1),
  pyriteEffect = pts(contrast("media", "pyrite")$estimate),
  pyriteLower = pts(contrast("media", "pyrite")$lower), pyriteUpper = pts(contrast("media", "pyrite")$upper),
  dkEffect = pts(-contrast("media", "fewer_substantive")$estimate),
  dkLower = pts(-contrast("media", "fewer_substantive")$upper),
  dkUpper = pts(-contrast("media", "fewer_substantive")$lower),
  claimEffect = pts(contrast("fewer_substantive", "dk_offered")$estimate),
  claimLower = pts(contrast("fewer_substantive", "dk_offered")$lower),
  claimUpper = pts(contrast("fewer_substantive", "dk_offered")$upper),
  scaleEffect = pts(-contrast("dk_offered", "scale_strict")$estimate),
  scaleLower = pts(-contrast("dk_offered", "scale_strict")$upper),
  scaleUpper = pts(-contrast("dk_offered", "scale_strict")$lower),
  totalEffect = pts(-contrast("media", "scale_strict")$estimate),
  muslimMedia = pct(share("media", "religion")), muslimDk = pct(share("dk_offered", "religion")),
  muslimScale = pct(share("scale_strict", "religion")),
  illegalMedia = pct(share("media", "illegal")), illegalDk = pct(share("dk_offered", "illegal")),
  illegalScale = pct(share("scale_strict", "illegal")),
  deficitMedia = pct(share("media", "deficit")), deficitPyrite = pct(share("pyrite", "deficit")),
  deficitDk = pct(share("dk_offered", "deficit")), deficitFewer = pct(share("fewer_substantive", "deficit")),
  deficitScale = pct(share("scale_strict", "deficit"))
)
write_macros(values, "tabs/macros.tex")

features |>
  dplyr::arrange(dplyr::desc(estimate)) |>
  dplyr::transmute(
    label, successes, n,
    share = paste0(round(100 * estimate), " [", round(100 * lower), ", ", round(100 * upper), "]")
  ) |>
  write_table("tabs/features.tex", "lrrl", c("Feature", "Items", "Of", "Percent [95\\% CI]"))

graded |>
  dplyr::mutate(
    statement = stringr::str_extract(
      question, "(?<= - ).*?\\.(?= Do you)|^Do you think it is true or false that .*?\\?"
    ),
    statement = dplyr::coalesce(statement, question),
    statement = stringr::str_remove(statement, "^Do you think it is true or false that "),
    statement = paste0(toupper(substr(statement, 1, 1)), substring(statement, 2)),
    statement = latex_escape(stringr::str_remove(statement, "\\?$"))
  ) |>
  dplyr::transmute(statement, date, incorrect = round(100 * incorrect), confident = round(100 * confident_incorrect)) |>
  write_table("tabs/graded.tex", "p{8.5cm}lrr", c("Statement", "Fielded", "Wrong side", "Definitely wrong"))

read_strict_csv("docs/item_decisions.csv") |>
  dplyr::filter(!keep) |>
  dplyr::count(drop_category) |>
  dplyr::arrange(dplyr::desc(n)) |>
  dplyr::transmute(reason = latex_escape(drop_category), n) |>
  write_table("tabs/exclusions.tex", "p{11cm}r", c("Reason for exclusion", "Items"))
