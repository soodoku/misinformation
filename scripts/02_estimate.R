write_output <- \(x, name) readr::write_csv(x, file.path(table_dir, name), na = "")

prepared <- readRDS(prepared_data_file)
corpus <- prepared$corpus
items <- prepared$items
write_output(items, "corpus_items.csv")
write_output(corpus_features(items), "corpus_features.csv")
write_output(corpus_regression(items), "corpus_regression.csv")
write_output(graded_items(items), "corpus_graded.csv")
write_output(dplyr::count(corpus, keep, drop_category, drop_reason), "corpus_exclusions.csv")

july <- prepared$july
mc <- prepared$mc
scale <- prepared$scale
write_output(arm_shares(mc, scale), "arm_shares.csv")
write_output(dk_shares(mc, scale), "dk_shares.csv")
contrasts <- tibble::tribble(
  ~from, ~to,
  "media", "pyrite",
  "media", "fewer_substantive",
  "fewer_substantive", "dk_offered",
  "media", "dk_offered",
  "dk_offered", "scale_strict",
  "media", "scale_strict"
)
purrr::pmap(contrasts, \(from, to) pooled_contrast(mc, scale, from, to)) |>
  purrr::list_rbind() |>
  write_output("arm_contrasts.csv")
write_output(dplyr::count(july, arm), "arm_sizes.csv")
