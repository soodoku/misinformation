verify_sources()
corpus <- read_corpus()
july <- read_july()
prepared <- list(
  corpus = corpus,
  items = code_features(score_corpus(corpus)),
  july = july,
  mc = mc_answers(july),
  scale = scale_answers(july)
)
saveRDS(prepared, prepared_data_file)
