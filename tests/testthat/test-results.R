root <- "../.."
read_output <- \(name) readr::read_csv(file.path(root, "tabs", name), show_col_types = FALSE)
expect_near <- \(actual, expected, tolerance) expect_lte(abs(actual - expected), tolerance)

test_that("arm shares match the 2018 draft's Table 2 where the key is unchanged", {
  # Computed by the 2018 code, which keyed these items the same way. The
  # deficit item is re-keyed, and the draft missed media-wording answers to the
  # scientists item that carried trailing spaces, so both are left out.
  draft <- tibble::tribble(
    ~measure, ~birth, ~religion, ~illegal, ~death, ~increase, ~fraud, ~mmr,
    "media", .139, .274, .380, .338, .291, .354, .139,
    "pyrite", .144, .232, .316, .236, .260, .368, .124,
    "fewer_substantive", .083, .186, .158, .130, .229, .194, .099,
    "dk_offered", .073, .175, .228, .167, .207, .159, .081
  ) |>
    tidyr::pivot_longer(-measure, names_to = "question", values_to = "draft")
  shares <- read_output("arm_shares.csv") |> dplyr::inner_join(draft, by = c("measure", "question"))
  expect_equal(nrow(shares), 28)
  purrr::walk2(shares$estimate, shares$draft, \(a, e) expect_near(a, e, 0.0005))
})

test_that("corrected keys score the items as the text implies", {
  items <- read_output("corpus_items.csv")
  get <- \(i, column) items[[column]][items$id == i]
  # Selzer, September 2016: 75% born in the U.S., 11% not, 14% not sure.
  expect_near(get(1, "incorrect"), 0.11, 0.001)
  # Vote-tally tampering (false): the right side is "probably" plus "definitely not true".
  expect_near(get(251, "correct"), 0.62, 0.001)
  # A government-run plan alongside private plans is not in the law.
  expect_true(get(75, "incorrect") > get(75, "correct"))
})

test_that("every kept item has shares that sum to about 100", {
  items <- read_output("corpus_items.csv")
  expect_equal(nrow(items), 121)
  expect_true(all(abs(items$correct + items$incorrect + items$dk - 1) < 1e-9))
})
