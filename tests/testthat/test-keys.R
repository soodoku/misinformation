root <- "../.."
purrr::walk(list.files(file.path(root, "R"), full.names = TRUE), source)
rules <- readr::read_csv(file.path(root, "docs", "response_keys.csv"), show_col_types = FALSE)
numeric <- readr::read_csv(file.path(root, "docs", "numeric_keys.csv"), show_col_types = FALSE)
key <- \(topic, response, id = 0) key_response(id, topic, response, rules, numeric)

test_that("response rules handle the cases that tripped the old coding", {
  expect_equal(key("Russia Meddling", "Yes, Russia interfered but not to help Trump"), "incorrect")
  expect_equal(key("Russia Meddling", "Yes, Russia interfered to help Trump"), "correct")
  expect_equal(key("Obama Citizenship", "Not sure which state in the US"), "correct")
  expect_equal(key("Obama Citizenship", "Not sure which country"), "incorrect")
  expect_equal(key("Obama Citizenship", "It is not clear whether Obama was born in the US or not"), "dk")
  expect_equal(key("Obama Religion", "Not too well"), "correct")
  expect_equal(key("Obama Religion", "Don't know-heard different things (Vol.)"), "dk")
})

test_that("numeric items are keyed to the figure on the fielding date", {
  expect_equal(key("Iraq Mil. Deaths", "Around 3,000 (Correct)", 156), "correct")
  expect_equal(key("Iraq Mil. Deaths", "Around 4,000", 156), "incorrect")
  expect_equal(key("Iraq Mil. Deaths", "Other (Vol.)", 156), "dk")
})

test_that("unmatched responses stop the pipeline", {
  expect_error(key("Obama Religion", "zzz"), "No key")
})

test_that("the survey file carries no direct identifiers", {
  path <- file.path(root, "data", "raw", "mturk_july_2017.csv")
  data <- readr::read_csv(path, col_types = readr::cols(.default = "c"))
  data <- data[-(1:2), ]
  for (column in c("IPAddress", "LocationLatitude", "LocationLongitude", "city", "postal_code")) {
    expect_true(all(is.na(data[[column]])), label = paste(column, "is blank"))
  }
  cells <- stats::na.omit(unlist(dplyr::select(data, -dplyr::matches("Version$")), use.names = FALSE))
  expect_false(any(stringr::str_detect(cells, "(?<![\\d.])(?:\\d{1,3}\\.){3}\\d{1,3}(?![\\d.])")))
  expect_false(any(stringr::str_detect(cells, "[\\w.+-]+@[\\w-]+\\.[A-Za-z]{2,}")))
})
