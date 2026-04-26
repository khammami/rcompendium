## stop_if_not_string() ----

test_that("stop_if_not_string() errors", {
  given <- 12
  family <- "Doe"

  expect_error(
    stop_if_not_string(given),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_string(given, family),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_string(family, given),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )

  given <- TRUE
  family <- "Doe"

  expect_error(
    stop_if_not_string(given),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_string(given, family),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_string(family, given),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )

  given <- NULL
  family <- "Doe"

  expect_error(
    stop_if_not_string(given),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_string(given, family),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_string(family, given),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )

  given <- c("Marie", "Jeanne")
  family <- "Doe"

  expect_error(
    stop_if_not_string(given),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_string(given, family),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_string(family, given),
    "Argument 'given' must be a character of length 1.",
    fixed = TRUE
  )
})

test_that("stop_if_not_string() works", {
  given <- "John"
  family <- "Doe"
  orcid <- "0000-0000-0000-0000"

  expect_silent(stop_if_not_string())
  expect_null(x <- stop_if_not_string())

  expect_silent(stop_if_not_string(given))
  expect_null(x <- stop_if_not_string(given))

  expect_silent(stop_if_not_string(given, family, orcid))
  expect_null(x <- stop_if_not_string(given, orcid, family))
})
