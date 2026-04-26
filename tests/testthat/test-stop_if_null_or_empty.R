## stop_if_null_or_empty() ----

test_that("stop_if_null_or_empty() errors", {
  expect_error(
    stop_if_null_or_empty(NULL, "given"),
    "Argument 'given' is required but is NULL or empty.",
    fixed = TRUE
  )

  expect_error(
    stop_if_null_or_empty("", "given"),
    "Argument 'given' is required but is NULL or empty.",
    fixed = TRUE
  )

  expect_error(
    stop_if_null_or_empty(character(0), "given"),
    "Argument 'given' is required but is NULL or empty.",
    fixed = TRUE
  )
})

test_that("stop_if_null_or_empty() works", {
  expect_silent(
    stop_if_null_or_empty("John", "given")
  )

  expect_null(
    x <- stop_if_null_or_empty("John", "given")
  )
})
