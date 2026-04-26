## stop_if_not_logical() ----

test_that("stop_if_not_logical() errors", {
  quiet <- 12
  overwrite <- TRUE

  expect_error(
    stop_if_not_logical(quiet),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_logical(quiet, overwrite),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_logical(overwrite, quiet),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )

  quiet <- "string"
  overwrite <- TRUE

  expect_error(
    stop_if_not_logical(quiet),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_logical(quiet, overwrite),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_logical(overwrite, quiet),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )

  quiet <- NULL
  overwrite <- TRUE

  expect_error(
    stop_if_not_logical(quiet),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_logical(quiet, overwrite),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_logical(overwrite, quiet),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )

  quiet <- c(TRUE, TRUE)
  overwrite <- TRUE

  expect_error(
    stop_if_not_logical(quiet),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_logical(quiet, overwrite),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )

  expect_error(
    stop_if_not_logical(overwrite, quiet),
    "Argument 'quiet' must be a logical of length 1.",
    fixed = TRUE
  )
})

test_that("stop_if_not_logical() works", {
  quiet <- TRUE
  overwrite <- FALSE
  open <- FALSE

  expect_silent(stop_if_not_logical())
  expect_null(x <- stop_if_not_logical())

  expect_silent(stop_if_not_logical(quiet))
  expect_null(x <- stop_if_not_logical(quiet))

  expect_silent(stop_if_not_logical(quiet, overwrite, open))
  expect_null(x <- stop_if_not_logical(quiet, open, overwrite))
})
