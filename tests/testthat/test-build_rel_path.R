## build_rel_path() ----

test_that("build_rel_path() works", {
  path <- file.path("README")

  output <- build_rel_path(path)
  expect_true(inherits(output, "character"))
  expect_equal(length(output), 1L)
  expect_equal(output, path)

  path <- file.path("subdir", "README")

  output <- build_rel_path(path)
  expect_true(inherits(output, "character"))
  expect_equal(length(output), 1L)
  expect_equal(output, path)
})
