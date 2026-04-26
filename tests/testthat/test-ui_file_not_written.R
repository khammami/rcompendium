## ui_file_not_written() ----

test_that("ui_file_not_written() works - verbose", {
  with_local_project({
    path <- file.path(".github", "dependabot.yaml")

    expect_message(ui_file_not_written(path))
    expect_null(x <- ui_file_not_written(path))

    expect_message(ui_file_not_written(path, quiet = FALSE))
    expect_null(x <- ui_file_not_written(path, quiet = FALSE))
  })
})

test_that("ui_file_not_written() works - quiet", {
  with_local_project({
    path <- file.path(".github", "dependabot.yaml")

    expect_silent(ui_file_not_written(path, quiet = TRUE))
    expect_null(x <- ui_file_not_written(path, quiet = TRUE))
  })
})
