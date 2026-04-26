## ensure_dir_exists() ----

test_that("ensure_dir_exists() works - dir not exists", {
  with_local_project({
    invisible(file.create(".here"))

    path <- build_full_path(file.path("R"))

    expect_silent(ensure_dir_exists(path))
    expect_null(x <- ensure_dir_exists(path))

    expect_true(dir.exists(path))

    path <- build_full_path(file.path("tests", "testthat"))

    expect_silent(ensure_dir_exists(path))
    expect_null(x <- ensure_dir_exists(path))

    expect_true(dir.exists(path))
  })
})

test_that("ensure_dir_exists() works - dir exists", {
  with_local_project({
    invisible(file.create(".here"))

    path <- build_full_path(file.path("man"))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    expect_silent(ensure_dir_exists(path))
    expect_null(x <- ensure_dir_exists(path))

    expect_true(dir.exists(path))

    path <- build_full_path(file.path("man", "figures"))
    dir.create(path, recursive = TRUE, showWarnings = FALSE)

    expect_silent(ensure_dir_exists(path))
    expect_null(x <- ensure_dir_exists(path))

    expect_true(dir.exists(path))
  })
})
