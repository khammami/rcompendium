## assert_file_not_exists_or_overwrite() ----

test_that("assert_file_not_exists_or_overwrite() errors", {
  with_local_project({
    initialize_project(quiet = TRUE)

    invisible(file.create("README"))

    expect_error(
      assert_file_not_exists_or_overwrite("README", overwrite = FALSE),
      paste0(
        "The file 'README' already exists. ",
        "To replace it, please use `overwrite = TRUE`."
      ),
      fixed = TRUE
    )
  })
})

test_that("assert_file_not_exists_or_overwrite() works - overwrite is TRUE", {
  with_local_project({
    initialize_project(quiet = TRUE)

    expect_silent(
      assert_file_not_exists_or_overwrite("README", overwrite = TRUE)
    )

    expect_null(
      x <- assert_file_not_exists_or_overwrite("README", overwrite = TRUE)
    )

    invisible(file.create("README"))

    expect_silent(
      assert_file_not_exists_or_overwrite("README", overwrite = TRUE)
    )

    expect_null(
      x <- assert_file_not_exists_or_overwrite("README", overwrite = TRUE)
    )
  })
})

test_that("assert_file_not_exists_or_overwrite() works - file not exists", {
  with_local_project({
    initialize_project(quiet = TRUE)

    expect_silent(
      assert_file_not_exists_or_overwrite("README", overwrite = FALSE)
    )

    expect_null(
      x <- assert_file_not_exists_or_overwrite("README", overwrite = FALSE)
    )
  })
})
