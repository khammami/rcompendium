## should_create_file() ----

test_that("should_create_file() works - file not exists", {
  with_local_project({
    invisible(file.create(".here"))

    expect_true(
      should_create_file("README", overwrite = FALSE)
    )

    expect_true(
      should_create_file("README", overwrite = TRUE)
    )
  })
})

test_that("should_create_file() works - file exists", {
  with_local_project({
    invisible(file.create(".here"))
    invisible(file.create("README"))

    expect_false(
      should_create_file("README", overwrite = FALSE)
    )

    expect_true(
      should_create_file("README", overwrite = TRUE)
    )
  })
})
