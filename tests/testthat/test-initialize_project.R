## initialize_project() ----

test_that("initialize_project() errors", {
  with_local_project({
    path <- file.path(getwd(), "README")
    invisible(file.create(path))

    expect_error(
      initialize_project(quiet = TRUE),
      paste0(
        "The path '",
        getwd(),
        "' is not empty and does not appear to be an R project."
      ),
      fixed = TRUE
    )
  })
})

test_that("initialize_project() works - no additional files", {
  with_local_project({
    expect_no_message(initialize_project(quiet = TRUE))
    expect_null(x <- initialize_project(quiet = TRUE))
    expect_true(file.exists(".here"))
  })

  with_local_project({
    file.create(".here")
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("DESCRIPTION")
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    dir.create(".git")
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    dir.create(".vscode")
    file.create(file.path(".vscode", "settings.json"))
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("renv.lock")
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("pkgtest.Rproj")
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("_pkgdown.yaml")
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("_pkgdown.yml")
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("_quarto.yaml")
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("_quarto.yml")
    expect_no_message(initialize_project(quiet = TRUE))
  })
})

test_that("initialize_project() works - with additional files", {
  with_local_project({
    file.create(".here")
    path <- file.path(getwd(), "README")
    invisible(file.create(path))
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("DESCRIPTION")
    path <- file.path(getwd(), "README")
    invisible(file.create(path))
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    dir.create(".git")
    path <- file.path(getwd(), "README")
    invisible(file.create(path))
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    dir.create(".vscode")
    path <- file.path(getwd(), "README")
    invisible(file.create(path))
    file.create(file.path(".vscode", "settings.json"))
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("renv.lock")
    path <- file.path(getwd(), "README")
    invisible(file.create(path))
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("pkgtest.Rproj")
    path <- file.path(getwd(), "README")
    invisible(file.create(path))
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("_pkgdown.yaml")
    path <- file.path(getwd(), "README")
    invisible(file.create(path))
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("_pkgdown.yml")
    path <- file.path(getwd(), "README")
    invisible(file.create(path))
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("_quarto.yaml")
    path <- file.path(getwd(), "README")
    invisible(file.create(path))
    expect_no_message(initialize_project(quiet = TRUE))
  })

  with_local_project({
    file.create("_quarto.yml")
    path <- file.path(getwd(), "README")
    invisible(file.create(path))
    expect_no_message(initialize_project(quiet = TRUE))
  })
})
