## assert_project_file_detected() ----

test_that("assert_project_file_detected() works - Not a project", {
  with_local_project({
    expect_false(assert_project_file_detected())
  })
})

test_that("assert_project_file_detected() works - Is a project", {
  with_local_project({
    file.create(".here")
    expect_true(assert_project_file_detected())
  })

  with_local_project({
    file.create("DESCRIPTION")
    expect_true(assert_project_file_detected())
  })

  with_local_project({
    dir.create(".git")
    expect_true(assert_project_file_detected())
  })

  with_local_project({
    dir.create(".vscode")
    file.create(file.path(".vscode", "settings.json"))
    expect_true(assert_project_file_detected())
  })

  with_local_project({
    file.create("renv.lock")
    expect_true(assert_project_file_detected())
  })

  with_local_project({
    file.create("pkgtest.Rproj")
    expect_true(assert_project_file_detected())
  })

  with_local_project({
    file.create("_pkgdown.yaml")
    expect_true(assert_project_file_detected())
  })

  with_local_project({
    file.create("_pkgdown.yml")
    expect_true(assert_project_file_detected())
  })

  with_local_project({
    file.create("_quarto.yaml")
    expect_true(assert_project_file_detected())
  })

  with_local_project({
    file.create("_quarto.yml")
    expect_true(assert_project_file_detected())
  })
})
