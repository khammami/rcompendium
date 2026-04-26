## ui_r_profile_content() ----

test_that("ui_r_profile_content() works", {
  with_local_project({
    r_profile <- file.path(getwd(), ".Rprofile")

    withr::local_envvar(
      list(R_PROFILE_USER = r_profile)
    )

    expect_no_message(suppressMessages(ui_r_profile_content(
      "This is a message"
    )))

    expect_null(
      x <- suppressMessages(ui_r_profile_content("This is a message"))
    )
  })
})
