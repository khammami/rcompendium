## - All CLI output is suppressed via suppressMessages() — cli writes to the
##   message stream, so this is both correct and sufficient.
## - External calls (gert, gh) are mocked with local_mocked_bindings().
## - SSH tests avoid mocking base::file.exists() (causes hangs). Instead,
##   check_setup() is tested via a real temp directory written to disk, which
##   is reliable and fast.
## - Helpers use expr-first signatures so bare { } blocks bind correctly.

# helpers ----

make_git_cfg <- function(name = NULL, email = NULL, gh_user = NULL) {
  rows <- list()
  if (!is.null(name)) {
    rows <- c(rows, list(list(name = "user.name", value = name)))
  }
  if (!is.null(email)) {
    rows <- c(rows, list(list(name = "user.email", value = email)))
  }
  if (!is.null(gh_user)) {
    rows <- c(rows, list(list(name = "github.user", value = gh_user)))
  }
  if (!length(rows)) {
    return(data.frame(
      name = character(),
      value = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    name = vapply(rows, `[[`, character(1L), "name"),
    value = vapply(rows, `[[`, character(1L), "value"),
    stringsAsFactors = FALSE
  )
}

make_gh_info <- function(
  login = "jdoe",
  scopes = "repo, workflow",
  token = "ghp_fake"
) {
  list(
    login = login,
    html_url = paste0("https://github.com/", login),
    scopes = scopes,
    token = token
  )
}

## expr-first so bare { } blocks bind to the right argument
with_creds <- function(
  expr,
  given = "Jane",
  family = "Doe",
  email = "jane@example.com",
  orcid = NULL,
  protocol = NULL
) {
  opts <- list(
    rcompendium.given = given,
    rcompendium.family = family,
    rcompendium.email = email,
    rcompendium.orcid = orcid,
    rcompendium.protocol = protocol
  )
  withr::with_options(opts, expr)
}

## Silence all cli output for the duration of expr
quietly <- function(expr) suppressMessages(expr)

## Shared mock setup — full happy-path external state
mock_git_ok <- function() {
  local_mocked_bindings(
    git_config_global = function() make_git_cfg("Jane", "jane@x.com", "jdoe"),
    .package = "gert"
  )
}
mock_gh_ok <- function() {
  local_mocked_bindings(
    gh_whoami = function(...) make_gh_info(),
    .package = "gh"
  )
}
mock_gh_fail <- function() {
  local_mocked_bindings(
    gh_whoami = function(...) stop("no token"),
    .package = "gh"
  )
}

# return contract ----

test_that("returns an invisible named logical vector", {
  mock_git_ok()
  mock_gh_ok()
  with_creds({
    vis <- withVisible(quietly(check_setup()))
    expect_true(is.logical(vis$value))
    expect_false(is.null(names(vis$value)))
    expect_false(vis$visible)
  })
})

# git installation ----

test_that("git_installed TRUE on a normal machine", {
  mock_git_ok()
  mock_gh_ok()
  with_creds({
    res <- quietly(check_setup())
    expect_true(res[["git_installed"]])
  })
})

test_that("git_installed FALSE short-circuits git config keys", {
  local_mocked_bindings(
    Sys.which = function(names) setNames(rep("", length(names)), names),
    .package = "base"
  )
  mock_gh_ok()
  with_creds({
    res <- quietly(check_setup())
    expect_false(res[["git_installed"]])
    expect_false(res[["git_user_name"]])
    expect_false(res[["git_user_email"]])
    expect_false(res[["git_github_user"]])
  })
})

# git global config ----

test_that("git config keys reflect what gert returns", {
  local_mocked_bindings(
    git_config_global = function() make_git_cfg("Jane", "jane@x.com"), # no gh_user
    .package = "gert"
  )
  mock_gh_ok()
  with_creds({
    res <- quietly(check_setup())
    expect_true(res[["git_user_name"]])
    expect_true(res[["git_user_email"]])
    expect_false(res[["git_github_user"]]) # missing → FALSE (soft)
  })
})

test_that("git_config_global() error treated as all-missing", {
  local_mocked_bindings(
    git_config_global = function() stop("oops"),
    .package = "gert"
  )
  mock_gh_ok()
  with_creds({
    res <- quietly(check_setup())
    expect_false(res[["git_user_name"]])
    expect_false(res[["git_user_email"]])
  })
})

# GitHub PAT ----

test_that("github_pat TRUE and pat_scopes TRUE with repo+workflow", {
  mock_git_ok()
  local_mocked_bindings(
    gh_whoami = function(...) make_gh_info(scopes = "repo, workflow"),
    .package = "gh"
  )
  with_creds({
    res <- quietly(check_setup())
    expect_true(res[["github_pat"]])
    expect_true(res[["pat_scopes"]])
  })
})

test_that("pat_scopes TRUE with repo-only (workflow is soft)", {
  mock_git_ok()
  local_mocked_bindings(
    gh_whoami = function(...) make_gh_info(scopes = "repo"),
    .package = "gh"
  )
  with_creds({
    res <- quietly(check_setup())
    expect_true(res[["github_pat"]])
    expect_true(res[["pat_scopes"]])
  })
})

test_that("pat_scopes FALSE when repo absent", {
  mock_git_ok()
  local_mocked_bindings(
    gh_whoami = function(...) make_gh_info(scopes = "read:user"),
    .package = "gh"
  )
  with_creds({
    res <- quietly(check_setup())
    expect_true(res[["github_pat"]])
    expect_false(res[["pat_scopes"]])
  })
})

test_that("github_pat FALSE when gh_whoami() errors", {
  mock_git_ok()
  mock_gh_fail()
  with_creds({
    res <- quietly(check_setup())
    expect_false(res[["github_pat"]])
    expect_false(res[["pat_scopes"]])
  })
})

test_that("github_pat FALSE when token is empty string", {
  mock_git_ok()
  local_mocked_bindings(
    gh_whoami = function(...) make_gh_info(token = ""),
    .package = "gh"
  )
  with_creds({
    res <- quietly(check_setup())
    expect_false(res[["github_pat"]])
  })
})

# rcompendium credentials ----

test_that("rprofile_credentials TRUE when required fields set", {
  mock_git_ok()
  mock_gh_ok()
  with_creds({
    res <- quietly(check_setup())
    expect_true(res[["rprofile_credentials"]])
  })
})

test_that("rprofile_credentials FALSE when any required field missing", {
  mock_git_ok()
  mock_gh_ok()
  for (drop in c(
    "rcompendium.given",
    "rcompendium.family",
    "rcompendium.email"
  )) {
    opts <- list(
      rcompendium.given = "Jane",
      rcompendium.family = "Doe",
      rcompendium.email = "jane@x.com"
    )
    opts[[drop]] <- NULL
    withr::with_options(opts, {
      res <- quietly(check_setup())
      expect_false(
        res[["rprofile_credentials"]],
        label = paste("rprofile_credentials with", drop, "= NULL")
      )
    })
  }
})

test_that("rprofile_credentials TRUE when optional orcid/protocol absent", {
  mock_git_ok()
  mock_gh_ok()
  withr::with_options(
    list(
      rcompendium.given = "Jane",
      rcompendium.family = "Doe",
      rcompendium.email = "jane@x.com",
      rcompendium.orcid = NULL,
      rcompendium.protocol = NULL
    ),
    {
      res <- quietly(check_setup())
      expect_true(res[["rprofile_credentials"]])
    }
  )
})

# SSH key ----

test_that("ssh_key absent from results when protocol is not ssh", {
  mock_git_ok()
  mock_gh_ok()
  # NULL protocol (default) and explicit "https" both skip the SSH block
  for (proto in list(NULL, "https")) {
    withr::with_options(
      list(
        rcompendium.given = "Jane",
        rcompendium.family = "Doe",
        rcompendium.email = "jane@x.com",
        rcompendium.protocol = proto
      ),
      {
        res <- quietly(check_setup())
        expect_false(
          "ssh_key" %in% names(res),
          label = paste("ssh_key absent for protocol =", deparse(proto))
        )
      }
    )
  }
})

test_that("ssh_key FALSE when protocol ssh and no key file exists", {
  mock_git_ok()
  mock_gh_ok()
  # Use a guaranteed-empty temp dir as the home so no ~/.ssh/id_*.pub exists
  empty_home <- withr::local_tempdir()
  withr::with_envvar(
    list(HOME = empty_home, USERPROFILE = empty_home),
    {
      # path.expand uses HOME on Linux/Mac; override it for the mock
      local_mocked_bindings(
        path.expand = function(path) {
          sub("^~", empty_home, path)
        },
        .package = "base"
      )
      withr::with_options(
        list(
          rcompendium.given = "Jane",
          rcompendium.family = "Doe",
          rcompendium.email = "jane@x.com",
          rcompendium.protocol = "ssh"
        ),
        {
          res <- quietly(check_setup())
          expect_true("ssh_key" %in% names(res))
          expect_false(res[["ssh_key"]])
        }
      )
    }
  )
})

test_that("ssh_key TRUE when key file exists", {
  mock_git_ok()
  mock_gh_ok()
  fake_home <- withr::local_tempdir()
  dir.create(file.path(fake_home, ".ssh"))
  writeLines(
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFakeKey you@example.com",
    file.path(fake_home, ".ssh", "id_ed25519.pub")
  )
  local_mocked_bindings(
    path.expand = function(path) sub("^~", fake_home, path),
    .package = "base"
  )
  withr::with_options(
    list(
      rcompendium.given = "Jane",
      rcompendium.family = "Doe",
      rcompendium.email = "jane@x.com",
      rcompendium.protocol = "ssh"
    ),
    {
      res <- quietly(check_setup())
      expect_true(res[["ssh_key"]])
    }
  )
})

# suggest_fixes ----

test_that("suggest_fixes = TRUE is the default", {
  expect_true(formals(check_setup)[["suggest_fixes"]])
})

test_that("suggest_fixes = FALSE runs without error", {
  mock_git_ok()
  mock_gh_fail()
  with_creds({
    expect_no_error(quietly(check_setup(suggest_fixes = FALSE)))
  })
})

# hard-fail / soft-fail classification ----

soft_keys <- c("git_github_user", "pat_scopes", "ssh_key")
hard_fails <- function(res) res[!names(res) %in% soft_keys & !res]

test_that("all hard checks pass in a fully configured environment", {
  mock_git_ok()
  local_mocked_bindings(
    gh_whoami = function(...) make_gh_info(scopes = "repo, workflow"),
    .package = "gh"
  )
  withr::with_options(
    list(
      rcompendium.given = "Jane",
      rcompendium.family = "Doe",
      rcompendium.email = "jane@x.com"
    ),
    {
      res <- quietly(check_setup())
      expect_equal(length(hard_fails(res)), 0L)
    }
  )
})

test_that("github_user and pat_scopes failures are soft (never hard)", {
  local_mocked_bindings(
    git_config_global = function() make_git_cfg("Jane", "jane@x.com"), # no github.user
    .package = "gert"
  )
  local_mocked_bindings(
    gh_whoami = function(...) make_gh_info(scopes = "read:user"), # bad scopes
    .package = "gh"
  )
  with_creds({
    res <- quietly(check_setup())
    expect_equal(length(hard_fails(res)), 0L)
  })
})

test_that("missing PAT is a hard failure", {
  mock_git_ok()
  mock_gh_fail()
  with_creds({
    res <- quietly(check_setup())
    expect_true("github_pat" %in% names(hard_fails(res)))
  })
})

test_that("missing credentials produce hard failures", {
  mock_git_ok()
  mock_gh_ok()
  withr::with_options(
    list(
      rcompendium.given = NULL,
      rcompendium.family = NULL,
      rcompendium.email = NULL
    ),
    {
      res <- quietly(check_setup())
      expect_true("rprofile_credentials" %in% names(hard_fails(res)))
    }
  )
})
