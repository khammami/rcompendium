#' Check the rcompendium setup
#'
#' @description
#' Diagnoses whether the local environment is correctly configured to use
#' `rcompendium`. Checks git installation, global git config, GitHub PAT,
#' rcompendium credentials in `~/.Rprofile`, and SSH key registration (if
#' the `ssh` protocol is in use). Prints a colour-coded status report and
#' invisibly returns a named logical vector so callers can act on the result
#' programmatically.
#'
#' @param suggest_fixes `logical`. If `TRUE` (default), print the exact
#'   commands needed to fix every failing check.
#'
#' @return Invisibly, a named `logical` vector. Each element is `TRUE` when
#'   the corresponding check passed and `FALSE` otherwise. Names:
#'   `"git_installed"`, `"git_user_name"`, `"git_user_email"`,
#'   `"git_github_user"`, `"github_pat"`, `"pat_scopes"`,
#'   `"rprofile_credentials"`, `"ssh_key"` (only present when protocol is
#'   `"ssh"`).
#'
#' @export
#'
#' @family setup functions
#'
#' @examples
#' \dontrun{
#' library(rcompendium)
#'
#' check_setup()
#'
#' ## Suppress fix suggestions:
#' check_setup(suggest_fixes = FALSE)
#'
#' ## Act programmatically on the result:
#' status <- check_setup()
#' if (!status["github_pat"]) {
#'   cli::cli_alert_danger("You still need to set up a GitHub PAT.")
#' }
#' }
check_setup <- function(suggest_fixes = TRUE) {
  results <- logical(0)

  ## helpers ----

  # Emit a fix block only when the user wants suggestions.
  show_fix <- function(...) {
    if (!suggest_fixes) {
      return(invisible(NULL))
    }
    cli::cli_bullets(c(" " = " ", i = "To fix:"))
    cli::cli_code(...)
  }

  # Read a single key from git global config (returns NA if missing/empty).
  get_git_cfg <- function(cfg, key) {
    if (is.null(cfg)) {
      return(NA_character_)
    }
    val <- cfg$value[cfg$name == key]
    if (length(val) == 0L || !nzchar(val[[1L]])) NA_character_ else val[[1L]]
  }

  # Read a rcompendium option from the session (returns NA if missing/empty).
  read_option <- function(key) {
    val <- getOption(paste0("rcompendium.", key))
    if (!is.null(val) && nzchar(val)) val else NA_character_
  }

  ## 1. git installation ----

  cli::cli_h1("git")

  git_path <- Sys.which("git")
  git_installed <- nzchar(git_path)
  results["git_installed"] <- git_installed

  if (git_installed) {
    git_version <- tryCatch(
      {
        out <- system2("git", args = "--version", stdout = TRUE, stderr = FALSE)
        if (length(out) == 0) "unknown version" else out[[1]]
      },
      error = function(e) "unknown version"
    )
    cli::cli_alert_success(
      "git is installed {cli::col_silver(git_version)}"
    )
  } else {
    cli::cli_alert_danger("git is {.strong not} installed (or not on PATH)")
    if (suggest_fixes) {
      cli::cli_bullets(c(
        i = "Download from {.url https://git-scm.com/downloads} then restart R."
      ))
    }
    # Short-circuit remaining git checks
    results["git_user_name"] <- FALSE
    results["git_user_email"] <- FALSE
    results["git_github_user"] <- FALSE
  }

  ## 2. git global config ----

  cli::cli_h1("git global config")

  if (!git_installed) {
    cli::cli_alert_warning("Skipped {cli::col_silver('(git not available)')}")
  } else {
    git_cfg <- tryCatch(gert::git_config_global(), error = function(e) NULL)

    ## user.name ----
    user_name <- get_git_cfg(git_cfg, "user.name")
    git_user_name <- !is.na(user_name)
    results["git_user_name"] <- git_user_name

    if (git_user_name) {
      cli::cli_alert_success("{.field user.name} {cli::col_silver(user_name)}")
    } else {
      cli::cli_alert_danger("{.field user.name} is not set")
      show_fix('gert::git_config_global_set("user.name", "Jane Doe")')
    }

    ## user.email ----
    user_email <- get_git_cfg(git_cfg, "user.email")
    git_user_email <- !is.na(user_email)
    results["git_user_email"] <- git_user_email

    if (git_user_email) {
      cli::cli_alert_success(
        "{.field user.email} {cli::col_silver(user_email)}"
      )
    } else {
      cli::cli_alert_danger("{.field user.email} is not set")
      show_fix('gert::git_config_global_set("user.email", "jane@example.com")')
    }

    ## github.user (optional) ----
    gh_user_cfg <- get_git_cfg(git_cfg, "github.user")
    git_github_user <- !is.na(gh_user_cfg)
    results["git_github_user"] <- git_github_user

    if (git_github_user) {
      cli::cli_alert_success(
        "{.field github.user} {cli::col_silver(gh_user_cfg)}"
      )
    } else {
      cli::cli_alert_warning(
        "{.field github.user} not set {cli::col_silver('(optional, but recommended)')}"
      )
      show_fix('gert::git_config_global_set("github.user", "jdoe")')
    }
  }

  ## 3. GitHub PAT ----

  cli::cli_h1("GitHub PAT")

  gh_info <- tryCatch(gh::gh_whoami(), error = function(e) NULL)
  github_pat <- !is.null(gh_info) &&
    !is.null(gh_info[["token"]]) &&
    nzchar(gh_info[["token"]])
  results["github_pat"] <- github_pat

  if (github_pat) {
    login <- gh_info[["login"]]
    url <- gh_info[["html_url"]]
    cli::cli_alert_success("PAT valid for {.href [{login}]({url})}")

    ## Scopes ----
    scopes <- gh_info[["scopes"]]
    has_repo <- grepl("repo", scopes)
    has_workflow <- grepl("workflow", scopes)
    results["pat_scopes"] <- has_repo

    if (has_repo && has_workflow) {
      cli::cli_alert_success(
        "PAT scopes sufficient {cli::col_silver('({scopes})')}"
      )
    } else if (has_repo) {
      cli::cli_alert_warning(
        "PAT has {.field repo} but not {.field workflow} \u2013
        GitHub Actions CI setup will fail"
      )
      if (suggest_fixes) {
        cli::cli_bullets(c(
          i = "Regenerate at {.url https://github.com/settings/tokens} with
               {.val repo} + {.val workflow} scopes,
               then run {.run gitcreds::gitcreds_set()}."
        ))
      }
    } else {
      cli::cli_alert_danger(
        "PAT scopes insufficient {cli::col_silver('(found: {scopes})')} \u2013
        needs at least {.field repo}"
      )
      if (suggest_fixes) {
        cli::cli_bullets(c(
          i = "Regenerate at {.url https://github.com/settings/tokens} with
               {.val repo} + {.val workflow},
               then {.run gitcreds::gitcreds_set()}."
        ))
      }
    }
  } else {
    cli::cli_alert_danger("No GitHub PAT found")
    if (suggest_fixes) {
      cli::cli_bullets(c(
        "1" = "Create a token at {.url https://github.com/settings/tokens}
               with {.val repo} and {.val workflow} scopes.",
        "2" = "Store it: {.run gitcreds::gitcreds_set()}",
        "3" = "Restart R and re-run {.run rcompendium::check_setup()}."
      ))
    }
    results["pat_scopes"] <- FALSE
  }

  ## 4. rcompendium credentials (~/.Rprofile) ----

  cli::cli_h1("rcompendium credentials {cli::col_silver('(~/.Rprofile)')}")

  cred_meta <- list(
    given = list(required = TRUE),
    family = list(required = TRUE),
    email = list(required = TRUE),
    orcid = list(required = FALSE),
    protocol = list(required = FALSE)
  )

  any_required_missing <- FALSE

  for (key in names(cred_meta)) {
    val <- read_option(key)
    is_set <- !is.na(val)
    required <- cred_meta[[key]]$required

    if (is_set) {
      cli::cli_alert_success(
        "{.field rcompendium.{key}} {cli::col_silver(val)}"
      )
    } else if (required) {
      any_required_missing <- TRUE
      cli::cli_alert_danger("{.field rcompendium.{key}} is not set")
      hint <- paste0('set_credentials(', key, ' = "your_', key, '_here")')
      show_fix(hint)
    } else {
      cli::cli_alert_warning(
        "{.field rcompendium.{key}} not set {cli::col_silver('(optional)')}"
      )
      if (suggest_fixes) {
        hint <- if (identical(key, "protocol")) {
          'set_credentials(protocol = "ssh")  # or "https"'
        } else {
          'set_credentials(orcid = "0000-0000-0000-0000")'
        }
        show_fix(hint)
      }
    }
  }

  results["rprofile_credentials"] <- !any_required_missing

  ## 5. SSH key  (only when protocol = "ssh") ----

  protocol <- read_option("protocol")
  protocol <- if (is.na(protocol)) "https" else protocol

  if (identical(protocol, "ssh")) {
    cli::cli_h1("SSH key {cli::col_silver('(protocol = \"ssh\")')}")

    ssh_candidates <- c(
      path.expand("~/.ssh/id_ed25519.pub"),
      path.expand("~/.ssh/id_rsa.pub"),
      path.expand("~/.ssh/id_ecdsa.pub")
    )
    existing_keys <- ssh_candidates[file.exists(ssh_candidates)]
    local_key_found <- length(existing_keys) > 0L

    if (!local_key_found) {
      results["ssh_key"] <- FALSE
      cli::cli_alert_danger(
        "No local SSH public key found {cli::col_silver('(~/.ssh/id_*.pub)')}"
      )
      if (suggest_fixes) {
        cli::cli_bullets(c(
          "1" = 'Generate: {.run system("ssh-keygen -t ed25519 -C \\"you@example.com\\"")}',
          "2" = "Add the public key at {.url https://github.com/settings/ssh/new}."
        ))
      }
    } else {
      results["ssh_key"] <- TRUE

      ## Try to verify the key is registered with GitHub via the API ----
      gh_keys <- if (github_pat) {
        tryCatch(gh::gh("/user/keys"), error = function(e) NULL)
      } else {
        NULL
      }

      ssh_key_registered <- FALSE

      if (!is.null(gh_keys) && length(gh_keys) > 0L) {
        for (pub_path in existing_keys) {
          pub_line <- tryCatch(
            readLines(pub_path, warn = FALSE)[[1L]],
            error = function(e) NULL
          )
          if (!is.null(pub_line)) {
            parts <- strsplit(pub_line, " ")[[1L]]
            key_body <- if (length(parts) >= 2L) parts[[2L]] else ""
            for (gh_key in gh_keys) {
              if (
                nzchar(key_body) &&
                  grepl(
                    substr(key_body, 1L, 20L),
                    gh_key[["key"]],
                    fixed = TRUE
                  )
              ) {
                ssh_key_registered <- TRUE
                break
              }
            }
          }
          if (ssh_key_registered) break
        }
      }

      if (ssh_key_registered) {
        cli::cli_alert_success(
          "Local SSH key found and registered with GitHub"
        )
      } else if (!is.null(gh_keys)) {
        cli::cli_alert_warning(
          "Local SSH key found but {.strong not} detected among your GitHub keys"
        )
        copy_cmd <- switch(
          Sys.info()[["sysname"]],
          Windows = "clip < ~/.ssh/id_ed25519.pub",
          Darwin = "pbcopy < ~/.ssh/id_ed25519.pub",
          "xclip -sel clip < ~/.ssh/id_ed25519.pub"
        )
        if (suggest_fixes) {
          cli::cli_bullets(c(
            "1" = "Copy your public key: {.code {copy_cmd}}",
            "2" = "Add it at {.url https://github.com/settings/ssh/new}."
          ))
        }
      } else {
        # No PAT available to cross-check; local key is the best we can do.
        cli::cli_alert_success(
          "Local SSH key found
          {cli::col_silver('(GitHub registration not verified \u2013 no PAT)')}"
        )
      }
    }
  }

  ## 6. Summary ----

  cli::cli_rule()

  # Keys that are warnings rather than hard blockers
  soft_keys <- c("git_github_user", "pat_scopes", "ssh_key")
  hard_fails <- results[!names(results) %in% soft_keys & !results]

  if (length(hard_fails) == 0L) {
    cli::cli_alert_success(
      "All required checks passed \u2013 ready to use {.pkg rcompendium}!"
    )
  } else {
    n <- length(hard_fails)
    cli::cli_alert_danger(
      "{n} check{?s} failed. Fix the issue{?s} above before running
       {.run rcompendium::new_package()} or {.run rcompendium::new_compendium()}."
    )
  }

  invisible(results)
}
