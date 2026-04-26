#' List all functions in the package
#'
#' @description
#' This function returns a list of all the functions (exported and internal)
#' available with the project. As this function scans the `NAMESPACE` and the
#' `R/` folder, it is recommended to:
#' 
#' - store all the functions in the `R/` folder
#' - run [devtools::document()] before to update the `NAMESPACE`
#'
#' @return A list of two vectors:
#' * `external`, a vector of exported functions name;
#' * `internal`, a vector of internal functions name.
#'
#' @export
#'
#' @family utilities functions
#'
#' @examples
#' \dontrun{
#' ## Update NAMESPACE ----
#' devtools::document()
#'
#' ## List all implemented functions ----
#' get_all_functions()
#' }

get_all_functions <- function() {
  stop_if_missing_r_dir()
  stop_if_missing_r_files()

  detect_r_function_names()
}
