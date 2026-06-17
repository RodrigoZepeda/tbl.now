#' Package load hook
#'
#' Registers the package's S7 methods when the namespace is loaded.
#'
#' @param ... Ignored (the `libname`/`pkgname` passed by [base::loadNamespace()]).
#'
#' @return `NULL`, invisibly. Called for its side effect.
#'
#' @keywords internal
#' @noRd
.onLoad <- function(...) {
  S7::methods_register()
}
