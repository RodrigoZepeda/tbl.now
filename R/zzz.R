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

  # `diseasenowcasting::predict()` returns an S7 object whose class is
  # "diseasenowcasting::nowcast_prediction". A `::` cannot appear in an S3
  # method name, so `tidy.diseasenowcasting::nowcast_prediction` is unwritable
  # and `@exportS3Method` cannot express it -- register it by hand instead.
  registerS3method(
    "tidy", "diseasenowcasting::nowcast_prediction",
    tidy_nowcast_prediction,
    envir = asNamespace("generics")
  )
}
