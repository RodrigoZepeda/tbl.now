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
  #
  # From diseasenowcasting 2.1.0 the package supplies this method itself (and
  # re-exports the `generics` generic rather than declaring its own, which is
  # what used to make a bare `tidy()` return its parameter table instead of a
  # nowcast). Registering on top of that would silently override another
  # package's method with an identical one, so only step in when it is absent --
  # which keeps older diseasenowcasting versions working.
  already_registered <- !is.null(
    utils::getS3method(
      "tidy", "diseasenowcasting::nowcast_prediction",
      optional = TRUE, envir = asNamespace("generics")
    )
  )
  if (!already_registered) {
    registerS3method(
      "tidy", "diseasenowcasting::nowcast_prediction",
      tidy_nowcast_prediction,
      envir = asNamespace("generics")
    )
  }
}
