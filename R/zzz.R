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
  # diseasenowcasting 2.1.0 supplies this method itself (and re-exports the
  # `generics` generic rather than declaring its own, which is what used to make
  # a bare `tidy()` return its parameter table instead of a nowcast).
  # Registering on top of that would silently override another package's method,
  # so only step in when it is absent.
  #
  # That is the whole handover plan: diseasenowcasting 2.2.0 removes its method,
  # this check then finds nothing, and `tidy_nowcast_prediction()` takes over
  # without either package needing to know which version the other is at. The
  # two were reconciled first, so the swap is not a behaviour change -- see the
  # note on the function itself.
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

  # Same problem, our own class this time: `tbl_nowcast` is S7, so `class()` is
  # "tbl.now::tbl_nowcast" and `tidy.tbl.now::tbl_nowcast` is not a writable S3
  # method name. Nothing else can own this one, so it is registered
  # unconditionally.
  registerS3method(
    "tidy", "tbl.now::tbl_nowcast", tidy_tbl_nowcast,
    envir = asNamespace("generics")
  )

  # Same `::`-in-the-class-name problem, and one more reason besides: assigning
  # an S7 method onto the imported `as_tibble` copies tibble's generic into this
  # namespace, and its `rownames = pkgconfig::get_config(...)` default then reads
  # to `R CMD check` as an undeclared dependency on a package we never use.
  registerS3method(
    "as_tibble", "tbl.now::tbl_nowcast", as_tibble_tbl_nowcast,
    envir = asNamespace("tibble")
  )
}
