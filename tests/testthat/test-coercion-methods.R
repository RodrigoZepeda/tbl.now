# Every `tbl_now_to_<pkg>()` must also be reachable through that package's OWN
# coercion verb, where the package has one.
#
# The point is that a user already fluent in `baselinenowcast` should be able to
# write `as_reporting_triangle(nowobj)` and get the same object as
# `tbl_now_to_baselinenowcast(nowobj)` -- the foreign method is a thin wrapper,
# not a second implementation.
#
# This file is a REGISTRY as much as a test: adding a converter without deciding
# what its coercion verb is fails `every converter is accounted for` below.

skip_on_cran()

# converter -> the package it targets, and that package's coercion generic.
# `generic = NA` means the package exposes none; `reason` must say so, and is
# checked against reality by `packages without a coercion generic really have
# none`.
COERCION_REGISTRY <- list(
  list(
    converter = "tbl_now_to_baselinenowcast", package = "baselinenowcast",
    generic = "as_reporting_triangle", reason = NA_character_
  ),
  list(
    converter = "tbl_now_to_tsibble", package = "tsibble",
    generic = "as_tsibble", reason = NA_character_
  ),
  list(
    converter = "tbl_now_to_data_table", package = "data.table",
    generic = "as.data.table", reason = NA_character_
  ),
  list(
    converter = "tbl_now_to_epidist", package = "epidist",
    generic = "as_epidist_linelist_data", reason = NA_character_
  ),
  list(
    converter = "tbl_now_to_epidist", package = "epidist",
    generic = "as_epidist_aggregate_data", reason = NA_character_
  ),
  list(
    converter = "tbl_now_to_epinowcast", package = "epinowcast", generic = NA_character_,
    reason = "epinowcast exports no S3 generic at all; its entry point is
              enw_preprocess_data(), a plain function."
  ),
  list(
    converter = "tbl_now_to_EpiNow2", package = "EpiNow2", generic = NA_character_,
    reason = "EpiNow2's generics are all print/summary-shaped; it has no
              coercion verb, and its entry points take a bare data.frame."
  ),
  list(
    converter = "tbl_now_to_nobbs", package = "NobBS", generic = NA_character_,
    reason = "NobBS exports no S3 generic at all; NobBS() takes a bare
              data.frame."
  ),
  list(
    converter = "tbl_now_to_surveillance", package = "surveillance", generic = NA_character_,
    reason = "surveillance's as.epidata()/as.hhh4simslist() are different
              concepts (epidemic-model data, simulation lists), and
              linelist2sts() is a plain function, not a generic."
  )
)

registry_tbl_now <- function() {
  data <- data.frame(
    event_date = rep(as.Date("2021-01-04") + 0:9, each = 3),
    report_date = rep(as.Date("2021-01-04") + 0:9, each = 3) + 0:2,
    n = 5L
  )
  tbl_now(data,
    event_date = "event_date", report_date = "report_date", case_count = "n",
    data_type = "count-incidence", event_units = "days", report_units = "days",
    verbose = FALSE
  )
}

test_that("every tbl_now_to_* converter is accounted for in the registry", {
  exported <- getNamespaceExports("tbl.now")
  converters <- sort(grep("^tbl_now_to_", exported, value = TRUE))
  registered <- sort(unique(vapply(COERCION_REGISTRY, `[[`, character(1), "converter")))

  # A new converter must be given a coercion verb, or an explicit written reason
  # why the target package has none. Failing here is the prompt to decide.
  expect_setequal(converters, registered)
})

test_that("the registered coercion methods exist and dispatch on tbl_now", {
  for (entry in COERCION_REGISTRY) {
    if (is.na(entry$generic)) next
    skip_if_not_installed(entry$package)

    method <- utils::getS3method(
      entry$generic, "tbl_now",
      optional = TRUE, envir = asNamespace(entry$package)
    )
    expect_true(
      is.function(method),
      label = paste0(entry$generic, ".tbl_now registered on ", entry$package)
    )
  }
})

test_that("each coercion method is a wrapper, not a second implementation", {
  x <- registry_tbl_now()

  for (entry in COERCION_REGISTRY) {
    if (is.na(entry$generic)) next
    skip_if_not_installed(entry$package)

    generic <- getExportedValue(entry$package, entry$generic)
    converter <- getExportedValue("tbl.now", entry$converter)

    through_generic <- suppressWarnings(suppressMessages(generic(x)))
    # The aggregate-data method is the one that passes an argument through, so
    # it is compared against the converter called the same way.
    direct <- suppressWarnings(suppressMessages(
      if (identical(entry$generic, "as_epidist_aggregate_data")) {
        converter(x, format = "aggregate", verbose = FALSE)
      } else if (identical(entry$converter, "tbl_now_to_baselinenowcast")) {
        converter(x, format = "matrix", verbose = FALSE)
      } else {
        converter(x, verbose = FALSE)
      }
    ))

    expect_equal(
      through_generic, direct,
      label = paste0(entry$generic, "() output"),
      expected.label = paste0(entry$converter, "() output")
    )
  }
})

test_that("packages without a coercion generic really have none", {
  # The `reason` fields are a claim about another package. Claims rot, so check
  # them: if a package GAINS a coercion verb, this fails and we go and use it.
  coercion_like <- function(package) {
    ns <- asNamespace(package)
    exported <- getNamespaceExports(package)
    is_generic <- vapply(exported, function(nm) {
      object <- tryCatch(get(nm, envir = ns), error = function(e) NULL)
      is.function(object) &&
        any(grepl("UseMethod", deparse(body(object)), fixed = TRUE))
    }, logical(1))
    grep("^as[._]|^as$", exported[is_generic], value = TRUE)
  }

  known_unrelated <- list(
    surveillance = c("as.epidata", "as.hhh4simslist")
  )

  for (entry in COERCION_REGISTRY) {
    if (!is.na(entry$generic)) next
    skip_if_not_installed(entry$package)

    found <- setdiff(
      coercion_like(entry$package), known_unrelated[[entry$package]] %||% character(0)
    )
    expect_equal(
      found, character(0),
      label = paste0(entry$package, " coercion generics (documented as none)")
    )
  }
})
