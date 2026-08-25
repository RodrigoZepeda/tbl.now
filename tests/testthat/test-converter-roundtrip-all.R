# Every shape a `tbl_now_to_*()` produces must either come back through
# `as_tbl_now()`, or be on the list of shapes that provably cannot.
#
# "Cannot" is a real category and it is not a shortcut: a `surveillance` line
# list has no report dimension left to recover, and an `estimate_dist()` frame is
# a delay schema rather than case data. Those are recorded with a reason, and the
# registry FAILS when a converter is missing from it -- so a new converter has to
# decide which it is.

skip_on_cran()

# converter (+ its arguments) -> can it come back?
ROUNDTRIP_REGISTRY <- list(
  list(
    id = "baselinenowcast/matrix", package = "baselinenowcast",
    convert = function(x) tbl_now_to_baselinenowcast(x, format = "matrix", verbose = FALSE),
    back = TRUE, reason = NA_character_
  ),
  list(
    id = "baselinenowcast/triangle_list", package = "baselinenowcast",
    convert = function(x) tbl_now_to_baselinenowcast(x, format = "triangle_list", verbose = FALSE),
    back = TRUE, reason = NA_character_
  ),
  list(
    id = "baselinenowcast/long", package = "baselinenowcast",
    convert = function(x) tbl_now_to_baselinenowcast(x, format = "long", verbose = FALSE),
    back = FALSE,
    reason = "a bare long data.frame with no class; `as_tbl_now.data.frame()`
              would take it, but only if the caller re-states every column, so
              it is not a round trip."
  ),
  list(
    id = "epinowcast/preprocessed", package = "epinowcast",
    convert = function(x) tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE),
    back = TRUE, reason = NA_character_
  ),
  list(
    id = "tsibble", package = "tsibble",
    convert = function(x) tbl_now_to_tsibble(x, verbose = FALSE),
    back = TRUE, reason = NA_character_
  ),
  list(
    id = "data.table", package = "data.table",
    convert = function(x) tbl_now_to_data_table(x, verbose = FALSE),
    back = TRUE, reason = NA_character_
  ),
  list(
    id = "epidist/linelist", package = "epidist",
    convert = function(x) tbl_now_to_epidist(x, verbose = FALSE),
    back = TRUE, reason = NA_character_
  ),
  list(
    id = "EpiNow2/estimate_truncation", package = "EpiNow2",
    convert = function(x) {
      tbl_now_to_EpiNow2(x, target = "estimate_truncation", verbose = FALSE, quiet = TRUE)
    },
    back = TRUE, reason = NA_character_
  ),
  list(
    id = "EpiNow2/estimate_infections", package = "EpiNow2",
    convert = function(x) tbl_now_to_EpiNow2(x, verbose = FALSE, quiet = TRUE),
    back = FALSE,
    reason = "one date/confirm series: the report dimension has been collapsed
              away, so there is nothing to reconstruct a reporting triangle
              from."
  ),
  list(
    id = "EpiNow2/estimate_dist", package = "EpiNow2",
    convert = function(x) {
      tbl_now_to_EpiNow2(x, target = "estimate_dist", verbose = FALSE, quiet = TRUE)
    },
    back = FALSE,
    reason = "a delay-distribution schema (censoring windows), not case data."
  ),
  list(
    id = "nobbs", package = "NobBS",
    convert = function(x) tbl_now_to_nobbs(x, verbose = FALSE),
    back = FALSE,
    reason = "a plain data.frame line list with no class to dispatch on; it
              carries both dates, so `tbl_now()` rebuilds it directly, but there
              is no `as_tbl_now()` method and inventing one would claim a
              round trip the class cannot support."
  ),
  list(
    id = "surveillance/linelist", package = "surveillance",
    convert = function(x) tbl_now_to_surveillance(x, verbose = FALSE),
    back = FALSE,
    reason = "same as NobBS: an unclassed data.frame."
  ),
  list(
    id = "surveillance/sts", package = "surveillance",
    convert = function(x) tbl_now_to_surveillance(x, format = "sts", verbose = FALSE),
    back = FALSE,
    reason = "an `sts` holds the observed curve by event date only -- the report
              dimension is gone, so the reporting triangle cannot be rebuilt."
  )
)

roundtrip_fixture <- function() {
  engine_fixture(
    units = "days", data_type = "count-incidence",
    n_strata = 0L, n_periods = 25L, max_delay = 3L
  )
}

test_that("every tbl_now_to_* shape is accounted for in the round-trip registry", {
  exported <- getNamespaceExports("tbl.now")
  converters <- sort(grep("^tbl_now_to_", exported, value = TRUE))

  covered <- vapply(ROUNDTRIP_REGISTRY, function(entry) {
    body_text <- paste(deparse(body(entry$convert)), collapse = " ")
    hit <- converters[vapply(converters, grepl, logical(1), x = body_text, fixed = TRUE)]
    if (length(hit) == 0) NA_character_ else hit[which.max(nchar(hit))]
  }, character(1))

  # Every converter must appear in at least one registry entry. A new one fails
  # here until somebody decides whether its output can come back.
  expect_setequal(unique(stats::na.omit(covered)), converters)
})

for (entry in ROUNDTRIP_REGISTRY) {
  local({
    this <- entry

    test_that(paste0("as_tbl_now() round trip: ", this$id), {
      skip_if_not_installed(this$package)

      x <- roundtrip_fixture()
      converted <- suppressWarnings(suppressMessages(this$convert(x)))

      if (!isTRUE(this$back)) {
        # Documented as un-returnable. Assert that is still TRUE, so the day a
        # method appears (or the class changes) this test says so.
        #
        # The fallback classes do not count: `as_tbl_now.data.frame()` matches
        # anything rectangular, but it takes the column roles as arguments --
        # that is the caller restating the object, not a round trip.
        fallbacks <- c("data.frame", "tbl_df", "tbl", "list", "default")
        classes <- setdiff(class(converted), fallbacks)
        has_method <- any(vapply(classes, function(cls) {
          !is.null(utils::getS3method("as_tbl_now", cls, optional = TRUE))
        }, logical(1)))
        expect_false(
          has_method,
          label = paste0(this$id, " has an as_tbl_now() method (registry says none)")
        )
        return(invisible(NULL))
      }

      back <- suppressWarnings(suppressMessages(
        if (identical(this$id, "tsibble")) {
          # The tsibble index is the REPORT date by default, so the event date
          # has to be named on the way back.
          as_tbl_now(converted,
            event_date = "event_time", report_date = "report_time",
            verbose = FALSE
          )
        } else if (identical(this$id, "data.table")) {
          as_tbl_now(converted,
            event_date = "event_time", report_date = "report_time",
            case_count = "n", data_type = "count-incidence", verbose = FALSE
          )
        } else {
          as_tbl_now(converted, verbose = FALSE)
        }
      ))

      expect_true(is_tbl_now(back), label = paste0(this$id, " returns a tbl_now"))

      # Content, not just class: the cases that survived the trip must be the
      # cases that went in. Where a conversion is lossy by design the totals can
      # only fall, never rise -- a rise means something was double-counted.
      before <- sum(get_latest_reported_cases(x)[["n"]], na.rm = TRUE)
      count_col <- get_case_count(back)
      after <- if (is.null(count_col)) {
        nrow(get_latest_reported_cases(back))
      } else {
        sum(get_latest_reported_cases(back)[[count_col]], na.rm = TRUE)
      }
      expect_lte(after, before)
      expect_gt(after, 0)
    })
  })
}
