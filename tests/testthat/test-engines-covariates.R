# Declared covariates must either reach the model, or be complained about.
#
# A covariate is a promise: it was declared because somebody wants the model to
# use it. Three outcomes are possible and only one of them is unacceptable ---
#
#   used     the target carries the column, and the model can be told to use it
#   dropped  the target has nowhere to put it, AND SAYS SO
#   silent   dropped with no warning        <- the user believes it is in the
#                                              model when it is not
#
# Materialised temporal-effect columns are covariates for this purpose (item
# 4.1): they are exactly the case where somebody asked for an effect and would
# otherwise never learn it was ignored.

skip_on_cran()

# Which converters can carry a covariate column through, and which cannot.
COVARIATE_SPEC <- list(
  list(
    id = "baselinenowcast/long", package = "baselinenowcast", carries = TRUE,
    convert = function(x) tbl_now_to_baselinenowcast(x, format = "long", verbose = FALSE)
  ),
  list(
    id = "baselinenowcast/matrix", package = "baselinenowcast", carries = FALSE,
    convert = function(x) tbl_now_to_baselinenowcast(x, format = "matrix", verbose = FALSE)
  ),
  list(
    id = "baselinenowcast/triangle_list", package = "baselinenowcast", carries = FALSE,
    convert = function(x) {
      tbl_now_to_baselinenowcast(x, format = "triangle_list", verbose = FALSE)
    }
  ),
  list(
    id = "epinowcast", package = "epinowcast", carries = FALSE,
    convert = function(x) tbl_now_to_epinowcast(x, verbose = FALSE, quiet = TRUE)
  ),
  list(
    id = "EpiNow2", package = "EpiNow2", carries = FALSE,
    convert = function(x) tbl_now_to_EpiNow2(x, verbose = FALSE, quiet = TRUE)
  ),
  list(
    id = "nobbs", package = "NobBS", carries = TRUE,
    convert = function(x) tbl_now_to_nobbs(x, verbose = FALSE)
  ),
  list(
    id = "surveillance", package = "surveillance", carries = TRUE,
    convert = function(x) tbl_now_to_surveillance(x, verbose = FALSE)
  ),
  list(
    id = "tsibble", package = "tsibble", carries = TRUE,
    convert = function(x) tbl_now_to_tsibble(x, verbose = FALSE)
  ),
  list(
    id = "data.table", package = "data.table", carries = TRUE,
    convert = function(x) tbl_now_to_data_table(x, verbose = FALSE)
  )
)

covariate_fixture <- function() {
  engine_fixture(
    units = "days", data_type = "count-incidence",
    n_strata = 0L, n_covariates = 2L, n_periods = 20L
  )
}

#' Columns present in whatever a converter returned
converted_columns <- function(object) {
  if (is.matrix(object)) {
    return(colnames(object))
  }
  if (inherits(object, "enw_preprocess_data")) {
    return(unlist(lapply(object, function(el) {
      tryCatch(colnames(el[[1]]) %||% colnames(el), error = function(e) NULL)
    })))
  }
  if (is.list(object) && !is.data.frame(object)) {
    return(unlist(lapply(object, function(el) {
      tryCatch(colnames(el), error = function(e) NULL)
    })))
  }
  colnames(object)
}

for (entry in COVARIATE_SPEC) {
  local({
    this <- entry

    test_that(paste0("covariates are used or complained about: ", this$id), {
      skip_if_not_installed(this$package)

      x <- covariate_fixture()
      covariates <- get_covariates(x)
      expect_length(covariates, 2L)

      warnings <- character()
      converted <- withCallingHandlers(
        suppressMessages(this$convert(x)),
        warning = function(cnd) {
          warnings <<- c(warnings, conditionMessage(cnd))
          invokeRestart("muffleWarning")
        }
      )

      kept <- intersect(covariates, converted_columns(converted))
      complained <- any(grepl("covariate", warnings, ignore.case = TRUE))

      if (isTRUE(this$carries)) {
        expect_setequal(kept, covariates)
        # Carrying them and ALSO warning would be a lie in the other direction.
        expect_false(complained, label = paste0(this$id, " warns despite carrying"))
      } else {
        expect_length(kept, 0L)
        # The failure this test exists for: dropped in silence.
        expect_true(
          complained,
          label = paste0(this$id, " warns that covariates were dropped")
        )
      }
    })
  })
}

test_that("temporal effects count as covariates", {
  skip_if_not_installed("baselinenowcast")

  # Item 4.1: an effect somebody asked for, silently ignored, is the same
  # failure as a covariate silently dropped -- and more likely, because the
  # columns are generated rather than typed out.
  x <- engine_fixture(units = "days", n_periods = 30L) |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE))

  expect_gt(length(get_temporal_effects(x)), 0L)

  warnings <- character()
  withCallingHandlers(
    suppressMessages(tbl_now_to_baselinenowcast(x, verbose = FALSE)),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("covariate", warnings, ignore.case = TRUE)))

  # ...and the long format, which CAN carry them, must not warn.
  long_warnings <- character()
  long <- withCallingHandlers(
    suppressMessages(tbl_now_to_baselinenowcast(x, format = "long", verbose = FALSE)),
    warning = function(cnd) {
      long_warnings <<- c(long_warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("covariate", long_warnings, ignore.case = TRUE)))
  expect_true(any(grepl("day_of_week", colnames(long))))
})

test_that("calendar effects are categorical, Fourier terms are continuous", {
  # The distinction that matters downstream:
  #
  #   day_of_week / day_of_month / month_of_year / week_of_year  ->  FACTOR
  #   weekend                                                    ->  0/1 dummy
  #   seasons (Fourier sin/cos)                                  ->  numeric
  #
  # A calendar position handed to a model as a NUMBER says "Saturday is seven
  # times Sunday". Since 0.21.0 they are unordered factors, so a model formula
  # gives them dummy (treatment) contrasts -- the same encoding
  # `epinowcast`'s own `metareference` uses for `day_of_week`.
  #
  # Unordered matters: R gives ORDERED factors polynomial contrasts, which would
  # fit a trend across weekdays and reintroduce the problem.
  #
  # `temporal_effects` is lazy: the columns do not exist until
  # `compute_temporal_effects()` runs, which the converters do internally.
  x <- engine_fixture(units = "days", n_periods = 400L) |>
    add_temporal_effects(temporal_effects(
      day_of_week = TRUE, week_of_year = TRUE, month_of_year = TRUE, seasons = 365
    ))
  materialised <- suppressWarnings(suppressMessages(compute_temporal_effects(x)))
  effect_cols <- get_temporal_effect_cols(materialised)
  expect_gt(length(effect_cols), 0L)

  calendar <- grep("day_of_week|week_of_year|month_of_year", effect_cols, value = TRUE)
  fourier <- grep("season", effect_cols, value = TRUE)
  expect_gt(length(calendar), 0L)
  expect_gt(length(fourier), 0L)

  for (column in calendar) {
    values <- materialised[[column]]
    expect_s3_class(values, "factor")
    expect_false(
      is.ordered(values),
      label = paste0(column, " is UNORDERED (ordered would give polynomial contrasts)")
    )
    expect_lte(nlevels(values), 53L)
    # Dummy contrasts: one column per level after the first, all 0/1.
    dummies <- contrasts(values)
    expect_true(all(dummies %in% c(0, 1)), label = paste0(column, " uses dummy contrasts"))
  }
  for (column in fourier) {
    values <- materialised[[column]]
    expect_true(is.numeric(values))
    # Genuinely continuous, and bounded to [-1, 1] as sin/cos must be.
    expect_gt(length(unique(values)), 53L)
    expect_lte(max(abs(values), na.rm = TRUE), 1 + 1e-8)
  }
})

test_that("materialised effects survive into the formats that carry covariates", {
  skip_if_not_installed("baselinenowcast")

  x <- engine_fixture(units = "days", n_periods = 30L) |>
    add_temporal_effects(temporal_effects(day_of_week = TRUE))

  long <- suppressWarnings(suppressMessages(
    tbl_now_to_baselinenowcast(x, format = "long", verbose = FALSE)
  ))
  expect_true(any(grepl("day_of_week", colnames(long))))
})

test_that("no covariates declared means no covariate warning anywhere", {
  x <- engine_fixture(units = "days", n_covariates = 0L, n_periods = 20L)

  for (entry in COVARIATE_SPEC) {
    if (!requireNamespace(entry$package, quietly = TRUE)) next
    warnings <- character()
    withCallingHandlers(
      suppressMessages(entry$convert(x)),
      warning = function(cnd) {
        warnings <<- c(warnings, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    )
    expect_false(
      any(grepl("covariate", warnings, ignore.case = TRUE)),
      label = paste0(entry$id, " is silent about covariates when there are none")
    )
  }
})
