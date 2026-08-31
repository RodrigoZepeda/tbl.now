#' Create a `tbl_now` object
#'
#' @description `r lifecycle::badge('experimental')`
#'
#' Surveillance data arrives late. A case that happened on Monday may only reach
#' the surveillance system on Thursday, so counts for the most recent days always
#' look artificially low. *Nowcasting* corrects that artifact: it estimates how
#' many cases have already happened but have not been reported yet.
#'
#' To do that, a model needs two dates for every case -- when it **happened**
#' (`event_date`) and when it was **reported** (`report_date`) -- together with
#' the date you are standing on (`now`). `tbl_now()` takes an ordinary
#' `data.frame` and records which of its columns play those roles, so you only
#' have to say it once.
#'
#' The result still behaves like a `tibble`: `dplyr` verbs, `$`, `[` and
#' `ggplot2` keep working, and every `tbl.now` function knows where to find the
#' dates without being told again.
#'
#' @details
#' The minimum you must supply is `event_date` and `report_date` (or one of them
#' plus a `delay` column, from which the other is reconstructed). Everything else
#' is optional and can be added later with [add_strata()], [add_covariates()],
#' [add_confirmation()] and the rest of the [add()] family.
#'
#' Once the object exists the usual path is [summary()][tbl_now_summary] to see
#' what is in the data, [diagnose()] to see what is wrong with it,
#' [autoplot()][autoplot.tbl_now] to look at it, and [run_nowcast()] to fit a
#' model. `vignette("tbl.now")` walks through that path end to end.
#'
#' @param data A `data.frame` or `tibble` to be converted.
#'
#' @param event_date [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#' name of the column containing the event date. Optional when `delay` is
#' provided together with `report_date`; the event date will be computed as
#' `report_date - delay`.
#'
#' @param report_date [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#' name of the column containing the report date. Optional when `delay` is
#' provided together with `event_date`; the report date will be computed as
#' `event_date + delay`.
#'
#' @param delay (optional) [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#' or `NULL` (default). Name of a numeric column containing the delay (in
#' `event_units`) between `event_date` and `report_date`. When provided with
#' only one of `event_date` or `report_date`, the missing date is reconstructed
#' from the known date and the delay. Requires units to be known (either
#' specified via `event_units` or inferrable from the provided date column).
#'
#' @param case_count (optional) [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html) or `NULL`
#' Name of the column with the case counts if `data_type` is "count-incidence"
#' or "count-cumulative".
#'
#' @param is_censored (optional)
#' [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html) or `NULL` (default).
#' The name of a column containing either `TRUE` or `FALSE` indicating whether
#' the `report_date` is correctly specified or corresponds to a `batch` and thus
#' is censored. In other words, if the `report_date` is accurately measured
#' set `is_censored = FALSE` but if the `report_date` corresponds to an error
#' and is only an upper bound of the real report date
#' set `is_censored = TRUE`.
#'
#' @param strata (optional) [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#' or `NULL` (default). Name of different variables (column names) in strata.
#' Strata correspond to variables that are of interest by themselves.
#' For example if it is of interest to generate nowcasts by gender then
#' `gender` is a `strata`.
#'
#' @param covariates (optional) [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#' or `NULL` (default). Name of different variables
#' (column names) that influence the nowcast but are not strata.
#' For example precipitation might influence a dengue nowcast but in general it
#' is not of interest to generate nowcasts by precipitation levels.
#'
#' @param now (optional) Date or `NULL` (default). The date that is considered the `now` of the
#' nowcast. If no `now` is given then the function automatically uses the last
#' `event_date`.
#'
#' @param t_effects (optional) Either `NULL` (default), a [temporal_effects()] object
#' or a character vector with the names of the columns containing the temporal effects.
#'
#' @param event_units (optional) Character. Either "auto" (default), "days",
#' "weeks", "months", "years" or "numeric".
#'
#' @param report_units (optional) Character. Either "auto" (default), "days",
#' "weeks", "months", "years" or "numeric".
#'
#' @param data_type (optional) Character. Either "auto", "linelist" or
#' "count-incidence" or "count-cumulative". See section below for
#' an explanation on data types.
#'
#' @param confirmation_date (optional)
#' [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#' column holding a **third** date: the day the report was resolved. Influenza is
#' the picture to keep in mind -- symptoms begin (the event), the patient sees a
#' doctor (the report), and days later a swab comes back. The assumed timeline is
#' `event_date <= report_date <= confirmation_date <= now`. Leave `NULL` (the
#' default) for the usual two-date object. See [add_confirmation()].
#'
#' @param confirmation_type (optional)
#' [tidy-select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
#' column saying what the resolution *was*: `"confirmed"`, `"retracted"` (it was
#' reported, but it is not a case after all), `"pending"` or `NA`. **`"pending"`
#' means reported and still waiting**, so it carries no confirmation date --
#' which is a different thing from a result that was never recorded (`NA`). A
#' confirmation date with no type warns rather than guessing, because a date
#' alone cannot say whether the case was confirmed or retracted.
#'
#' @param confirmation_units (optional) Character. Either `"auto"` (default),
#' `"days"`, `"weeks"`, `"months"`, `"years"` or `"numeric"` -- the grid the
#' confirmation date lives on, resolved the same way as `report_units`.
#'
#' @param verbose (optional) Logical. Whether to throw a message. Default = `TRUE`.
#'
#' @param force (optional) Logical. Whether to force computation overwriting pre-existing variables.
#' Default = `FALSE`.
#'
#' @param warn_non_uniqueness (optional) Logical. Whether to throw a warning if data
#' has multiple observations for same event and report date (conditional on covariates
#' and strata)
#'
#' @param align_weeks (optional) Logical. If both event and report units are weeks
#' and `align_weeks = TRUE` it ensures that all weeks start in a Sunday so that
#' week differences and `.delays` are all integer.
#'
#' @param ... Additional metadata to be stored as attributes on the object. Use
#' this for provenance you want to travel with the data -- `data_source`,
#' `citation`, `population` -- and read it back with [tbl_now_attributes()].
#'
#' Because anything unmatched lands here, a misspelled argument name would
#' otherwise be accepted in silence. Names close enough to a real argument to be
#' a typo (`case_col` for `case_count`, `stata` for `strata`) warn instead; the
#' warning is safe to ignore if the name really was metadata.
#'
#' @section Attributes:
#'
#' The following attributes are part of a `tbl_now` and are validated
#' by the [validate_tbl_now()] function:
#'
#' \describe{
#'   \item{event_date}{Name of the column refering to the event of interest.}
#'   \item{report_date}{Name of the column refering to when the event of interest was reported.}
#'   \item{strata}{Names of the columns corresponding to the strata (for modelling).}
#'   \item{covariates}{Names of the columns corresponding to covariates (for modelling).}
#'   \item{case_count}{Column containing the number of observations for that moment if `data_type` is `count-incidence` or `count-cumulative`.}
#'   \item{temporal_effects}{Names of the columns refering to the temporal effects.}
#'   \item{now}{Date of the `now` for a nowcast.}
#'   \item{is_censored}{Column indicating whether the measurement is noisy (only upper bound) or not.}
#'   \item{event_units}{Either `days`, `weeks`, `months`, `years` or `numeric`. Corresponds to the units of `event_date`}
#'   \item{report_units}{Either `days`, `weeks`, `months`, `years` or `numeric`. Corresponds to the units of `report_date`}
#'   \item{data_type}{Either `linelist`, `count-incidence` or `count-cumulative` depending on whether it is linelist data
#'   or count data with incidence (each report date's incidence) or cumulative (overall known cases at report date)}
#'   \item{confirmation_date}{Name of the column with the (optional) third date: when the report was resolved.}
#'   \item{confirmation_type}{Name of the column saying what that resolution was (`"confirmed"`, `"retracted"`, `"pending"`).}
#'   \item{confirmation_units}{Units of `confirmation_date`, resolved like `report_units`.}
#'   \item{computed_temporal_effect_cols}{Names of the temporal-effect columns that have actually been materialised in the data by [compute_temporal_effects()].}
#' }
#'
#' You can  list all `tbl_now` related attributes in a specific `tbl_now` with [tbl_now_attributes()].
#'
#' @section Data types:
#' The following data-types are admitted at `tbl_now` objects.
#'
#' *Linelist*
#'
#' Each row is an individual that was reported at `report_date` as happening at `event_date`.
#'
#' ```{r, cache=TRUE}
#' df <- data.frame(
#'  patient     = 1:6,
#'  event_date  = c(rep(as.Date("2020/09/12"), 3),
#'                  rep(as.Date("2020/09/13"), 3)),
#'  report_date = c(as.Date("2020/09/12"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/15")))
#' print(df)
#' ```
#'
#' *Count-incidence*
#'
#' Each `report_date`-`event_date` combination contains the total number of
#' cases observed _exactly_ at `report_date` for `event_date`.
#'
#' ```{r, cache=TRUE}
#' df <- data.frame(
#'  n           = c(7, 1, 9, 5, 0, 2),
#'  event_date  = c(rep(as.Date("2020/09/12"), 3),
#'                  rep(as.Date("2020/09/13"), 3)),
#'  report_date = c(as.Date("2020/09/12"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/15")))
#' print(df)
#' ```
#'
#' *Count-cumulative*
#'
#' Each `report_date`-`event_date` combination contains the total number of
#' cases observed up until `report_date` for `event_date`. The most recent
#' `report_date` contains the best estimation of cases happening at `event_date`.
#'
#' ```{r, cache=TRUE}
#' df <- data.frame(
#'  n           = c(1,5, 8, 2, 2, 4),
#'  event_date  = c(rep(as.Date("2020/09/12"), 3),
#'                  rep(as.Date("2020/09/13"), 3)),
#'  report_date = c(as.Date("2020/09/12"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/13"),
#'                  as.Date("2020/09/14"),
#'                  as.Date("2020/09/15")))
#' print(df)
#' ```
#'
#' The [to_count()] function allows you to easily convert from between different
#' data-types.
#'
#' @examples
#' # `denguedat` is a linelist: one row per dengue case, with the week symptoms
#' ## began (`onset_week`) and the week the case reached the surveillance system
#' ## (`report_week`).
#' data(denguedat)
#' head(denguedat)
#'
#' # Tell tbl.now which column plays which role. `now` defaults to the last
#' # event date seen in the data.
#' ndata <- tbl_now(denguedat,
#'   event_date = onset_week,
#'   report_date = report_week,
#'   strata = gender
#' )
#'
#' # Printing reports back the roles it recorded, and the `now` it chose.
#' ndata
#'
#' # A `tbl_now` is still a tibble, so ordinary manipulation works ...
#' ndata$newcolumn <- "something"
#' ndata[1:10, ]
#'
#' # ... including dplyr verbs.
#' ndata |>
#'   dplyr::filter(report_week <= as.Date("1991-01-02"))
#'
#' # Dropping a strata column simply forgets that stratum.
#' ndata |> dplyr::select(-gender)
#'
#' # But dropping a column the class depends on demotes the object back to a
#' ## plain tibble (with a warning): without an event date it can no longer
#' # describe a nowcast.
#' suppressWarnings(
#'   ndata |> dplyr::select(-onset_week)
#' )
#'
#' @return An object of class `tbl_now`: the input `data` as a `tibble`, carrying
#' extra attributes that record which columns hold the event date, report date,
#' strata, covariates and so on, plus the `now` of the nowcast. List them with
#' [tbl_now_attributes()].
#'
#' @seealso
#' [as_tbl_now()] to convert an object created by another nowcasting package;
#' [to_count()] to move between linelist and aggregated count data;
#' [tbl_now_attributes()] to list what the object recorded;
#' [validate_tbl_now()] and [diagnose()] to check it;
#' [summary()][tbl_now_summary] to describe it;
#' [autoplot()][autoplot.tbl_now] to plot it;
#' [run_nowcast()] to fit a nowcast.
#'
#' @export
tbl_now <- function(data,
                    event_date = NULL,
                    report_date = NULL,
                    delay = NULL,
                    strata = NULL,
                    covariates = NULL,
                    case_count = NULL,
                    is_censored = NULL,
                    confirmation_date = NULL,
                    confirmation_type = NULL,
                    confirmation_units = "auto",
                    now = NULL,
                    event_units = "auto",
                    report_units = "auto",
                    data_type = "auto",
                    t_effects = character(0),
                    verbose = TRUE,
                    force = FALSE,
                    warn_non_uniqueness = TRUE,
                    align_weeks = FALSE,
                    ...) {
  # Check the data frame data--------
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a {.code data.frame}")
  }

  if (dplyr::is.grouped_df(data)) {
    cli::cli_warn("{.arg data} is grouped by {colnames(dplyr::group_keys(data))}. Ungrouping.")
    data <- data |> dplyr::ungroup()
  }


  # Capture quosures first so we can detect NULL vs supplied and avoid the
  # tidyselect "external vector" deprecation in .tbl_now_eval_select().
  event_date_quo <- rlang::enquo(event_date)
  report_date_quo <- rlang::enquo(report_date)
  delay_quo <- rlang::enquo(delay)
  strata_quo <- rlang::enquo(strata)
  covariates_quo <- rlang::enquo(covariates)
  case_count_quo <- rlang::enquo(case_count)
  is_censored_quo <- rlang::enquo(is_censored)
  confirmation_date_quo <- rlang::enquo(confirmation_date)
  confirmation_type_quo <- rlang::enquo(confirmation_type)

  # Get event date column
  if (!rlang::quo_is_null(event_date_quo)) {
    event_col_select <- .tbl_now_eval_select(event_date_quo, data)
    event_date <- colnames(data)[event_col_select]
  } else {
    event_date <- character(0)
  }

  if (!rlang::quo_is_null(report_date_quo)) {
    report_col_select <- .tbl_now_eval_select(report_date_quo, data)
    report_date <- colnames(data)[report_col_select]
  } else {
    report_date <- character(0)
  }

  if (!rlang::quo_is_null(delay_quo)) {
    delay_col_select <- .tbl_now_eval_select(delay_quo, data)
    delay_col <- colnames(data)[delay_col_select]
  } else {
    delay_col <- character(0)
  }

  has_event <- length(event_date) == 1
  has_report <- length(report_date) == 1
  has_delay <- length(delay_col) == 1

  # Validate that we have enough information to determine both dates
  if (!has_event && !has_report) {
    cli::cli_abort("Must provide at least one of {.arg event_date} or {.arg report_date}.")
  }
  if (!has_event && !has_delay) {
    cli::cli_abort(
      "Must provide {.arg event_date}, or provide {.arg delay} together with {.arg report_date}."
    )
  }
  if (!has_report && !has_delay) {
    cli::cli_abort(
      "Must provide {.arg report_date}, or provide {.arg delay} together with {.arg event_date}."
    )
  }

  # Reconstruct the missing date column from delay
  if (!has_event && has_report && has_delay) {
    pre_units <- if (event_units == "auto") {
      infer_units(data, date_column = report_date[1], date_units = "auto")
    } else {
      event_units
    }
    if (delay_col[1] == ".event_date") {
      cli::cli_abort("Delay column cannot be named {.val .event_date}; that name is reserved for the reconstructed column.")
    }
    data <- .reconstruct_date_from_delay(data,
      known_col = report_date[1],
      delay_col = delay_col[1], units = pre_units,
      new_col_name = ".event_date", direction = "subtract"
    )
    event_date <- ".event_date"
    event_units <- pre_units
    if (verbose) cli::cli_alert_info("Computed {.val .event_date} from {.val {report_date[1]}} - delay ({pre_units}).")
  } else if (!has_report && has_event && has_delay) {
    pre_units <- if (event_units == "auto") {
      infer_units(data, date_column = event_date[1], date_units = "auto")
    } else {
      event_units
    }
    if (delay_col[1] == ".report_date") {
      cli::cli_abort("Delay column cannot be named {.val .report_date}; that name is reserved for the reconstructed column.")
    }
    data <- .reconstruct_date_from_delay(data,
      known_col = event_date[1],
      delay_col = delay_col[1], units = pre_units,
      new_col_name = ".report_date", direction = "add"
    )
    report_date <- ".report_date"
    event_units <- pre_units
    if (report_units == "auto") report_units <- pre_units
    if (verbose) cli::cli_alert_info("Computed {.val .report_date} from {.val {event_date[1]}} + delay ({pre_units}).")
  }

  # If the user's delay column is named ".delay" it will conflict with time_cols_to_numeric;
  # drop it now since it will be recomputed from the numerics.
  if (has_delay && delay_col[1] == ".delay" && ".delay" %in% colnames(data)) {
    data <- data |> dplyr::select(-!!as.symbol(".delay"))
  }

  case_count_select <- .tbl_now_eval_select(case_count_quo, data)
  case_count <- colnames(data)[case_count_select]
  if (length(case_count) == 0) case_count <- NULL

  is_censored_select <- .tbl_now_eval_select(is_censored_quo, data)
  is_censored <- colnames(data)[is_censored_select]

  confirmation_date_select <- .tbl_now_eval_select(confirmation_date_quo, data)
  confirmation_date <- colnames(data)[confirmation_date_select]
  if (length(confirmation_date) == 0) confirmation_date <- NULL

  confirmation_type_select <- .tbl_now_eval_select(confirmation_type_quo, data)
  confirmation_type <- colnames(data)[confirmation_type_select]
  if (length(confirmation_type) == 0) confirmation_type <- NULL

  if (is.null(confirmation_date) && !is.null(confirmation_type)) {
    cli::cli_abort(c(
      "{.arg confirmation_type} was given without a {.arg confirmation_date}.",
      "i" = "An outcome needs a date to sit on. Supply both, or neither."
    ))
  }

  # Fill in / validate the outcome column, and check the timeline.
  resolved_confirmation <- .resolve_confirmation_type(
    data, confirmation_date, confirmation_type, verbose = verbose
  )
  data <- resolved_confirmation$data
  confirmation_type <- resolved_confirmation$confirmation_type
  if (length(is_censored) == 0) is_censored <- NULL

  strata_select <- .tbl_now_eval_select(strata_quo, data)
  strata <- colnames(data)[strata_select]
  if (length(strata) == 0) strata <- NULL

  covariates_select <- .tbl_now_eval_select(covariates_quo, data)
  covariates <- colnames(data)[covariates_select]
  if (length(covariates) == 0) covariates <- NULL


  if (length(case_count) > 1) {
    cli::cli_abort(
      "{.code case_count} has to be either `NULL` or a character with just one column name."
    )
  }

  # Check the date columns are dates-------
  check_date_columns(data, event_date = event_date, report_date = report_date)

  # Check the strata-----
  num_strata <- length(strata)
  if (!is.null(strata) && num_strata > 1) {
    if (!is.character(strata)) {
      cli::cli_abort("{.arg strata} must be either `NULL` or a string of column names")
    }

    for (st in strata) {
      if (!(st %in% colnames(data))) {
        cli::cli_abort("{.arg strata} = {st} not found in data")
      }
    }
  }

  # Check the covariates
  num_covariates <- length(covariates)
  if (!is.null(covariates) && num_covariates > 1) {
    if (!is.character(covariates)) {
      cli::cli_abort("{.arg covariates} must be either `NULL` or a string of column names")
    }

    for (cv in covariates) {
      if (!(cv %in% colnames(data))) {
        cli::cli_abort("{.arg covariates} = {cv} not found in data")
      }
    }
  }

  # Infer automatic variables------

  # Infer the now
  # The event <= report <= confirmation timeline is checked by
  # `validate_tbl_now()` at the end of this function, through the same findings
  # engine `diagnose()` uses. Checking it here as well would warn twice.

  # A confirmation is an OBSERVATION, so it moves the `now` forward exactly as a
  # report does: the as-of moment is the last thing anybody knew.
  now <- infer_now(data,
    now = now, event_date = event_date, report_date = report_date,
    confirmation_date = confirmation_date
  )

  # Infer the date_units whether it is daily, weekly, monthly or yearly
  event_units <- infer_units(data, date_column = event_date, date_units = event_units)
  report_units <- infer_units(data, date_column = report_date, date_units = report_units)
  confirmation_units <- if (is.null(confirmation_date)) {
    NULL
  } else {
    # Early in an outbreak there may be only one confirmation, or none, and a
    # single date has no spacing to infer a grid from. Fall back to the REPORT
    # units rather than refusing the object: the confirmation lives on the same
    # calendar as the report it resolves.
    tryCatch(
      infer_units(data, date_column = confirmation_date, date_units = confirmation_units),
      error = function(e) report_units
    )
  }

  # Get whether data is count or line data
  data_type <- infer_data_type(data,
    data_type = data_type,
    event_date = event_date, report_date = report_date,
    strata = strata,
    is_censored = is_censored,
    case_count = case_count, verbose = verbose
  )

  # Capture all other attributes
  other_attrs <- list(...)

  # `...` is a deliberate escape hatch for extra metadata, but it also swallows
  # misspelled argument names in silence -- `case_col = "n"` sets a useless
  # attribute and leaves the data typed as a linelist. Warn when an unmatched
  # name is close enough to a real one to be a typo.
  .warn_near_miss_dots(names(other_attrs))

  # === 3. Attribute Assignment ===
  data <- dplyr::as_tibble(data)

  # Set the core attributes (if adding new attributes here change in validate_tbl_now too)
  attr(data, "event_date") <- event_date
  attr(data, "report_date") <- report_date
  attr(data, "case_count") <- case_count
  attr(data, "strata") <- strata
  attr(data, "covariates") <- covariates
  attr(data, "now") <- now
  attr(data, "event_units") <- event_units
  attr(data, "report_units") <- report_units
  attr(data, "data_type") <- data_type
  attr(data, "is_censored") <- is_censored
  attr(data, "confirmation_date") <- confirmation_date
  attr(data, "confirmation_type") <- confirmation_type
  attr(data, "confirmation_units") <- confirmation_units

  # Add all other attributes from ...
  for (attr_name in names(other_attrs)) {
    attr(data, attr_name) <- other_attrs[[attr_name]]
  }

  # Add report_num and event_num the numerical columns
  if (".event_num" %in% colnames(data) && !force) {
    cli::cli_abort(
      "Data already has a column named {.val .event_num} which this class uses to save the numeric version of the event_date. Please rename your {.val .event_num} column."
    )
  }
  if (".report_num" %in% colnames(data) && !force) {
    cli::cli_abort(
      "Data already has a column named {.val .report_num} which this class uses to save the numeric version of the report_date. Please rename your {.val .report_num} column."
    )
  }

  # Convert time columns to numeric
  data <- time_cols_to_numeric(data,
    event_date = event_date, report_date = report_date,
    event_units = event_units, report_units = report_units,
    force = force
  )

  # `.confirmation_num` sits on the SAME anchor as `.event_num`/`.report_num`
  # (the earliest event date), so the three are directly comparable, and
  # `.confirmation_delay` is the report-to-resolution time.
  if (!is.null(confirmation_date)) {
    data <- .add_confirmation_num(
      data,
      event_date = event_date, confirmation_date = confirmation_date,
      confirmation_units = confirmation_units, force = force
    )
  }


  # === 4. Class Assignment ===
  # Prepend the S3 class. It will inherit from data.frame.
  class(data) <- c("tbl_now", class(data))

  # Temporal effects: stored lazily as a list of specs; columns computed only by compute_temporal_effects()
  attr(data, "temporal_effects") <- list()
  attr(data, "computed_temporal_effect_cols") <- character(0)
  if (!is.null(t_effects) && S7::S7_inherits(t_effects, class = temporal_effects)) {
    data <- add_temporal_effects(data, t_effects = t_effects)
  } else if (!is.null(t_effects) && is.character(t_effects)) {
    # Caller supplies already-computed column names directly  (for example in `to_count`)
    attr(data, "computed_temporal_effect_cols") <- t_effects
  }

  # Validate
  validate_tbl_now(data, warn_non_uniqueness = warn_non_uniqueness)

  # Align weeks
  if (align_weeks && get_report_units(data) == "weeks" && get_event_units(data) == "weeks") {
    data <- data |> align_weeks()
  }


  return(data)
}


#' Safe `tidyselect::eval_select()` wrapper
#'
#' Avoids the tidyselect "external vector" deprecation warning. Handles:
#' 1. literal string(s), e.g. `event_date = "onset_week"`;
#' 2. a `NULL` expression (empty selection);
#' 3. a symbol/call that *evaluates* to a character vector (e.g. internal calls
#'    such as `strata = get_strata(.data)`, or `v <- "sex"; tbl_now(d, strata = v)`)
#'    — wrapped in `all_of()` so tidyselect does not treat it as an external vector;
#' 4. anything else (bare column names, tidy-select expressions) -> `eval_select`.
#'
#' @param quo A quosure capturing the selection (from [rlang::enquo()]).
#' @param data The data frame to select columns from.
#'
#' @return A named integer vector of selected column positions (the
#'   [tidyselect::eval_select()] return value).
#'
#' @keywords internal
#' @noRd
.tbl_now_eval_select <- function(quo, data) {
  .EVAL_FAILED <- new.env(parent = emptyenv()) # sentinel for failed eval_tidy

  expr <- rlang::quo_get_expr(quo)

  # Case 1: explicit NULL (e.g. strata = NULL passed directly)
  if (is.null(expr)) {
    return(stats::setNames(integer(0), character(0)))
  }

  # Case 2: literal string(s) — fast path, no eval_select needed
  if (is.character(expr)) {
    idx <- match(expr, colnames(data))
    missing <- expr[is.na(idx)]
    if (length(missing) > 0) {
      cli::cli_abort("Column{?s} {.val {missing}} not found in data.")
    }
    return(stats::setNames(idx, expr))
  }

  # Case 3 + 4: symbol, call, or complex tidy-select expression.
  # Try evaluating in the quosure's own environment first.
  val <- tryCatch(rlang::eval_tidy(quo), error = function(e) .EVAL_FAILED)

  if (identical(val, .EVAL_FAILED)) {
    # Could not evaluate (bare column names like `sex`, tidy-select calls like
    # `c(sex, age)`, `starts_with("x")` …) — let eval_select resolve them.
    tidyselect::eval_select(quo, data)
  } else if (is.null(val)) {
    # Expression evaluates to NULL (e.g. `strata_var <- NULL`)
    stats::setNames(integer(0), character(0))
  } else if (is.character(val)) {
    # Expression evaluates to a character vector of column names — use all_of()
    # to suppress the tidyselect "external vector" deprecation.
    if (length(val) == 0L) {
      return(stats::setNames(integer(0), character(0)))
    }
    tidyselect::eval_select(rlang::expr(dplyr::all_of(!!val)), data)
  } else {
    # Fallback: hand off to eval_select for anything else
    tidyselect::eval_select(quo, data)
  }
}

#' Reconstruct a missing date column from a known date and an integer delay
#'
#' Used by [tbl_now()] when the user supplies only one of `event_date` /
#' `report_date` together with a `delay` column: the missing date is rebuilt by
#' adding (or subtracting) the delay, in the appropriate time `units`, from the
#' known date.
#'
#' @param data A data frame containing `known_col` and `delay_col`.
#' @param known_col Name of the known date (or numeric) column.
#' @param delay_col Name of the (numeric) delay column.
#' @param units Time units of the delay: one of `"days"`, `"weeks"`,
#'   `"months"`, `"years"` or `"numeric"`.
#' @param new_col_name Name of the date column to create.
#' @param direction `"add"` (reconstruct the report date) or `"subtract"`
#'   (reconstruct the event date).
#'
#' @return `data` with the reconstructed date column `new_col_name` added.
#'
#' @keywords internal
#' @noRd
.reconstruct_date_from_delay <- function(data, known_col, delay_col, units, new_col_name, direction) {
  known_vals <- data[[known_col]]
  delay_vals <- data[[delay_col]]

  if (!is.numeric(delay_vals)) {
    cli::cli_abort("Delay column {.val {delay_col}} must be numeric.")
  }

  int_delay <- as.integer(round(delay_vals))

  if (units == "numeric") {
    new_vals <- if (direction == "add") known_vals + delay_vals else known_vals - delay_vals
  } else if (units == "days") {
    new_vals <- if (direction == "add") known_vals + lubridate::days(int_delay) else known_vals - lubridate::days(int_delay)
  } else if (units == "weeks") {
    new_vals <- if (direction == "add") known_vals + lubridate::weeks(int_delay) else known_vals - lubridate::weeks(int_delay)
  } else if (units == "months") {
    new_vals <- if (direction == "add") lubridate::`%m+%`(known_vals, months(int_delay)) else lubridate::`%m-%`(known_vals, months(int_delay))
  } else if (units == "years") {
    new_vals <- if (direction == "add") known_vals + lubridate::years(int_delay) else known_vals - lubridate::years(int_delay)
  } else {
    cli::cli_abort("Unknown units: {.val {units}}. Must be one of: days, weeks, months, years, numeric.")
  }

  data[[new_col_name]] <- new_vals
  return(data)
}

#' Warn when a name passed through `...` looks like a misspelled argument
#'
#' `tbl_now()` keeps unmatched `...` names as user metadata, which means a typo
#' in a real argument name is accepted without complaint. This flags the names
#' that are within a small edit distance of one of `tbl_now()`'s own arguments,
#' which is the case that is almost never intentional.
#'
#' @param dot_names Character vector of names supplied through `...`.
#'
#' @return `NULL`, invisibly. Called for the warning.
#'
#' @keywords internal
#' @noRd
.warn_near_miss_dots <- function(dot_names) {
  if (length(dot_names) == 0) {
    return(invisible(NULL))
  }
  dot_names <- dot_names[nzchar(dot_names)]
  if (length(dot_names) == 0) {
    return(invisible(NULL))
  }

  known <- setdiff(names(formals(tbl_now)), c("data", "...", "verbose"))

  # A typo is short-range: at most a third of the longer of the two names, capped
  # at three edits. Scaling by the LONGER name is what lets `case_col` reach
  # `case_count` (distance 3) while `data_source` stays clear of `data_type`
  # (distance 5). Only the single closest argument is offered.
  for (supplied in dot_names) {
    distances <- utils::adist(supplied, known)[1, ]
    budgets <- pmin(3L, pmax(1L, floor(pmax(nchar(supplied), nchar(known)) / 3)))
    # Typos rarely change the first letter, and requiring it to match is what
    # keeps `source` from being read as a misspelling of `force`.
    same_initial <- substr(known, 1, 1) == substr(supplied, 1, 1)
    hits <- known[distances <= budgets & same_initial & known != supplied]
    if (length(hits) == 0) next
    closest <- hits[which.min(distances[known %in% hits])]
    cli::cli_warn(c(
      "{.arg {supplied}} is not an argument of {.fn tbl_now}; it was stored as
       metadata instead.",
      "i" = "Did you mean {.arg {closest}}?",
      "!" = "If {.arg {supplied}} really is metadata you meant to attach, this
             warning is safe to ignore."
    ))
  }
  invisible(NULL)
}
