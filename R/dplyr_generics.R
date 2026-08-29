# This is mostly built using the
# **Extending dplyr with new data frame subclasses** at
# <https://dplyr.tidyverse.org/reference/dplyr_extending.html?q=reconstruct#ref-usage>
# and the code from **ExtendDataframes**
# <https://github.com/joshwlambert/ExtendDataFrames/blob/main/R/subset-reconstruct.R>


#' Check that an object is a valid `tbl_now`
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Two ways of asking the same question -- *is this object a well-formed
#' [tbl_now()]?*
#'
#' * `is_tbl_now()` answers quietly with `TRUE` or `FALSE`. Use it in an `if`.
#' * `validate_tbl_now()` answers loudly: it stops with an error explaining what
#'   is wrong, and warns about the merely suspicious. Use it when you want the
#'   pipeline to halt rather than carry on with a broken object.
#'
#' Neither checks whether the data are *good* -- only whether the object is put
#' together correctly. For the quality of the data itself, use [diagnose()].
#'
#' @param x An object to check.
#'
#' @inheritParams tbl_now
#' @param warn_now Boolean. Whether to warn if `now` falls before the last report
#'   date, or unreasonably far into the future.
#'
#' @return
#' `is_tbl_now()` returns a single `TRUE` or `FALSE`.
#'
#' `validate_tbl_now()` returns `TRUE` invisibly; it is called for the error or
#' warning it raises when the object is malformed.
#'
#' @details
#' `validate_tbl_now()` and [diagnose()] share one implementation. This function
#' is the *condition* presentation of it: it aborts on the `error` findings and
#' warns about the `warning` ones. [diagnose()] is the *data* presentation, and
#' additionally reports the `note`-level observations that would make every
#' `dplyr` verb noisy if they were emitted here.
#'
#' @seealso
#' [diagnose()] for the same findings returned as a tibble, plus the softer
#' notes; [tbl_now()] to build a valid object; [tbl_now_attributes()] to see what
#' it recorded. The
#' [*Describing and diagnosing a tbl_now* article](https://rodrigozepeda.github.io/tbl.now/articles/describing-and-diagnosing.html)
#' explains what each finding means.
#'
#' @examples
#' data(denguedat)
#' ndata <- tbl_now(denguedat,
#'   event_date = "onset_week",
#'   report_date = "report_week", verbose = FALSE
#' )
#'
#' # A well-formed object passes both checks.
#' is_tbl_now(ndata)
#' validate_tbl_now(ndata)
#'
#' # A plain data.frame is not a tbl_now ...
#' is_tbl_now(data.frame(x = 1:3))
#'
#' # ... and asking for validation says so, with a reason. (Wrapped in try()
#' # because it is meant to fail here.)
#' try(validate_tbl_now(data.frame(x = 1:3)))
#'
#' @export
validate_tbl_now <- function(x, warn_non_uniqueness = FALSE, warn_now = TRUE) {
  # One implementation, two presentations: `diagnose()` returns the findings as
  # data, this returns them as the conditions the class has always emitted.
  #
  # `deep = FALSE` is what keeps that affordable. This function runs inside
  # `tbl_now_can_reconstruct()`, i.e. on every `dplyr` verb, so the engine is
  # told to skip any work that costs a pass over the data and only ever yields
  # a note. `floor = "note"` then reports the errors, the warnings, and the one
  # note this function has always shown as an alert.
  findings <- .tbl_now_findings(
    x,
    checks = .diagnose_validation_checks(),
    by_strata = FALSE,
    warn_non_uniqueness = warn_non_uniqueness,
    warn_now = warn_now,
    floor = "note",
    deep = FALSE,
    assert = FALSE,
    fn = "validate_tbl_now"
  )

  .tbl_now_emit_findings(findings)

  return(invisible(TRUE))
}

#' Decides whether `tbl_now` object can be reconstructed from input
#'
#' @description Uses `tbl_now_reconstruct_internal()` to determine whether the
#' data input can be reconstructed in a valid `tbl_now` object. If it can
#' not, it is returned as a `data.frame`.
#'
#' @inheritParams tbl_now
#'
#' @return A `tbl_now` object (if the input is valid) or a `data.frame`
#' @keywords internal
#' @noRd
tbl_now_reconstruct <- function(data, template) {
  tbl_now_reconstruct_internal(data, template)
}

#' Checks whether the `tbl_now` object is valid
#'
#' @description This is a wrapper for [`validate_tbl_now`] in a [`tryCatch()`]
#' in order to not error if the input object is invalid and returns `TRUE` or
#' `FALSE` on if the object is valid. If the object is valid it can be
#' "reconstructed" and not downgraded to a `data.frame`.
#'
#' @inheritParams tbl_now
#'
#' @return A boolean logical (`TRUE` or `FALSE`)
#' @keywords internal
#' @noRd
tbl_now_can_reconstruct <- function(data) {
  # check whether input is valid, ignoring its class
  valid <- tryCatch(
    {
      validate_tbl_now(data)
    },
    error = function(cnd) FALSE
  )

  # return boolean
  !isFALSE(valid)
}

#' Reconstruct a tbl_now
#'
#' @inheritParams tbl_now
#'
#' @return A `tbl_now` object or a `data.frame`
#'
#' @keywords internal
#' @noRd
tbl_now_reconstruct_internal <- function(data, template) {
  # Copy over *all* attributes except the data.frame essentials
  attrs <- attributes(template)

  keep_attrs <- attrs[setdiff(names(attrs), c("names", "row.names", "class", "groups"))]

  # Enforce protected columns
  protected_cols <- get_protected_cols(template)

  # Check the protected columns and return as data.frame instead
  missing_protected <- setdiff(protected_cols, names(data))
  if (length(missing_protected) > 0) {
    cli::cli_warn("Dropped protected column(?s): {.val {missing_protected}}. Returning a `tibble`")
    return(dplyr::as_tibble(data))
  }

  # Reattach attributes
  for (nm in names(keep_attrs)) {
    attr(data, nm) <- keep_attrs[[nm]]
  }

  # Update strata if columns were dropped
  if (!is.null(get_num_strata(template)) && get_num_strata(template) > 0) {
    # Get the strata still  here
    strata <- intersect(get_strata(template), names(data))

    # Reattach
    attr(data, "strata") <- strata
  }

  # Update covariates if columns were dropped
  if (!is.null(get_num_covariates(template)) && get_num_covariates(template) > 0) {
    # Get the covariates still  here
    covariates <- intersect(get_covariates(template), names(data))

    # Reattach
    attr(data, "covariates") <- covariates
  }

  # Preserve the lazy temporal_effects spec unchanged (it does not reference columns)
  # Only prune the computed_temporal_effect_cols to columns that still exist
  existing_computed <- attr(template, "computed_temporal_effect_cols")
  if (!is.null(existing_computed)) {
    attr(data, "computed_temporal_effect_cols") <- intersect(existing_computed, names(data))
  }


  # Re-infer now if rows changed----
  original_now <- get_now(template)
  now <- tryCatch(
    infer_now(
      data,
      now = original_now,
      event_date = attrs$event_date,
      report_date = attrs$report_date
    ),
    error = function(e) attrs$now
  )
  attr(data, "now") <- now
  if (!identical(original_now, now)) {
    cli::cli_warn("Changed `now` from {.val {original_now}} to {.val {now}}")
  }

  # Restore class
  class(data) <- class(template)

  return(data)
}

#' @rdname validate_tbl_now
#' @export
is_tbl_now <- function(x) {
  inherits(x, "tbl_now") && tbl_now_can_reconstruct(x)
}

#' Subset function for `tbl_now`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Accesors to `tbl_now` elements (rows and columns) as if
#' it was a data.frame
#'
#' @details
#' If the subsetting invalidates the class then a `data.frame`
#' is returned.
#'
#' @param x A `tbl_now` object
#' @inheritParams base::subset
#'
#' @return A `tbl_now` object or a `data.frame`
#' @name assign_tbl
#' @export

#' @rdname assign_tbl
#' @export
`[.tbl_now` <- function(x, ...) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' @rdname assign_tbl
#' @export
`[.grouped_tbl_now` <- function(x, ...) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' Set names on `tbl_now` class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Function for modifying the names of a `tbl_now`
#'
#' @details If the modifying the names invalidates the `tbl_now` object
#' the subsetting will return a data frame with the other attributes of the
#' class preserved.
#'
#' @inheritParams base::names
#'
#' @return A `tbl_now` object or a `data.frame`
#' @name names_tbl_now
#' @export

#' @rdname names_tbl_now
#' @export
`names<-.tbl_now` <- function(x, value) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' @rdname names_tbl_now
#' @export
`names<-.grouped_tbl_now` <- function(x, value) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' Set accessor for `tbl_now` class
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Accessor for `tbl_now` columns
#'
#' @param x A `tbl_now` object
#'
#' @inheritParams base::Extract
#'
#' @return A `tbl_now` object or a `data.frame`
#' @name money_tbl_now
#' @export

#' @rdname money_tbl_now
#' @export
`$<-.tbl_now` <- function(x, name, value) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' @rdname money_tbl_now
#' @export
`$<-.grouped_tbl_now` <- function(x, name, value) {
  out <- NextMethod()
  tbl_now_reconstruct(out, x)
}

#' @importFrom dplyr dplyr_row_slice
#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.tbl_now <- function(data, i, ...) {
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' @importFrom dplyr dplyr_col_modify
#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.tbl_now <- function(data, cols) {
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' @importFrom dplyr dplyr_reconstruct
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.tbl_now <- function(data, template) {
  tbl_now_reconstruct(data, template)
}

#' @importFrom dplyr group_by
#' @exportS3Method dplyr::group_by
group_by.tbl_now <- function(.data, ..., .add = FALSE, drop = dplyr::group_by_drop_default(.data)) {
  grouped_tbl <- NextMethod()

  if (dplyr::is.grouped_df(grouped_tbl)) {
    # Extract grouping information from default method
    grouping_structure <- dplyr::group_data(grouped_tbl)
    # Restore original attributes and add grouping information
    x <- new_grouped_tbl_now(.data, groups = grouping_structure)
  } else {
    # Edge case: no groups were actually provided — reconstruct without losing temporal-effects spec
    x <- do.call(tbl_now, c(
      list(
        data = .data,
        event_date = get_event_date(.data),
        report_date = get_report_date(.data),
        strata = get_strata(.data),
        covariates = get_covariates(.data),
        is_censored = get_is_censored(.data),
        now = get_now(.data),
        event_units = get_event_units(.data),
        report_units = get_event_units(.data),
        data_type = get_data_type(.data),
        case_count = get_case_count(.data),
        verbose = FALSE,
        force = TRUE,
        warn_non_uniqueness = FALSE
      ),
      .confirmation_rebuild_args(.data, .data)
    ))
    attr(x, "temporal_effects") <- get_temporal_effects(.data)
    attr(x, "computed_temporal_effect_cols") <- intersect(get_temporal_effect_cols(.data), names(x))
  }
  x
}

#' @importFrom dplyr rowwise
#' @exportS3Method dplyr::rowwise
rowwise.tbl_now <- function(data, ...) {
  cli::cli_alert_warning(
    "`rowwise` has not yet been implemented for `tbl_now`. Returning a `data.frame`"
  )

  NextMethod()
}


#' @importFrom dplyr dplyr_row_slice
#' @exportS3Method dplyr::dplyr_row_slice
dplyr_row_slice.grouped_tbl_now <- function(data, i, ...) {
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' @importFrom dplyr dplyr_col_modify
#' @exportS3Method dplyr::dplyr_col_modify
dplyr_col_modify.grouped_tbl_now <- function(data, cols) {
  out <- NextMethod()
  dplyr_reconstruct(out, data)
}

#' @importFrom dplyr dplyr_reconstruct
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.grouped_tbl_now <- function(data, template) {
  reconstructed <- tbl_now_reconstruct(data, template)

  if (is_tbl_now(reconstructed) && dplyr::is_grouped_df(template)) {
    # Re-compute group_data from the current row order of `data`,
    # not from the (now stale) template. This is critical after
    # arrange() reorders rows — copying template's group indices
    # would point to wrong rows and break subsequent mutate/lag.
    group_vars <- dplyr::group_vars(template)
    regrouped <- dplyr::group_by(
      dplyr::as_tibble(reconstructed),
      dplyr::across(dplyr::all_of(group_vars))
    )
    fresh_groups <- dplyr::group_data(regrouped)
    reconstructed <- new_grouped_tbl_now(reconstructed, groups = fresh_groups)
  }

  return(reconstructed)
}


#' Drop the `tbl_now` classes without touching anything else
#'
#' Removes the `tbl_now` / `grouped_tbl_now` classes from `x` (leaving any
#' underlying `grouped_df` / `tbl_df` / `data.frame` classes and all attributes
#' in place). Internal lazy-sensitive code uses this to call
#' [tibble::as_tibble()] / [as.data.frame()] *without* triggering the
#' `tbl_now` methods (which materialise the temporal-effect spec): coercion has
#' to stay lazy inside validation, reconstruction, `complete_zeroes()`, etc.
#' On a plain data frame it is a no-op.
#'
#' @param x Any object (typically a `tbl_now`).
#'
#' @return `x` with the `tbl_now` classes removed.
#'
#' @keywords internal
#' @noRd
.declass_tbl_now <- function(x) {
  class(x) <- setdiff(class(x), c("grouped_tbl_now", "tbl_now"))
  x
}

# Based on https://www.bio-ai.org/blog/extending-tibbles/
#' Construct a grouped `tbl_now`
#'
#' Builds a `grouped_tbl_now` from a `tbl_now` and a grouping data frame,
#' inserting the `tbl_now` class in the right position so the S3 methods keep
#' dispatching.
#'
#' @param x A `tbl_now`.
#' @param groups Grouping data frame (as produced by dplyr) to group by.
#'
#' @return A `grouped_tbl_now` object.
#'
#' @keywords internal
#' @noRd
new_grouped_tbl_now <- function(x, groups) {
  x <- dplyr::new_grouped_df(x = x, groups = groups, class = c("grouped_tbl_now"))

  # Get name before tbl_df otherwise it gets lost with the methods
  # see blog (https://www.bio-ai.org/blog/extending-tibbles/)
  tbl_df_location <- grep("tbl_df", class(x), fixed = TRUE)
  class(x) <- append(class(x), "tbl_now", after = tbl_df_location - 1)

  return(x)
}


#' @importFrom dplyr ungroup
#' @exportS3Method dplyr::ungroup
ungroup.grouped_tbl_now <- function(x, ...) {
  # Run default ungrouping.
  tbl <- NextMethod()

  if (dplyr::is_grouped_df(tbl)) {
    # If the tibble is still grouped, we dont need to do anything, as the default dplyr method doesnt call as_tibble
    x <- tbl
  } else {
    # Otherwise the tibble is completely ungrouped and we need to reapply custom attributes, but remove grouping
    # This is most simplest done by simply creating a new tibble subclass
    # This is an edge case if no groups are actually provided. Then simply return a regular subclass
    old_x <- x
    x <- do.call(tbl_now, c(
      list(
        data = tbl,
        event_date = get_event_date(old_x),
        report_date = get_report_date(old_x),
        strata = get_strata(old_x),
        covariates = get_covariates(old_x),
        is_censored = get_is_censored(old_x),
        now = get_now(old_x),
        event_units = get_event_units(old_x),
        report_units = get_report_units(old_x),
        data_type = get_data_type(old_x),
        case_count = get_case_count(old_x),
        verbose = FALSE,
        force = TRUE,
        warn_non_uniqueness = FALSE
      ),
      .confirmation_rebuild_args(old_x, tbl)
    ))
    attr(x, "temporal_effects") <- get_temporal_effects(old_x)
    attr(x, "computed_temporal_effect_cols") <- intersect(get_temporal_effect_cols(old_x), names(x))
  }
  return(x)
}


#' @importFrom dplyr summarise
#' @exportS3Method dplyr::summarise
summarise.tbl_now <- function(.data, ..., .by = NULL, .groups = NULL) {
  # Remove the tbl_now attribute
  class(.data) <- class(.data)[which(!(class(.data) %in% c("grouped_tbl_now", "tbl_now")))]

  # Do normal summarise. This is just a hack to avoid error "Can't supply both `.by` and `.groups`."
  if (is.null(.by) & !is.null(.groups)) {
    summarised_tbl <- dplyr::summarise(.data, ..., .groups = .groups)
  } else if (!is.null(.by) & is.null(.groups)) {
    summarised_tbl <- dplyr::summarise(.data, ..., .by = .by)
  } else if (is.null(.by) & is.null(.groups)) {
    summarised_tbl <- dplyr::summarise(.data, ...)
  } else {
    summarised_tbl <- dplyr::summarise(.data, ..., .by = .by, .groups = .groups)
  }

  result <- tryCatch(
    {
      ungrouped <- ungroup(summarised_tbl)
      tmp <- do.call(tbl_now, c(
        list(
          data = ungrouped,
          event_date = get_event_date(.data),
          report_date = get_report_date(.data),
          strata = get_strata(.data),
          covariates = get_covariates(.data),
          is_censored = get_is_censored(.data),
          now = get_now(.data),
          event_units = get_event_units(.data),
          report_units = get_event_units(.data),
          data_type = get_data_type(.data),
          case_count = get_case_count(.data),
          verbose = FALSE,
          force = TRUE,
          warn_non_uniqueness = FALSE,
          align_weeks = FALSE
        ),
        # Every fixed attribute list is a place the confirmation process can be
        # dropped in silence.
        .confirmation_rebuild_args(.data, ungrouped)
      ))
      attr(tmp, "temporal_effects") <- get_temporal_effects(.data)
      attr(tmp, "computed_temporal_effect_cols") <- intersect(get_temporal_effect_cols(.data), names(tmp))
      tmp
    },
    error = function(e) {
      cli::cli_warn("Dropping `tbl_now` attributes and converting to `tibble`")
      summarised_tbl
    }
  )

  result
}

#' @importFrom dplyr summarize
#' @exportS3Method dplyr::summarize
summarize.tbl_now <- function(.data, ..., .by = NULL, .groups = NULL) {
  summarise.tbl_now(.data, ..., .by = .by, .groups = .groups)
}

#' @importFrom dplyr summarise
#' @exportS3Method dplyr::summarise
summarise.grouped_tbl_now <- function(.data, ..., .by = NULL, .groups = NULL) {
  summarise.tbl_now(.data, ..., .by = .by, .groups = .groups)
}

#' @importFrom dplyr summarize
#' @exportS3Method dplyr::summarize
summarize.grouped_tbl_now <- function(.data, ..., .by = NULL, .groups = NULL) {
  summarise.tbl_now(.data, ..., .by = .by, .groups = .groups)
}

#' @importFrom dplyr reframe
#' @exportS3Method dplyr::reframe
reframe.tbl_now <- function(.data, ..., .by = NULL) {
  # Extract the grouping vars BEFORE stripping so we can pass them to dplyr
  # explicitly (via all_of()) rather than relying on the groups attribute,
  # which would cause dplyr's eval_select_by to fire an "external vector" warning.
  if (is.null(.by) && dplyr::is.grouped_df(.data)) {
    .by <- dplyr::group_vars(.data)
    if (length(.by) == 0L) .by <- NULL
  }

  # Strip tbl_now / grouping classes AND the groups attribute so dplyr sees a
  # plain data frame with no residual grouping metadata.
  class(.data) <- class(.data)[which(!(class(.data) %in% c("grouped_tbl_now", "tbl_now", "grouped_df")))]
  attr(.data, "groups") <- NULL

  # Do normal reframe — pass .by lazily (as a promise inside the call) so that
  # all_of() is evaluated inside dplyr's selecting context rather than eagerly
  # outside it, avoiding both the "external vector" and the "all_of() outside
  # selection" tidyselect deprecation warnings.
  if (!is.null(.by) && length(.by) > 0L) {
    reframed_tbl <- dplyr::reframe(.data, ..., .by = dplyr::all_of(.by))
  } else {
    reframed_tbl <- dplyr::reframe(.data, ...)
  }

  result <- tryCatch(
    {
      tmp <- tbl_now(
        data = reframed_tbl,
        event_date = get_event_date(.data),
        report_date = get_report_date(.data),
        strata = get_strata(.data),
        covariates = get_covariates(.data),
        is_censored = get_is_censored(.data),
        now = get_now(.data),
        event_units = get_event_units(.data),
        report_units = get_event_units(.data),
        data_type = get_data_type(.data),
        case_count = get_case_count(.data),
        verbose = FALSE,
        force = TRUE,
        warn_non_uniqueness = FALSE,
        align_weeks = FALSE
      )
      attr(tmp, "temporal_effects") <- get_temporal_effects(.data)
      attr(tmp, "computed_temporal_effect_cols") <- intersect(get_temporal_effect_cols(.data), names(tmp))
      tmp
    },
    error = function(e) {
      cli::cli_warn("Dropping `tbl_now` attributes and converting to `tibble`")
      reframed_tbl
    }
  )

  result
}

#' @importFrom dplyr reframe
#' @exportS3Method dplyr::reframe
reframe.grouped_tbl_now <- function(.data, ..., .by = NULL) {
  reframe.tbl_now(.data, ..., .by = .by)
}

#' @importFrom dplyr rename
#' @exportS3Method dplyr::rename
rename.tbl_now <- function(.data, ...) {
  # This is drawn from dplyr rename source code
  # https://github.com/tidyverse/dplyr/blob/main/R/rename.R
  loc <- tidyselect::eval_rename(rlang::expr(c(...)), .data)
  .data <- rename_attributes(.data, loc)

  # Use the normal rename
  NextMethod()
}

#' @importFrom dplyr rename_with
#' @exportS3Method dplyr::rename_with
rename_with.tbl_now <- function(.data, .fn, .cols = dplyr::everything(), ...) {
  # Resolve the columns to positions with .tbl_now_eval_select so that a plain
  # character .cols (e.g. "sex") does not trigger the tidyselect
  # "external vector" deprecation. We then rename by position directly instead
  # of delegating to NextMethod(): NextMethod() would re-evaluate the original
  # .cols promise inside dplyr's rename_with.data.frame and re-raise that warning.
  .fn <- rlang::as_function(.fn)
  loc <- .tbl_now_eval_select(rlang::enquo(.cols), .data)
  old_names <- names(.data)[loc]
  new_names <- vapply(old_names, .fn, character(1), ...)
  names(loc) <- new_names

  # Update the tbl_now attribute references (uses the *old* column names, so do
  # this before renaming the columns themselves).
  .data <- rename_attributes(.data, loc)

  # Rename the selected columns in place.
  all_names <- names(.data)
  all_names[loc] <- new_names
  names(.data) <- all_names

  .data
}

#' Update `tbl_now` attribute references after a rename
#'
#' When columns are renamed, the attributes that store column *names*
#' (`event_date`, `report_date`, `strata`, `covariates`, ...) must be updated to
#' the new names. Renaming a protected generated column downgrades the object to
#' a `tibble`.
#'
#' @param .data A `tbl_now` object being renamed.
#' @param loc A named integer vector (as returned by [tidyselect::eval_select()])
#'   mapping new names to the original column positions.
#'
#' @return The `.data` object with its `tbl_now` attributes updated to the new
#'   column names (or a downgraded `tibble` if a protected column was renamed).
#'
#' @keywords internal
#' @noRd
rename_attributes <- function(.data, loc) {
  # Check that .event_num, .report_num, .delay are not renamed
  loc_protected <- which(names(.data) %in% get_protected_generated_cols(.data))
  if (any(loc_protected %in% loc)) {
    changed_loc <- names(.data)[loc_protected][which(loc_protected %in% loc)]
    cli::cli_alert_warning(
      "Changing the name of protected columns {.val {changed_loc}} will result in a tibble"
    )
    .data <- dplyr::as_tibble(.data)
  }

  # Check the ones that changed and change in attributes (such as report date)
  protected_given_cols <- get_protected_given_cols(.data)
  loc_changeable <- which(names(.data) %in% protected_given_cols)
  if (any(loc_changeable %in% loc)) {
    changed_names <- names(.data)[loc_changeable[which(loc_changeable %in% loc)]]
    changed_attributes <- protected_given_cols[which(protected_given_cols %in% changed_names)]
    for (atrb in 1:length(changed_attributes)) {
      new_name_pos <- which(names(.data) == changed_attributes[atrb])
      attr(.data, names(changed_attributes)[atrb]) <- names(loc)[which(loc == new_name_pos)]
    }
  }

  # Check the strata
  protected_strata_cols <- get_strata(.data)
  loc_changeable <- which(names(.data) %in% protected_strata_cols)
  if (any(loc_changeable %in% loc)) {
    kept_names <- names(.data)[loc_changeable[which(!(loc_changeable %in% loc))]]
    new_names <- names(loc)[which(loc %in% loc_changeable)]
    attr(.data, "strata") <- c(kept_names, new_names)
  }

  # Check the covariates
  protected_covariate_cols <- get_covariates(.data)
  loc_changeable <- which(names(.data) %in% protected_covariate_cols)
  if (any(loc_changeable %in% loc)) {
    kept_names <- names(.data)[loc_changeable[which(!(loc_changeable %in% loc))]]
    new_names <- names(loc)[which(loc %in% loc_changeable)]
    attr(.data, "covariates") <- c(kept_names, new_names)
  }

  return(.data)
}
