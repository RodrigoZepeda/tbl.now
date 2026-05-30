# Pillar extension functions that work for pretty printing a `tbl_now`
# This file includes the following functions: `tbl_sum`, `tbl_format_footer` and `ctl_new_pillar`
# See the Custom Formatting vignette from pillar for more info:
# https://pillar.r-lib.org/articles/extending.html

# Helper: format a list of temporal-effect specs into a human-readable string
.format_temporal_effects_spec <- function(specs) {
  if (is.null(specs) || length(specs) == 0) return(NULL)

  parts <- vapply(specs, function(sp) {
    t      <- sp$t_effects
    effs   <- character(0)
    if (t@day_of_week)   effs <- c(effs, "day_of_week")
    if (t@weekend)       effs <- c(effs, "weekend")
    if (t@day_of_month)  effs <- c(effs, "day_of_month")
    if (t@month_of_year) effs <- c(effs, "month_of_year")
    if (t@week_of_year)  effs <- c(effs, "week_of_year")
    if (length(t@seasons) > 0) {
      periods <- t@seasons * t@season_length
      effs <- c(effs, paste0("season(", paste(periods, collapse = ","), ")"))
    }
    if (!is.null(t@holidays)) effs <- c(effs, "holidays")
    label <- if (sp$date_type == "event_date") "[event_date]" else "[report_date]"
    paste0(label, " ", paste(effs, collapse = ", "))
  }, character(1))

  paste(parts, collapse = " | ")
}

#' @importFrom pillar tbl_sum
#' @exportS3Method pillar::tbl_sum
tbl_sum.tbl_now <- function(x, ...) {
  default_header <- NextMethod()
  c(default_header,
    "Data type" = cli::cli_fmt({cli::cli_text("{.val {get_data_type(x)}}")}),
    "Frequency" = cli::cli_fmt({
      cli::cli_text("Event: {.code {get_event_units(x)}} | Report: {.code {get_report_units(x)}}")
    })
  )
}

#' @importFrom pillar tbl_format_footer
#' @exportS3Method pillar::tbl_format_footer
tbl_format_footer.tbl_now <- function(x, ...) {
  default_footer <- NextMethod()

  footer <- cli::cli_fmt({
    cli::cli_rule()
    cli::cli_text("Now: {.val {get_now(x)}} | Event date: {.val {get_event_date(x)}} | Report date: {.val {get_report_date(x)}}")
    if (length(get_is_censored(x)) > 0){
      cli::cli_text("Right-censored indicator: {.val {get_is_censored(x)}}")
    }
    if (get_num_strata(x) > 0){
      cli::cli_text("Strata: {.val {get_strata(x)}}")
    }
    if (get_num_covariates(x) > 0){
      cli::cli_text("Covariates: {.val {get_covariates(x)}}")
    }

    # Show temporal-effects spec and (if computed) the column names
    specs    <- get_temporal_effects(x)
    computed <- get_temporal_effect_cols(x)
    if (!is.null(specs) && length(specs) > 0) {
      spec_str <- .format_temporal_effects_spec(specs)
      if (length(computed) > 0) {
        cli::cli_text("T. effects: {spec_str}")
        cli::cli_text("T. effect cols: {.val {computed}}")
      } else {
        cli::cli_text("T. effects (lazy): {spec_str}")
      }
    }

    cli::cli_rule()
  })
  c(paste("#",footer), default_footer)
}

#' @importFrom pillar ctl_new_pillar
#' @exportS3Method pillar::ctl_new_pillar
ctl_new_pillar.tbl_now <- function(controller, x, width, ...) {
  out <- NextMethod()

  cval <- out$title[[1]][[1]]

  if (!is.null(cval)){
    if (cval == get_event_date(controller)) {
      annotation <- "[event_date]"
    } else if (cval == get_report_date(controller)) {
      annotation <- "[report_date]"
    } else if (!is.null(get_strata(controller)) && (cval %in% get_strata(controller))) {
      annotation <- "[strata]"
    } else if (!is.null(get_covariates(controller)) && (cval %in% get_covariates(controller))) {
      annotation <- "[covariate]"
    } else if (identical(cval, get_is_censored(controller))) {
      annotation <- "[is_censored]"
    } else if (identical(cval, get_case_count(controller))) {
      annotation <- "[cases]"
    } else if (length(get_temporal_effect_cols(controller)) > 0 &&
               (cval %in% get_temporal_effect_cols(controller))) {
      annotation <- "[t_effect]"
    } else {
      annotation <- "[...]"
    }
  } else {
    annotation <- NULL
  }

  pillar::new_pillar(list(
    title = out$title,
    type  = out$type,
    ncast = pillar::new_pillar_component(list(pillar::style_subtle(annotation)), width = 1),
    data  = out$data
  ))
}
