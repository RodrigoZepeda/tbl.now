# Pillar extension functions that work for pretty printing a `tbl_now`
# This file includes the following functions: `tbl_sum`, `tbl_format_footer` and `ctl_new_pillar`
# See the Custom Formatting vignette from pillar for more info:
# https://pillar.r-lib.org/articles/extending.html

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

  #Get the defaults
  num_strata     <- attr_default(x, "num_strata", 0)
  num_covariates <- attr_default(x, "num_covariates", 0)

  footer <- cli::cli_fmt({
    cli::cli_rule()
    cli::cli_text("Now: {.val {get_now(x)}} | Event date: {.val {get_event_date(x)}} | Report date: {.val {get_report_date(x)}}")
    if (num_strata > 0){
      cli::cli_text("Strata: {.val {get_strata(x)}}")
    }
    if (num_covariates > 0){
      cli::cli_text("Covariates: {.val {get_covariates(x)}}")
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
    } else if (!is.null(get_temporal_effects(controller)) && (cval %in% get_temporal_effects(controller))) {
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
