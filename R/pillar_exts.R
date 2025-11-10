# Pillar extension functions that work for pretty printing a `tbl_now`

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
    cli::cli_text("Now: {.val {attr(x, 'now')}} | Event date: {.val {attr(x, 'event_date')}} | Report date: {.val {attr(x, 'report_date')}}")
    if (num_strata > 0){
      cli::cli_text("Strata: {.val {attr(x, 'strata')}}")
    }
    if (num_covariates > 0){
      cli::cli_text("Covariates: {.val {attr(x, 'covariates')}}")
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
    if (cval == attr(controller, "event_date")) {
      annotation <- "[event_date]"
    } else if (cval == attr(controller, "report_date")) {
      annotation <- "[report_date]"
    } else if (!is.null(attr(controller,"strata")) && cval %in% attr(controller,"strata")) {
      annotation <- "[strata]"
    } else if (!is.null(attr(controller,"covariates")) && cval %in% attr(controller,"covariates")) {
      annotation <- "[covariate]"
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
