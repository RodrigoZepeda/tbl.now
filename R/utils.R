#' Function to get an attribute from an object or default value
#'
#' Checks if attribute exists in object and returns `default` if not
#'
#' @param x An object with attribute `name`
#' @param name The name of the attribute in `x`
#' @param default (optional) The default value of attribute `name` in `x`
#'
#' @examples
#' \dontrun{
#' #Create an object
#' x <- 42
#' attr(x, "meaning") <- "meaning of life"
#'
#' #See the attributes
#' attributes(x)
#'
#' #Return the meaning attribute
#' attr_default(x, "meaning")
#'
#' #Return null for attribute that doesn't exist
#' attr_default(x, "DOES_NOT_EXIST")
#'
#' #Or return default when it doesn't exist
#' attr_default(x, "DOES_NOT_EXIST", default = 15)
#'
#' }
#'
#' @keywords internal
attr_default <- function(x, name, default = NULL) {
  val <- attr(x, name, exact = TRUE)
  if (is.null(val)) default else val
}

# NEED TO IMPROVE IT TO WORK WITH COUNT BY DOING WEIGHTS
#' #' Summary of a `tbl_now`
#' #'
#' #' Generates a summary of a `tbl_now` object
#' #'
#' #' @inheritParams base::summary
#' #'
#' #' @return A `tibble` with summary statistics
#' #'
#' #' @export
#' summary.tbl_now <- function(x, ...) {
#'
#'   #Get the indicators by strata
#'   result_stratified <- dplyr::tibble()
#'   if (get_num_strata(x) > 0){
#'     for (st in get_strata(x)){
#'       result_stratified <- suppressWarnings({
#'         x %>%
#'           dplyr::group_by(dplyr::across(st)) %>%
#'           dplyr::summarise(
#'             min_report = min(!!as.symbol(".report_num")),
#'             q025       = quantile(!!as.symbol(".report_num"), 0.025),
#'             q05        = quantile(!!as.symbol(".report_num"), 0.05),
#'             q10        = quantile(!!as.symbol(".report_num"), 0.10),
#'             q25        = quantile(!!as.symbol(".report_num"), 0.25),
#'             median     = median(!!as.symbol(".report_num")),
#'             q75        = quantile(!!as.symbol(".report_num"), 0.75),
#'             q90        = quantile(!!as.symbol(".report_num"), 0.90),
#'             q95        = quantile(!!as.symbol(".report_num"), 0.95),
#'             q975       = quantile(!!as.symbol(".report_num"), 0.975),
#'             max_report = max(!!as.symbol(".report_num")),
#'             mean       = mean(!!as.symbol(".report_num")),
#'             sd         = sd(!!as.symbol(".report_num")),
#'           ) %>%
#'           dplyr::mutate(!!as.symbol("units") := get_event_units(x)) %>%
#'           dplyr::rename(!!as.symbol("strata_val") := !!as.symbol(st)) %>%
#'           dplyr::mutate(!!as.symbol("strata") := !!st)
#'         }) %>%
#'         dplyr::bind_rows(result_stratified)
#'     }
#'   }
#'
#'
#'
#'   #Get the indicators globally
#'   result_global <- suppressWarnings({
#'     x %>%
#'       dplyr::summarise(
#'         min_report = min(!!as.symbol(".report_num")),
#'         q025       = quantile(!!as.symbol(".report_num"), 0.025),
#'         q05        = quantile(!!as.symbol(".report_num"), 0.05),
#'         q10        = quantile(!!as.symbol(".report_num"), 0.10),
#'         q25        = quantile(!!as.symbol(".report_num"), 0.25),
#'         median     = median(!!as.symbol(".report_num")),
#'         q75        = quantile(!!as.symbol(".report_num"), 0.75),
#'         q90        = quantile(!!as.symbol(".report_num"), 0.90),
#'         q95        = quantile(!!as.symbol(".report_num"), 0.95),
#'         q975       = quantile(!!as.symbol(".report_num"), 0.975),
#'         max_report = max(!!as.symbol(".report_num")),
#'         mean       = mean(!!as.symbol(".report_num")),
#'         sd         = sd(!!as.symbol(".report_num")),
#'      ) %>%
#'       dplyr::mutate(!!as.symbol("units") := get_event_units(x)) %>%
#'       dplyr::mutate(!!as.symbol("strata_val") := "Overall") %>%
#'       dplyr::mutate(!!as.symbol("strata") := "Overall")
#'   })
#'
#'   return(result_global %>% dplyr::bind_rows(result_stratified))
#'
#' }
#'
#'
