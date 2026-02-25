#' Complete zeroes
#'
#' Takes a `tbl.now` object and completes observations
#' for event_dates or onset_weeks that have not been registered
#' by each strata with a 0
#'
#' @param x A `tbl.now` object.
#' @param max_delay Maximum delay to fill. For example if set to 5 it will complete
#' with 0's all reports with delays 0 to 4. But will not fill other delays (say 6)
#'
#' @examples
#' ndata <- dplyr::tibble(
#'   event  = rep(c(as.Date("2020/01/01"), as.Date("2020/01/01"),
#'                  as.Date("2020/01/02"), as.Date("2020/01/04"),
#'                  as.Date("2020/01/04")), 2),
#'   report = rep(c(as.Date("2020/01/01"), as.Date("2020/01/02"),
#'                  as.Date("2020/01/02"), as.Date("2020/01/04"),
#'                  as.Date("2020/01/05")), 2),
#'   n = rpois(10, lambda = 5),
#'   sex = c(rep("Male", 5), rep("Female", 5))
#' )
#' ndata <- tbl_now(ndata, event_date = event, report_date = report,
#'      verbose = FALSE, strata = sex, case_count = n, data_type = "count-incidence")
#'
#' #Notice that ndata has no 2020-01-03 event date
#' ndata
#'
#' #But complete zeroes adds it with a 0
#' complete_zeroes(ndata)
#'
#' #Also works for count-cumulative
#' ndata %>%
#'  to_count("count-cumulative") %>%
#'  complete_zeroes() %>%
#'  dplyr::arrange(event, sex, report)
#'
#' @export
complete_zeroes <- function(x, max_delay = NULL){

  if (is.null(max_delay)){
    max_delay <- suppressWarnings(
      x %>%
        dplyr::distinct(!!as.symbol(".delay")) %>%
        dplyr::pull() %>%
        max()
    )
  }

  if (!is_tbl_now(x)){
    cli::cli_abort(
      "Object `x` should be a `tbl.now`. See {.help tbl_now} on how to create one."
    )
  }

  if (get_data_type(x) == "linelist"){
    cli::cli_abort(
      "Can only complete 'count-incidence'  or 'count-cumualtive' data."
    )
  }

  #Get the initial event
  min_event <- suppressWarnings(
    x %>%
      dplyr::filter(!!as.symbol(get_event_date(x)) == min(!!as.symbol(get_event_date(x)))) %>%
      dplyr::distinct(!!as.symbol(get_event_date(x))) %>%
      dplyr::pull()
  )

  #Get the final event
  max_event <- suppressWarnings(
    x %>%
      dplyr::filter(!!as.symbol(get_event_date(x)) == max(!!as.symbol(get_event_date(x)))) %>%
      dplyr::distinct(!!as.symbol(get_event_date(x))) %>%
      dplyr::pull()
  )

  #Get the final report
  max_report <- suppressWarnings(
    x %>%
      dplyr::distinct(!!as.symbol(get_report_date(x))) %>%
      dplyr::pull() %>%
      max()
  )


  if (get_event_units(x) != get_report_units(x)){
    cli::cli_abort(
      "Cannot work when event and report units are different. Please input manually,"
    )
  }

  #Warn to recalculate temporal effects
  if (length(get_temporal_effects(x)) > 0){
    cli::cli_alert_warning(
      "Some temporal effects have been lost. Please recalculate and add with {.help add_temporal_effects}"
    )
  }

  #Check units to fill 0's
  if (get_event_units(x) == "weeks"){
    units_by <- "1 week"
  } else if (get_event_units(x) == "days"){
    units_by <- "1 day"
  } else if (get_event_units(x) == "numeric"){
    units_by <- 1
  }

  #Create a table with all dates
  event_dates <- dplyr::tibble(!!as.symbol(get_event_date(x)) := seq(min_event, max_event, by = units_by))

  event_dict <- event_dates %>%
    dplyr::mutate(.event_num_new = 0:(dplyr::n() - 1))

  #Add report num
  complete_x <- tidyr::expand_grid(
    event_dates,
      .delay = seq(0, max_delay, by = 1),
        x %>%
          as.data.frame() %>%
          dplyr::distinct(dplyr::across(dplyr::all_of(tbl.now::get_strata(x)))),
        x %>%
          as.data.frame() %>%
          dplyr::distinct(dplyr::across(dplyr::all_of(tbl.now::get_is_censored(x)))),
    ) %>%
    #Add the additional rows you lose by not completing them
    dplyr::bind_rows(
      x %>%
        as.data.frame() %>%
        dplyr::filter(!!as.symbol(".delay") > !!max_delay) %>%
        dplyr::distinct(
          dplyr::across(
            dplyr::all_of(
              c(get_strata(x), get_is_censored(x), get_event_date(x), get_report_date(x), ".delay")
            )
          )
        )
      ) %>%
    dplyr::distinct() %>%
    dplyr::left_join(event_dict, by = get_event_date(x)) %>%
    dplyr::mutate(!!as.symbol(".report_num_new") := !!as.symbol(".event_num_new") + !!as.symbol(".delay"))

  #Now add reports back
  if (get_event_units(x) == "weeks"){
    complete_x <- complete_x %>%
      dplyr::mutate(!!as.symbol(get_report_date(x)) := !!as.symbol(get_event_date(x)) + lubridate::weeks(!!as.symbol(".delay")))
  } else if (get_event_units(x) == "days"){
    complete_x <- complete_x %>%
      dplyr::mutate(!!as.symbol(get_report_date(x)) := !!as.symbol(get_event_date(x)) + lubridate::days(!!as.symbol(".delay")))
  } else if (get_event_units(x) == "numeric"){
    complete_x <- complete_x %>%
      dplyr::mutate(!!as.symbol(get_report_date(x)) := !!as.symbol(get_event_date(x)) + !!as.symbol(".delay"))
  }

  #Now complete
  x <- x %>%
    dplyr::full_join(complete_x, by = c(get_event_date(x), get_strata(x), get_report_date(x), ".delay"))

  #Remove and rename
  x <- x %>%
    dplyr::mutate(!!as.symbol(".event_num") := !!as.symbol(".event_num_new")) %>%
    dplyr::mutate(!!as.symbol(".report_num") := !!as.symbol(".report_num_new")) %>%
    dplyr::select(-!!as.symbol(".event_num_new"), -!!as.symbol(".report_num_new"))

  #Fix the 0 case for count-cumulative
  if (get_data_type(x) == "count-cumulative"){
    x <- x %>%
      dplyr::mutate(!!as.symbol(get_case_count(x)) :=
                      dplyr::if_else(is.na(!!as.symbol(get_case_count(x)) & !!as.symbol(".delay") == 0), 0, !!as.symbol(get_case_count(x))))


    if (max_delay > 0){
      for (dval in 1:max_delay){

        x <- x %>%
          dplyr::arrange(!!as.symbol(get_report_date(x))) %>%
          group_by(dplyr::pick(get_event_date(x), get_strata(x)), get_is_censored(x))

        x <- x %>%
          dplyr::mutate(!!as.symbol(get_case_count(x)) :=
                          dplyr::if_else(is.na(!!as.symbol(get_case_count(x))) & !!as.symbol(".delay") == !!dval,
                                         dplyr::lag(!!as.symbol(get_case_count(x)), default = 0.0), !!as.symbol(get_case_count(x)))) %>%
          ungroup()

      }
    }
  }

  # #Replace whatever is missing with 0
   x <- x %>%
     dplyr::mutate(!!as.symbol(get_case_count(x)) := tidyr::replace_na(!!as.symbol(get_case_count(x)), 0.0))

   # Don't look into the future
   x <- x %>%
     dplyr::filter(!!as.symbol(get_report_date(x)) < !!max_report)

  return(x)

}
