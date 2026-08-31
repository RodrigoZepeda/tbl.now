#' denguedat: Dengue fever individual-level reporting data from Puerto Rico
#'
#' Surveillance data from CDC Division of Vector-Borne Diseases.
#' 1990-2010 case reporting data included.
#'
#' @details
#' Each row represents a case with the columns indicating the following:
#' * `onset_week`:  the week of symptom onset.
#' * `report_week`: the week of case report.
#' * `gender`: the gender of the infected individual (randomly assigned with 0.5:0.5 probability
#' of "Male"/"Female").
#'
#' @note
#' Data originally from the `NobBS` package. While `onset_week` and `report_week` correspond to
#' actual observed data the `gender` was constructed exclusively for the examples of `NobBS`. It
#' is a synthetic (simulated) variable and does not correspond to any reality.
#'
#' @docType data
#'
#' @usage data(denguedat)
#'
#' @format A data frame with 52,987 rows (one per case) and 3 variables:
#' \describe{
#'   \item{onset_week}{`Date`. The week symptoms began -- the *event* date.}
#'   \item{report_week}{`Date`. The week the case reached the surveillance
#'     system -- the *report* date. Always on or after `onset_week`.}
#'   \item{gender}{`character`. `"Male"` or `"Female"`. Synthetic; see the note.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' MCGOUGH, Sarah F., et al. Nowcasting by Bayesian Smoothing: A flexible, generalizable model for
#' real-time epidemic tracking. PLoS computational biology, 2020, vol. 16, no 4, p. e1007735.
#'
#' @seealso
#' [tbl_now()] to declare the date columns; [summary()][tbl_now_summary] and
#' [diagnose()] to inspect the result; the package's other datasets --
#' [denguedat], [mpoxdat], [flusight], [covid_colombia], [covid_us] and
#' [hai_bucaramanga].
#'
#' @examples
#' data(denguedat)
#' head(denguedat)
#'
#' # The two dates every nowcast needs. Weekly data, twenty years of it.
#' range(denguedat$onset_week)
#'
#' # Declaring them turns the data frame into a tbl_now.
#' dengue <- tbl_now(denguedat,
#'   event_date = onset_week, report_date = report_week,
#'   strata = gender, verbose = FALSE
#' )
#' dengue
#'
#' # Most cases arrive within a week or two of onset; a few take much longer.
#' summary(as.numeric(dengue$.delay))
#' @md
"denguedat"

#' flusight: NHSN Weekly Hospital Respiratory Data from FluSight
#'
#' FluSight's weekly hospital admission prediction targets
#' based on the 'total number of new hospital admissions of patients with confirmed
#' influenza captured during the reporting week' reported through CDC's NHSN
#' (the dataset formerly known as HHS-Protect), Weekly Hospital Respiratory Data.
#' Data was downloaded on November 12th 2025.
#'
#' Data represents how many cases were considered influenza during the week of
#' *target_end_date* given the information known until week *as_of*. Note
#' that *as_of* is always one week ahead of *target_end_date*.
#'
#' This is count data with 452,567 rows and 4 columns:
#' * `as_of`: The report date -- the date the snapshot was taken, i.e. what was
#'   known as of that week.
#' * `target_end_date`: The event date -- the week the admissions occurred.
#' * `location_name`: State, district or territory (53 levels).
#' * `observation`: Case counts for those dates. `NA` for 1,152 rows.
#'
#' Together, `as_of`, `target_end_date` and `location_name` form a unique key.
#'
#' @section Duplicate rows removed:
#'
#' The upstream FluSight `time-series.csv` ships exact duplicate rows -- 39,139
#' of them in this snapshot. They were dropped with [dplyr::distinct()] before
#' the dataset was saved (issue #25), taking it from 491,706 to 452,567 rows.
#'
#' The removal is lossless: every repeated
#' (`as_of`, `target_end_date`, `location_name`) key carried an *identical*
#' `observation`, with no conflicting values anywhere in the file, so no
#' information was discarded and the key became unique. If you download the
#' upstream file yourself you will still see the duplicates and should
#' `distinct()` them before use.
#'
#' @references
#' Target data from Flusight. Online:
#' <https://github.com/cdcepi/FluSight-forecast-hub/blob/main/target-data/time-series.csv>
#'
#' @docType data
#'
#' @usage data(flusight)
#'
#' @format A data frame with 452,567 rows and 4 variables:
#' \describe{
#'   \item{as_of}{`Date`. The date this row's value was published -- the
#'     *report* date. The same week appears many times, once per publication.}
#'   \item{target_end_date}{`Date`. The week being reported on -- the *event*
#'     date.}
#'   \item{location_name}{`character`. US state or territory.}
#'   \item{observation}{`numeric`. Hospital admissions reported for that week as
#'     of that publication date.}
#' }
#'
#' @keywords datasets
#'
#' @seealso
#' [tbl_now()] to declare the date columns; [summary()][tbl_now_summary] and
#' [diagnose()] to inspect the result; the package's other datasets --
#' [denguedat], [mpoxdat], [flusight], [covid_colombia], [covid_us] and
#' [hai_bucaramanga].
#'
#' @examples
#' data(flusight)
#' head(flusight)
#'
#' ## This is count data: one row per (week, publication date, state).
#' nrow(flusight)
#' length(unique(flusight$location_name))
#'
#' # One state is enough to see the reporting process.
#' texas <- flusight[flusight$location_name == "Texas", ]
#' flu <- tbl_now(texas,
#'   event_date = target_end_date, report_date = as_of,
#'   case_count = observation, verbose = FALSE
#' )
#' flu
#'
#' # `as_of` is not always the same weekday, so some delays are not whole weeks.
#' ## `align_weeks()` fixes that.
#' mean(flu$.delay != round(flu$.delay))
#' @md
"flusight"

#' mpoxdat: Mpox reporting data from the 2022 New York City outbreak
#'
#' Surveillance line list data provided by the New York City (NYC) Health Department
#' at https://github.com/nychealth/mpox_nowcast_eval, to accompany a nowcasting
#' performance evaluation (doi: 10.2196/56495). Patients with a confirmed or probable mpox
#' diagnosis or illness onset from July 8 through September 30, 2022 were included. The original
#' dataset was aggregated and pre-processed as described in the note below.
#'
#' @details
#' This is **count** data: each row holds the number of cases sharing a
#' diagnosis date, a report date and a race. The columns are as follows:
#' * `dx_date`: is the specimen collection date of the first positive mpox laboratory result,
#' * `dx_report_date`: is the date the report of first positive mpox laboratory result was received by the NYC Health Department,
#' * `n`: the case count of individuals within those dates.
#' * `race`: the race corresponding to those cases. Race was randomly assigned with probabilities
#' "Non-Hispanic White" = 0.309, "Hispanic" = 0.283, "Black" = 0.202, "Asian" = 0.156, and "Other" = 0.05
#' which follow what has been reported for the US Census.
#'
#' @note
#' While `dx_date`, `dx_report_date` and `n` correspond to actual observed data the `race` was
#' constructed exclusively for the examples of this package. It is a synthetic (simulated)
#' variable and does not correspond to any reality.
#'
#' @references
#' ROHRER, Rebecca, et al. Nowcasting to Monitor Real-Time Mpox Trends During the 2022
#' Outbreak in New York City: Evaluation Using Reportable Disease Data Stratified by Race
#' or Ethnicity. Online Journal of Public Health Informatics, 2025, vol. 17, no 1, p. e56495.
#'
#' @docType data
#'
#' @usage data(mpoxdat)
#'
#' @format A data frame with 1,417 rows and 4 variables:
#' \describe{
#'   \item{dx_date}{`Date`. Specimen collection date of the first positive
#'     result -- the *event* date.}
#'   \item{dx_report_date}{`Date`. When the Health Department received that
#'     result -- the *report* date.}
#'   \item{race}{`character`. Synthetic; see the note.}
#'   \item{n}{`integer`. Number of cases with that combination.}
#' }
#'
#' @keywords datasets
#'
#' @seealso
#' [tbl_now()] to declare the date columns; [summary()][tbl_now_summary] and
#' [diagnose()] to inspect the result; the package's other datasets --
#' [denguedat], [mpoxdat], [flusight], [covid_colombia], [covid_us] and
#' [hai_bucaramanga].
#'
#' @examples
#' data(mpoxdat)
#' head(mpoxdat)
#'
#' # Count data, and daily rather than weekly -- unlike denguedat.
#' mpox <- tbl_now(mpoxdat,
#'   event_date = dx_date, report_date = dx_report_date,
#'   case_count = n, strata = race, verbose = FALSE
#' )
#' mpox
#'
#' # A short, sharp outbreak: about three months of data.
#' range(mpoxdat$dx_date)
#' sum(mpoxdat$n)
#' @md
"mpoxdat"

#' covid_us: CDC COVID-19 Case Surveillance Public Use Data (2020-2021)
#'
#' A compact aggregation of the U.S. CDC's individual-level COVID-19 case
#' surveillance database, prepared to illustrate **batch reporting**. Each row is
#' a unique (event date, report date) pair with the number of cases `n`.
#'
#' In the nowcasting context the **event date** is `cdc_case_earliest_dt` (the
#' earlier of the clinical/specimen date and the date the case was received by
#' CDC) and the **report date** is `cdc_report_dt` (the date the case was first
#' reported to CDC). The delay between them is enormous and heavily right-skewed:
#' cases were reported to CDC not smoothly but in large backlog dumps -- a textbook
#' batch-reporting pattern that [diagnose_batches()] and [transport_discriminant()]
#' recover.
#'
#' Cases are kept when both their event date and their report date fall between
#' 2020-01-01 and 2021-12-31 -- a self-consistent "as of the end of 2021" snapshot,
#' so the epidemic and its reporting are seen over the same two years. The handful
#' of rows whose report date precedes their event date (data-entry errors) were
#' dropped. See `data-raw/covid_us.R` for the exact duckdb aggregation of the
#' 14 GB source file.
#'
#' @format A data frame with three variables:
#' \describe{
#'   \item{cdc_case_earliest_dt}{`Date`. The event date -- the earlier of the
#'     clinical date and the date received by CDC.}
#'   \item{cdc_report_dt}{`Date`. The report date -- when the case was first
#'     reported to CDC.}
#'   \item{n}{`integer`. Number of cases with this (event date, report date)
#'     pair.}
#' }
#'
#' @source Centers for Disease Control and Prevention (CDC), COVID-19 Response.
#'   *COVID-19 Case Surveillance Public Use Data* (version date: June 21, 2024).
#'   \url{https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf/about_data}.
#'   COVID-19 case surveillance data are collected by jurisdictions and reported
#'   voluntarily to CDC.
#'
#' @docType data
#'
#' @usage data(covid_us)
#'
#' @keywords datasets
#'
#' @seealso
#' [tbl_now()] to declare the date columns; [summary()][tbl_now_summary] and
#' [diagnose()] to inspect the result; the package's other datasets --
#' [denguedat], [mpoxdat], [flusight], [covid_colombia], [covid_us] and
#' [hai_bucaramanga].
#'
#' @examples
#' data(covid_us)
#' tn <- tbl_now(
#'   covid_us,
#'   event_date  = cdc_case_earliest_dt,
#'   report_date = cdc_report_dt,
#'   case_count  = n,
#'   data_type   = "count-incidence",
#'   verbose     = FALSE
#' )
#' tn
#' @md
"covid_us"

#' #' vectordat: Vector borne reportable diseases from Mexico (Feb-Dec 2024)
#' #'
#' #' Surveillance count data provided by Mexico's General Directorate
#' #' of Epidemiology (Direccion General de Epidemiologia)
#' #' at https://www.gob.mx/cms/uploads/attachment/file/965318/Datos_abiertos_historicos_etv_2024.pdf.
#' #' Data comes from the National System of Epidemiologic Surveillance (SINAVE)
#' #' and was downloaded on May 30th 2025. The dataset includes all suspected
#' #' vector-borne disease cases for the states of Veracruz, Guerrero and Tabasco.
#' #' The original dataset was aggregated and pre-processed.
#' #'
#' #' @details
#' #' This is count data with each row representing case counts. The columns are as follows:
#' #' * `update`: date when the report was first registered on the database.
#' #' * `symptoms`: date when the patient reported symptoms started.
#' #' * `sex`: biological sex (either male or female)
#' #' * `n`: the case count of individuals within those dates for each sex
#' #' * `state`: state where the report was originated
#' #'
#' #' @note Dates are provided as present in the dataset and contain errors.
#' #' Theoretically no individual should have an `update` after
#' #' their `symptoms`; however the database does contain cases like that
#' #' which are close to what practitioners find in the wild.
#' #'
#' #' @docType data
#' #'
#' #' @usage data(vectordat)
#' #'
#' #' @format A data frame.
#' #'
#' #' @keywords vector borne diseases Mexico
#' #'
#' #' @examples
#' #' data(vectordat)
#' #' @md
#' "vectordat"
