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

#' covid_us: CDC COVID-19 Case Surveillance Public Use Data (2020)
#'
#' A compact aggregation of the U.S. CDC's individual-level COVID-19 case
#' surveillance database. It is the package's worked example for two different
#' things: **batch reporting**, and the **validation process** -- the optional
#' third date a surveillance record can carry.
#'
#' Each row is a unique (onset date, specimen date, CDC report date, status,
#' sex) combination with the number of cases `n`.
#'
#' @details
#' # The three dates
#'
#' The source file carries four date columns. `cdc_case_earliest_dt` is derived
#' by CDC as the earliest of the others, and equals `onset_dt` for 99.997% of
#' the rows kept here, so it is dropped as redundant. The three that remain are
#' the only chain that runs forward in time, and they map onto the three roles a
#' [tbl_now()] knows about:
#'
#' \describe{
#'   \item{`onset_dt`}{the **event** -- symptoms begin.}
#'   \item{`pos_spec_dt`}{the **report** -- the first positive specimen is
#'     collected, which is when the surveillance system first sees the case.}
#'   \item{`cdc_report_dt`}{the **validation** -- the case is registered at CDC
#'     with a status.}
#' }
#'
#' # `current_status` and `validation_levels`
#'
#' `current_status` is kept in CDC's own words rather than recoded, because
#' translating it is exactly what `tbl_now(validation_levels = )` is for:
#'
#' ```r
#' validation_levels = c(
#'   "Laboratory-confirmed case" = "confirmed",
#'   "Probable Case"             = "pending"
#' )
#' ```
#'
#' A *probable* case is one that met the clinical and epidemiological criteria
#' without meeting the laboratory-confirmed definition. Every row here has a
#' positive specimen, so "probable" means the specimen was collected and the
#' case was never laboratory-settled -- `"pending"` in this package's
#' vocabulary. Note what is **not** there: CDC does not withdraw cases, so
#' `"retracted"` does not occur in this dataset. It is a two-outcome validation
#' process, and code that needs a retraction has to look elsewhere.
#'
#' The relationship between the outcome and the validation delay is real rather
#' than fabricated: probable cases are registered a median of 2 days after the
#' specimen, laboratory-confirmed ones 4 days.
#'
#' # What was kept
#'
#' Cases where all three dates are present, correctly ordered
#' (`onset_dt <= pos_spec_dt <= cdc_report_dt`) and falling entirely within
#' 2020 -- a self-consistent "as of the end of 2020" snapshot. Rows out of order
#' are data-entry errors; rows missing a date cannot be placed on the chain at
#' all. See `data-raw/covid_us.R` for the exact duckdb aggregation of the 14 GB
#' source file.
#'
#' The reporting delay is enormous and heavily right-skewed: cases reached CDC
#' not smoothly but in large backlog dumps -- a textbook batch-reporting pattern
#' that [diagnose_batches()] and [transport_discriminant()] recover.
#'
#' @format A data frame with 192,953 rows and six variables:
#' \describe{
#'   \item{onset_dt}{`Date`. The event date -- symptom onset.}
#'   \item{pos_spec_dt}{`Date`. The report date -- collection of the first
#'     positive specimen.}
#'   \item{cdc_report_dt}{`Date`. The validation date -- when the case was
#'     registered at CDC.}
#'   \item{current_status}{`character`. CDC's classification, either
#'     `"Laboratory-confirmed case"` or `"Probable Case"`. Map it with
#'     `validation_levels` (see above).}
#'   \item{sex}{`character`. `"Female"`, `"Male"`, `"Other"`, `"Unknown"` or
#'     `"Missing"`.}
#'   \item{n}{`integer`. Number of cases sharing that combination.}
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
#' [tbl_now()] to declare the date columns; [add_validation_date()][add] to
#' attach the third one to an object that has none; [validation_counts] to count
#' the outcomes; [summary()][tbl_now_summary] and [diagnose()] to inspect the
#' result; the package's other datasets -- [denguedat], [mpoxdat], [flusight],
#' [covid_colombia] and [hai_bucaramanga].
#'
#' @examples
#' data(covid_us)
#'
#' # The two-date object: onset -> positive specimen.
#' tn <- tbl_now(
#'   covid_us,
#'   event_date  = onset_dt,
#'   report_date = pos_spec_dt,
#'   case_count  = n,
#'   strata      = sex,
#'   data_type   = "count-incidence",
#'   verbose     = FALSE
#' )
#' tn
#'
#' # The third date, with CDC's labels translated to this package's vocabulary.
#' tn3 <- tbl_now(
#'   covid_us,
#'   event_date       = onset_dt,
#'   report_date      = pos_spec_dt,
#'   validation_date  = cdc_report_dt,
#'   validation_type  = current_status,
#'   validation_levels = c(
#'     "Laboratory-confirmed case" = "confirmed",
#'     "Probable Case"             = "pending"
#'   ),
#'   case_count = n,
#'   strata     = sex,
#'   data_type  = "count-incidence",
#'   verbose    = FALSE
#' )
#' has_validation(tn3)
#' get_validation_levels(tn3)
#'
#' # "How many cases were there" now has more than one answer.
#' head(get_latest_reported_cases(tn3))
#' head(get_latest_confirmed(tn3))
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
