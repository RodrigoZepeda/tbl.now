#' Healthcare-Associated Infections -- Bucaramanga, Colombia 2016-2023
#'
#' A line list of healthcare-associated infections (IAAS, *Infecciones Asociadas
#' a la Atencion en Salud*) notified in the municipality of Bucaramanga,
#' Santander, Colombia, between January 2016 and January 2023. Each row is one
#' notified infection: a specimen taken from a hospitalised patient, the
#' laboratory result, and the isolated microorganism.
#'
#' In the nowcasting context the **event date** is `specimen_date` (when the
#' sample was taken from the patient) and the **report date** is `report_date`
#' (when the laboratory result was issued). The delay between the two is the
#' specimen-to-result turnaround that a nowcasting model would estimate and
#' correct for. A third date, `received_date`, records when the laboratory
#' received the sample and splits the delay into a transport and a processing
#' leg -- but see the warnings below before relying on it.
#'
#' The column names and all categorical values have been translated from the
#' original Spanish. The dataset has been trimmed to the columns relevant to a
#' delay or nowcasting analysis; see `data-raw/hai_bucaramanga.R` in the
#' package sources for the full translation tables and the list of dropped
#' columns.
#'
#' @section Data quality -- read this first:
#'
#' This is a real, unpolished open-data extract, and it is included partly
#' *because* it is messy: it is a realistic exercise for the delay diagnostics in
#' this package ([test_delay_drift()], [test_delay_changepoint()],
#' [plot_delay_profiles()]). Nothing below has been silently repaired.
#'
#' \describe{
#'   \item{Missing dates}{The source uses `1900-01-01` as an undocumented
#'     missing-date sentinel. It has been converted to `NA` here -- left in, it
#'     produces delays of about -45,000 days. After conversion `specimen_date`
#'     is present for 1,105 records (77.7%), `report_date` for 840 (59.0%) and
#'     `received_date` for only 229 (16.1%).}
#'   \item{`received_date` is largely unusable}{Beyond being 84% missing, it
#'     stops entirely at 2019-12-01, so the three-date chain exists only for the
#'     first third of the study period. Prefer the `specimen_date` ->
#'     `report_date` pair.}
#'   \item{Negative delays}{88 records (10.7% of the otherwise-valid pairs) have
#'     a `report_date` *before* their `specimen_date`, by up to 331 days. These
#'     are kept exactly as recorded. Filter them out before fitting anything.}
#'   \item{Exact duplicates}{100 records are byte-identical duplicates of another
#'     record, sharing the `id` that the source documents as a unique
#'     autonumber. All 1,423 rows are shipped for fidelity; use
#'     [dplyr::distinct()] to reduce to the 1,323 unique records.}
#'   \item{Clinically-confirmed cases}{For the 333 cases confirmed on clinical
#'     grounds rather than by laboratory, the source wrote the literal string
#'     `"CONFIRMADO POR CLINICA"` into the `muestra`, `nombre prueba` and
#'     `microorganismo` fields. Those are `NA` here; the information is preserved
#'     in `case_type`.}
#'   \item{Sparsity}{Only 738 records (51.9%) support a non-negative
#'     `specimen_date` -> `report_date` delay, spread over 488 distinct event
#'     dates -- roughly 1.5 cases per event date. That is thin for fitting a
#'     nowcasting model, though ample for delay diagnostics. Aggregating to
#'     weeks or months is usually necessary.}
#' }
#'
#' The delay distribution is strongly bimodal: the median is 3 days but the 90th
#' percentile is 92 days, which makes this a useful test case for delay
#' diagnostics that assume a unimodal delay.
#'
#' @format A [tibble][tibble::tibble] with 1,423 rows and 13 variables:
#' \describe{
#'   \item{id}{`integer`. The source's `Orden` autonumber. Documented as unique
#'     but repeated for 100 records (see above), so it is *not* a reliable key.}
#'   \item{specimen_date}{`Date`. The event date -- when the sample was taken
#'     from the patient. `NA` for 318 records.}
#'   \item{received_date}{`Date`. When the laboratory received the sample. `NA`
#'     for 1,194 records and absent after 2019-12-01.}
#'   \item{report_date}{`Date`. The report date -- when the laboratory result was
#'     issued. `NA` for 583 records.}
#'   \item{specimen}{`factor`, 6 levels. Sample type: `"Whole blood"`,
#'     `"Urine"`, `"Secretions"`, `"Other sterile fluids"`, `"Sputum"`,
#'     `"Bronchoalveolar lavage"`. `NA` for clinically-confirmed cases.}
#'   \item{test}{`factor`, 7 levels. Laboratory test performed, e.g.
#'     `"Blood culture"`, `"Urine culture"`. `NA` for clinically-confirmed
#'     cases.}
#'   \item{microorganism}{`character`, 85 distinct values. The isolated organism,
#'     in conventional binomial form, e.g. `"Klebsiella pneumoniae"`. `NA` for
#'     clinically-confirmed cases. Where the source recorded only a genus the
#'     value is `"<Genus> spp."`.}
#'   \item{sex}{`factor`. `"Female"` or `"Male"`.}
#'   \item{age_group}{`ordered factor`, 11 levels from `"<1"` to `"70+"`.}
#'   \item{case_type}{`factor`. `"Laboratory-confirmed"` (1,090) or
#'     `"Clinically-confirmed"` (333).}
#'   \item{final_condition}{`factor`. Patient status on discharge: `"Alive"` or
#'     `"Dead"`. `NA` for 8 records.}
#'   \item{icu_type}{`factor`. Intensive-care unit where the case occurred:
#'     `"Adult"`, `"Paediatric"` or `"Neonatal"`.}
#'   \item{institution}{`integer`, 1-10. Anonymised code of the reporting
#'     health institution.}
#' }
#'
#' @source Secretaria de Salud y Ambiente de Bucaramanga, via the Colombian
#'   national open-data portal
#'   \url{https://www.datos.gov.co/Salud-y-Protecci-n-Social/48-Infecciones-asociadas-a-la-atenci-n-en-salud-IA/w4zx-wbff/about_data}.
#'   Retrieved 2026-08-17. Column names and categorical values translated from
#'   Spanish; see `data-raw/hai_bucaramanga.R` for the mapping.
#'
#' @seealso [covid_colombia] for a second Colombian surveillance dataset,
#'   [test_delay_drift()] and [test_delay_changepoint()] for the delay
#'   diagnostics this dataset is well suited to.
#'
#' @examples
#' data(hai_bucaramanga)
#'
#' # The source ships 100 exact duplicates and a number of unusable rows.
#' # Reduce to unique records with a valid, non-negative reporting delay.
#' iaas_clean <- dplyr::distinct(hai_bucaramanga) |>
#'   dplyr::filter(
#'     !is.na(specimen_date), !is.na(report_date),
#'     report_date >= specimen_date
#'   )
#' nrow(iaas_clean)
#'
#' # Roughly 1.5 cases per event date, so aggregate to weeks before building a
#' # tbl_now for anything model-shaped.
#' iaas_now <- tbl_now(
#'   iaas_clean,
#'   event_date  = "specimen_date",
#'   report_date = "report_date",
#'   verbose     = FALSE
#' )
#' iaas_now
#'
#' # The delay is strongly bimodal: a 3-day median with a long secondary mode.
#' quantile(iaas_now$.delay, c(0.5, 0.75, 0.9, 0.99), na.rm = TRUE)
"hai_bucaramanga"
