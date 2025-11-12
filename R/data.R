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
#' actual observed data the `gender` was constructed exclusively for the examples of `NobBS`. Its
#' a synthetic (simulated) variable and does not correspond to any reality.
#'
#' @docType data
#'
#' @usage data(denguedat)
#'
#' @format A data frame.
#'
#' @keywords dengue
#'
#' @references
#' MCGOUGH, Sarah F., et al. Nowcasting by Bayesian Smoothing: A flexible, generalizable model for
#' real-time epidemic tracking. PLoS computational biology, 2020, vol. 16, no 4, p. e1007735.
#'
#' @examples
#' data(denguedat)
#' @md
"denguedat"

#' mpoxdat: Mpox reporting data from the 2022 New York City outbreak
#'
#' Surveillance line list data provided by the New York City (NYC) Health Department
#' at https://github.com/nychealth/mpox_nowcast_eval, to accompany a nowcasting
#' performance evaluation (doi: 10.2196/56495). Patients with a confirmed or probable mpox
#' diagnosis or illness onset from July 8 through September 30, 2022 were included. The original
#' dataset was aggregated and pre-processed as described in the note below.
#'
#' @details
#' This is line-list data with each row representing case counts. The columns are as follows:
#' * `dx_date`: is the specimen collection date of the first positive mpox laboratory result,
#' * `dx_report_date`: is the date the report of first positive mpox laboratory result was received by the NYC Health Department,
#' * `n`: the case count of individuals within those dates.
#' * `race`: the race corresponding to those cases. Race was randomly assigned with probabilities
#' "Non-Hispanic White" = 0.309, "Hispanic" = 0.283, "Black" = 0.202, "Asian" = 0.156, and "Other" = 0.05
#' which follow what has been reported for the US Census.
#'
#' @note
#' While `dx_date`, `dx_report_date` and `n` correspond to actual observed data the `race` was
#' constructed exclusively for the examples of this package. Its a synthetic (simulated) variable
#' and does not correspond to any reality.
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
#' @format A data frame.
#'
#' @keywords mpox
#'
#' @examples
#' data(mpoxdat)
#' @md
"mpoxdat"

#' covidat: COVID-19 reporting data from Mexico City (2020-2022)
#'
#' Surveillance count data provided by the Mexico City's Health Ministry
#' at https://datos.cdmx.gob.mx/dataset/base-covid-sinave. Data comes
#' from the National System of Epidemiologic Surveillance (SINAVE) and
#' corresponds to the update of January 9th 2024, 12:35 (UTC-05:00). The
#' dataset includes all confirmed COVID-19 cases registered in the city.
#' The original dataset was aggregated and pre-processed as described in the
#' note below.
#'
#' @details
#' This is count data with each row representing case counts. The columns are as follows:
#' * `date_of_registry`: date when the report was registered on the database.
#' * `date_of_symptom_onset`: date when the patient reported symptoms started.
#' * `sex`: biological sex (either male or female)
#' * `n`: the case count of individuals within those dates for each sex
#'
#' @note Dates are provided as present in the dataset and contain errors.
#' Theoretically no individual should have a `date_of_symptom_onset` after
#' their `date_of_registry`; however the database does contain cases like that
#' which are close to what practitioners find in the wild.
#'
#' @docType data
#'
#' @usage data(covidat)
#'
#' @format A data frame.
#'
#' @keywords covid
#'
#' @examples
#' data(covidat)
#' @md
"covidat"

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
