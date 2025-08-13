#' @importFrom dplyr mutate case_when left_join select if_else coalesce
#' @importFrom tidyr pivot_longer expand_grid
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract str_to_lower str_c
#' @importFrom stats rnorm runif
#' @importFrom rlang .data
NULL

#' Simulate EPIC EHR Data
#'
#' Generates synthetic patient data resembling EPIC electronic health record format,
#' including clinical measurements and diagnostic notes across multiple visits.
#'
#' @param n_patients Integer. Number of patients to simulate (default: 10)
#' @param n_visits Integer. Number of visits per patient (default: 3)
#' @param start_date Date. Starting date for patient visits (default: "2022-01-01")
#' @param interval Character. Time interval between visits, e.g., "3 months", "2 weeks", "30 days" (default: "3 months")
#'
#' @return A tibble with columns:
#'   \item{patient_id}{Patient identifier}
#'   \item{contact_date}{Date of clinical contact}
#'   \item{body_mass_index}{BMI measurement}
#'   \item{systolic_blood_pressure}{Systolic BP measurement}
#'   \item{diastolic_blood_pressure}{Diastolic BP measurement}
#'   \item{notes}{Clinical notes with diagnostic information}
#'
#' @details
#' Generates realistic clinical measurements:
#' - BMI: Normal distribution (mean=31, sd=3)
#' - Systolic BP: Normal distribution (mean=122, sd=12), bounded 90-200
#' - Diastolic BP: Normal distribution (mean=76, sd=9), bounded 50-120
#' 
#' Diagnostic notes are added based on clinical thresholds:
#' - Hypertension: SBP ≥130 or DBP ≥80
#' - Obesity: BMI ≥30 (60% probability)
#'
#' @examples
#' # Basic usage
#' epic_data <- simulate_epic()
#' 
#' # Custom parameters
#' epic_data <- simulate_epic(n_patients = 20, n_visits = 5, interval = "6 months")
#'
#' @export
simulate_epic <- function(n_patients = 10, n_visits = 3, start_date = as.Date("2022-01-01"), interval = "3 months") {
  interval_days <- case_when(
    str_detect(interval, "month") ~ as.numeric(str_extract(interval, "\\d+")) * 30,
    str_detect(interval, "week") ~ as.numeric(str_extract(interval, "\\d+")) * 7,
    str_detect(interval, "day") ~ as.numeric(str_extract(interval, "\\d+")),
    TRUE ~ 90
  )
  
  grid <- expand_grid(
    patient_id = seq_len(n_patients),
    visit = seq_len(n_visits)
  ) |>
    mutate(contact_date = start_date + (.data$visit - 1) * interval_days)
  
  bmi <- round(rnorm(nrow(grid), 31, 3), 2)
  sbp <- pmax(90, pmin(200, round(rnorm(nrow(grid), 122, 12))))
  dbp <- pmax(50, pmin(120, round(rnorm(nrow(grid), 76, 9))))
  
  date_range <- seq(as.Date("2019-01-01"), as.Date("2022-12-31"), by = "day")
  dx_hyp <- ifelse(
    sbp >= 130 | dbp >= 80, 
    paste0("Diagnosed Hypertension ", sample(date_range, nrow(grid), replace = TRUE)), 
    NA
  )
  dx_ob <- ifelse(
    bmi >= 30 & runif(nrow(grid)) < 0.6, 
    paste0("Diagnosed Obesity ", sample(date_range, nrow(grid), replace = TRUE)), 
    NA
  )
  
  notes <- trimws(paste(dx_hyp, dx_ob))
  
  tibble(
    patient_id = grid$patient_id,
    contact_date = grid$contact_date,
    body_mass_index = bmi,
    systolic_blood_pressure = sbp,
    diastolic_blood_pressure = dbp,
    notes = if_else(notes == "", NA_character_, notes)
  )
}

#' Simulate EHR Data Extraction from EPIC
#'
#' Simulates realistic data extraction errors and noise that occur when extracting
#' clinical measurements from electronic health records.
#'
#' @param epic_raw Tibble. Source EPIC data from \code{simulate_epic()}
#' @param p_missing Numeric. Proportion of values that fail to extract (default: 0.12)
#' @param p_exact Numeric. Proportion of successfully extracted values that are exact matches (default: 0.75)
#' @param bmi_noise_sd Numeric. Standard deviation for BMI measurement noise (default: 0.6)
#' @param bp_noise_range Integer vector. Range for blood pressure measurement variation (default: -6:6)
#'
#' @return A tibble with the same structure as \code{epic_raw} but with extraction errors:
#'   missing values, measurement noise, and transcription errors
#'
#' @details
#' Simulates three types of extraction outcomes:
#' 1. Missing values: Complete extraction failure (proportion = p_missing)
#' 2. Exact matches: Perfect extraction (proportion = p_exact among non-missing)
#' 3. Noisy values: Extraction with measurement error (remaining proportion)
#'
#' Noise models:
#' - BMI: Gaussian noise with specified standard deviation
#' - Blood pressure: Random integer variation within specified range
#'
#' @examples
#' epic_data <- simulate_epic()
#' ehr_data <- simulate_ehr_from_epic(epic_data)
#' 
#' # High error scenario
#' ehr_data <- simulate_ehr_from_epic(epic_data, p_missing = 0.2, p_exact = 0.6)
#'
#' @export
simulate_ehr_from_epic <- function(epic_raw,
                                   p_missing = 0.12,
                                   p_exact = 0.75,
                                   bmi_noise_sd = 0.6,
                                   bp_noise_range = -6:6) {
  
  mutate_row <- function(x, p_miss, p_ok, cont_noise_fun) {
    case_when(
      runif(length(x)) < p_miss ~ NA_real_,
      runif(length(x)) < p_ok ~ x,
      TRUE ~ cont_noise_fun(x)
    )
  }
  
  cont_noise_bmi <- function(x) round(x + rnorm(length(x), 0, bmi_noise_sd), 2)
  cont_noise_bp <- function(x) x + sample(bp_noise_range, length(x), replace = TRUE)
  
  epic_raw |>
    mutate(
      patient_id = .data$patient_id,
      contact_date = .data$contact_date,
      body_mass_index = mutate_row(.data$body_mass_index, p_missing, p_exact, cont_noise_bmi),
      systolic_blood_pressure = mutate_row(.data$systolic_blood_pressure, p_missing, p_exact, cont_noise_bp),
      diastolic_blood_pressure = mutate_row(.data$diastolic_blood_pressure, p_missing, p_exact, cont_noise_bp),
      .keep = "none"
    )
}

#' Run EHR Data Extraction Audit
#'
#' Compares extracted EHR data against source EPIC data to identify discrepancies,
#' validate accuracy, and search clinical notes for missing structured data.
#'
#' @param ehr_wide Tibble. Extracted EHR data from \code{simulate_ehr_from_epic()}
#' @param epic_raw Tibble. Source EPIC data from \code{simulate_epic()}
#' @param roadmap Tibble. Mapping of variables to keyword search terms with columns:
#'   \code{variable} and \code{if_missing_search_for}
#'
#' @return A tibble with audit results containing:
#'   \item{patient_id}{Patient identifier}
#'   \item{contact_date}{Date of clinical contact}
#'   \item{variable}{Clinical measurement type}
#'   \item{extracted_value}{Value from EHR extraction}
#'   \item{epic_value}{Ground truth value from EPIC}
#'   \item{reviewed_value}{Validated/corrected value}
#'   \item{finding}{Audit classification (see Details)}
#'   \item{if_missing_search_for}{Keyword searched when data missing}
#'   \item{notes}{Clinical notes}
#'
#' @details
#' Audit classifications:
#' \describe{
#'   \item{Extracted Value Correct}{Perfect match between extracted and source values}
#'   \item{Extracted Value Incorrect}{Discrepancy detected; source value used for correction}
#'   \item{\[Keyword\]}{Missing structured data but diagnostic keyword found in notes}
#'   \item{Not Found}{No structured data or relevant keywords identified}
#' }
#'
#' The \code{reviewed_value} provides the most reliable value for each measurement:
#' correct extracted values, corrected values for discrepancies, or NA when unavailable.
#'
#' @examples
#' epic_data <- simulate_epic()
#' ehr_data <- simulate_ehr_from_epic(epic_data)
#' 
#' roadmap <- data.frame(
#'   variable = c("body_mass_index", "systolic_blood_pressure", "diastolic_blood_pressure"),
#'   if_missing_search_for = c("Obesity", "Hypertension", "Hypertension")
#' )
#' 
#' audit_results <- run_audit(ehr_data, epic_data, roadmap)
#'
#' @export
run_audit <- function(ehr_wide, epic_raw, roadmap) {
  variables <- c("body_mass_index", "systolic_blood_pressure", "diastolic_blood_pressure")
  
  ehr_long <- ehr_wide |>
    pivot_longer(
      cols = tidyselect::all_of(variables),
      names_to = "variable",
      values_to = "extracted_value"
    ) |>
    left_join(roadmap, by = "variable")

  epic_long <- epic_raw |>
    pivot_longer(
      cols = tidyselect::all_of(variables),
      names_to = "variable",
      values_to = "epic_value"
    ) |>
    select(.data$patient_id, .data$contact_date, .data$variable, .data$epic_value, .data$notes)

  audit_base <- ehr_long |>
    left_join(epic_long, by = c("patient_id", "contact_date", "variable")) |>
    mutate(
      extracted_value = if_else(!is.na(.data$extracted_value), round(.data$extracted_value), NA_real_),
      epic_value = if_else(!is.na(.data$epic_value), round(.data$epic_value), NA_real_),
      extracted_missing = is.na(.data$extracted_value),
      epic_missing = is.na(.data$epic_value),
      match_flag = !.data$extracted_missing & !.data$epic_missing & .data$extracted_value == .data$epic_value,
      keyword_found = .data$extracted_missing &
        str_detect(
          str_to_lower(coalesce(.data$notes, "")),
          str_c("\\b", str_to_lower(.data$if_missing_search_for), "\\b")
        )
    )

  audit_base |>
    mutate(
      reviewed_value = case_when(
        !.data$extracted_missing & .data$match_flag ~ .data$extracted_value,
        !.data$extracted_missing & !.data$match_flag & !.data$epic_missing ~ .data$epic_value,
        TRUE ~ NA_real_
      ),
      finding = case_when(
        !.data$extracted_missing & .data$match_flag ~ "Extracted Value Correct",
        !.data$extracted_missing & !.data$match_flag & !.data$epic_missing ~ "Extracted Value Incorrect",
        .data$keyword_found ~ .data$if_missing_search_for,
        TRUE ~ "Not Found"
      )
    ) |>
    select(
      .data$patient_id, .data$contact_date, .data$variable,
      .data$extracted_value, .data$epic_value,
      .data$reviewed_value, .data$finding,
      .data$if_missing_search_for, .data$notes
    )
}

#' Default Variable-Keyword Roadmap
#'
#' Default mapping of clinical variables to diagnostic keywords for missing data search.
#'
#' @format A tibble with 3 rows and 2 variables:
#' \describe{
#'   \item{variable}{Clinical measurement variable name}
#'   \item{if_missing_search_for}{Diagnostic keyword to search in notes when structured data is missing}
#' }
#'
#' @examples
#' data(roadmap)
#' roadmap
#'
#' @export
roadmap <- tibble(
  variable = c("body_mass_index", "systolic_blood_pressure", "diastolic_blood_pressure"),
  if_missing_search_for = c("Obesity", "Hypertension", "Hypertension")
)