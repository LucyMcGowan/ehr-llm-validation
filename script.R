library(tidyverse)

set.seed(1)

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
    mutate(contact_date = start_date + (visit - 1) * interval_days)
  
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
      patient_id = patient_id,
      contact_date = contact_date,
      body_mass_index = mutate_row(body_mass_index, p_missing, p_exact, cont_noise_bmi),
      systolic_blood_pressure = mutate_row(systolic_blood_pressure, p_missing, p_exact, cont_noise_bp),
      diastolic_blood_pressure = mutate_row(diastolic_blood_pressure, p_missing, p_exact, cont_noise_bp),
      .keep = "none"
    )
}

run_audit <- function(ehr_wide, epic_raw, roadmap) {
  variables <- c("body_mass_index", "systolic_blood_pressure", "diastolic_blood_pressure")
  
  ehr_long <- ehr_wide |>
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable",
      values_to = "extracted_value"
    ) |>
    left_join(roadmap, by = "variable")

  epic_long <- epic_raw |>
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable",
      values_to = "epic_value"
    ) |>
    select(patient_id, contact_date, variable, epic_value, notes)

  audit_base <- ehr_long |>
    left_join(epic_long, by = c("patient_id", "contact_date", "variable")) |>
    mutate(
      extracted_value = if_else(!is.na(extracted_value), round(extracted_value), NA_real_),
      epic_value = if_else(!is.na(epic_value), round(epic_value), NA_real_),
      extracted_missing = is.na(extracted_value),
      epic_missing = is.na(epic_value),
      match_flag = !extracted_missing & !epic_missing & extracted_value == epic_value,
      keyword_found = extracted_missing &
        str_detect(
          str_to_lower(coalesce(notes, "")),
          str_c("\\b", str_to_lower(if_missing_search_for), "\\b")
        )
    )

  audit_base |>
    mutate(
      reviewed_value = case_when(
        !extracted_missing & match_flag ~ extracted_value,
        !extracted_missing & !match_flag & !epic_missing ~ epic_value,
        TRUE ~ NA_real_
      ),
      finding = case_when(
        !extracted_missing & match_flag ~ "Extracted Value Correct",
        !extracted_missing & !match_flag & !epic_missing ~ "Extracted Value Incorrect",
        keyword_found ~ if_missing_search_for,
        TRUE ~ "Not Found"
      )
    ) |>
    select(
      patient_id, contact_date, variable,
      extracted_value, epic_value,
      reviewed_value, finding,
      if_missing_search_for, notes
    )
}
  
epic_raw <- simulate_epic(
  n_patients = 12, 
  n_visits = 3, 
  start_date = as.Date("2022-01-01"), 
  interval = "3 months"
)

ehr_sim <- simulate_ehr_from_epic(
  epic_raw, 
  p_missing = 0.15, 
  p_exact = 0.7, 
  bmi_noise_sd = 0.8, 
  bp_noise_range = -8:8
)

roadmap <- tibble(
  variable = c("body_mass_index", "systolic_blood_pressure", "diastolic_blood_pressure"),
  if_missing_search_for = c("Obesity", "Hypertension", "Hypertension")
)

audit <- run_audit(ehr_sim, epic_raw, roadmap)

audit |> 
  filter(variable == "body_mass_index") |> 
  pivot_longer(cols = c(reviewed_value, extracted_value)) |>
  select(name, value) |>
  ggplot(aes(value, fill = name)) +
  geom_bar(position = position_dodge2(preserve = "single")) + 
  scale_fill_manual(
    values = c("cornflower blue", "orange"),
    labels = c("Extracted Value", "Validated Value")
  ) + 
  theme_minimal() + 
  labs(x = "BMI", fill = "")


