library(ellmer)
library(dplyr)

roadmap <- readr::read_csv(here::here("data-raw/audit_roadmap.csv"))

## No context (no examples) -----

c <- chat_google_gemini()

make_data <- function(
  creat_c,
  alb,
  bmi,
  sbp,
  dbp,
  a1c,
  chol,
  trig,
  crp,
  hcst,
  df_name
) {
  df <- data.frame(
    Variable_Name = c(
      "CREAT_C",
      "ALB",
      "BMI",
      "BP_SYSTOLIC",
      "BP_DIASTOLIC",
      "A1C",
      "CHOL",
      "TRIG",
      "CRP",
      "HCST"
    ),
    If_Missing_Search_For = c(
      creat_c,
      alb,
      bmi,
      sbp,
      dbp,
      a1c,
      chol,
      trig,
      crp,
      hcst
    )
  )
  assign(df_name, df, envir = .GlobalEnv)
}

tool_data <- tool(
  make_data,
  description = "Create a data frame with text ICD Description search terms separated by ; for ICD descriptions to match diagnoses when missing from chart review",
  arguments = list(
    creat_c = type_string(
      "Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's creatinine clearance is at an unhealthy (low) level, separated by a semicolon."
    ),
    alb = type_string(
      "Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's serum albumin is at an unhealthy (high) level, separated by a semicolon."
    ),
    bmi = type_string(
      "Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's body mass index (BMI) is at an unhealthy (high) level, separated by a semicolon."
    ),
    sbp = type_string(
      "Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's systolic blood pressure is at an unhealthy (high) level, separated by a semicolon."
    ),
    dbp = type_string(
      "Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's diastolic blood pressure is at an unhealthy (high) level, separated by a semicolon."
    ),
    a1c = type_string(
      "Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's hemoglobin A1c (HbA1c) is at an unhealthy (high) level, separated by a semicolon."
    ),
    chol = type_string(
      "Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's total cholesterol is at an unhealthy (high) level, separated by a semicolon."
    ),
    trig = type_string(
      "Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's trigylcerides is at an unhealthy (high) level, separated by a semicolon."
    ),
    crp = type_string(
      "Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's C-reactive protein is at an unhealthy (high) level, separated by a semicolon."
    ),
    hcst = type_string(
      "Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's homocysteine is at an unhealthy (high) level, separated by a semicolon."
    ),
    df_name = type_string("Name of the data frame")
  )
)

c$register_tool(tool_data)
c$chat(
  "Please propose an exhaustive list of terms (avoiding acronyms) that will be used to search ICD Descriptions to identify each of the missing biomarkers and create a data frame with these codes. I want you to repeat this process 20 times, creating a new data frame each time with each having a unique name starting with `df_nocontext`. Each time you repeat this, be sure to make as exhaustive a list as possible. These lists can vary."
)


## Combine datasets ----

dfs <- paste0("df_nocontext_", 1:20)
df_list <- mget(dfs)
df_nocontext <- bind_rows(df_list, .id = "id")
save(df_nocontext, file = here::here("data-raw/no_context-llm.rda"))

## With context (examples) -----

c_context <- chat_google_gemini()

## Add examples to tool_data 
tool_data <- tool(
  make_data,
  description = "Create a data frame with text ICD Description search terms separated by ; for ICD descriptions to match diagnoses when missing from chart review",
  arguments = list(
    creat_c = type_string("Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's creatinine clearance is at an unhealthy (low) level (e.g., renal failure, renal insufficiency, acute kidney injury, and chronic renal failure), separated by a semicolon."),
    alb = type_string("Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's serum albumin is at an unhealthy (high) level, separated by a semicolon."),
    bmi = type_string("Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's body mass index (BMI) is at an unhealthy (high) level (e.g., Obesity, morbid obesity, Grade I obesity, Grade II obesity, Grade III obesity), separated by a semicolon."),
    sbp = type_string("Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's systolic blood pressure is at an unhealthy (high) level (e.g., hypertension), separated by a semicolon."),
    dbp = type_string("Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's diastolic blood pressure is at an unhealthy (high) level (e.g., hypertension), separated by a semicolon."),
    a1c = type_string("Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's hemoglobin A1c (HbA1c) is at an unhealthy (high) level (e.g., diabetes, impaired glycemic control), separated by a semicolon."),
    chol = type_string("Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's total cholesterol is at an unhealthy (high) level (e.g., hypercholesterolemia), separated by a semicolon."),
    trig = type_string("Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's trigylcerides is at an unhealthy (high) level (e.g., hypertriglyceridemia), separated by a semicolon."),
    crp = type_string("Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's C-reactive protein is at an unhealthy (high) level (e.g., sepsis, infection, autoimmune inflammatory syndrome), separated by a semicolon."),
    hcst = type_string("Search terms for ICD Text Descriptions to detect diagnoses that would suggest that a patient's homocysteine is at an unhealthy (high) level (e.g., hyperhomocysteinemia, vitamin deficiency), separated by a semicolon."),
    df_name = type_string("Name of the data frame")
  )
)

c_context$register_tool(tool_data)
c_context$chat(
  "Please propose an exhaustive list of terms (avoiding acronyms) that will be used to search ICD Descriptions to identify each of the missing biomarkers and create a data frame with these codes. I want you to repeat this process 20 times, creating a new data frame each time with each having a unique name starting with `df_context`. Each time you repeat this, be sure to make as exhaustive a list as possible. These lists can vary."
)

# Convert clinicians' roadmap to a readable format for the prompt
# df_text <- readLines(here::here("data-raw/audit_roadmap.csv"))
# df_formatted <- paste(df_text, collapse = "\n")
# 
# c_context$chat(paste(
#   "Here's an a data frame that content matter experts came up with, I want you to add to this, so be sure each list contains these values at least:",
#   df_formatted,
#   "Please propose an exhaustive list of terms (avoiding acronyms) that will be used to search ICD Descriptions to identify each of the missing biomarkers and create a data frame with these codes. I want you to repeat this process 20 times, creating a new data frame each time with each having a unique name starting with `df_context`. Each time you repeat this, be sure to make as exhaustive a list as possible. These lists can vary.",
#   sep = "\n\n"
# ))

dfs <- paste0("df_context_", 1:20)
df_list <- mget(dfs)
df_context <- bind_rows(df_list, .id = "id")
save(df_context, file = here::here("data-raw/context-llm.rda"))
