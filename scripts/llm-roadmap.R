library(ellmer)
library(dplyr)

roadmap <- readr::read_csv(here::here("data-raw/audit_roadmap.csv"))

## No context -----

c <- chat_google_gemini()

make_data <- function(creat_c, alb, bmi, sbp, dbp, a1c, chol, trig, crp, hcst, df_name) {
  df <- data.frame(
    Variable_Name = c("CREAT_C", "ALB", "BMI", "BP_SYSTOLIC", "BP_DIASTOLIC", "A1C", "CHOL", "TRIG", "CRP", "HCST"),
    If_Missing_Search_For = c(creat_c, alb, bmi, sbp, dbp, a1c, chol, trig, crp, hcst)
  )
  assign(df_name, df, envir = .GlobalEnv)
}

#append_data <- function(original_data, new_data, df_name = "df") {
#  df <- dplyr::bind_rows(original_data, new_data)
#  assign(df_name, df, envir = .GlobalEnv)
#
#}

#tool_append <- tool(
#  append_data, 
#  description = "Append new data with previously created dataset.",
#  arguments = list(
#    original_data = type_object("Original data frame"),
#    new_data = type_object("New data frame to append"),
#    df_name = type_string("Name of dataframe")
#  )
#)


tool_data <- tool(
  make_data,
  description = "Create a data frame with text ICD Description search terms separated by ; for ICD descriptions to match diagnoses when missing from chart review",
  arguments = list(
    creat_c = type_string("Search terms for ICD Text Descriptions to detect renal failure, separated by a semicolon"),
    alb = type_string("Search terms for ICD Text Descriptions to detect serum albumin, separated by a semicolon"),
    bmi = type_string("Search terms for ICD Text Descriptions to detect obesity, separated by a semicolon"),
    sbp = type_string("Search terms for ICD Text Descriptions to detect hypertension, separated by a semicolon"),
    dbp = type_string("Search terms for ICD Text Descriptions to detect hypertension, separated by a semicolon"),
    a1c = type_string("Search terms for ICD Text Descriptions to detect diabetes, separated by a semicolon"),
    chol = type_string("Search terms for ICD Text Descriptions to detect hypercholesterolemia , separated by a semicolon"),
    trig = type_string("Search terms for ICD Text Descriptions to detect hypertriglyceridemia , separated by a semicolon"),
    crp = type_string("Search terms for ICD Text Descriptions to detect sepsis, separated by a semicolon"),
    hcst = type_string("Search terms for ICD Text Descriptions to detect vitamin deficiency, separated by a semicolon"),
    df_name = type_string("Name of the data frame")
  )
)
c$register_tool(tool_data)
#c$register_tool(tool_append)
c$chat("Please propose an exhaustive list of terms that will be used to search ICD Descriptions to identify each of the diagnoses and create a data frame for each. I want you to repeat this process 20 times, creating a new dataframe each time with each having a unique name starting with `df_nocontext`. Each time be sure to make as exhaustive a list as possible but they can vary.")


## Combine datasets ----

library(ggplot2)
library(tidyr)

dfs <- paste0("df_nocontext_", 1:20)
save(list = dfs, file = here::here("data-raw/no_context-llm.rda"))

## With context -----

c_context <- chat_google_gemini()

c_context$register_tool(tool_data)

# Convert to a readable format for the prompt

df_text <- capture.output(print(roadmap))
df_formatted <- paste(df_text, collapse = "\n")

c_context$chat(paste("Here's an a data frame that content matter experts came up with, I want you to add to this, so be sure each list contains these values at least:",
             df_formatted,
             "Please propose an exhaustive list of terms that will be used to search ICD Descriptions to identify each of the diagnoses and create a data frame for each. I want you to repeat this process 20 times, creating a new dataframe each time with each having a unique name starting with `df_context`. Each time be sure to make as exhaustive a list as possible but they can vary.",
             sep = "\n\n"))

dfs <- paste0("df_context_", 1:20)
save(list = dfs, file = here::here("data-raw/context-llm.rda"))

#readr::write_csv(df_context, here::here("data-raw/llm_context_roadmap.csv"))
#readr::write_csv(df_nocontext, here::here("data-raw/llm_nocontext_roadmap.csv"))