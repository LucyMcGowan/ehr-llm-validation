library(ellmer)

## No context -----

c <- chat_google_gemini()

make_data <- function(creat_c, bmi, sbp, dbp, a1c, chol, trig, crp, hcst, df_name) {
  df <- data.frame(
    Variable_Name = c("CREAT_C", "BMI", "BP_SYSTOLIC", "BP_DIASTOLIC", "A1C", "CHOL", "TRIG", "CRP", "HCST"),
    If_Missing_Search_For = c(creat_c, bmi, sbp, dbp, a1c, chol, trig, crp, hcst)
  )
  assign(df_name, df, envir = .GlobalEnv)

}

tool_data <- tool(
  make_data,
  description = "Create a data frame with text ICD Description search terms separated by ; for ICD descriptions to match diagnoses when missing from chart review",
  arguments = list(
    creat_c = type_string("Search terms for ICD Text Descriptions to detect renal failure, separated by a semicolon"),
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
c$chat("Please propose an exhaustive list of terms that will be used to search ICD Descriptions to identify each of the diagnoses and update a data frame called `df_nocontext` with them.")

## With context -----

c_context <- chat_google_gemini()

c_context$register_tool(tool_data)

roadmap <- readr::read_csv(here::here("data-raw/audit_roadmap.csv"))

# Convert to a readable format for the prompt

df_text <- capture.output(print(roadmap, row.names = FALSE))
df_formatted <- paste(df_text, collapse = "\n")

c_context$chat(paste("Here's an example of the data frame structure I want you to create:",
             df_formatted,
             "Please propose an exhaustive list of terms that will be used to search ICD Descriptions to identify each of the diagnoses and create a data frame called `df_context` with them.",
             sep = "\n\n"))

readr::write_csv(df_context, here::here("data-raw/llm_context_roadmap.csv"))
readr::write_csv(df_nocontext, here::here("data-raw/llm_nocontext_roadmap.csv"))
