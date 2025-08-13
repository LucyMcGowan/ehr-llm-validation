library(ellmer)

## No context -----

c <- chat_google_gemini()

make_data <- function(bmi, sbp, dbp, df_name) {
  df <- data.frame(
    Variable_Name = c("BMI", "SBP", "DBP"),
    If_Missing_Search_For = c(bmi, sbp, dbp)
  )
  assign(df_name, df, envir = .GlobalEnv)

}

tool_data <- tool(
  make_data,
  description = "Create a data frame with text ICD Description search terms separated by ; for ICD descriptions to match diagnoses when missing from chart review",
  arguments = list(
    bmi = type_string("Search terms for ICD Text Descriptions to detect obesity, separated by a semicolon"),
    sbp = type_string("Search terms for ICD Text Descriptions  to detect hypertension, separated by a semicolon"),
    dbp = type_string("Search terms for ICD Text Descriptions  to detect hypertension, separated by a semicolon"),
    df_name = type_string("Name of the data frame")
  )
)
c$register_tool(tool_data)
c$chat("Please propose as many terms as you can that will be used to search ICD Descriptions to identify obesity and hypertension and update a data frame called `df_nocontext` with them.")

## With context -----

c_context <- chat_google_gemini()

c_context$register_tool(tool_data)

roadmap <- data.frame(
  Variable_Name = c("BMI", "SBP", "DBP"),
    If_Missing_Search_For = c("OBESITY; OBESITY GRADE I; OBESITY GRADE II; OBESITY GRADE III", "HYPERTENSION", "HYPERTENSION")
)

# Convert to a readable format for the prompt
df_text <- capture.output(print(roadmap, row.names = FALSE))
df_formatted <- paste(df_text, collapse = "\n")

c_context$chat(paste("Here's an example of the data frame structure I want you to create:",
             df_formatted,
             "Please propose as many terms as you can that will be used to search ICD Descriptions to identify obesity and hypertension and update a data frame called `df_context` with them.",
             sep = "\n\n"))