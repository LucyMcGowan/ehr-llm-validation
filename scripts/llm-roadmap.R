library(ellmer)

c <- chat_google_gemini()

make_data <- function(bmi, sbp, dbp) {
  df <- data.frame(
    Variable_Name = c("BMI", "SBP", "DBP"),
    If_Missing_Search_For = c(bmi, sbp, dbp)
  )
  assign("search_terms_df", df, envir = .GlobalEnv)

}

tool_data <- tool(
  make_data,
  description = "Create a data frame with text ICD Description search terms separated by ; for ICD descriptions to match diagnoses when missing from chart review",
  arguments = list(
    bmi = type_string("Search terms for ICD Text Descriptions to detect obesity, separated by a semicolon"),
    sbp = type_string("Search terms for ICD Text Descriptions  to detect hypertension, separated by a semicolon"),
    dbp = type_string("Search terms for ICD Text Descriptions  to detect hypertension, separated by a semicolon")
  )
)
c$register_tool(tool_data)
c$chat("Please propose as many terms as you can that will be used to search ICD Descriptions to identify obesity and hypertension and update the dataframe with them.")
