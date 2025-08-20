library(dplyr)

# Make one data frame of the superset of LLM roadmaps
## Load separate dataframes for each LLM roadmap
load(here::here("data-raw/no_context-llm.rda"))

## Create function to concatenate all the unique terms across all LLM roadmaps
extract_terms <- function(df, id_value, variable_name) {
  terms <- unlist(strsplit(
    paste(
      df |>
        filter(
          id == glue::glue("df_nocontext_{id_value}"),
          Variable_Name == variable_name
        ) |>
        select(If_Missing_Search_For),
      collapse = ";"
    ),
    ";"
  ))
  terms <- unique(stringr::str_trim(tolower(terms[terms != ""])))
  data.frame(term = terms, df = paste0("df_", id_value))
}

## Extract terms for each of the 10 ALI components, and combine them into a 10-row dataframe
ali_components = c(
  "ALB",
  "A1C",
  "BMI",
  "BP_SYSTOLIC",
  "BP_DIASTOLIC",
  "CHOL",
  "CREAT_C",
  "CRP",
  "HCST",
  "TRIG"
) ### List of components
super_roadmap = data.frame() ### Initialize empty dataframe to hold superset roadmap

for (comp in ali_components) {
  all_terms_comp <- do.call(
    rbind,
    Map(extract_terms, list(df_nocontext), 1:20, comp)
  ) |>
    pull(term) |>
    unique() |>
    paste(collapse = "; ")

  super_roadmap = super_roadmap |>
    bind_rows(data.frame(
      Variable_Name = comp,
      If_Missing_Search_For = all_terms_comp
    ))
}

## Save it
super_roadmap |>
  write.csv(
    here::here("data-raw/llm_nocontext_superset_roadmap.csv"),
    row.names = FALSE
  )

## Load separate dataframes for each LLM roadmap
load(here::here("data-raw/context-llm.rda"))

extract_terms <- function(df, id_value, variable_name) {
  terms <- unlist(strsplit(
    paste(
      df |>
        filter(
          id == glue::glue("df_context_{id_value}"),
          Variable_Name == variable_name
        ) |>
        select(If_Missing_Search_For),
      collapse = ";"
    ),
    ";"
  ))
  terms <- unique(stringr::str_trim(tolower(terms[terms != ""])))
  data.frame(term = terms, df = paste0("df_", id_value))
}

## Extract terms for each of the 10 ALI components, and combine them into a 10-row dataframe
super_roadmap = data.frame() ### Initialize empty dataframe to hold superset roadmap

for (comp in ali_components) {
  all_terms_comp <- do.call(
    rbind,
    Map(extract_terms, list(df_context), 1:20, comp)
  ) |>
    pull(term) |>
    unique() |>
    paste(collapse = "; ")

  super_roadmap = super_roadmap |>
    bind_rows(data.frame(
      Variable_Name = comp,
      If_Missing_Search_For = all_terms_comp
    ))
}

## Save it
super_roadmap |>
  write.csv(
    here::here("data-raw/llm_context_superset_roadmap.csv"),
    row.names = FALSE
  )
