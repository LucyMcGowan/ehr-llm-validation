# Load packages
library(dplyr) ## for data wrangling
library(tidyr) ## for separating rows
library(stringr) ## to clean up strings

# Read in ICD-10 Codes with Descriptions
icd10 = read.csv(here::here("~/Documents/ehr-llm-validation/data-raw/icd10cm-codes-2026.csv")) |>#read.csv(here::here("data-raw/icd10cm-codes-2026.csv")) |>
  mutate(
    DX_DESC = toupper(DX_DESC), ## Convert to all CAPS for easier search
    DX_DESC = str_trim(DX_DESC)
  ) ## Trim whitespace

# Read in audit roadmaps
## Original roadmap
roadmap = read.csv(here::here("~/Documents/ehr-llm-validation/data-raw/audit_roadmap.csv")) |>
  dplyr::select(Variable_Name, If_Missing_Search_For) |>
  separate_longer_delim(cols = If_Missing_Search_For, delim = ";") |> ## Create separate rows for each variable, keyword combo
  mutate(
    If_Missing_Search_For = toupper(If_Missing_Search_For), ## Convert to all CAPS for easier search
    If_Missing_Search_For = str_trim(string = If_Missing_Search_For), ## Trim whitespace
    If_Missing_Search_For = str_replace_all(
      If_Missing_Search_For,
      "[[:punct:]]",
      ""
    ), ## Remove punctuation
    If_Missing_Search_For = str_replace_all(If_Missing_Search_For, "\\s+", ".*")
  )

## LLM roadmap with context
roadmap_llm_context = read.csv(here::here(
  "~/Documents/ehr-llm-validation/data-raw/llm_context_superset_roadmap.csv"
)) |>
  dplyr::select(Variable_Name, If_Missing_Search_For) |>
  separate_longer_delim(cols = If_Missing_Search_For, delim = ";") |> ## Create separate rows for each variable, keyword combo
  mutate(
    If_Missing_Search_For = toupper(If_Missing_Search_For), ## Convert to all CAPS for easier search
    If_Missing_Search_For = str_trim(string = If_Missing_Search_For), ## Trim whitespace
    If_Missing_Search_For = str_replace_all(
      If_Missing_Search_For,
      "[[:punct:]]",
      ""
    ), ## Remove punctuation
    If_Missing_Search_For = str_replace_all(If_Missing_Search_For, "\\s+", ".*")
  )

## LLM roadmap no context
roadmap_llm_nocontext = read.csv(here::here(
  "~/Documents/ehr-llm-validation/data-raw/llm_nocontext_superset_roadmap.csv"
)) |>
  dplyr::select(Variable_Name, If_Missing_Search_For) |>
  separate_longer_delim(cols = If_Missing_Search_For, delim = ";") |> ## Create separate rows for each variable, keyword combo
  mutate(
    If_Missing_Search_For = toupper(If_Missing_Search_For), ## Convert to all CAPS for easier search
    If_Missing_Search_For = str_trim(string = If_Missing_Search_For), ## Trim whitespace
    If_Missing_Search_For = str_replace_all(
      If_Missing_Search_For,
      "[[:punct:]]",
      ""
    ), ## Remove punctuation
    If_Missing_Search_For = str_replace_all(If_Missing_Search_For, "\\s+", ".*")
  )

# Create a crosswalk between our diagnosis codes and the ICD codes/descriptions
## Clinicians' Original Roadmap
matches = icd10 |>
  cross_join(roadmap) |>
  filter(str_detect(
    DX_DESC,
    regex(If_Missing_Search_For, ignore_case = TRUE)
  )) |>
  group_by(Variable_Name, DX_CODE, DX_DESC) |>
  summarise(
    matched_terms = str_trim(paste(If_Missing_Search_For, collapse = "; ")),
    .groups = "drop"
  )
matches |> 
  nrow() ## 1234 matching ICD codes
matches |> 
  write.csv(here::here("~/Documents/ehr-llm-validation/data-raw/all_icd10_matches_original.csv"), 
            row.names = FALSE)

## LLMs with context roadmap
matches_llm_context = icd10 |>
  cross_join(roadmap_llm_context) |>
  filter(str_detect(
    DX_DESC,
    regex(If_Missing_Search_For, ignore_case = TRUE)
  )) |>
  group_by(Variable_Name, DX_CODE, DX_DESC) |>
  summarise(
    matched_terms_llm_context = str_trim(paste(
      If_Missing_Search_For,
      collapse = "; "
    )),
    .groups = "drop"
  )
matches_llm_context |> 
  nrow() ## 1853 matching ICD codes
matches_llm_context |> 
  write.csv(here::here("~/Documents/ehr-llm-validation/data-raw/all_icd10_matches_llm_context.csv"), 
            row.names = FALSE)

## LLMs without context roadmap
matches_llm_nocontext = icd10 |>
  cross_join(roadmap_llm_nocontext) |>
  filter(str_detect(
    DX_DESC,
    regex(If_Missing_Search_For, ignore_case = TRUE)
  )) |>
  group_by(Variable_Name, DX_CODE, DX_DESC) |>
  summarise(
    matched_terms_llm_nocontext = str_trim(paste(
      If_Missing_Search_For,
      collapse = "; "
    )),
    .groups = "drop"
  )
matches_llm_nocontext |> 
  nrow() ## 539 matching ICD codes
matches_llm_nocontext |> 
  write.csv(here::here("~/Documents/ehr-llm-validation/data-raw/all_icd10_matches_llm_nocontext.csv"), 
            row.names = FALSE)
