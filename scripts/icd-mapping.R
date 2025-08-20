# Load packages
library(dplyr) ## for data wrangling
library(tidyr) ## for separating rows
library(stringr) ## to clean up strings

# Read in ICD-10 Codes with Descriptions
icd10 = read.csv(here::here("data-raw/icd10cm-codes-2026.csv")) |>
  mutate(
    DX_DESC = toupper(DX_DESC), ## Convert to all CAPS for easier search
    DX_DESC = str_trim(DX_DESC)
  ) ## Trim whitespace

# Read in patient DX from sample of N = 1000
pat_dx = read.csv(here::here("data-raw/patient_data/dx_2022-09-29.csv")) |>
  mutate(CONTACT_DATE = as.Date(CONTACT_DATE, format = "%m/%d/%y")) |> ## convert the CONTACT_DATE to Date
  filter(CONTACT_DATE <= "2020-03-10") |> ## only keep diagnoses on or before 2020-03-10
  mutate(DX_CODE = sub(pattern = "\\.", replacement = "", x = DX_CODE)) ## Remove periods to merge with icd10

# Merge DX Descriptions into patient data
pat_dx = pat_dx |>
  left_join(y = icd10)

## Some ICD codes in the patient DX data didn't map to icd10, so manually recode them
### Happened for 1438 patients' DX 
pat_dx |> 
  filter(is.na(DX_DESC)) |> 
  nrow()
### Manual recoding / descriptions 
icd10_fix = read.csv(here::here("data-raw/fix-weird-icd-codes.csv")) |> 
  rename(DX_CODE = OLD_DX_CODE, 
         FIX_DX_CODE = DX_CODE, 
         FIX_DX_DESC = DX_DESC)
### Merge into pat_dx
pat_dx = pat_dx |> 
  left_join(icd10_fix) |> 
  mutate(DX_CODE = if_else(condition = is.na(DX_CODE), 
                           true = FIX_DX_CODE, 
                           false = DX_CODE), 
         DX_DESC = if_else(condition = is.na(DX_DESC), 
                           true = FIX_DX_DESC, 
                           false = DX_DESC)) |> 
  select(-FIX_DX_CODE, -FIX_DX_DESC) 

### Now, only left with 51 ICD codes in patient data that don't match ICD 
pat_dx |> 
  filter(is.na(DX_DESC)) |> 
  nrow()
### And they all seem to be... missing DX_CODE to begin with? 
pat_dx |> 
  filter(is.na(DX_DESC)) |> 
  pull(DX_CODE) |> 
  table()

# Read in audit roadmaps
## Original roadmap
roadmap = read.csv(here::here("data-raw/audit_roadmap.csv")) |>
  dplyr::select(Variable_Name, If_Missing_Search_For) |>
  separate_longer_delim(cols = If_Missing_Search_For, delim = ";") |> ## Create separate rows for each variable, keyword combo
  mutate(
    If_Missing_Search_For = toupper(If_Missing_Search_For), ## Convert to all CAPS for easier search
    If_Missing_Search_For = str_trim(string = If_Missing_Search_For), ## Trim whitespace
    If_Missing_Search_For = str_replace_all(If_Missing_Search_For, "\\s+", ".*")
  )

## LLM roadmap with context
roadmap_llm_context = read.csv(here::here(
  "data-raw/llm_context_superset_roadmap.csv"
)) |>
  dplyr::select(Variable_Name, If_Missing_Search_For) |>
  separate_longer_delim(cols = If_Missing_Search_For, delim = ";") |> ## Create separate rows for each variable, keyword combo
  mutate(
    If_Missing_Search_For = toupper(If_Missing_Search_For), ## Convert to all CAPS for easier search
    If_Missing_Search_For = str_trim(string = If_Missing_Search_For), ## Trim whitespace
    If_Missing_Search_For = str_replace_all(If_Missing_Search_For, "\\s+", ".*")
  )

## LLM roadmap no context
roadmap_llm_nocontext = read.csv(here::here(
  "data-raw/llm_nocontext_superset_roadmap.csv"
)) |>
  dplyr::select(Variable_Name, If_Missing_Search_For) |>
  separate_longer_delim(cols = If_Missing_Search_For, delim = ";") |> ## Create separate rows for each variable, keyword combo
  mutate(
    If_Missing_Search_For = toupper(If_Missing_Search_For), ## Convert to all CAPS for easier search
    If_Missing_Search_For = str_trim(string = If_Missing_Search_For), ## Trim whitespace
    If_Missing_Search_For = str_replace_all(If_Missing_Search_For, "\\s+", ".*")
  )

# Summarize unique DX per patient during study period
dx_uq = pat_dx |>
  dplyr::select(DX_CODE, DX_DESC) |>
  unique()

# Create a crosswalk between our diagnosis codes and the ICD codes/descriptions
matches = dx_uq |>
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

matches_llm_context = dx_uq |>
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

matches_llm_nocontext = dx_uq |>
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

# Merge crosswalk into patient diagnoses and create indicator of matched terms
pat_dx_flags = pat_dx |>
  left_join(matches, by = c("DX_CODE", "DX_DESC")) |>
  left_join(
    matches_llm_context,
    by = c("Variable_Name", "DX_CODE", "DX_DESC")
  ) |>
  left_join(
    matches_llm_nocontext,
    by = c("Variable_Name", "DX_CODE", "DX_DESC")
  ) |>
  mutate(
    has_match = !is.na(matched_terms),
    matched_terms = str_trim(replace_na(matched_terms, "")),
    has_match_llm_context = !is.na(matched_terms_llm_context),
    matched_terms_llm_context = str_trim(replace_na(
      matched_terms_llm_context,
      ""
    )),
    has_match_llm_nocontext = !is.na(matched_terms_llm_nocontext),
    matched_terms_llm_nocontext = str_trim(replace_na(
      matched_terms_llm_nocontext,
      ""
    ))
  )

# Check the first few rows of matches -- Looks great!
pat_dx_flags |>
  filter(has_match) |>
  head()

# Save
pat_dx_flags |>
  filter(has_match) |>
  write.csv(
    here::here("data-raw/patient_data/dx_original_roadmap.csv"),
    row.names = FALSE
  )
pat_dx_flags |>
  filter(has_match_llm_context) |>
  write.csv(
    here::here("data-raw/patient_data/dx_llm_context_superset_roadmap.csv"),
    row.names = FALSE
  )
pat_dx_flags |>
  filter(has_match_llm_nocontext) |>
  write.csv(
    here::here("data-raw/patient_data/dx_llm_nocontext_superset_roadmap.csv"),
    row.names = FALSE
  )


plot_data <- pat_dx_flags |>
  pivot_longer(
    cols = c(has_match, has_match_llm_context, has_match_llm_nocontext),
    names_to = "variable",
    values_to = "value"
  ) |>
  count(variable, value)

library(ggplot2)

ggplot(plot_data, aes(x = variable, y = n, fill = value)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = n), position = position_dodge()) +
  theme_minimal()
