dx_llm_context = read.csv(here::here("data-raw/patient_data/dx_llm_context_superset_roadmap.csv"))

dx_llm_context |> 
  select(Variable_Name, DX_CODE, DX_DESC, matched_terms_llm_context) |> 
  arrange(Variable_Name, DX_CODE) |> 
  unique() |> 
  nrow() ## n = 275 diagnoses given to Ashish to review

dx_llm_context_clinician = read.csv(here::here("data-raw/patient_data/dx_llm_context_superset_clinician_reviewed_roadmap.csv"))

