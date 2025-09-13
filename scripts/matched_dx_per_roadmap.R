dx_original = read.csv(here::here("data-raw/patient_data/dx_original_roadmap.csv"))

dx_original |> 
  select(Variable_Name, DX_CODE, DX_DESC, matched_terms) |> 
  arrange(Variable_Name, DX_CODE) |> 
  unique() |> 
  nrow() ## n = 211 diagnoses matched patients in our sample

dx_llm_context = read.csv(here::here("data-raw/patient_data/dx_llm_context_superset_roadmap.csv"))

dx_llm_context |> 
  select(Variable_Name, DX_CODE, DX_DESC, matched_terms_llm_context) |> 
  arrange(Variable_Name, DX_CODE) |> 
  unique() |> 
  nrow() ## n = 275 diagnoses matched patients in our sample (given to Ashish to review)

dx_llm_context_clinician = read.csv(here::here("data-raw/patient_data/dx_llm_context_superset_clinician_reviewed_roadmap.csv")) 
dx_llm_context_clinician |> 
  select(Variable_Name, DX_CODE, DX_DESC, matched_terms_llm_context_clinician) |> 
  arrange(Variable_Name, DX_CODE) |> 
  unique() |> 
  nrow() ## n = 240 diagnoses matched after Ashish reviewed

dx_llm_nocontext = read.csv(here::here("data-raw/patient_data/dx_llm_nocontext_superset_roadmap.csv"))
dx_llm_nocontext |> 
  select(Variable_Name, DX_CODE, DX_DESC, matched_terms_llm_nocontext) |> 
  arrange(Variable_Name, DX_CODE) |> 
  unique() |> 
  nrow() ## n = 118 diagnoses matched patients in our sample
