clin_roadmap = read.csv("~/Documents/ehr-llm-validation/data-raw/llm_context_loop_icd10_superset_roadmap_clinician_reviewed.csv")
clin_roadmap |> 
	nrow() ## 421 ICD codes matched in the sample

search_terms = clin_roadmap |> 
	dplyr::pull(MATCHED_TERMS) |> 
	stringr::str_split(pattern = ";") |> 
	unlist() |> 
	as.vector() |> 
	unique()
search_terms |> 
	length() ## 115 unique search terms
