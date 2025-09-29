clin_roadmap = read.csv("~/Documents/ehr-llm-validation/data-raw/llm_context_superset_roadmap_clinician_reviewed.csv")
clin_roadmap |> 
	nrow() ## 243 ICD codes matched in the sample

search_terms = clin_roadmap |> 
	dplyr::pull(matched_terms_llm_context_clinician) |> 
	stringr::str_split(pattern = ";") |> 
	unlist() |> 
	as.vector() |> 
	unique()
search_terms |> 
	length()
