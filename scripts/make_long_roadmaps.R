make_terms_long = function(row, roadmap) {
  stringr::str_trim(stringr::str_split(string = roadmap$If_Missing_Search_For[row], 
                                       pattern = ";", simplify = TRUE)) |> 
    data.frame() |> 
    mutate(Variable_Name = roadmap$Variable_Name[row]) |> 
    select(Variable_Name, everything()) |> 
    setNames(c("Variable_Name", "Search_Term"))
}

original = read.csv(here::here("~/Documents/ehr-llm-validation/data-raw/audit_roadmap.csv"))
original_long = do.call(rbind, lapply(X = 1:10, FUN = make_terms_long, roadmap = original)) |> 
  filter(Search_Term != "")
original_long |> 
  nrow() ## 20 suggested search terms (before matching to ICD)
original_long |> 
  write.csv(here::here("~/Documents/ehr-llm-validation/data-raw/original_roadmap_long.csv"))

llm_nocontext = read.csv(here::here("~/Documents/ehr-llm-validation/data-raw/llm_nocontext_superset_roadmap.csv"))
llm_nocontext_long = do.call(rbind, lapply(X = 1:10, FUN = make_terms_long, roadmap = llm_nocontext)) 
llm_nocontext_long |> 
  nrow() ## 656 suggested additional search terms (before matching to ICD)
llm_nocontext_long |> 
  write.csv(here::here("~/Documents/ehr-llm-validation/data-raw/llm_nocontext_superset_roadmap_long.csv"))

llm_context = read.csv(here::here("~/Documents/ehr-llm-validation/data-raw/llm_context_superset_roadmap.csv"))
llm_context_long = do.call(rbind, lapply(X = 1:10, FUN = make_terms_long, roadmap = llm_context)) 
llm_context_long |> 
  nrow() ## 950 suggested additional search terms (before matching to ICD)
llm_context_long |> 
  write.csv(here::here("~/Documents/ehr-llm-validation/data-raw/llm_context_superset_roadmap_long.csv"))

# llm_context_clinician = read.csv(here::here("~/Documents/ehr-llm-validation/data-raw/patient_data/dx_llm_context_superset_clinician_reviewed_roadmap.csv")) |> 
#   select(Variable_Name, matched_terms_llm_context_clinician) |> 
#   unique()
# 
# llm_context_clinician_long = data.frame()
# for (v in unique(llm_context_clinician$Variable_Name)) { 
#   temp = llm_context_clinician |> 
#     filter(Variable_Name == v) |> 
#     pull(matched_terms_llm_context_clinician) |> 
#     stringr::str_split(pattern = ";", simplify = TRUE) |> 
#     stringr::str_trim()
#   temp = unique(temp[temp != ""])
#   llm_context_clinician_long = data.frame(Variable_Name = v, 
#                                           Search_Term = temp) |> 
#     dplyr::bind_rows(llm_context_clinician_long)
# }
# llm_context_clinician_long |> 
#   nrow() ## 86 
