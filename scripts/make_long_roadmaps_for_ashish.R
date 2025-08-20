make_terms_long = function(row, roadmap) {
  stringr::str_trim(stringr::str_split(string = roadmap$If_Missing_Search_For[row], 
                                       pattern = ";", simplify = TRUE)) |> 
    data.frame() |> 
    mutate(Variable_Name = roadmap$Variable_Name[row]) |> 
    select(Variable_Name, everything()) |> 
    setNames(c("Variable_Name", "Search_Term"))
}

llm_nocontext = read.csv(here::here("data-raw/llm_nocontext_superset_roadmap.csv"))
do.call(rbind, lapply(X = 1:10, FUN = make_terms_long, roadmap = llm_nocontext)) |> 
  write.csv(here::here("data-raw/llm_nocontext_superset_roadmap_long.csv"))

llm_context = read.csv(here::here("data-raw/llm_context_superset_roadmap.csv"))
do.call(rbind, lapply(X = 1:10, FUN = make_terms_long, roadmap = llm_nocontext)) |> 
  write.csv(here::here("data-raw/llm_context_superset_roadmap_long.csv"))
