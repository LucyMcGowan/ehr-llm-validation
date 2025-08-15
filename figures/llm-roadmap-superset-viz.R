df_list <- mget(dfs)

extract_terms <- function(df, i, variable_name = "BMI") {
  terms <- unlist(strsplit(paste(df |> filter(Variable_Name == variable_name) |> select(If_Missing_Search_For), collapse = ";"), ";"))
  terms <- unique(stringr::str_trim(tolower(terms[terms != ""])))
  data.frame(term = terms, df = paste0("df_", i))
}

plot_bars <- function(variable_name) {
  all_terms <- do.call(rbind, Map(extract_terms, df_list, 1:20, variable_name))
  
  term_matrix <- all_terms |>
    mutate(present = !is.na(df))
  
  roadmap_terms <- extract_terms(roadmap, 1, variable_name) |> 
    select(term) |>
    mutate(in_roadmap = TRUE)
  
  term_matrix <- term_matrix |>
    left_join(roadmap_terms, by = "term")
  
  sums <- term_matrix |>
    group_by(term, in_roadmap) |>
    summarise(total = sum(present))
  
  term_matrix <- term_matrix |>
    left_join(sums, by = "term") |>
    mutate(term = reorder(term, total))
  
  
  
  ggplot(sums, aes(x = reorder(term, total), y = total, fill = in_roadmap)) +
    geom_col() + 
    coord_flip()
}

plot_bars("BP_SYSTOLIC")

ggplot(term_matrix, aes(x = df, y = term)) +
  geom_point(aes(color = present)) +
  theme_minimal() 

## Combine datasets ----
df_list <- mget(dfs)
plot_bars("BP_SYSTOLIC")
plot_bars("CRP")