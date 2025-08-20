library(ggplot2)
library(dplyr)

# Define colors
paper_colors = c("#ff99ff", "#787ff6", "#8bdddb", "#7dd5f6", "#ffbd59")

roadmap <- readr::read_csv(here::here("data-raw/audit_roadmap.csv"))
load(here::here("data-raw/context-llm.rda"))

extract_terms <- function(df, i, variable_name = "BMI") {
  terms <- unlist(strsplit(
    paste(
      df |>
        filter(
          id == glue::glue("df_context_{i}"),
          Variable_Name == variable_name
        ) |>
        select(If_Missing_Search_For),
      collapse = ";"
    ),
    ";"
  ))
  terms <- unique(stringr::str_trim(tolower(terms[terms != ""])))
  data.frame(term = terms, df = paste0("df_", i))
}

# Separate all words per LLM roadmap into their own row 
all_terms <- do.call(rbind, 
                     lapply(X = unique(df_context$Variable_Name), 
                            FUN = function(x) 
                              cbind(do.call(
                                rbind,
                                Map(extract_terms, list(df_context), 1:20, x)
                              ), comp = x
                              )
                     )
)

# Initialize term_matrix
term_matrix <- all_terms |>
  mutate(present = !is.na(df))

extract_roadmap <- function(df, i, variable_name = "BMI") {
  terms <- unlist(strsplit(
    paste(
      df |>
        filter(Variable_Name == variable_name) |>
        select(If_Missing_Search_For),
      collapse = ";"
    ),
    ";"
  ))
  terms <- unique(stringr::str_trim(tolower(terms[terms != ""])))
  data.frame(term = terms, df = paste0("df_", i))
}

# Separate all words in clinicians' roadmap into their own row 
roadmap_terms <- do.call(rbind, 
                         lapply(X = unique(df_context$Variable_Name), 
                                FUN = function(x) 
                                  extract_roadmap(roadmap, 1, x) |>
                                  select(term) |>
                                  mutate(in_roadmap = TRUE, 
                                         comp = x)
                         )
)

# Join LLM + clinicians' roadmaps 
term_matrix <- term_matrix |>
  left_join(roadmap_terms, by = c("term", "comp")) |> 
  mutate(in_roadmap = if_else(condition = is.na(in_roadmap), 
                              true = FALSE, 
                              false = in_roadmap))

# Sum the number of LLM roadmaps where each term appears 
sums <- term_matrix |>
  group_by(comp, term, in_roadmap) |>
  summarise(total = sum(present)) 

# Faceted plot of all variables 
# ggplot(sums, aes(x = reorder(term, total), 
#                  y = total, 
#                  fill = in_roadmap)) +
#   geom_col() +
#   xlab("Search Terms") + 
#   ylab("Number of LLM Roadmaps") + 
#   scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 5)) + 
#   coord_flip() + 
#   facet_wrap(~comp, scales = "free", ncol = 5) + 
#   scale_fill_manual(values = paper_colors, 
#                     name = "In Clinicians' Roadmap?") + 
#   theme_minimal(base_size = 14) + 
#   theme(legend.position = "top", 
#         legend.title = element_text(face = "bold"), 
#         axis.title = element_text(face = "bold"), 
#         strip.background = element_rect(fill = "black"), 
#         strip.text = element_text(color = "white", face = "bold")) 
