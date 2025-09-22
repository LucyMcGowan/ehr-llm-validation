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

# Separate plots per variable
plot_bars <- function(variable_name) {
  all_terms <- do.call(
    rbind,
    Map(extract_terms, list(df_context), 1:20, variable_name)
  )

  term_matrix <- all_terms |>
    mutate(present = !is.na(df))

  roadmap_terms <- extract_roadmap(roadmap, 1, variable_name) |>
    select(term) |>
    mutate(in_roadmap = TRUE)

  term_matrix <- term_matrix |>
    left_join(roadmap_terms, by = "term") |> 
    mutate(in_roadmap = if_else(condition = is.na(in_roadmap), 
                                true = FALSE, 
                                false = in_roadmap))

  sums <- term_matrix |>
    group_by(term, in_roadmap) |>
    summarise(total = sum(present)) 

  ggplot(sums, aes(x = reorder(term, total), 
                   y = total, 
                   fill = in_roadmap)) +
    geom_col() +
    xlab("Search Term") + 
    ylab("Number of LLM Roadmaps") + 
    #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 5)) + 
    coord_flip() + 
    scale_fill_manual(values = paper_colors[c(2,3)], 
                      #name = "In Clinicians' Roadmap?", 
                      guide = "none") + 
    theme_minimal(base_size = 14) + 
    theme(legend.position = "top", 
          legend.justification = "left",
          legend.direction = "horizontal",
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
          legend.title = element_text(face = "bold"), 
          axis.title = element_text(face = "bold")) #+ 
    #ggtitle(label = variable_name)
}

# ggpubr::ggarrange(plotlist = lapply(X = unique(df_context$Variable_Name), 
#                                     FUN = plot_bars), 
#                   common.legend = TRUE, ncol = 2, nrow = 5)

save_plot_bars <- function(variable_name) {
  ggsave(filename = here::here(paste0("figures/search-term-frequency-plots/llm-context/", tolower(variable_name), ".pdf")), 
         plot = plot_bars(variable_name), device = "pdf", 
         width = 8, height = 14, units = "in")
}

sapply(X = unique(df_context$Variable_Name), 
       FUN = save_plot_bars)

plot_points <- function(variable_name) {
  all_terms <- do.call(
    rbind,
    Map(extract_terms, list(df_context), 1:20, variable_name)
  )
  
  term_matrix <- all_terms |>
    mutate(present = !is.na(df))
  
  roadmap_terms <- extract_roadmap(roadmap, 1, variable_name) |>
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
  
  ggplot(term_matrix, aes(x = df, y = term)) +
    geom_point(aes(color = present)) +
    theme_minimal()
}

plot_points("BMI")

library(ggplot2)
library(dplyr)

# Define colors
paper_colors = c("#ff99ff", "#787ff6", "#8bdddb", "#7dd5f6", "#ffbd59")

roadmap <- readr::read_csv(here::here("data-raw/audit_roadmap.csv"))
load(here::here("data-raw/no_context-llm.rda"))

extract_terms <- function(df, i, variable_name = "BMI") {
  terms <- unlist(strsplit(
    paste(
      df |>
        filter(
          id == glue::glue("df_nocontext_{i}"),
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

# Separate plots per variable
plot_bars <- function(variable_name) {
  all_terms <- do.call(
    rbind,
    Map(extract_terms, list(df_nocontext), 1:20, variable_name)
  )
  
  term_matrix <- all_terms |>
    mutate(present = !is.na(df))
  
  roadmap_terms <- extract_roadmap(roadmap, 1, variable_name) |>
    select(term) |>
    mutate(in_roadmap = TRUE)
  
  term_matrix <- term_matrix |>
    left_join(roadmap_terms, by = "term") |> 
    mutate(in_roadmap = if_else(condition = is.na(in_roadmap), 
                                true = FALSE, 
                                false = in_roadmap))
  
  sums <- term_matrix |>
    group_by(term, in_roadmap) |>
    summarise(total = sum(present)) 
  
  ggplot(sums, aes(x = reorder(term, total), 
                   y = total, 
                   fill = in_roadmap)) +
    geom_col() +
    xlab("Search Term") + 
    ylab("Number of LLM Roadmaps") + 
    #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 5)) + 
    coord_flip() + 
    scale_fill_manual(values = paper_colors[c(2,3)], 
                      #name = "In Clinicians' Roadmap?", 
                      guide = "none") + 
    theme_minimal(base_size = 14) + 
    theme(legend.position = "top", 
          legend.justification = "left",
          legend.direction = "horizontal",
          legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
          legend.title = element_text(face = "bold"), 
          axis.title = element_text(face = "bold")) #+ 
  #ggtitle(label = variable_name)
}

# ggpubr::ggarrange(plotlist = lapply(X = unique(df_context$Variable_Name), 
#                                     FUN = plot_bars), 
#                   common.legend = TRUE, ncol = 2, nrow = 5)

save_plot_bars <- function(variable_name) {
  ggsave(filename = here::here(paste0("figures/search-term-frequency-plots/llm-nocontext/", tolower(variable_name), ".pdf")), 
         plot = plot_bars(variable_name), device = "pdf", 
         width = 8, height = 14, units = "in")
}

sapply(X = unique(df_nocontext$Variable_Name), 
       FUN = save_plot_bars)
