# Load packages
library(dplyr) ## To wrangle data
library(tidyr) ## To transform data
library(ggplot2) ## To create plots
library(ggtext) ## To italicize part of titles/labels
library(stringr) ## To wrap plot titles/text

# Define colors
paper_colors = c("#ff99ff", "#787ff6", "#8bdddb", "#7dd5f6", "#ffbd59")

# Define vector of IDs of chart review patients 
val_pat_id = read.csv("~/Documents/Allostatic_load_audits/all_ali_dat.csv") |> 
  filter(VALIDATED) |> 
  pull(PAT_MRN_ID)

# Function to produce heatmaps of EHR vs. Roadmap and then Roadmap vs. Chart Review
make_heatmaps = function(data_path, roadmap_label, save_to, ehr_vs_chart = FALSE) {
  # Load data, contains: 
  ## Unvalidated ALI components (original extracted EHR data) for all 1000 patients -- "ALI_COMPONENT"
  ## Unvalidated ALI components augmented using audit roadmap for all 1000 patients -- SUPP_ALI_COMPONENT
  ## Validated ALI components (chart review) for only 100 patients -- CHART_ALI_COMPONENT
  all_data = read.csv(data_path) |> 
    ### Keep only the columns we need 
    select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT, COMP_FLAG) |> 
    ### Make the factor labels prettier for plots 
    mutate(SUPP_ALI_COMPONENT = factor(x = SUPP_ALI_COMPONENT,
                                       levels = c(1, 0, NA), 
                                       labels = c("Yes", "No", "Missing"), exclude = NULL),
           #### Check for chart review components where values out of study period were given 
           CHART_ALI_COMPONENT = if_else(condition = !is.na(COMP_FLAG) & stringr::str_detect(string = COMP_FLAG, pattern = Variable_Name), 
                                         true = -1, false = CHART_ALI_COMPONENT),
           CHART_ALI_COMPONENT = factor(x = CHART_ALI_COMPONENT, 
                                        levels = c(1, 0, NA, -1), 
                                        labels = c("Yes", "No", "Missing", "Value Beyond Study Period"), exclude = NULL), 
           ALI_COMPONENT = factor(x = ALI_COMPONENT, 
                                  levels = c(1, 0, NA), 
                                  labels = c("Yes", "No", "Missing"), exclude = NULL))
  
  ## Plot heatmap of SUPP_ALI_COMPONENT vs. ALI_COMPONENT
  ### Create dataframe of all combinations so that the geom_tile() will be complete
  all_combo = expand.grid(SUPP_ALI_COMPONENT = c("Yes", "No", "Missing"), 
                          ALI_COMPONENT = c("Yes", "No", "Missing"))
  all_data |> 
    group_by(SUPP_ALI_COMPONENT, ALI_COMPONENT) |> 
    summarize(num = n()) |>
    full_join(all_combo) |> 
    mutate(num = if_else(condition = is.na(num),
                         true = 0, 
                         false = num)) |> 
    ggplot(aes(x = ALI_COMPONENT, y = SUPP_ALI_COMPONENT, fill = num)) +
    geom_tile(color = "black",
              lwd = 0.5,
              linetype = 1) + 
    geom_text(aes(label = num), 
              color = "black", 
              size = 5) + 
    scale_fill_gradientn(colors = paper_colors, 
                         name = "Data\nPoints:", 
                         guide = guide_colorbar(frame.colour = "black", 
                                                ticks.colour = "black", 
                                                barwidth = 1, 
                                                barheight = 10)) + 
    theme_minimal(base_size = 14) +
    theme(legend.position = "right",
          title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          axis.text = element_markdown(),
          legend.title = element_text(face = "bold"), 
          strip.background = element_rect(fill = "black"), 
          strip.text = element_text(face = "bold", color = "white")) + 
    labs(x = "Unvalidated Allostatic Load Index\nComponent (from the EHR)",
         y = roadmap_label) 
  ggsave(filename = save_to[1],
         device = "png", width = 8, height = 4, units = "in")
  
  ## Plot heatmap of SUPP_ALI_COMPONENT vs. CHART_ALI_COMPONENT
  ### Create dataframe of all combinations so that the geom_tile() will be complete
  all_combo = expand.grid(CHART_ALI_COMPONENT = c("Yes", "No", "Missing", "Value Beyond Study Period"), 
                          SUPP_ALI_COMPONENT = c("Yes", "No", "Missing"))
  all_data |> 
    filter(PAT_MRN_ID %in% val_pat_id) |> ### Susbet to patients undergoing chart review
    group_by(CHART_ALI_COMPONENT, SUPP_ALI_COMPONENT) |> 
    summarize(num = n()) |>
    full_join(all_combo) |> 
    mutate(num = if_else(condition = is.na(num),
                         true = 0, 
                         false = num)) |> 
    ggplot(aes(x = CHART_ALI_COMPONENT, y = SUPP_ALI_COMPONENT, fill = num)) +
    geom_tile(color = "black",
              lwd = 0.5,
              linetype = 1) + 
    geom_text(aes(label = num), 
              color = "black", 
              size = 5) + 
    scale_fill_gradientn(colors = paper_colors, 
                         name = "Data\nPoints:", 
                         guide = guide_colorbar(frame.colour = "black", 
                                                ticks.colour = "black", 
                                                barwidth = 1, 
                                                barheight = 10)) + 
    theme_minimal(base_size = 14) +
    theme(legend.position = "right",
          title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          axis.text = element_markdown(),
          legend.title = element_text(face = "bold"), 
          strip.background = element_rect(fill = "black"), 
          strip.text = element_text(face = "bold", color = "white")) + 
    labs(x = "Validated Allostatic Load Index\nComponent (from Chart Review)", 
         y = roadmap_label,) 
  ggsave(filename = save_to[2],
         device = "png", width = 8, height = 4, units = "in")
  
  if(ehr_vs_chart) {
    ## Plot heatmap of ALI_COMPONENT vs. CHART_ALI_COMPONENT
    ### Create dataframe of all combinations so that the geom_tile() will be complete
    all_combo = expand.grid(ALI_COMPONENT = c("Yes", "No", "Missing"), 
                            CHART_ALI_COMPONENT = c("Yes", "No", "Missing"))
    all_data |> 
      filter(PAT_MRN_ID %in% val_pat_id) |> ### Susbet to patients undergoing chart review
      group_by(ALI_COMPONENT, CHART_ALI_COMPONENT) |> 
      summarize(num = n()) |>
      full_join(all_combo) |> 
      mutate(num = if_else(condition = is.na(num),
                           true = 0, 
                           false = num)) |> 
      ggplot(aes(x = ALI_COMPONENT, y = CHART_ALI_COMPONENT, fill = num)) +
      geom_tile(color = "black",
                lwd = 0.5,
                linetype = 1) + 
      geom_text(aes(label = num), 
                color = "black", 
                size = 5) + 
      scale_fill_gradientn(colors = paper_colors, 
                           name = "Data\nPoints:", 
                           guide = guide_colorbar(frame.colour = "black", 
                                                  ticks.colour = "black", 
                                                  barwidth = 1, 
                                                  barheight = 10)) + 
      theme_minimal(base_size = 14) +
      theme(legend.position = "right",
            title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            axis.text = element_markdown(),
            legend.title = element_text(face = "bold"), 
            strip.background = element_rect(fill = "black"), 
            strip.text = element_text(face = "bold", color = "white")) + 
      labs(x = "Unvalidated Allostatic Load Index\nComponent (from the EHR)", 
           y = "Validated Allostatic Load Index\nComponent (from Chart Review)",) 
    ggsave(filename = save_to[3],
           device = "png", width = 9, height = 4, units = "in")
  }
}