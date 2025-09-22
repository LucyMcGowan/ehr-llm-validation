# Load packages
library(dplyr) ## To wrangle data
library(tidyr) ## To transform data
library(flowchart) ## To make a flowchart visualization

# Define colors
paper_colors = c("#ff99ff", "#787ff6", "#8bdddb", "#7dd5f6", "#ffbd59")

# Define vector of IDs of chart review patients 
val_pat_id = read.csv("~/Documents/Allostatic_load_audits/all_ali_dat.csv") |> 
  filter(VALIDATED) |> 
  pull(PAT_MRN_ID)

# Write a function to load data and make flowchart comparing 
## Roadmap to chart review (in validated subset of 100 patients)
make_flowchart = function(data_path, save_as, start_with_ehr = FALSE, then_chart_review = FALSE) {
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
                                       labels = c("Unhealthy", 
                                                  "Healthy", 
                                                  "Missing"), 
                                       exclude = NULL),
           #### Check for chart review components where values out of study period were given 
           CHART_ALI_COMPONENT = if_else(condition = !is.na(COMP_FLAG) & stringr::str_detect(string = COMP_FLAG, pattern = Variable_Name), 
                                         true = -1, false = CHART_ALI_COMPONENT),
           CHART_ALI_COMPONENT = factor(x = CHART_ALI_COMPONENT, 
                                        levels = c(1, 0, NA, -1), 
                                        labels = c("Unhealthy", 
                                                   "Healthy", 
                                                   "Missing", 
                                                   "Protocol Error"), exclude = NULL), 
           ALI_COMPONENT = factor(x = ALI_COMPONENT, 
                                  levels = c(1, 0, NA), 
                                  labels = c("Unhealthy", 
                                             "Healthy", 
                                             "Missing"), 
                                  exclude = NULL)) |> 
    filter(PAT_MRN_ID %in% val_pat_id)
  
  # ## Plot heatmap of SUPP_ALI_COMPONENT vs. CHART_ALI_COMPONENT
  # ### Create dataframe of all combinations so that the geom_tile() will be complete
  # all_combo = expand.grid(CHART_ALI_COMPONENT = c("Yes", "No", "Missing", "Value Beyond Study Period"), 
  #                         SUPP_ALI_COMPONENT = c("Yes", "No", "Missing"))
  # flow_data = all_data |> 
  #   filter(PAT_MRN_ID %in% val_pat_id) |> ### Susbet to patients undergoing chart review
  #   group_by(CHART_ALI_COMPONENT, SUPP_ALI_COMPONENT) |> 
  #   summarize(num = n()) |>
  #   full_join(all_combo) |> 
  #   mutate(num = if_else(condition = is.na(num),
  #                        true = 0, 
  #                        false = num)) |> 
  #   rename(from = CHART_ALI_COMPONENT, 
  #          to = SUPP_ALI_COMPONENT)
  
  if (start_with_ehr) {
    if (then_chart_review) {
      all_data |> 
        as_fc(label = "Data Points Validated") |> 
        fc_split(ALI_COMPONENT, show_zero = TRUE) |>
        fc_split(CHART_ALI_COMPONENT, show_zero = TRUE) |>
        fc_modify( # modifying only boxes 4 and 5
          ~ . |>
            mutate(
              bg_fill = ifelse(id %in% c(1), paper_colors[5], bg_fill),
              bg_fill = ifelse(id %in% c(2, 5, 9, 13, 15), paper_colors[1], bg_fill),
              bg_fill = ifelse(id %in% c(3, 6, 10, 14, 16), paper_colors[2], bg_fill),
              bg_fill = ifelse(id %in% c(4, 7, 11, 15, 17), paper_colors[3], bg_fill),
              bg_fill = ifelse(id %in% c(8, 12, 16), paper_colors[4], bg_fill),
              #text_color = ifelse(id == 4, "white", text_color),
              #bg_fill = ifelse(id == 5, "violet", bg_fill)
            )
        ) |>
        fc_draw(canvas_bg = NULL) |> 
        fc_export(here::here(save_as), 
                  width = 10000, height = 2500, res = 700) 
    } else {
      all_data |> 
        as_fc(label = "Data Points Validated") |> 
        fc_split(ALI_COMPONENT, show_zero = TRUE) |>
        fc_split(SUPP_ALI_COMPONENT, show_zero = TRUE) |>
        fc_modify( # modifying only boxes 4 and 5
          ~ . |>
            mutate(
              bg_fill = ifelse(id %in% c(1), paper_colors[5], bg_fill),
              bg_fill = ifelse(id %in% c(2, 5, 8, 11, 14), paper_colors[1], bg_fill),
              bg_fill = ifelse(id %in% c(3, 6, 9, 12, 15), paper_colors[2], bg_fill),
              bg_fill = ifelse(id %in% c(4, 7, 10, 13, 16), paper_colors[3], bg_fill),
              #bg_fill = ifelse(id %in% c(8, 12, 16), paper_colors[4], bg_fill),
              #text_color = ifelse(id == 4, "white", text_color),
              #bg_fill = ifelse(id == 5, "violet", bg_fill)
            )
        ) |>
        fc_draw(canvas_bg = NULL) |> 
        fc_export(here::here(save_as), 
                  width = 10000, height = 2500, res = 700) 
    }
  } else {
    all_data |> 
      as_fc(label = "Data Points Validated") |> 
      fc_split(CHART_ALI_COMPONENT, show_zero = TRUE) |>
      fc_split(SUPP_ALI_COMPONENT, show_zero = TRUE) |> 
      fc_modify( # modifying only boxes 4 and 5
        ~ . |>
          mutate(
            bg_fill = ifelse(id %in% c(1), paper_colors[5], bg_fill),
            bg_fill = ifelse(id %in% c(2, 6, 9, 12, 15), paper_colors[1], bg_fill),
            bg_fill = ifelse(id %in% c(3, 7, 10, 13, 16), paper_colors[2], bg_fill),
            bg_fill = ifelse(id %in% c(4, 8, 11, 14, 17), paper_colors[3], bg_fill),
            bg_fill = ifelse(id %in% c(5), paper_colors[4], bg_fill),
            #text_color = ifelse(id == 4, "white", text_color),
            #bg_fill = ifelse(id == 5, "violet", bg_fill)
          )
      ) |>
      fc_draw(canvas_bg = NULL) |> 
      fc_export(here::here(save_as), 
                width = 10000, height = 2500, res = 700) 
  }
}

# Make plots for chart review vs. EHR
make_flowchart(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_original_roadmap.csv", 
               save_as = "~/Documents/ehr-llm-validation/figures/ehr_vs_chart_flowchart.png", 
               start_with_ehr = TRUE, 
               then_chart_review = TRUE)

# Make plots for original roadmap vs. chart review
make_flowchart(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_original_roadmap.csv", 
               save_as = "~/Documents/ehr-llm-validation/figures/chart_vs_original_flowchart.png")

# Make plots for original roadmap vs. EHR
make_flowchart(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_original_roadmap.csv", 
               save_as = "~/Documents/ehr-llm-validation/figures/ehr_vs_original_flowchart.png", 
               start_with_ehr = TRUE)

# Make plots for LLMs (without context) roadmap vs. chart reviews
make_flowchart(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_nocontext_roadmap.csv", 
               save_as = "~/Documents/ehr-llm-validation/figures/chart_vs_llm_nocontext_flowchart.png")

# Make plots for LLMs (without context) roadmap vs. EHR
make_flowchart(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_nocontext_roadmap.csv", 
               save_as = "~/Documents/ehr-llm-validation/figures/ehr_vs_llm_nocontext_flowchart.png", 
               start_with_ehr = TRUE)

# Make plots for LLMs (with context) roadmap vs. chart review
make_flowchart(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_context_roadmap.csv", 
               save_as = "~/Documents/ehr-llm-validation/figures/chart_vs_llm_context_flowchart.png")

# Make plots for LLMs (with context) roadmap vs. EHR
make_flowchart(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_context_roadmap.csv", 
               save_as = "~/Documents/ehr-llm-validation/figures/ehr_vs_llm_context_flowchart.png", 
               start_with_ehr = TRUE)

# Make plots for LLMs (with context) + clinician roadmap vs. chart review
make_flowchart(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_context_clinician_roadmap.csv", 
               save_as = "~/Documents/ehr-llm-validation/figures/chart_vs_llm_context_clinician_flowchart.png")

# Make plots for LLMs (with context) + clinician roadmap vs. EHR
make_flowchart(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_context_clinician_roadmap.csv", 
               save_as = "~/Documents/ehr-llm-validation/figures/ehr_vs_llm_context_clinician_flowchart.png", 
               start_with_ehr = TRUE)
