# Source plot-making function
source("figures/make_heatmaps.R")

# Make plots for original roadmap
make_heatmaps(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_original_roadmap.csv", 
              roadmap_label = "Augmented Allostatic Load Index\nComponent (Original Roadmap)", 
              save_to = c("~/Documents/ehr-llm-validation/figures/orig_vs_ehr_heatmap.png", 
                          "~/Documents/ehr-llm-validation/figures/chart_vs_orig_heatmap.png", 
                          "~/Documents/ehr-llm-validation/figures/chart_vs_ehr_heatmap.png"), 
              ehr_vs_chart = TRUE)

# Make plots for LLMs (without context) roadmap
make_heatmaps(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_nocontext_roadmap.csv", 
              roadmap_label = "Augmented Allostatic Load Index\nComponent (LLMs w/o Context Roadmap)", 
              save_to = c("~/Documents/ehr-llm-validation/figures/llm_nocontext_vs_ehr_heatmap.png", 
                          "~/Documents/ehr-llm-validation/figures/chart_vs_llm_nocontext_heatmap.png"))

# Make plots for LLMs (with context) roadmap
make_heatmaps(data_path = "~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_context_roadmap.csv", 
              roadmap_label = "Augmented Allostatic Load Index\nComponent (LLMs w/ Context Roadmap)", 
              save_to = c("~/Documents/ehr-llm-validation/figures/llm_context_vs_ehr_heatmap.png", 
                          "~/Documents/ehr-llm-validation/figures/chart_vs_llm_context_heatmap.png"))