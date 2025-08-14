# Source plot-making function
source("figures/make_heatmaps.R")

# Make plots for original roadmap
make_heatmaps(data_path = "~/Documents/Allostatic_load_audits/ICD-Codes/all_ali_dat_w_icd.csv", 
              roadmap_label = "Augmented Allostatic Load Index\nComponent (Original Roadmap)", 
              save_to = c("~/Documents/ehr-llm-validation/figures/orig_vs_ehr_heatmap.png", 
                          "~/Documents/ehr-llm-validation/figures/chart_vs_orig_heatmap.png"))

# Make plots for original roadmap
make_heatmaps(data_path = "~/Documents/Allostatic_load_audits/ICD-Codes/all_ali_dat_w_icd.csv", 
              roadmap_label = "Augmented Allostatic Load Index\nComponent (Original Roadmap)", 
              save_to = c("~/Documents/ehr-llm-validation/figures/orig_vs_ehr_heatmap.png", 
                          "~/Documents/ehr-llm-validation/figures/chart_vs_orig_heatmap.png"))