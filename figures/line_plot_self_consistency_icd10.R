# Load packages 
library(tidyr) ## to unpivot data
library(dplyr) ## for data wrangling
library(ggplot2) ## for pretty plots
library(purrr) ## to accumulate keywords

# Define colors 
cols = c("#ff99ff", "#8bdddb", "#787ff6", "#ffbd59", "#7dd5f6", "#ff884d")

# Load data
## Matched search terms 
both_llm_matched = read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/dx_llm_nocontext_loop_icd10_superset_roadmap_separate_prompts.csv") |> 
  mutate(method = "LLM (Baseline)") |> 
  select(id, Variable_Name, DX_CODE, method) |> 
  bind_rows(
    read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/dx_llm_context_loop_icd10_superset_roadmap_separate_prompts.csv") |> 
      mutate(method = "LLM (Context)") |> 
      select(id, Variable_Name, DX_CODE, method) 
  ) |> 
  mutate(num = as.numeric(stringr::str_replace(id, ".*_", ""))) 

### Loop over and count [cumulative] number of ICD codes matching search terms
dx_llm_matched = data.frame()
for (n in 1:20) {
  subset_upto_n = both_llm_matched |> 
    filter(num <= n)
  
  dx_llm_matched = subset_upto_n |> 
    group_by(method, Variable_Name) |> 
    summarize(num_cum_keywords_matched = n_distinct(DX_CODE)) |> 
    mutate(num = n) |> 
    bind_rows(dx_llm_matched)
}  

# Make line plot 
plot_data = dx_llm_matched |> 
  mutate(method = factor(x = method, 
                         levels = c("LLM (Baseline)", "LLM (Context)")), 
         Variable_Name = factor(x = Variable_Name,
                                levels = c("A1C", "ALB", "BMI", "BP_DIASTOLIC", "BP_SYSTOLIC", 
                                           "CHOL", "CREAT_C", "CRP", "HCST",  "TRIG"),
                                labels = c("HBA1C", "ALB", "BMI",  "DBP", "SBP", 
                                           "CHOL", "CC", "CRP", "HCST",  "TRIG"))
  ) |> 
  arrange(method, Variable_Name) 
## Check for rows where the 20th list had no new matches, so it's currently NA 
# plot_data |> 
#   group_by(Variable_Name, method, what) |> 
#   summarize(max_num = max(num)) |> 
#   filter(max_num < 20)
# ### Serum albumin 
# add_alb_row = plot_data |> 
#   filter(Variable_Name == "ALB", method == "LLM (Context)", what == "Matched", num == 19) |> 
#   mutate(num = 20)
# ### Triglycerides 
# add_trig_row = plot_data |> 
#   filter(Variable_Name == "TRIG", method == "LLM (Context)", what == "Matched", num == 19) |> 
#   mutate(num = 20)
# 
# plot_data = plot_data |> 
#   bind_rows(add_alb_row) |> 
#   bind_rows(add_trig_row) 

# Find the scaling factor between your two variables
plot_data |> 
  arrange(Variable_Name, method, num) |> 
  group_by(Variable_Name, method) |> 
  fill(num_cum_keywords_matched, .direction = "down") |> 
  ggplot(aes(x = num, color = method)) + 
  #geom_line(aes(y = num_cum_keywords_proposed, group = method), linewidth = 1.5, linetype = "dashed", na.rm = TRUE) + 
  geom_line(aes(y = num_cum_keywords_matched), linewidth = 1.5, linetype = "solid", na.rm = TRUE) + 
  facet_wrap(~Variable_Name, scales = "free") + 
  theme_minimal(base_size = 20) + 
  labs(x = "Number of Times LLMs Prompted", y = "Number of Unique ICD-10 Codes Matched") + 
  theme(title = element_text(face = "bold"), 
        legend.position = c(0.75, 0.15),  # adjust these values to fine-tune
        legend.justification = c(0.5, 0.5),
        legend.title = element_text(face = "bold"), 
        legend.direction = "vertical",
        legend.background = element_rect(fill = "white", color = "white"), 
        strip.text = element_text(face = "bold", color = "white"), 
        strip.background = element_rect(fill = "black")) + 
  scale_color_manual(values = cols[c(3, 6)], #c(cols[2], "#787ff6", cols[5]),  #"lightgrey", 
                     name = "Roadmap\nEnhancement:", 
                     labels = function(x) stringr::str_wrap(x, width = 30)) + 
  scale_linetype_discrete(name = "Status:") 
## Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/line_plot_self_consistency_icd10.png", 
       device = "png", width = 14, height = 7, units = "in")  
