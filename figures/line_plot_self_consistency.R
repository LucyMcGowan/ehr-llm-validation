# Load packages 
library(tidyr) ## to unpivot data
library(dplyr) ## for data wrangling
library(ggplot2) ## for pretty plots
library(purrr) ## to accumulate keywords

# Define colors 
cols = c("#ff99ff", "#8bdddb", "#787ff6", "#ffbd59", "#7dd5f6", "#ff884d")

# Load data
## ALI components before and after validation (waves separately)
load(here::here("data-raw/no_context-llm-loop-icd10.rda"))
load(here::here("data-raw/context-llm-loop-icd10.rda"))
both_llm = df_nocontext |> 
  mutate(method = "LLM (Baseline)") |> 
  bind_rows(
    df_context |> 
      mutate(method = "LLM (Context)")
  ) |> 
  mutate(num = as.numeric(stringr::str_replace(id, ".*_", ""))) |> 
  group_by(method, Variable_Name) |> 
  arrange(num) |> 
  mutate(num_keywords = stringr::str_count(string = If_Missing_Search_For, pattern = ";") + 1, 
         cum_keywords = accumulate(.x = If_Missing_Search_For, .f = union), 
         cum_keywords = map_chr(cum_keywords, ~ paste(.x, collapse = ", ")),
         num_cum_keywords = stringr::str_count(string = cum_keywords, pattern = ";") + 1)

# Make forest plot 
both_llm |> 
  mutate(method = factor(x = method, 
                         levels = c("LLM (Baseline)", "LLM (Context)")), 
         Variable_Name = factor(x = Variable_Name,
                                levels = c("A1C", "ALB", "BMI", "BP_DIASTOLIC", "BP_SYSTOLIC", 
                                           "CHOL", "CREAT_C", "CRP", "HCST",  "TRIG"),
                                labels = c("HBA1C", "ALB", "BMI",  "DBP", "SBP", 
                                           "CHOL", "CC", "CRP", "HCST",  "TRIG"))
         ) |> 
  ggplot(aes(x = num, y = num_cum_keywords, color = method)) + 
  geom_line(linewidth = 1.5) + 
  facet_wrap(~Variable_Name, scales = "free") + 
  theme_minimal(base_size = 20) + 
  labs(x = "Number of Times Prompted", y = "Number of Unique Search Terms Proposed") + 
  theme(title = element_text(face = "bold"), 
        legend.position = "top", 
        legend.title = element_text(face = "bold"), 
        legend.background = element_rect(fill = "white"), 
        strip.text = element_text(face = "bold", color = "white"), 
        strip.background = element_rect(fill = "black")) + 
  scale_color_manual(values = cols[c(3, 6)], #c(cols[2], "#787ff6", cols[5]),  #"lightgrey", 
                     name = "Roadmap Enhancement:", 
                     labels = function(x) stringr::str_wrap(x, width = 30))
## Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/line_plot_self_consistency_proposed.png", 
       device = "png", width = 14, height = 7, units = "in")  
