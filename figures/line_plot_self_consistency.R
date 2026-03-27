# Load packages 
library(tidyr) ## to unpivot data
library(dplyr) ## for data wrangling
library(ggplot2) ## for pretty plots
library(purrr) ## to accumulate keywords

# Define colors 
cols = c("#ff99ff", "#8bdddb", "#787ff6", "#ffbd59", "#7dd5f6", "#ff884d")

# Load data
## Proposed search terms 
load(here::here("data-raw/no_context-llm-loop-icd10.rda"))
load(here::here("data-raw/context-llm-loop-icd10.rda"))
both_llm_proposed = df_nocontext |> 
  mutate(method = "LLM (Baseline)") |> 
  bind_rows(
    df_context |> 
      mutate(method = "LLM (Context)")
  ) |> 
  mutate(num = as.numeric(stringr::str_replace(id, ".*_", ""))) |> 
  arrange(num) |> 
  group_by(method, Variable_Name) |> 
  mutate(num_keywords = stringr::str_count(string = If_Missing_Search_For, pattern = ";") + 1, 
         cum_keywords = accumulate(.x = If_Missing_Search_For, .f = union), 
         cum_keywords = map_chr(cum_keywords, ~ paste(.x, collapse = ", ")),
         num_cum_keywords = stringr::str_count(string = cum_keywords, pattern = ";") + 1, 
         what = "Proposed")

## Proposed search terms 
both_llm_matched = read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/dx_llm_nocontext_loop_icd10_superset_roadmap_separate_prompts.csv") |> 
  mutate(method = "LLM (Baseline)") |> 
  rename(matched_terms = matched_terms_llm_nocontext_loop_icd10) |> 
  select(id, Variable_Name, matched_terms, method) |> 
  bind_rows(
    read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/dx_llm_context_loop_icd10_superset_roadmap_separate_prompts.csv") |> 
      mutate(method = "LLM (Context)") |> 
      rename(matched_terms = matched_terms_llm_context_loop_icd10) |> 
      select(id, Variable_Name, matched_terms, method) 
  ) |> 
  mutate(num = as.numeric(stringr::str_replace(id, ".*_", ""))) |> 
  arrange(method, Variable_Name, num) |> 
  distinct() |> 
  group_by(num, method, Variable_Name) |> 
  summarise(If_Missing_Search_For = paste(unique(unlist(strsplit(matched_terms, ";\\s*"))), collapse = "; "),
            .groups = "drop") |> 
  arrange(num, Variable_Name, method) |> 
  group_by(Variable_Name, method) |> 
  mutate(cum_keywords = NA_character_, 
         what = "Matched")

### Loop over and count [cumulative] number of matched terms
for (i in seq_len(nrow(both_llm_matched))) {
  current_var    <- both_llm_matched$Variable_Name[i]
  current_method <- both_llm_matched$method[i]
  current_num    <- both_llm_matched$num[i]
  
  past_terms <- both_llm_matched %>%
    filter(Variable_Name == current_var, method == current_method, num <= current_num) %>%
    pull(If_Missing_Search_For)
  
  both_llm_matched$cum_keywords[i] <- paste(unique(unlist(strsplit(past_terms, ";\\s*"))), collapse = "; ")
}

## Calculate [cumulative] numbers of keywords 
both_llm_matched = both_llm_matched |> 
  mutate(num_keywords = stringr::str_count(If_Missing_Search_For, ";"),
         num_cum_keywords = stringr::str_count(cum_keywords, ";"))

# Make forest plot 
both_llm_proposed |> 
  bind_rows(both_llm_matched) |> 
  mutate(method = factor(x = method, 
                         levels = c("LLM (Baseline)", "LLM (Context)")), 
         Variable_Name = factor(x = Variable_Name,
                                levels = c("A1C", "ALB", "BMI", "BP_DIASTOLIC", "BP_SYSTOLIC", 
                                           "CHOL", "CREAT_C", "CRP", "HCST",  "TRIG"),
                                labels = c("HBA1C", "ALB", "BMI",  "DBP", "SBP", 
                                           "CHOL", "CC", "CRP", "HCST",  "TRIG"))
         ) |> 
  arrange(method, Variable_Name) |> 
  ggplot(aes(x = num, y = num_cum_keywords, color = method, linetype = what)) + 
  geom_line(linewidth = 1.5) + 
  facet_wrap(~Variable_Name, scales = "free") + 
  theme_minimal(base_size = 20) + 
  labs(x = "Number of Times Prompted", y = "Number of Unique Search Terms") + 
  theme(title = element_text(face = "bold"), 
        legend.position = c(0.75, 0.15),  # adjust these values to fine-tune
        legend.justification = c(0.5, 0.5),
        legend.title = element_text(face = "bold"), 
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "white", color = "white"), 
        strip.text = element_text(face = "bold", color = "white"), 
        strip.background = element_rect(fill = "black")) + 
  scale_color_manual(values = cols[c(3, 6)], #c(cols[2], "#787ff6", cols[5]),  #"lightgrey", 
                     name = "Roadmap\nEnhancement:", 
                     labels = function(x) stringr::str_wrap(x, width = 30)) + 
  scale_linetype_discrete(name = "Status:")
## Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/line_plot_self_consistency.png", 
       device = "png", width = 14, height = 7, units = "in")  
