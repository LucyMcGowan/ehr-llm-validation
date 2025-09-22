# Load packages 
library(tidyr) ## to unpivot data
library(dplyr) ## for data wrangling
library(ggplot2) ## for pretty plots
library(ggpattern) ## for patterned bars 

# Define colors 
cols = c("#ff99ff", "#8bdddb", "#787ff6", "#ffbd59", "#7dd5f6", "#ff884d")

# Load data
## ALI components before and after validation (waves separately)
all_data = read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_original_roadmap.csv") |> 
  mutate(CHART_ALI_COMPONENT = if_else(condition = !is.na(ALI_COMPONENT) & is.na(CHART_ALI_COMPONENT), 
                                       true = ALI_COMPONENT, 
                                       false = CHART_ALI_COMPONENT)) |> 
  select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, CHART_ALI_COMPONENT) |> 
  left_join(
    read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_context_clinician_roadmap.csv") |> 
      mutate(CHART_ALI_COMPONENT = if_else(condition = !is.na(ALI_COMPONENT) & is.na(CHART_ALI_COMPONENT), 
                                           true = ALI_COMPONENT, 
                                           false = CHART_ALI_COMPONENT)) |> 
      select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT) |> 
      rename(LLM_CONTEXT_CLINICIAN_ALI_COMPONENT = SUPP_ALI_COMPONENT)
  ) |> 
  pivot_longer(cols = c(ALI_COMPONENT:LLM_CONTEXT_CLINICIAN_ALI_COMPONENT), 
               names_to = "DATA", values_to = "COMPONENT")

# Create new dataframe with number of missing values per ALI component
num_miss = all_data |> 
  group_by(PAT_MRN_ID, DATA) |> 
  summarize(NUM_MISSING = sum(is.na(COMPONENT))) |> 
  mutate(DATA = factor(x = DATA, 
                       levels = c("ALI_COMPONENT", 
                                  "CHART_ALI_COMPONENT", 
                                  "LLM_ALI_COMPONENT",
                                  "ORIG_ALI_COMPONENT", 
                                  "LLM_CONTEXT_CLINICIAN_ALI_COMPONENT",
                                  "LLM_CONTEXT_ALI_COMPONENT"), 
                       labels = c("Extracted EHR Data", 
                                  "Expert Chart Reviews", 
                                  "Algorithm w/ LLM (Baseline)",
                                  "Algorithm w/ Clinicians' Original", 
                                  "Algorithm w/ LLM (Context + Clinicians)", 
                                  "Algorithm w/ LLM (Context)"))) 

all_combn = expand.grid(DATA = c("Extracted EHR Data", 
                                 "Expert Chart Reviews", 
                                 "Algorithm w/ LLM (Context + Clinicians)"), 
                        NUM_MISSING = 0:10)

# Create bar plot of missing values per component (colored by wave)
num_miss |> 
  group_by(DATA, NUM_MISSING) |> 
  summarize(n = n()) |> 
  full_join(all_combn) |> 
  mutate(n = if_else(condition = is.na(n), true = 0, false = n)) |>
  ggplot(aes(x = NUM_MISSING, 
             y = n, 
             fill = DATA)) + 
  geom_bar(stat = "identity", 
           position = position_dodge(width = 1), 
           color = "black") + 
  geom_text(aes(label = n), 
            vjust = -1, 
            size = 4, 
            position = position_dodge(width = 1)) + 
  theme_minimal(base_size = 20) + 
  labs(x = "Number of Missing Allostatic Load Index Components", 
       y = "Number of Patients") +
  theme(title = element_text(face = "bold"), 
        legend.position = "inside", 
        legend.position.inside = c(1, 0.75),
        legend.title = element_text(face = "bold"), 
        legend.justification = "right", 
        legend.background = element_rect(fill = "white")) + 
  scale_fill_manual(values = cols, name = "Data Source:") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  scale_x_continuous(breaks = 0:10)

## Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/missing_by_patient_full_sample.png", 
       device = "png", width = 14, height = 7, units = "in")

## Calculate median non-missing 
num_miss |> 
  group_by(DATA) |> 
  summarize(q1_nonmiss = quantile(x = (10 - NUM_MISSING), 0.25),
            median_nonmiss = median(10 - NUM_MISSING), 
            q3_nonmiss = quantile(x = (10 - NUM_MISSING), 0.75))
