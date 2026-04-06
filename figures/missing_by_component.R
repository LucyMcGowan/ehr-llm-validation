# Load packages 
library(tidyr) ## to unpivot data
library(dplyr) ## for data wrangling
library(ggplot2) ## for pretty plots
library(ggpattern) ## for patterned bars

# Define colors 
cols = c("#ff99ff", "#8bdddb", "#787ff6", "#ffbd59", "#7dd5f6", "#ff884d")

# Define vector of IDs of chart review patients 
val_pat_id = read.csv("~/Documents/Allostatic_load_audits/all_ali_dat.csv") |> 
  filter(VALIDATED) |> 
  pull(PAT_MRN_ID)

# Load data
## ALI components before and after validation (waves separately)
all_data = read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_original_roadmap.csv") |> 
  mutate(CHART_ALI_COMPONENT = if_else(condition = !is.na(ALI_COMPONENT) & is.na(CHART_ALI_COMPONENT), 
                                       true = ALI_COMPONENT, 
                                       false = CHART_ALI_COMPONENT)) |> 
  select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT) |> 
  rename(ORIG_ALI_COMPONENT = SUPP_ALI_COMPONENT) |> 
  left_join(
    read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_nocontext_loop_icd10_roadmap.csv") |> 
      mutate(CHART_ALI_COMPONENT = if_else(condition = !is.na(ALI_COMPONENT) & is.na(CHART_ALI_COMPONENT), 
                                           true = ALI_COMPONENT, 
                                           false = CHART_ALI_COMPONENT)) |> 
      select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT) |> 
      rename(LLM_ALI_COMPONENT = SUPP_ALI_COMPONENT)
  ) |> 
  left_join(
    read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_context_loop_icd10_clinician_roadmap.csv") |> 
      mutate(CHART_ALI_COMPONENT = if_else(condition = !is.na(ALI_COMPONENT) & is.na(CHART_ALI_COMPONENT), 
                                           true = ALI_COMPONENT, 
                                           false = CHART_ALI_COMPONENT)) |> 
      select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT) |> 
      rename(LLM_CONTEXT_CLINICIAN_ALI_COMPONENT = SUPP_ALI_COMPONENT)
  ) |> 
  left_join(
    read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_context_loop_icd10_roadmap.csv") |> 
      mutate(CHART_ALI_COMPONENT = if_else(condition = !is.na(ALI_COMPONENT) & is.na(CHART_ALI_COMPONENT), 
                                           true = ALI_COMPONENT, 
                                           false = CHART_ALI_COMPONENT)) |> 
      select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT) |> 
      rename(LLM_CONTEXT_ALI_COMPONENT = SUPP_ALI_COMPONENT)
  ) |> 
  pivot_longer(cols = c(ALI_COMPONENT:LLM_CONTEXT_ALI_COMPONENT), 
               names_to = "DATA", values_to = "COMPONENT") |> 
  filter(PAT_MRN_ID %in% val_pat_id) ## subset to n = 100 chart reviewed patients

# Create new dataframe with number of missing values per ALI component
num_miss = all_data |> 
  group_by(Variable_Name, DATA) |> 
  summarize(NUM_MISSING = sum(is.na(COMPONENT)))

# Create bar plot of missing values per component (colored by wave)
order_levels = num_miss |> 
  filter(DATA == "ALI_COMPONENT") |> 
  arrange(desc(NUM_MISSING)) |> 
  pull(Variable_Name)
bar_plot2 = num_miss |> 
  mutate(Variable_Name = factor(x = Variable_Name, 
                                levels = order_levels, 
                                labels = c("CC", "HCST", "CRP", "HBA1C", "CHOL", "TRIG", "ALB", "BMI", "SBP", "DBP")), # , 
                                # labels = c("Creatinine Clearance", "Homo-\ncysteine",
                                #            "C-Reactive Protein", "Hemoglobin A1C", 
                                #            "Cholest-\nerol", "Trigly-\ncerides", 
                                #            "Serum Albumin", "Body Mass Index", 
                                #            "Systolic Blood Pressure", 
                                #            "Diastolic Blood Pressure")), 
         DATA = factor(x = DATA, 
                       levels = c("ALI_COMPONENT", 
                                  "CHART_ALI_COMPONENT", 
                                  "LLM_ALI_COMPONENT",
                                  "ORIG_ALI_COMPONENT", 
                                  "LLM_CONTEXT_CLINICIAN_ALI_COMPONENT",
                                  "LLM_CONTEXT_ALI_COMPONENT"), 
                       labels = c("Extracted EHR Data", 
                                  "Expert Chart Reviews", 
                                  "Algorithm w/ LLMs (Baseline)",
                                  "Algorithm w/ Clinicians' Original", 
                                  "Algorithm w/ LLMs (Context + Clinicians)", 
                                  "Algorithm w/ LLMs (Context)"))) |> 
  ggplot(aes(x = Variable_Name, 
             y = NUM_MISSING, 
             fill = DATA)) + 
  geom_bar(stat = "identity", 
           position = position_dodge(width = 1), 
           color = "black") + 
  geom_text(aes(label=NUM_MISSING), 
            vjust = -0.25, 
            size = 4.5, 
            position = position_dodge(width = 1)) + 
  theme_minimal(base_size = 20) + 
  labs(x = "ALI Component", 
       y = "Number of Patients Missing the Component") +
  theme(title = element_text(face = "bold"), 
        legend.position = "inside", 
        legend.position.inside = c(1, 0.75),
        legend.title = element_text(face = "bold"), 
        legend.justification = "right", 
        legend.background = element_rect(fill = "white"), 
        strip.text = element_text(face = "bold", color = "white"), 
        strip.background = element_rect(fill = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(color = "black")) + 
  scale_fill_manual(values = cols, name = "Data:", guide = "none") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 8)) + 
  facet_wrap(~DATA, ncol = 1)
  #facet_grid(rows = vars(DATA), strip.position = "top")
bar_plot2 

## Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/missing_by_component_revised.png", 
       device = "png", width = 9, height = 13, units = "in")

# Calculate number recovered per roadmap per component 
num_recov = num_miss |> 
  group_by(Variable_Name) |> 
  mutate(NUM_MISSING_EHR = max(NUM_MISSING), 
         NUM_RECOVERED = NUM_MISSING_EHR - NUM_MISSING)
num_recov |> 
  mutate(Variable_Name = factor(x = Variable_Name, 
                                levels = order_levels, 
                                labels = c("CC", "HCST", "CRP", "HBA1C", "CHOL", "TRIG", "ALB", "BMI", "SBP", "DBP")), # , 
         # labels = c("Creatinine Clearance", "Homo-\ncysteine",
         #            "C-Reactive Protein", "Hemoglobin A1C", 
         #            "Cholest-\nerol", "Trigly-\ncerides", 
         #            "Serum Albumin", "Body Mass Index", 
         #            "Systolic Blood Pressure", 
         #            "Diastolic Blood Pressure")), 
         DATA = factor(x = DATA, 
                       levels = c("ALI_COMPONENT", 
                                  "CHART_ALI_COMPONENT", 
                                  "LLM_ALI_COMPONENT",
                                  "ORIG_ALI_COMPONENT", 
                                  "LLM_CONTEXT_CLINICIAN_ALI_COMPONENT",
                                  "LLM_CONTEXT_ALI_COMPONENT"), 
                       labels = c("Extracted EHR Data", 
                                  "Expert Chart Reviews", 
                                  "Algorithm w/ LLMs (Baseline)",
                                  "Algorithm w/ Clinicians' Original", 
                                  "Algorithm w/ LLMs (Context + Clinicians)", 
                                  "Algorithm w/ LLMs (Context)"))) |> 
  ggplot(aes(x = Variable_Name, 
             y = NUM_RECOVERED, 
             fill = DATA)) + 
  geom_bar(stat = "identity", 
           position = position_dodge(width = 1), 
           color = "black") + 
  geom_text(aes(label=NUM_RECOVERED), 
            vjust = -0.25, 
            size = 4.5, 
            position = position_dodge(width = 1)) + 
  theme_minimal(base_size = 20) + 
  labs(x = "ALI Component", 
       y = "Number of Patients Missing the Component") +
  theme(title = element_text(face = "bold"), 
        legend.position = "inside", 
        legend.position.inside = c(1, 0.75),
        legend.title = element_text(face = "bold"), 
        legend.justification = "right", 
        legend.background = element_rect(fill = "white"), 
        strip.text = element_text(face = "bold", color = "white"), 
        strip.background = element_rect(fill = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(color = "black")) + 
  scale_fill_manual(values = cols, name = "Data:", guide = "none") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 8)) + 
  facet_wrap(~DATA, ncol = 1)
## Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/recovered_by_component_revised.png", 
       device = "png", width = 9, height = 13, units = "in")
num_recov |> 
  group_by(DATA) |> 
  summarize(MAX_NUM_RECOVERED = max(NUM_RECOVERED))
