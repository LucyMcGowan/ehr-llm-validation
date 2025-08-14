# Load packages 
library(tidyr) ## to unpivot data
library(dplyr) ## for data wrangling
library(ggplot2) ## for pretty plots

# Define colors 
cols = c("#ff99ff", "#8bdddb", "#787ff6", "#ffbd59", "#7dd5f6")

# Load data
## ALI components before and after validation (waves separately)
all_data = read.csv("~/Documents/Allostatic_load_audits/ICD-Codes/all_ali_dat_w_icd.csv") |> 
  mutate(CHART_ALI_COMPONENT = if_else(condition = !is.na(ALI_COMPONENT) & is.na(CHART_ALI_COMPONENT), 
                                       true = ALI_COMPONENT, 
                                       false = CHART_ALI_COMPONENT)) |> 
  select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT) |> 
  pivot_longer(cols = c(ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT), 
               names_to = "DATA", values_to = "COMPONENT")

# Create new dataframe with number of missing values per ALI component
num_miss = all_data |> 
  group_by(Variable_Name, DATA) |> 
  summarize(Num_Missing = sum(is.na(COMPONENT)))

# Create bar plot of missing values per component (colored by wave)
order_levels = num_miss |> 
  filter(DATA == "ALI_COMPONENT") |> 
  arrange(desc(Num_Missing)) |> 
  pull(Variable_Name)
num_miss |> 
  mutate(Variable_Name = factor(x = Variable_Name, 
                                levels = order_levels, 
                                labels = c("Creatinine Clearance", "Homo-\ncysteine",
                                           "C-Reactive Protein", "Hemoglobin A1C", 
                                           "Cholest-\nerol", "Trigly-\ncerides", 
                                           "Serum Albumin", "Body Mass Index", 
                                           "Systolic Blood Pressure", 
                                           "Diastolic Blood Pressure")), 
         DATA = factor(x = DATA, 
                       levels = c("ALI_COMPONENT", "SUPP_ALI_COMPONENT", "CHART_ALI_COMPONENT"), 
                       labels = c("Unvalidated EHR Data", 
                                  "EHR Data Supplemented with ICD Codes", 
                                  "Chart Review Validation"))) |> 
  ggplot(aes(x = Variable_Name, 
             y = Num_Missing, 
             fill = DATA)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           color = "black") + 
  geom_text(aes(label=Num_Missing), 
            vjust = -1, 
            size = 3, 
            position = position_dodge(width = 1)) + 
  theme_minimal(base_size = 12) + 
  labs(x = "Allostatic Load Index Component", 
       y = "Number of Patients", 
       title = "A) Missing Values by Component") +
  theme(title = element_text(face = "bold"), 
        legend.position = "inside", 
        legend.position.inside = c(1, 0.9),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.justification = "right", 
        legend.background = element_rect(fill = "white")) + 
  scale_fill_manual(values = cols, name = "Data:") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 8))

## Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/missing_by_component.png", 
       device = "png", width = 10, height = 6, units = "in")
