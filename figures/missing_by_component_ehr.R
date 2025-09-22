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
  select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT)

# Create new dataframe with number of missing values per ALI component
num_miss = all_data |> 
  group_by(Variable_Name) |> 
  summarize(NUM_MISSING = sum(is.na(ALI_COMPONENT)))

# Create bar plot of missing values per component (colored by wave)
order_levels = num_miss |> 
  arrange(desc(NUM_MISSING)) |> 
  pull(Variable_Name)
bar_plot = num_miss |> 
  mutate(Variable_Name = factor(x = Variable_Name, 
                                levels = order_levels, 
                                labels = c("Creatinine Clearance", "Homo-\ncysteine",
                                           "C-Reactive Protein", "Hemoglobin A1C", 
                                           "Cholest-\nerol", "Trigly-\ncerides", 
                                           "Serum Albumin", "Body Mass Index", 
                                           "Systolic Blood Pressure", 
                                           "Diastolic Blood Pressure"))) |> 
  ggplot(aes(x = Variable_Name, 
             y = NUM_MISSING)) + 
  geom_bar(stat = "identity", 
           position = position_dodge(width = 1), 
           color = "black", 
           fill = cols[1]) + 
  geom_text(aes(label=NUM_MISSING), 
            vjust = -1, 
            size = 4, 
            position = position_dodge(width = 1)) + 
  theme_minimal(base_size = 20) + 
  labs(x = "Missing Allostatic Load Index Component", 
       y = "Number of Patients") +
  theme(title = element_text(face = "bold"), 
        legend.position = "inside", 
        legend.position.inside = c(1, 0.75),
        legend.title = element_text(face = "bold"), 
        legend.justification = "right", 
        legend.background = element_rect(fill = "white")) + 
  scale_fill_manual(values = cols, name = "Data:") + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 8))
bar_plot 

## Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/ehr_missing_by_component.png", 
       device = "png", width = 14, height = 7, units = "in")