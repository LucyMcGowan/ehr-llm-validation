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
  group_by(PAT_MRN_ID) |> 
  summarize(NUM_MISSING = sum(is.na(ALI_COMPONENT)))

# Create bar plot of missing values per component (colored by wave)
bar_plot = num_miss |> 
  group_by(NUM_MISSING) |> 
  summarize(n = n()) |> 
  ggplot(aes(x = NUM_MISSING, 
             y = n)) + 
  geom_bar(stat = "identity", 
           position = position_dodge(width = 1), 
           color = "black", 
           fill = cols[1]) + 
  geom_text(aes(label=n), 
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
  scale_fill_manual(values = cols, name = "Data:") + #, labels = function(x) stringr::str_wrap(x, width = 70)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  scale_x_continuous(breaks = 0:10)
bar_plot 

## Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/ehr_missing_by_patient.png", 
       device = "png", width = 14, height = 7, units = "in")
