library(patchwork)

## Figure 2
source("~/Documents/ehr-llm-validation/figures/missing_by_patient.R")
source("~/Documents/ehr-llm-validation/figures/missing_by_component.R")
bar_plot1 + bar_plot2 + plot_annotation(tag_levels = "A")
### Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/fig2_missing_by_patient_and_component_revised.png", 
       device = "png", width = 20, height = 16, units = "in")

## Figure 3
source("~/Documents/ehr-llm-validation/figures/missing_by_patient_full_sample.R")
source("~/Documents/ehr-llm-validation/figures/missing_by_component_full_sample.R")

bar_plot1 + bar_plot2 + plot_annotation(tag_levels = "A")

## Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/fig3_missing_by_patient_and_component_full_sample.png", 
       device = "png", width = 20, height = 12, units = "in")
