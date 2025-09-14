# Load packages 
library(tidyr) ## to unpivot data
library(dplyr) ## for data wrangling
library(ggplot2) ## for pretty plots
library(ggpattern) ## for patterned bars 

# Define colors 
cols = c("#ff99ff", "#8bdddb", "#787ff6", "#ffbd59", "#7dd5f6", "#ff884d")

# Load data
## ALI components before and after validation (waves separately)
all_data = read.csv(here::here("Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_original_roadmap.csv")) |> 
  select(PAT_MRN_ID, ANY_ENCOUNTERS, AGE_AT_ENCOUNTER, ALI, CHART_ALI) |> 
  unique() |> 
  left_join(
    read.csv(here::here("Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_context_clinician_roadmap.csv")) |> 
      select(PAT_MRN_ID, SUPP_ALI) |> 
      rename(ALG_AUG_ALI = SUPP_ALI) |> 
      unique()
  ) 

# Create bar plot of missing values per component (colored by wave)
scatter_plot = all_data |> 
  pivot_longer(cols = CHART_ALI:ALG_AUG_ALI, names_to = "DATA", values_to = "VAL") |> 
  mutate(DATA = factor(x = DATA, 
                       levels = c("ALI", 
                                  "CHART_ALI", 
                                  "ALG_AUG_ALI"), 
                       labels = c("Unvalidated EHR Data", 
                                  "Chart Review Validation", 
                                  "Clinicians Reviewed LLMs with Context Roadmap (Augmented)"))) |> 
  ggplot(aes(x = ALI, 
             y = VAL, 
             shape = DATA, 
             color = DATA, 
             group = DATA)) + 
  geom_point(size = 2) + 
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "black") + 
  geom_smooth(se = FALSE) + 
  theme_minimal(base_size = 14) + 
  labs(x = "Unvalidated Allostatic Load Index", 
       y = "Validated/Augmented Allostatic Load Index ") + 
  scale_color_manual(values = cols[c(2, 5)], guide = FALSE) + 
  scale_shape_manual(values = c(17, 15), guide = FALSE) + 
  coord_equal() + 
  facet_wrap(~DATA) + 
  theme(title = element_text(face = "bold"), 
        legend.position = "inside", 
        legend.position.inside = c(0.05, 0.9),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.justification = "left", 
        legend.background = element_rect(fill = "white"), 
        strip.text = element_text(face = "bold", color = "white"), 
        strip.background = element_rect(fill = "black"))
scatter_plot

## Save it 
ggsave(filename = here::here("Documents/ehr-llm-validation/figures/compare_ali_validated_augmented.png"), 
       device = "png", width = 14, height = 7, units = "in")

box_plot = all_data |> 
  pivot_longer(cols = ALI:ALG_AUG_ALI, names_to = "DATA", values_to = "VAL") |> 
  mutate(DATA = factor(x = DATA, 
                       levels = c("ALI", 
                                  "CHART_ALI", 
                                  "ALG_AUG_ALI"), 
                       labels = c("Unvalidated EHR Data", 
                                  "Chart Review Validation", 
                                  "Clinicians Reviewed LLMs with\nContext Roadmap (Augmented)"))) |>
  filter(DATA != "Chart Review Validation") |> 
  ggplot(aes(x = DATA, 
             y = VAL, 
             fill = DATA)) + 
  geom_boxplot() + 
  theme_minimal(base_size = 14) + 
  labs(x = "Data Source", 
       y = "Allostatic Load Index ") + 
  scale_fill_manual(values = cols[c(2, 5)], guide = "none") + 
  theme(title = element_text(face = "bold"), 
        legend.position = "inside", 
        legend.position.inside = c(0.05, 0.9),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.justification = "left", 
        legend.background = element_rect(fill = "white")) 
box_plot
## Save it 
ggsave(filename = here::here("Documents/ehr-llm-validation/figures/compare_ali_validated_augmented_boxplot.png"), 
       device = "png", width = 7, height = 7, units = "in")
