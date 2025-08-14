# //////////////////////////////////////////////////////////////////////
# Replicate Figure XX //////////////////////////////////////////////////
# Caption begins "XXX ..." /////////////////////////////////////////////
# //////////////////////////////////////////////////////////////////////

# Load packages
library(dplyr) ## To wrangle data
library(tidyr) ## To transform data
library(ggplot2) ## To create plots
library(ggtext) ## To italicize part of titles/labels
library(stringr) ## To wrap plot titles/text

# Define colors
paper_colors = c("#ff99ff", "#787ff6", "#8bdddb", "#7dd5f6", "#ffbd59")

# Load data
## ALI components before and after validation (waves separately)
all_data = read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_nocontext_validated.csv") |> 
  select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT) 

## Merge validated + unvalidated 
all_data = all_data |> 
  mutate(SUPP_ALI_COMPONENT = factor(x = SUPP_ALI_COMPONENT, 
                                     levels = c(1, 0, NA), 
                                     labels = c("Yes", "No", "Missing"), exclude = NULL),
         CHART_ALI_COMPONENT = factor(x = CHART_ALI_COMPONENT, 
                                      levels = c(1, 0, NA), 
                                      labels = c("Yes", "No", "Missing"), exclude = NULL), 
         ALI_COMPONENT = factor(x = ALI_COMPONENT, 
                                levels = c(1, 0, NA), 
                                labels = c("Yes", "No", "Missing"), exclude = NULL))

## Calculate error rates and data recovery (EHR vs. ICD-Supplemented)
### True positive rate (TPR) = P(ALI_COMPONENT = 1 | CHART_ALI_COMPONENT = 1)
### False positive rate (FPR) = P(ALI_COMPONENT = 1 | CHART_ALI_COMPONENT = 0)
all_data |> 
  summarize(TPR = sum(ALI_COMPONENT == "Yes" & SUPP_ALI_COMPONENT == "Yes") / sum(SUPP_ALI_COMPONENT == "Yes" & ALI_COMPONENT != "Missing"), 
            FPR = sum(ALI_COMPONENT == "Yes" & SUPP_ALI_COMPONENT == "Yes") / sum(SUPP_ALI_COMPONENT == "Yes" & ALI_COMPONENT != "Missing"), 
            Recovery = sum(ALI_COMPONENT == "Missing" & SUPP_ALI_COMPONENT != "Missing") / sum(ALI_COMPONENT == "Missing"))

## Plot boxplot of coefficient estimates
all_combo = expand.grid(SUPP_ALI_COMPONENT = c("Yes", "No", "Missing"), 
                        ALI_COMPONENT = c("Yes", "No", "Missing"))
all_data |> 
  group_by(SUPP_ALI_COMPONENT, ALI_COMPONENT) |> 
  summarize(num = n()) |>
  full_join(all_combo) |> 
  mutate(num = if_else(condition = is.na(num),
                       true = 0, 
                       false = num)) |> 
  ggplot(aes(x = ALI_COMPONENT, y = SUPP_ALI_COMPONENT, fill = num)) +
  geom_tile(color = "black",
            lwd = 0.5,
            linetype = 1) + 
  geom_text(aes(label = num), 
            color = "black", 
            size = 5) + 
  scale_fill_gradientn(colors = paper_colors, 
                       name = "Data\nPoints:", 
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black", 
                                              barwidth = 1, 
                                              barheight = 10)) + 
  theme_minimal(base_size = 14) +
  theme(legend.position = "right",
        title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.text = element_markdown(),
        legend.title = element_text(face = "bold"), 
        strip.background = element_rect(fill = "black"), 
        strip.text = element_text(face = "bold", color = "white")) + 
  labs(x = "Unvalidated Allostatic Load Index\nComponent (from the EHR)",
       y = "Validated Allostatic Load Index\nComponent (from Chart Review)") 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/llm_nocontext_vs_ehr_heatmap.png",
       device = "png", width = 8, height = 4, units = "in")

## Calculate error rates and data recovery (EHR + ICD vs. Chart Review)
### True positive rate (TPR) = P(ALI_COMPONENT = 1 | CHART_ALI_COMPONENT = 1)
### False positive rate (FPR) = P(ALI_COMPONENT = 1 | CHART_ALI_COMPONENT = 0)
all_data |> 
  summarize(TPR = sum(SUPP_ALI_COMPONENT == "Yes" & CHART_ALI_COMPONENT == "Yes") / sum(CHART_ALI_COMPONENT == "Yes" & SUPP_ALI_COMPONENT != "Missing"), 
            FPR = sum(SUPP_ALI_COMPONENT == "Yes" & CHART_ALI_COMPONENT == "Yes") / sum(CHART_ALI_COMPONENT == "Yes" & SUPP_ALI_COMPONENT != "Missing"), 
            Recovery = sum(SUPP_ALI_COMPONENT == "Missing" & CHART_ALI_COMPONENT != "Missing") / sum(SUPP_ALI_COMPONENT == "Missing"))

## Plot boxplot of coefficient estimates
all_combo = expand.grid(CHART_ALI_COMPONENT = c("Yes", "No", "Missing"), 
                        SUPP_ALI_COMPONENT = c("Yes", "No", "Missing"))
all_data |> 
  group_by(CHART_ALI_COMPONENT, SUPP_ALI_COMPONENT) |> 
  summarize(num = n()) |>
  full_join(all_combo) |> 
  mutate(num = if_else(condition = is.na(num),
                       true = 0, 
                       false = num)) |> 
  ggplot(aes(x = SUPP_ALI_COMPONENT, y = CHART_ALI_COMPONENT, fill = num)) +
  geom_tile(color = "black",
            lwd = 0.5,
            linetype = 1) + 
  geom_text(aes(label = num), 
            color = "black", 
            size = 5) + 
  scale_fill_gradientn(colors = paper_colors, 
                       name = "Data\nPoints:", 
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "black", 
                                              barwidth = 1, 
                                              barheight = 10)) + 
  theme_minimal(base_size = 14) +
  theme(legend.position = "right",
        title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"), 
        strip.background = element_rect(fill = "black"), 
        strip.text = element_text(face = "bold", color = "white")) + 
  labs(x = "ICD-Supplemented Allostatic Load Index\nComponent (from the EHR)",
       y = "Validated Allostatic Load Index\nComponent (from Chart Review)") 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/chart_vs_llm_nocontext_heatmap.png",
       device = "png", width = 8, height = 4, units = "in")
