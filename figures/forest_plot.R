# Load packages 
library(tidyr) ## to unpivot data
library(dplyr) ## for data wrangling
library(ggplot2) ## for pretty plots

# Define colors 
cols = c("#ff99ff", "#8bdddb", "#787ff6", "#ffbd59", "#7dd5f6", "#ff884d")

# Load data
## ALI components before and after validation (waves separately)
all_data = read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_original_roadmap.csv") |> 
  select(PAT_MRN_ID, ANY_ENCOUNTERS, AGE_AT_ENCOUNTER, ALI, CHART_ALI) |> 
  unique() |> 
  left_join(
    read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_context_clinician_roadmap.csv") |> 
      select(PAT_MRN_ID, SUPP_ALI) |> 
      rename(ALG_AUG_ALI = SUPP_ALI) |> 
      unique()
  ) 

# Reschedule ALI and AGE_AT_FIRST_ENCOUNTER 
all_data = all_data |> 
  mutate(AGE_AT_ENCOUNTER = (AGE_AT_ENCOUNTER - 18) / 10, 
         ALI = ALI / 0.1, 
         CHART_ALI = CHART_ALI / 0.1, 
         ALG_AUG_ALI = ALG_AUG_ALI / 0.1, 
         VAL = !is.na(CHART_ALI))

# Fit naive model 
naive_mod = glm(formula = ANY_ENCOUNTERS ~ ALI + AGE_AT_ENCOUNTER, 
                family = "binomial", 
                data = all_data)
res = coefficients(summary(naive_mod)) |> 
  bind_cols(confint(naive_mod) |> 
              data.frame() |> 
              rename(lb = X2.5.., ub = X97.5..)) |> 
  mutate(coeff = c("intercept", "ali", "age"), 
         model = "naive")

# Fit augmented model 
aug_mod = glm(formula = ANY_ENCOUNTERS ~ ALG_AUG_ALI + AGE_AT_ENCOUNTER, 
              family = "binomial", 
              data = all_data)
res = coefficients(summary(aug_mod)) |> 
  bind_cols(confint(aug_mod) |> 
              data.frame() |> 
              rename(lb = X2.5.., ub = X97.5..) ) |> 
  mutate(coeff = c("intercept", "ali", "age"), 
         model = "augmented") |> 
  bind_rows(res) |> 
  select(1, 5:8)

# Add the SMLEs 
res = res |> 
  rbind(data.frame(Estimate = log(c(0.23, 1.12, 1.11)), 
                   lb = log(c(0.16, 1.09, 1.02)), 
                   ub = log(c(0.33, 1.15, 1.2)), 
                   coeff  = c("intercept", "ali", "age"),
                   model  = "previous"))

# Make forest plot 
colnames(res) = c("Estimate", "LB", "UB", "Coefficient", "Model")
res |> 
  mutate(Model = factor(x = Model, 
                        levels = c("naive", "previous", "augmented"), 
                        labels = c("Extracted EHR Data", 
                                   "Extracted EHR Data + Expert Chart Reviews", 
                                   "Algorithm w/ LLM (Context + Clinicians)")), 
         Coefficient = factor(x = Coefficient, 
                              levels = c("intercept", "ali", "age"), 
                              labels = c("Intercept (Baseline Odds)", 
                                         "Allostatic Load Index", 
                                         "Age at First Encounter"))) |> 
  ggplot(aes(x = Model, y = exp(Estimate), color = Model)) + 
  geom_hline(data = data.frame(Coefficient = factor(x = c("Intercept (Baseline Odds)", 
                                                          "Allostatic Load Index", 
                                                          "Age at First Encounter"), 
                                                    levels = c("Intercept (Baseline Odds)", 
                                                               "Allostatic Load Index", 
                                                               "Age at First Encounter"), 
                                                    labels = c("Intercept (Baseline Odds)", 
                                                               "Allostatic Load Index", 
                                                               "Age at First Encounter")), 
                               hline = c(NA, 1, 1)), 
             aes(yintercept = hline), linetype = 2, colour = "black") + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = exp(LB), ymax = exp(UB)), 
                lwd = 1.2) + 
  facet_wrap(~Coefficient, scales = "free") + 
  theme_minimal(base_size = 20) + 
  labs(y = "Expected Odds/Odds Ratio") + 
  theme(title = element_text(face = "bold"), 
        legend.position = "right", 
        legend.title = element_text(face = "bold"), 
        legend.background = element_rect(fill = "white"), 
        strip.text = element_text(face = "bold", color = "white"), 
        strip.background = element_rect(fill = "black"), 
        axis.text.x = element_blank()) + 
  scale_color_manual(values = c(cols[2], "lightgrey", cols[5]), 
                     name = "Data Source:", 
                     labels = function(x) stringr::str_wrap(x, width = 12))
## Save it 
ggsave(filename = "~/Documents/ehr-llm-validation/figures/forest_plot_full_sample.png", 
       device = "png", width = 14, height = 7, units = "in")  
