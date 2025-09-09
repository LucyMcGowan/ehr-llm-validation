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
all_data = read.csv(here::here("data-raw/patient_data/ali_dat_original_roadmap.csv")) |> 
  mutate(CHART_ALI_COMPONENT = if_else(condition = !is.na(ALI_COMPONENT) & is.na(CHART_ALI_COMPONENT), 
                                       true = ALI_COMPONENT, 
                                       false = CHART_ALI_COMPONENT)) |> 
  select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT) |> 
  rename(ORIG_ALI_COMPONENT = SUPP_ALI_COMPONENT) |> 
  left_join(
    read.csv(here::here("data-raw/patient_data/ali_dat_llm_nocontext_roadmap.csv")) |> 
      mutate(CHART_ALI_COMPONENT = if_else(condition = !is.na(ALI_COMPONENT) & is.na(CHART_ALI_COMPONENT), 
                                           true = ALI_COMPONENT, 
                                           false = CHART_ALI_COMPONENT)) |> 
      select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT) |> 
      rename(LLM_ALI_COMPONENT = SUPP_ALI_COMPONENT)
  ) |> 
  left_join(
    read.csv(here::here("data-raw/patient_data/ali_dat_llm_context_clinician_roadmap.csv")) |> 
      mutate(CHART_ALI_COMPONENT = if_else(condition = !is.na(ALI_COMPONENT) & is.na(CHART_ALI_COMPONENT), 
                                           true = ALI_COMPONENT, 
                                           false = CHART_ALI_COMPONENT)) |> 
      select(PAT_MRN_ID, Variable_Name, ALI_COMPONENT, SUPP_ALI_COMPONENT, CHART_ALI_COMPONENT) |> 
      rename(LLM_CONTEXT_CLINICIAN_ALI_COMPONENT = SUPP_ALI_COMPONENT)
  ) |> 
  left_join(
    read.csv(here::here("data-raw/patient_data/ali_dat_llm_context_roadmap.csv")) |> 
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
  group_by(PAT_MRN_ID, DATA) |> 
  summarize(NUM_MISSING = sum(is.na(COMPONENT)))

# Create bar plot of missing values per component (colored by wave)
bar_plot = num_miss |> 
  mutate(DATA = factor(x = DATA, 
                       levels = c("ALI_COMPONENT", 
                                  "CHART_ALI_COMPONENT", 
                                  "LLM_ALI_COMPONENT",
                                  "ORIG_ALI_COMPONENT", 
                                  "LLM_CONTEXT_CLINICIAN_ALI_COMPONENT",
                                  "LLM_CONTEXT_ALI_COMPONENT"), 
                       labels = c("Unvalidated EHR Data", 
                                  "Chart Review Validation", 
                                  "LLMs without Context Roadmap (Augmented)",
                                  "Clinicians' Original Roadmap (Augmented)", 
                                  "Clinicians Reviewed LLMs with Context Roadmap (Augmented)", 
                                  "LLMs with Context Roadmap (Augmented)"))) |> 
  group_by(DATA, NUM_MISSING) |> 
  summarize(n = n()) |> 
  ggplot(aes(x = NUM_MISSING, 
             y = n, 
             fill = DATA, 
             group = DATA)) + 
  geom_bar(stat = "identity", 
           position = "dodge", 
           color = "black") + 
  geom_text(aes(label=n), 
            vjust = -1, 
            size = 3, 
            position = position_dodge(width = 1)) + 
  theme_minimal(base_size = 14) + 
  labs(x = "Number of Missing Allostatic Load Index Components", 
       y = "Number of Patients") + #, 
       #title = "Number of Missing ALI Components by Patient") +
  theme(title = element_text(face = "bold"), 
        legend.position = "inside", 
        legend.position.inside = c(1, 0.8),
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.justification = "right", 
        legend.background = element_rect(fill = "white")) + 
  scale_fill_manual(values = cols, name = "Data:", 
                    labels = function(x) str_wrap(x, width = 60)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  scale_x_continuous(breaks = 0:10)
bar_plot

## Save it 
ggsave(filename = here::here("figures/missing_by_patient.png"), 
       device = "png", width = 14, height = 7, units = "in")

## Calculate median non-missing 
med_nonmiss = num_miss |> 
  group_by(DATA) |> 
  summarize(median_miss = median(NUM_MISSING), 
            q1_nonmiss = quantile(x = (10 - NUM_MISSING), 0.25),
            median_nonmiss = median(10 - NUM_MISSING), 
            q3_nonmiss = quantile(x = (10 - NUM_MISSING), 0.75))

bar_plot + 
  facet_wrap(~DATA) + 
  # geom_text(data = med_nonmiss, aes(label = median_nonmiss),
  #           x = Inf, y = Inf, hjust = 1, vjust = 0) + 
  theme(strip.background = element_rect(fill = "black"), 
        strip.text = element_text(color = "white", face = "bold")) + 
  scale_fill_manual(values = cols, guide = "none") 

## Save it 
ggsave(filename = here::here("figures/missing_by_patient_faceted.png"), 
       device = "png", width = 10, height = 6, units = "in")

# Create bar plot of missing values per component (colored by wave)
## But include chart review next to each one 
# num_miss |> 
#   filter(DATA != "CHART_ALI_COMPONENT") |> 
#   mutate(FAC_DATA = DATA) |> 
#   bind_rows(
#     num_miss |> 
#       filter(DATA == "CHART_ALI_COMPONENT") |> 
#       mutate(FAC_DATA = "ALI_COMPONENT")
#   ) |> 
#   bind_rows(
#     num_miss |> 
#       filter(DATA == "CHART_ALI_COMPONENT") |> 
#       mutate(FAC_DATA = "ORIG_ALI_COMPONENT")
#   ) |> 
#   bind_rows(
#     num_miss |> 
#       filter(DATA == "CHART_ALI_COMPONENT") |> 
#       mutate(FAC_DATA = "LLM_ALI_COMPONENT")
#   ) |> 
#   bind_rows(
#     num_miss |> 
#       filter(DATA == "CHART_ALI_COMPONENT") |> 
#       mutate(FAC_DATA = "LLM_CONTEXT_ALI_COMPONENT")
#   ) |> 
#   mutate(DATA = factor(x = DATA, 
#                        levels = c("CHART_ALI_COMPONENT", 
#                                   "ALI_COMPONENT", 
#                                   "ORIG_ALI_COMPONENT", 
#                                   "LLM_ALI_COMPONENT", 
#                                   "LLM_CONTEXT_ALI_COMPONENT"), 
#                        labels = c("Chart Review Validation", 
#                                   "Unvalidated EHR Data", 
#                                   "Augmented (Original Roadmap)", 
#                                   "LLMs without Context Roadmap (Augmented)",
#                                   "LLMs with Context Roadmap (Augmented)")), 
#          FAC_DATA = factor(x = FAC_DATA, 
#                        levels = c("ALI_COMPONENT", 
#                                   "CHART_ALI_COMPONENT", 
#                                   "ORIG_ALI_COMPONENT", 
#                                   "LLM_ALI_COMPONENT", 
#                                   "LLM_CONTEXT_ALI_COMPONENT"), 
#                        labels = c("Unvalidated EHR Data", 
#                                   "Chart Review Validation", 
#                                   "Augmented (Original Roadmap)", 
#                                   "LLMs without Context Roadmap (Augmented)",
#                                   "LLMs with Context Roadmap (Augmented)")), 
#          CHART_REVIEW = factor(x = DATA == "Chart Review Validation", 
#                                levels = c(TRUE, FALSE), 
#                                labels = c("Chart Review", "Not Chart Review"))) |> 
#   group_by(CHART_REVIEW, FAC_DATA, DATA, NUM_MISSING) |> 
#   summarize(n = n()) |> 
#   ggplot(aes(x = NUM_MISSING, 
#              y = n, 
#              fill = DATA, 
#              pattern = CHART_REVIEW)) + 
#   geom_bar(stat = "identity", 
#            position = "dodge", 
#            color = "black") + 
#   geom_text(aes(label=n), 
#             vjust = -1, 
#             size = 3, 
#             position = position_dodge(width = 1)) + 
#   geom_bar_pattern(stat = "identity", 
#                    position = position_dodge(preserve = "single"),
#                    color = "black", 
#                    pattern_fill = "black",
#                    pattern_angle = 45,
#                    pattern_density = 0.1,
#                    pattern_spacing = 0.025,
#                    pattern_key_scale_factor = 0.6) + 
#   facet_wrap(~FAC_DATA) + 
#   scale_fill_manual(values = cols, guide = "none") + 
#   theme_minimal(base_size = 14) + 
#   labs(x = "Number of Missing Allostatic Load Index Components", 
#        y = "Number of Patients", 
#        title = "Number of Missing ALI Components by Patient") +
#   theme(title = element_text(face = "bold"), 
#         legend.position = "inside", 
#         #legend.position.inside = c(1, 0.9),
#         legend.text = element_text(size = 12), 
#         legend.title = element_text(size = 12, face = "bold"), 
#         legend.justification = "right", 
#         legend.background = element_rect(fill = "white"), 
#         strip.background = element_rect(fill = "black"), 
#         strip.text = element_text(color = "white", face = "bold")) + 
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
#   scale_x_continuous(breaks = 0:10) + 
#   scale_pattern_discrete(guide = "none")
# 
# ## Save it 
# ggsave(filename = here::here("figures/missing_by_patient_faceted_sidebyside_chartreview.png"), 
#        device = "png", width = 10, height = 6, units = "in")
