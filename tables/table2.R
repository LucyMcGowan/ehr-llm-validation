# Load packages
library(dplyr) ## for data wrangling
library(tidyr) ## for filling in NAs after joining
library(tableone) ## for descriptive stats

# Read in data 
## Patient demographics
demos = read.csv("~/Documents/Allostatic_load_audits/Raw_Data/demo_2022-09-29.csv")
## Diagnosis codes 
dx = read.csv("~/Documents/Allostatic_load_audits/Raw_Data/dx_2022-09-29.csv") |> 
  group_by(PAT_MRN_ID) |> ### Group by patient ID
  summarize(NUM_DX = n_distinct(DX_CODE)) ### And count DX codes per patient
## Healthcare utilization
util = read.csv("~/Documents/Allostatic_load_audits/Raw_Data/Data-Request_934_ED_HOSP_VISITS_11_21_2023.csv") |>
  group_by(PAT_MRN_ID) |> ### Group by patient ID
  summarize(NUM_UTIL = n()) ### And count hospitalizations / ED visits per patient 
## Unvalidated ALI data 
unval = read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_original_roadmap.csv") |> 
  rename(UNVAL_ALI = ALI) |> 
  group_by(PAT_MRN_ID, UNVAL_ALI) |> 
  summarize(MISS_UNVAL = sum(is.na(ALI_COMPONENT)))

# Combine data 
all = demos |> 
  left_join(dx) |> ## Merge in count of diagnosis codes
  replace_na(replace = list(NUM_DX = 0)) |> ## If patient not present in diagnosis codes, make NUM_DX = 0
  left_join(util) |> 
  replace_na(replace = list(NUM_UTIL = 0)) |> ## If patient not present in healthcare utilization, make NUM_UTIL = 0
  mutate(ENGAGED = NUM_UTIL > 0) |> ## Create indicator of engaging in care
  left_join(unval)

# Define vector of IDs of chart review patients 
val_pat_id = read.csv("~/Documents/Allostatic_load_audits/all_ali_dat.csv") |> 
  filter(VALIDATED) |> 
  pull(PAT_MRN_ID)

# Create indicator of being validated
all = all |> 
  mutate(VALIDATED = PAT_MRN_ID %in% val_pat_id)

# Make Table 2 (but Table 1 style)
desc_tab = CreateTableOne(data = all, 
                          vars = c("AGE_AT_ENCOUNTER", 
                                   "SEX", "RACE", "ETHNICITY", 
                                   "UNVAL_ALI", "MISS_UNVAL",
                                   "NUM_DX", "NUM_UTIL", "ENGAGED"),
                          includeNA = TRUE, 
                          strata = "VALIDATED", 
                          addOverall = TRUE, 
                          test = FALSE)
print(desc_tab, nonnormal = c("AGE_AT_ENCOUNTER", 
                              "UNVAL_ALI", "MISS_UNVAL",
                              "NUM_DX", "NUM_UTIL"))
kableone(print(desc_tab, nonnormal = c("AGE_AT_ENCOUNTER", 
                                       "UNVAL_ALI", "MISS_UNVAL",
                                       "NUM_DX", "NUM_UTIL")), format = "latex")
