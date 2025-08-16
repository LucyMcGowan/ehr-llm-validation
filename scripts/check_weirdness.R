library(dplyr)

# Define vector of IDs of chart review patients 
val_pat_id = read.csv("~/Documents/Allostatic_load_audits/all_ali_dat.csv") |> 
  filter(VALIDATED) |> 
  pull(PAT_MRN_ID)

temp = read.csv("~/Documents/ehr-llm-validation/data-raw/patient_data/ali_dat_llm_nocontext_roadmap.csv") |> 
  filter(PAT_MRN_ID %in% val_pat_id)

## Check: There are 13 components that chart review recovered but the roadmap didn't
temp |> 
  filter(CHART_ALI_COMPONENT == 1, is.na(SUPP_ALI_COMPONENT))
### I looked at PAT_MRN_ID = 700088. For creatinine clearance, the auditors just found missing labs. (Nothing the roadmap can do.)
#### For homocysteine, auditors noted "folate deficiency, Vit D deficiency." These things are not in the diagnosis codes either. 
### Looking at PAT_MRN_ID = 2340961, auditors noted quite a few "acute kidney injuries." I don't see this in diagnosis codes! 

## Check: There are 16 components that the roadmap recovered but chart review didn't
temp |> 
  filter(is.na(CHART_ALI_COMPONENT), SUPP_ALI_COMPONENT == 1)
### But... 5 of these are because roadmap inherited the values from the EHR, which didn't agree with chart review 
temp |> 
  filter(is.na(CHART_ALI_COMPONENT), SUPP_ALI_COMPONENT == 1, is.na(ALI_COMPONENT))
### I looked at PAT_MRN_ID = 1712690. Seems like they were diagnosed with vitamin deficiency, but for some reason the auditors just marked them dead? 
### I also checked PAT_MRN_ID == 479949. All of their labs were missing (must've been new to the healthcare system). 
#### They were diagnosed with "essential (primary) hypertension." I think instead of looking for the diagnoses, 
#### the auditors recorded measurements from 06/2020, which then get thrown out because they're outside the study period.

with(temp, table(CHART_ALI_COMPONENT, SUPP_ALI_COMPONENT)) ## Check: All four cells agree with heatmap
with(temp, table(is.na(CHART_ALI_COMPONENT), is.na(SUPP_ALI_COMPONENT))) ## Check: All four cells agree with heatmap



temp |> 
  filter(is.na(CHART_ALI_COMPONENT) & is.na(SUPP_ALI_COMPONENT))
