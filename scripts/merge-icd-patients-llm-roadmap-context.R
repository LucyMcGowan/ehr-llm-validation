# Load packages
library(tidyr) ## for pivots 
library(dplyr) ## for data wrangling

# Start with the EHR ALI Data 
ehr_ali = read.csv("~/Documents/Allostatic_load_audits/ali_dat.csv") 

## Make a long version for merging with ICD and comparison to chart reviews
ehr_ali_long = ehr_ali |> 
  pivot_longer(cols = A1C:TRIG, 
               names_to = "Variable_Name", 
               values_to = "ALI_COMPONENT")
nrow(ehr_ali_long) ## Check: 10,000 rows (one per patient per component)

# Merge in the ICD mapping
icd_dx = read.csv("data-raw/patient_data/roadmap_llm_context_validated.csv") |> 
  dplyr::select(PAT_MRN_ID, DX_CODE, DX_DESC, Variable_Name, matched_terms) |> 
  unique() |> 
  group_by(PAT_MRN_ID, Variable_Name) |> ## Some people had multiple ICD codes per component
  summarize(DX_CODE = paste(DX_CODE, collapse = "+"), ## Combine the DX codes...
            DX_DESC = paste(DX_DESC, collapse = ";"), ## .. and DX descriptions ... 
            matched_terms = paste(matched_terms, collapse = ";")) ## and matched terms. 

# Join ICD mapping into *all* EHR ALI Data 
ehr_ali_long = ehr_ali_long |> 
  left_join(icd_dx)
nrow(ehr_ali_long) ## Notice: Still 10,000 rows after join! One per patient per component. 

# Define ALI_COMPONENT = 1 if any matched_terms present
ehr_ali_long = ehr_ali_long |> 
  mutate(SUPP_ALI_COMPONENT = if_else(condition = is.na(ALI_COMPONENT) & !is.na(matched_terms), ## ALI component was missing and auxiliary information found 
                                      true = 1, ## goes from NA --> 1 (unhealthy)
                                      false = ALI_COMPONENT)) |> ## otherwise stays the same as in EHR
  group_by(PAT_MRN_ID) |> ## group by patient 
  mutate(SUPP_ALI_NUM = sum(SUPP_ALI_COMPONENT, na.rm = TRUE), ## then re-calculate ALI numerator...  
         SUPP_ALI_DENOM = sum(!is.na(SUPP_ALI_COMPONENT)), ## ... denominator ...  
         SUPP_ALI = SUPP_ALI_NUM / SUPP_ALI_DENOM) ## ... and final score using the ICD-supplemented components.

# Read in all-waves validation data 
chart_reviews = read.csv("~/Documents/Allostatic_load_audits/ali_dat_audit1.csv") |> 
  bind_rows(
    read.csv("~/Documents/Allostatic_load_audits/ali_dat_audit2.csv")
  )

## Make a long version so we can compare to ICD-supplemented 
chart_reviews_long = chart_reviews |> 
  pivot_longer(cols = A1C:TRIG, names_to = "COMPONENT", values_to = "Value") |> 
  rename(CHART_ALI_NUM = ALI_NUM, 
         CHART_ALI_DENOM = ALI_DENOM, 
         CHART_ALI = ALI, 
         CHART_ALI_COMPONENT = Value, 
         Variable_Name = COMPONENT)
nrow(chart_reviews_long) ## Check: 1000 rows (one per audited patient per component)

## Merge EHR, EHR + ICD, and Chart Reviews (long)
all_dat = ehr_ali_long |> 
  left_join(chart_reviews_long)
nrow(ehr_ali_long) ## Notice: Still 10,000 rows after join! One per patient per component. 

all_dat |> 
  write.csv("data-raw/patient_data/ali_dat_llm_context_validated.csv", 
            row.names = FALSE)
