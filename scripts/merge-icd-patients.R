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

# Merge in the ICD mapping
icd_dx = read.csv("~/Documents/Allostatic_load_audits/ICD-Codes/dx_2022-09-29_w_icd.csv") |> 
  dplyr::select(PAT_MRN_ID, DX_CODE, DX_DESC, Variable_Name, matched_terms) |> 
  unique()

# Join ICD mapping into *all* EHR ALI Data 
ehr_ali_long = ehr_ali_long |> 
  left_join(icd_dx)

# Define ALI_COMPONENT = 1 if any matched_terms present
ehr_ali_long = ehr_ali_long |> 
  mutate(SUPP_ALI_COMPONENT = if_else(condition = is.na(ALI_COMPONENT) & !is.na(matched_terms), ## ALI component was missing and auxiliary information found 
                                      true = 1, ## goes from NA --> 1 (unhealthy)
                                      false = ALI_COMPONENT)) ## otherwise stays the same as in EHR

# Read in all-waves validation data 
chart_reviews = read.csv("~/Documents/Allostatic_load_audits/ali_dat_audit1.csv") |> 
  bind_rows(
    read.csv("~/Documents/Allostatic_load_audits/ali_dat_audit2.csv")
  )

## Make a long version so we can compare to ICD-supplemented 
chart_reviews_long = chart_reviews |> 
  pivot_longer(cols = A1C:TRIG, names_to = "COMPONENT", values_to = "Value")

