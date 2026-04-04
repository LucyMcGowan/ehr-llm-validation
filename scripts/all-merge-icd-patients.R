# Merge original roadmap into EHR + chart reviews 
source("~/Documents/ehr-llm-validation/scripts/merge-icd-patients-orig-roadmap.R")
## --> ali_dat_original_roadmap.csv

# Merge LLMs (no context) roadmap into EHR + chart reviews 
source("~/Documents/ehr-llm-validation/scripts/merge-icd-patients-llm-roadmap-no-context.R")
## --> ali_dat_llm_nocontext_roadmap.csv

# Merge LLMs (no context, for loop + ICD-10) roadmap into EHR + chart reviews 
source("~/Documents/ehr-llm-validation/scripts/merge-icd-patients-llm-roadmap-no-context-loop-icd10.R")
## --> ali_dat_llm_nocontext_loop_icd10_roadmap.csv

# Merge LLMs (context) roadmap into EHR + chart reviews 
source("~/Documents/ehr-llm-validation/scripts/merge-icd-patients-llm-roadmap-context.R")
## --> ali_dat_llm_context_roadmap.csv

# Merge LLMs (context, for loop + ICD-10) roadmap into EHR + chart reviews 
source("~/Documents/ehr-llm-validation/scripts/merge-icd-patients-llm-roadmap-context-loop-icd10.R")
## --> ali_dat_llm_context_loop_icd10_roadmap.csv

# Merge LLMs (context) roadmap into EHR + chart reviews 
source("~/Documents/ehr-llm-validation/scripts/merge-icd-patients-llm-roadmap-context-clinician.R")
## --> ali_dat_llm_context_clinician_roadmap.csv

# Merge LLMs (context, for loop + ICD-10) roadmap into EHR + chart reviews 
source("~/Documents/ehr-llm-validation/scripts/merge-icd-patients-llm-roadmap-context-loop-icd10-clinician.R")
## --> ali_dat_llm_context_loop_icd10_clinician_roadmap.csv