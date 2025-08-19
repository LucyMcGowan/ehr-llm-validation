# Merge original roadmap into EHR + chart reviews 
source("~/Documents/ehr-llm-validation/scripts/merge-icd-patients-orig-roadmap.R")
## --> ali_dat_original_roadmap.csv

# Merge LLMs (no context) roadmap into EHR + chart reviews 
source("~/Documents/ehr-llm-validation/scripts/merge-icd-patients-llm-roadmap-no-context.R")
## --> ali_dat_llm_nocontext_roadmap.csv

# Merge LLMs (context) roadmap into EHR + chart reviews 
source("~/Documents/ehr-llm-validation/scripts/merge-icd-patients-llm-roadmap-context.R")
## --> ali_dat_llm_context_roadmap.csv