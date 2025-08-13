library(janitor)
archive::archive_extract(
"https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/ICD10CM/2026/icd10cm-Code%20Descriptions-2026.zip",
files= "icd10cm-codes-2026.txt",dir = getwd())

icd_raw <- tibble::tibble(readr::read_lines("icd10cm-codes-2026.txt")) |>
clean_names()