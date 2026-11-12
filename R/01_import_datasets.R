# ============================================================
# Project: Clinical TLF Programming in R
# Script: 01_import_datasets.R
# Purpose: Import SDTM and ADaM datasets from CSV files
# ============================================================

# 1. Load required packages

library(readr)
library(here)

# 2. Define project directory
project_dir <- "C:/Users/barsa/OneDrive/Documents/GIthub_Clinical/Clinical_TLF_Programming_in_R"

raw_dir <- file.path(project_dir, "data", "raw")


# 3. Define dataset files

dataset_files <- c(
  "ae.csv",
  "dm.csv",
  "lbch.csv",
  "adae.csv",
  "adlbc.csv",
  "adsl.csv"
)

# 4. Check file availability

missing_files <- dataset_files[
  !file.exists(file.path(raw_dir, dataset_files))
]

if (length(missing_files) > 0) {
  stop(
    "Missing datasets: ",
    paste(missing_files, collapse = ", ")
  )
}

# 5. Import datasets

datasets <- lapply(
  file.path(raw_dir, dataset_files),
  read_csv,
  show_col_types = FALSE
)

# 6. Assign dataset names

names(datasets) <- tools::file_path_sans_ext(dataset_files)

# 7. Separate SDTM and ADaM datasets

sdtm_data <- datasets[c("ae", "dm", "lbch")]

adam_data <- datasets[c("adae", "adlbc", "adsl")]

# 8. Check successful import

print(names(datasets))

# 9. Display dataset dimensions

lapply(datasets, dim)

# 10. Display variable names

lapply(datasets, names)