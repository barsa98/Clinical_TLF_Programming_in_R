
# ============================================================
# Project: Clinical TLF Programming in R
# Script: 03_population_analysis.R
# Purpose: Subject disposition summary by treatment group
# Source: ADaM ADSL
# ============================================================

# 1. Load required packages ----------------------------------

library(dplyr)
library(tidyr)

# 2. Define analysis dataset ---------------------------------

adsl <- adam_data$adsl

# Treatment group order
treatment_order <- c(
  "Placebo",
  "Xanomeline Low Dose",
  "Xanomeline High Dose"
)

adsl <- adsl %>%
  mutate(
    trt01p = factor(trt01p, levels = treatment_order)
  )

# 3. Randomized subjects -------------------------------------

randomized_summary <- adsl %>%
  group_by(trt01p) %>%
  summarise(
    N = n(),
    .groups = "drop"
  )

# 4. Week 24 treatment completion ----------------------------

week24_summary <- adsl %>%
  group_by(trt01p) %>%
  summarise(
    Total = n(),

    Completed_Week24 = sum(
      comp24fl == "Y",
      na.rm = TRUE
    ),

    Not_Completed_Week24 = sum(
      comp24fl != "Y" | is.na(comp24fl)
    ),

    .groups = "drop"
  ) %>%
  mutate(
    Completed_Pct = round(
      Completed_Week24 / Total * 100, 1
    ),

    Discontinued_Pct = round(
      Not_Completed_Week24 / Total * 100, 1
    )
  )

# 5. Reasons for discontinuation -----------------------------

disposition_summary <- adsl %>%
  group_by(trt01p, dcdecod) %>%
  summarise(
    Subjects = n(),
    .groups = "drop"
  ) %>%
  group_by(trt01p) %>%
  mutate(
    Percentage = round(
      Subjects / sum(Subjects) * 100, 1
    )
  ) %>%
  ungroup()

# 6. Adverse event discontinuation ---------------------------

ae_discontinuation <- adsl %>%
  group_by(trt01p) %>%
  summarise(
    Total = n(),

    AE_Discontinuation = sum(
      dcdecod == "ADVERSE EVENT",
      na.rm = TRUE
    ),

    AE_Discontinuation_Pct = round(
      AE_Discontinuation / Total * 100, 1
    ),

    .groups = "drop"
  )

# 7. Week 26 study completion --------------------------------

week26_summary <- adsl %>%
  group_by(trt01p) %>%
  summarise(
    Total = n(),

    Completed_Week26 = sum(
      dcdecod == "COMPLETED",
      na.rm = TRUE
    ),

    Completed_Week26_Pct = round(
      Completed_Week26 / Total * 100, 1
    ),

    .groups = "drop"
  )

# 8. Overall disposition summary -----------------------------

overall_disposition <- adsl %>%
  summarise(
    Randomized = n(),

    Completed_Week24 = sum(
      comp24fl == "Y",
      na.rm = TRUE
    ),

    Not_Completed_Week24 = sum(
      comp24fl != "Y" | is.na(comp24fl)
    ),

    Completed_Week26 = sum(
      dcdecod == "COMPLETED",
      na.rm = TRUE
    ),

    AE_Discontinuation = sum(
      dcdecod == "ADVERSE EVENT",
      na.rm = TRUE
    )
  )

# 9. Print results -------------------------------------------

cat("\n--- Randomized Subjects ---\n")
print(randomized_summary)

cat("\n--- Week 24 Treatment Completion ---\n")
print(week24_summary)

cat("\n--- Reasons for Discontinuation ---\n")
print(disposition_summary)

cat("\n--- Adverse Event Discontinuation ---\n")
print(ae_discontinuation)

cat("\n--- Week 26 Study Completion ---\n")
print(week26_summary)

cat("\n--- Overall Disposition ---\n")
print(overall_disposition)



# Check completion status against final disposition
adsl %>%
  count(
    trt01p,
    comp24fl,
    dcdecod,
    .drop = FALSE
  ) %>%
  filter(
    dcdecod == "ADVERSE EVENT" |
      is.na(comp24fl)
  ) %>%
  arrange(trt01p, comp24fl)


# AE disposition by treatment group and reason
adsl %>%
  filter(dcdecod == "ADVERSE EVENT") %>%
  count(trt01p, dcreascd, .drop = FALSE) %>%
  arrange(trt01p, dcreascd)


adsl %>%
  count(trt01p, comp24fl, .drop = FALSE)


# Recalculate AE discontinuation before Week 24

ae_discontinuation_week24 <- adsl %>%
  filter(
    dcdecod == "ADVERSE EVENT",
    comp24fl == "N"
  ) %>%
  group_by(trt01p) %>%
  summarise(
    AE_Discontinuation = n(),
    .groups = "drop"
  ) %>%
  right_join(
    adsl %>%
      group_by(trt01p) %>%
      summarise(Total = n(), .groups = "drop"),
    by = "trt01p"
  ) %>%
  mutate(
    AE_Discontinuation = coalesce(AE_Discontinuation, 0L),
    Percentage = round(
      AE_Discontinuation / Total * 100, 1
    )
  )

ae_discontinuation_week24


adsl %>%
  filter(
    trt01p == "Xanomeline High Dose",
    comp24fl == "Y",
    dcdecod == "ADVERSE EVENT"
  ) %>%
  select(
    usubjid,
    trt01p,
    comp24fl,
    dcdecod,
    dcreascd,
    trtsdt,
    trtedt
  )



# ============================================================
# SECTION: EXPORT T01 - SUBJECT DISPOSITION AND POPULATIONS
# ============================================================

# Create output directory
output_dir <- here::here("output", "tables")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Collect validated T01 summary objects
t01_outputs <- list(
  Randomized_Summary = randomized_summary,
  Week24_Completion = week24_summary,
  Disposition_By_Reason = disposition_summary,
  AE_Discontinuation = ae_discontinuation,
  Week26_Completion = week26_summary,
  Overall_Disposition = overall_disposition
)


# ------------------------------------------------------------
# Export individual CSV files
# ------------------------------------------------------------

purrr::iwalk(
  t01_outputs,
  function(data, name) {
    readr::write_csv(
      data,
      file.path(
        output_dir,
        paste0("T01_", name, ".csv")
      )
    )
  }
)


# ------------------------------------------------------------
# Export all summaries into one Excel workbook
# ------------------------------------------------------------

writexl::write_xlsx(
  t01_outputs,
  file.path(
    output_dir,
    "T01_Subject_Disposition_and_Populations.xlsx"
  )
)


# ------------------------------------------------------------
# Confirm exported files
# ------------------------------------------------------------

list.files(output_dir, pattern = "^T01_")

