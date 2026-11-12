
# ============================================================
# T06-T07: LABORATORY DATA EXPLORATION
# ============================================================

adlbc <- adam_data[["adlbc"]]

dim(adlbc)


# 1. Dataset dimensions and structure
dim(adlbc)
str(adlbc)

# 2. Important variable names
names(adlbc) %>%
  grep(
    "USUBJID|TRTA|TRTP|PARAM|AVAL|BASE|CHG|AVISIT|ABLFL|ANL",
    .,
    value = TRUE
  )

# 3. Treatment groups
adlbc %>%
  count(trta, trtp)

# 4. Laboratory parameters
adlbc %>%
  distinct(paramcd, param) %>%
  arrange(paramcd)

# 5. Analysis visits
adlbc %>%
  distinct(avisitn, avisit) %>%
  arrange(avisitn)

# 6. Analysis flags
adlbc %>%
  summarise(
    Total_records = n(),
    Baseline_records = sum(ablfl == "Y", na.rm = TRUE),
    ANL01FL_records = sum(anl01fl == "Y", na.rm = TRUE),
    Missing_AVAL = sum(is.na(aval)),
    Missing_BASE = sum(is.na(base)),
    Missing_CHG = sum(is.na(chg))
  )

# 7. Records by parameter and visit
adlbc %>%
  count(paramcd, param, avisitn, avisit) %>%
  arrange(paramcd, avisitn)

# 8. Check duplicate subject-parameter-visit records
adlbc %>%
  count(usubjid, paramcd, avisit) %>%
  filter(n > 1) %>%
  arrange(paramcd, avisit, usubjid)

# 9. Check available treatment groups and subjects
adlbc %>%
  summarise(
    Unique_subjects = n_distinct(usubjid),
    Unique_parameters = n_distinct(paramcd),
    Unique_visits = n_distinct(avisit)
  )




# ============================================================
# T06-T07: QUALITY CONTROL
# ============================================================

# 1. Check number of records by treatment and visit
adlbc %>%
  count(trta, avisit) %>%
  arrange(trta, avisit)

# 2. Check baseline records
adlbc %>%
  filter(ablfl == "Y") %>%
  count(trta, paramcd, param)

# 3. Check analysis records
adlbc %>%
  filter(anl01fl == "Y") %>%
  count(trta, paramcd, param, avisit) %>%
  arrange(paramcd, avisit)

# 4. Check missing analysis values
adlbc %>%
  filter(anl01fl == "Y") %>%
  summarise(
    Missing_AVAL = sum(is.na(aval)),
    Missing_BASE = sum(is.na(base)),
    Missing_CHG = sum(is.na(chg))
  )

# 5. Check baseline change calculation
adlbc %>%
  filter(anl01fl == "Y", !is.na(aval), !is.na(base), !is.na(chg)) %>%
  summarise(
    Records_checked = n(),
    Mismatches = sum(abs(chg - (aval - base)) > 0.0001)
  )





# ============================================================
# INVESTIGATE ADLBC MISSING BASE/CHG AND VISIT LABELS
# ============================================================

# 1. Check missing BASE and CHG by visit
adlbc %>%
  filter(anl01fl == "Y") %>%
  group_by(avisit, ablfl) %>%
  summarise(
    Records = n(),
    Missing_BASE = sum(is.na(base)),
    Missing_CHG = sum(is.na(chg)),
    .groups = "drop"
  ) %>%
  arrange(avisit, ablfl) %>%
  print(n = Inf)

# 2. Check baseline records separately
adlbc %>%
  filter(anl01fl == "Y", ablfl == "Y") %>%
  summarise(
    Records = n(),
    Missing_AVAL = sum(is.na(aval)),
    Missing_BASE = sum(is.na(base)),
    Missing_CHG = sum(is.na(chg))
  )

# 3. Check non-baseline records separately
adlbc %>%
  filter(anl01fl == "Y", ablfl != "Y") %>%
  summarise(
    Records = n(),
    Missing_AVAL = sum(is.na(aval)),
    Missing_BASE = sum(is.na(base)),
    Missing_CHG = sum(is.na(chg))
  )

# 4. Investigate records with AVISIT = "."
adlbc %>%
  filter(avisit == ".") %>%
  count(trta, paramcd, param, ablfl, anl01fl) %>%
  arrange(paramcd) %>%
  print(n = 30)

# 5. Inspect examples of records with missing BASE or CHG
adlbc %>%
  filter(
    anl01fl == "Y",
    is.na(base) | is.na(chg)
  ) %>%
  select(
    usubjid, trta, paramcd, param,
    avisit, avisitn, ablfl, anl01fl,
    aval, base, chg
  ) %>%
  head(15)






# ============================================================
# ADLBC: ANALYSIS FLAG INVESTIGATION
# ============================================================

# 1. Distribution of analysis and baseline flags
adlbc %>%
  count(avisit, ablfl, anl01fl) %>%
  arrange(avisit, ablfl, anl01fl) %>%
  print(n = Inf)

# 2. Inspect baseline-flagged records
adlbc %>%
  filter(ablfl == "Y") %>%
  count(avisit, anl01fl)

# 3. Check records with missing BASE by parameter
adlbc %>%
  filter(anl01fl == "Y", is.na(base)) %>%
  count(paramcd, param, avisit) %>%
  arrange(paramcd, avisit) %>%
  print(n = 30)

# 4. Check parameter codes beginning with underscore
adlbc %>%
  distinct(paramcd, param) %>%
  filter(grepl("^_", paramcd)) %>%
  arrange(paramcd)

# 5. Inspect actual baseline records
adlbc %>%
  filter(ablfl == "Y") %>%
  select(
    usubjid, trta, paramcd, param,
    avisit, avisitn, ablfl, anl01fl,
    aval, base, chg
  ) %>%
  head(20)

# 6. Compare baseline and post-baseline record counts
adlbc %>%
  mutate(
    Record_Type = case_when(
      ablfl == "Y" ~ "Baseline flagged",
      avisit == "Baseline" ~ "Baseline visit",
      TRUE ~ "Other"
    )
  ) %>%
  count(Record_Type, anl01fl)









# Final check before correcting the analysis populations


# Compare parameter codes in baseline records
adlbc %>%
  filter(avisit == "Baseline") %>%
  mutate(
    Baseline_Flag = if_else(
      ablfl == "Y",
      "ABLFL = Y",
      "ABLFL missing"
    )
  ) %>%
  count(Baseline_Flag, paramcd) %>%
  arrange(Baseline_Flag, paramcd) %>%
  print(n = Inf)





# Check the filters currently used in T06/T07
# T06 should include baseline and post-baseline original parameters.
# T07 should include post-baseline records with valid CHG.

adlbc %>%
  filter(
    !grepl("^_", paramcd),
    (ablfl == "Y" | anl01fl == "Y")
  ) %>%
  count(avisit, paramcd) %>%
  arrange(paramcd, avisit) %>%
  print(n = 30)



# For T07, the intended analysis population is: 

adlbc %>%
  filter(
    !grepl("^_", paramcd),
    anl01fl == "Y",
    !is.na(chg)
  )



# T06: Check duplicate subject-parameter-visit records
t06_data <- adlbc %>%
  filter(
    !grepl("^_", paramcd),
    ablfl == "Y" | anl01fl == "Y"
  )

t06_duplicates <- t06_data %>%
  count(usubjid, paramcd, avisit) %>%
  filter(n > 1)

nrow(t06_duplicates)



# T07: Check duplicate subject-parameter-visit records
t07_data <- adlbc %>%
  filter(
    !grepl("^_", paramcd),
    anl01fl == "Y",
    !is.na(chg)
  )

t07_duplicates <- t07_data %>%
  count(usubjid, paramcd, avisit) %>%
  filter(n > 1)

nrow(t07_duplicates)



# Validate the number of subjects in each table


# T06: Record and subject counts
t06_data %>%
  group_by(paramcd, avisit, trta) %>%
  summarise(
    N_Records = n(),
    N_Subjects = n_distinct(usubjid),
    .groups = "drop"
  ) %>%
  print(n = 30)


# T07: Record and subject counts
t07_data %>%
  group_by(paramcd, avisit, trta) %>%
  summarise(
    N_Records = n(),
    N_Subjects = n_distinct(usubjid),
    .groups = "drop"
  ) %>%
  print(n = 30)


# ============================================================
# 1. FUNCTION FOR LABORATORY DESCRIPTIVE STATISTICS
# ============================================================

summarise_lab <- function(data, value_var) {

  # Treatment-wise summaries
  treatment_summary <- data %>%
    filter(!is.na(.data[[value_var]])) %>%
    group_by(paramcd, param, avisit, avisitn, trta) %>%
    summarise(
      N = n(),
      Mean = mean(.data[[value_var]]),
      SD = sd(.data[[value_var]]),
      Median = median(.data[[value_var]]),
      Min = min(.data[[value_var]]),
      Max = max(.data[[value_var]]),
      .groups = "drop"
    )

  # Overall summaries
  total_summary <- data %>%
    filter(!is.na(.data[[value_var]])) %>%
    group_by(paramcd, param, avisit, avisitn) %>%
    summarise(
      N = n(),
      Mean = mean(.data[[value_var]]),
      SD = sd(.data[[value_var]]),
      Median = median(.data[[value_var]]),
      Min = min(.data[[value_var]]),
      Max = max(.data[[value_var]]),
      .groups = "drop"
    ) %>%
    mutate(trta = "Total")

  # Combine treatment and total summaries
  bind_rows(treatment_summary, total_summary) %>%
    arrange(paramcd, avisitn, trta)
}


# ============================================================
# 2. T06: LABORATORY VALUES
# ============================================================

T06 <- summarise_lab(
  data = t06_data,
  value_var = "aval"
)


# ============================================================
# 3. T07: CHANGE FROM BASELINE
# ============================================================

T07 <- summarise_lab(
  data = t07_data,
  value_var = "chg"
)



# Validate the output


dim(T06)
dim(T07)

head(T06, 10)
head(T07, 10)

# Check treatment groups
T06 %>% count(trta)
T07 %>% count(trta)

# Check missing summary statistics
colSums(is.na(T06))
colSums(is.na(T07))




#  QC: confirm the missing SD values

# T06: Groups with missing SD
T06 %>%
  filter(is.na(SD)) %>%
  select(paramcd, avisit, trta, N, Mean, SD)

# T07: Groups with missing SD
T07 %>%
  filter(is.na(SD)) %>%
  select(paramcd, avisit, trta, N, Mean, SD)


T07 %>%
  filter(is.na(SD)) %>%
  select(paramcd, avisit, trta, N, Mean, SD)



# ============================================================
# 4. FORMAT LABORATORY SUMMARY TABLES
# ============================================================

format_lab_table <- function(data) {

  data %>%
    mutate(
      across(
        c(Mean, SD, Median, Min, Max),
        ~ round(.x, 2)
      )
    ) %>%
    pivot_longer(
      cols = c(N, Mean, SD, Median, Min, Max),
      names_to = "Statistic",
      values_to = "Value"
    ) %>%
    mutate(
      Statistic = factor(
        Statistic,
        levels = c("N", "Mean", "SD", "Median", "Min", "Max")
      ),
      trta = factor(
        trta,
        levels = c(
          "Placebo",
          "Xanomeline Low Dose",
          "Xanomeline High Dose",
          "Total"
        )
      )
    ) %>%
    arrange(paramcd, avisitn, Statistic) %>%
    pivot_wider(
      names_from = trta,
      values_from = Value
    ) %>%
    arrange(paramcd, avisitn, Statistic)
}


# ============================================================
# 5. FINAL T06 AND T07 TABLES
# ============================================================

T06_final <- format_lab_table(T06)

T07_final <- format_lab_table(T07)


# ============================================================
# 6. VALIDATE FORMATTED TABLES
# ============================================================

dim(T06_final)
dim(T07_final)

head(T06_final, 12)
head(T07_final, 12)






# ============================================================
# 7. FINAL COLUMN ORDER
# ============================================================

final_column_order <- c(
  "paramcd",
  "param",
  "avisit",
  "avisitn",
  "Statistic",
  "Placebo",
  "Xanomeline Low Dose",
  "Xanomeline High Dose",
  "Total"
)

T06_final <- T06_final %>%
  select(all_of(final_column_order))

T07_final <- T07_final %>%
  select(all_of(final_column_order))





# ============================================================
# 8. EXPORT LABORATORY TABLES
# ============================================================

output_dir <- here::here("output", "tables")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Export individual CSV files
readr::write_csv(
  T06_final,
  file.path(output_dir, "T06_Laboratory_Values.csv")
)

readr::write_csv(
  T07_final,
  file.path(output_dir, "T07_Laboratory_Change_from_Baseline.csv")
)

# Export combined Excel workbook
writexl::write_xlsx(
  list(
    T06_Laboratory_Values = T06_final,
    T07_Change_from_Baseline = T07_final
  ),
  path = file.path(output_dir, "Laboratory_Summary_Tables.xlsx")
)


dim(T06_final)
dim(T07_final)

names(T06_final)
names(T07_final)

list.files(output_dir, pattern = "T0[67]|Laboratory")