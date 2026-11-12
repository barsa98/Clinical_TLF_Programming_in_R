
# ============================================================
# T03-T05: ADVERSE EVENT DATA REVIEW
# ============================================================

# 1. Dataset structure
dim(adam_data$adae)
names(adam_data$adae)
glimpse(adam_data$adae)

# 2. Check treatment variables
adam_data$adae %>%
  count(trta, sort = TRUE)

# 3. Check treatment-emergent flag
adam_data$adae %>%
  count(trtemfl, .drop = FALSE)

# 4. Inspect important AE variables
adam_data$adae %>%
  select(
    any_of(c(
      "usubjid",
      "trta",
      "trtemfl",
      "aebodsys",
      "aebodsysn",
      "aedecod",
      "aesev",
      "aeser",
      "aerel",
      "aeacn"
    ))
  ) %>%
  glimpse()

# 5. Check available values in key variables
adam_data$adae %>%
  summarise(
    unique_subjects = n_distinct(usubjid),
    total_records = n(),
    missing_subjects = sum(is.na(usubjid))
  )

# 6. Inspect SOC, PT, severity and seriousness
adam_data$adae %>%
  summarise(
    SOC_values = n_distinct(aebodsys),
    PT_values = n_distinct(aedecod),
    Severity_values = n_distinct(aesev),
    Seriousness_values = n_distinct(aeser)
  )








# ============================================================
# T03-T05: AE VARIABLE VALIDATION
# ============================================================

adae <- adam_data$adae
adsl <- adam_data$adsl

# 1. Safety population by actual treatment
adsl %>%
  filter(saffl == "Y") %>%
  count(trt01a, name = "N")

# 2. ADAE treatment groups
adae %>%
  distinct(trta) %>%
  arrange(trta)

# 3. Confirm SOC variable consistency
adae %>%
  summarise(
    SOC_match = all(
      coalesce(aebodsys, "") == coalesce(aesoc, "")
    )
  )

# 4. Treatment-emergent AE records by treatment
adae %>%
  filter(trtemfl == "Y") %>%
  count(trta, name = "TEAE_records")

# 5. Distinct subjects with at least one TEAE
adae %>%
  filter(trtemfl == "Y") %>%
  group_by(trta) %>%
  summarise(
    Subjects_with_TEAE = n_distinct(usubjid),
    TEAE_records = n(),
    .groups = "drop"
  )

# 6. Severity categories
adae %>%
  filter(trtemfl == "Y") %>%
  count(aesev, .drop = FALSE)

# 7. Seriousness categories
adae %>%
  filter(trtemfl == "Y") %>%
  count(aeser, .drop = FALSE)

# 8. Serious TEAE records and distinct subjects
adae %>%
  filter(trtemfl == "Y", aeser == "Y") %>%
  group_by(trta) %>%
  summarise(
    Subjects_with_Serious_TEAE = n_distinct(usubjid),
    Serious_TEAE_records = n(),
    .groups = "drop"
  )







# ============================================================
# T03: OVERALL TREATMENT-EMERGENT ADVERSE EVENTS
# ============================================================

# 1. Prepare safety population
safety_pop <- adsl %>%
  filter(saffl == "Y") %>%
  select(
    usubjid,
    Treatment = trt01a
  ) %>%
  distinct()

# 2. Calculate safety population denominators
safety_denominator <- safety_pop %>%
  count(Treatment, name = "Denominator")

# 3. Identify subjects with at least one TEAE
teae_subjects <- adae %>%
  filter(trtemfl == "Y") %>%
  distinct(usubjid) %>%
  mutate(Has_TEAE = TRUE)

# 4. Count subjects with at least one TEAE by treatment
teae_summary <- safety_pop %>%
  left_join(teae_subjects, by = "usubjid") %>%
  mutate(
    Has_TEAE = coalesce(Has_TEAE, FALSE)
  ) %>%
  group_by(Treatment) %>%
  summarise(
    N = sum(Has_TEAE),
    .groups = "drop"
  )

# 5. Add denominators and calculate percentages
t03_summary <- teae_summary %>%
  left_join(safety_denominator, by = "Treatment") %>%
  mutate(
    Percentage = 100 * N / Denominator,
    Result = sprintf("%d (%.1f%%)", N, Percentage)
  )

# 6. Calculate overall total
total_row <- tibble(
  Treatment = "Total",
  Denominator = n_distinct(safety_pop$usubjid),
  N = n_distinct(
    semi_join(safety_pop, teae_subjects, by = "usubjid")$usubjid
  )
) %>%
  mutate(
    Percentage = 100 * N / Denominator,
    Result = sprintf("%d (%.1f%%)", N, Percentage)
  )

# 7. Combine treatment groups and total
treatment_order <- c(
  "Placebo",
  "Xanomeline Low Dose",
  "Xanomeline High Dose",
  "Total"
)

t03_summary <- bind_rows(
  t03_summary,
  total_row
) %>%
  mutate(
    Treatment = factor(Treatment, levels = treatment_order)
  ) %>%
  arrange(Treatment) %>%
  mutate(
    Treatment = as.character(Treatment)
  )

# 8. Final T03 table
T03 <- t03_summary %>%
  transmute(
    Parameter = "Subjects with at least one TEAE",
    Statistic = "n (%)",
    Treatment,
    Denominator,
    N,
    Percentage = round(Percentage, 1),
    Result
  )

# 9. Display results
print(T03, n = Inf)








# Check expected population denominators
T03 %>%
  select(Treatment, Denominator)

# Check subject counts and percentages
T03 %>%
  select(Treatment, N, Percentage, Result)

# Confirm no subject count exceeds its denominator
T03 %>%
  filter(N > Denominator)

# Independently verify overall TEAE subjects
adae %>%
  filter(trtemfl == "Y") %>%
  summarise(
    Unique_TEAE_subjects = n_distinct(usubjid)
  )





# ============================================================
# T04: SOC AND PREFERRED TERM VALIDATION
# ============================================================

# 1. TEAEs by System Organ Class
soc_check <- adae %>%
  filter(trtemfl == "Y") %>%
  group_by(trta, aebodsys) %>%
  summarise(
    AE_records = n(),
    Subjects = n_distinct(usubjid),
    .groups = "drop"
  )

print(soc_check, n = 30)

# 2. TEAEs by Preferred Term
pt_check <- adae %>%
  filter(trtemfl == "Y") %>%
  group_by(trta, aebodsys, aedecod) %>%
  summarise(
    AE_records = n(),
    Subjects = n_distinct(usubjid),
    .groups = "drop"
  )

print(pt_check, n = 30)

# 3. Check missing SOC and PT
adae %>%
  filter(trtemfl == "Y") %>%
  summarise(
    Missing_SOC = sum(is.na(aebodsys)),
    Missing_PT = sum(is.na(aedecod))
  )







# ============================================================
# T04: SOC AND PREFERRED TERM VALIDATION
# ============================================================

# 1. TEAEs by System Organ Class
soc_check <- adae %>%
  filter(trtemfl == "Y") %>%
  group_by(trta, aebodsys) %>%
  summarise(
    AE_records = n(),
    Subjects = n_distinct(usubjid),
    .groups = "drop"
  )

print(soc_check, n = 30)

# 2. TEAEs by Preferred Term
pt_check <- adae %>%
  filter(trtemfl == "Y") %>%
  group_by(trta, aebodsys, aedecod) %>%
  summarise(
    AE_records = n(),
    Subjects = n_distinct(usubjid),
    .groups = "drop"
  )

print(pt_check, n = 30)

# 3. Check missing SOC and PT
adae %>%
  filter(trtemfl == "Y") %>%
  summarise(
    Missing_SOC = sum(is.na(aebodsys)),
    Missing_PT = sum(is.na(aedecod))
  )






# ============================================================
# T04: TEAEs BY SYSTEM ORGAN CLASS AND PREFERRED TERM
# ============================================================

# 1. Prepare treatment-emergent AE records
teae_data <- adae %>%
  filter(trtemfl == "Y") %>%
  transmute(
    usubjid,
    Treatment = trta,
    SOC = aebodsys,
    PT = aedecod
  ) %>%
  distinct()

# 2. Prepare treatment denominators
treatment_denominator <- safety_pop %>%
  count(Treatment, name = "Denominator")

total_denominator <- tibble(
  Treatment = "Total",
  Denominator = n_distinct(safety_pop$usubjid)
)

t04_denominators <- bind_rows(
  treatment_denominator,
  total_denominator
)

# 3. Create SOC-level subject counts
soc_counts <- teae_data %>%
  distinct(usubjid, Treatment, SOC) %>%
  count(SOC, Treatment, name = "N") %>%
  mutate(
    Row_Type = "SOC",
    PT = NA_character_
  )

# 4. Create PT-level subject counts
pt_counts <- teae_data %>%
  distinct(usubjid, Treatment, SOC, PT) %>%
  count(SOC, PT, Treatment, name = "N") %>%
  mutate(
    Row_Type = "PT"
  )

# 5. Calculate overall SOC counts
soc_total <- teae_data %>%
  distinct(usubjid, SOC) %>%
  count(SOC, name = "N") %>%
  mutate(
    Treatment = "Total",
    Row_Type = "SOC",
    PT = NA_character_
  )

# 6. Calculate overall PT counts
pt_total <- teae_data %>%
  distinct(usubjid, SOC, PT) %>%
  count(SOC, PT, name = "N") %>%
  mutate(
    Treatment = "Total",
    Row_Type = "PT"
  )

# 7. Combine treatment-specific and overall counts
t04_counts <- bind_rows(
  soc_counts,
  pt_counts,
  soc_total,
  pt_total
)

# 8. Create all SOC/PT and treatment combinations
t04_categories <- t04_counts %>%
  distinct(Row_Type, SOC, PT)

t04_complete <- tidyr::crossing(
  t04_categories,
  Treatment = treatment_order
) %>%
  left_join(
    t04_counts,
    by = c("Row_Type", "SOC", "PT", "Treatment")
  ) %>%
  mutate(
    N = coalesce(N, 0L)
  ) %>%
  left_join(
    t04_denominators,
    by = "Treatment"
  ) %>%
  mutate(
    Percentage = 100 * N / Denominator,
    Result = sprintf("%d (%.1f%%)", N, Percentage)
  )

# 9. Arrange SOC and PT hierarchy
T04 <- t04_complete %>%
  mutate(
    Row_Type = factor(
      Row_Type,
      levels = c("SOC", "PT")
    ),
    Treatment = factor(
      Treatment,
      levels = treatment_order
    )
  ) %>%
  arrange(SOC, Row_Type, PT, Treatment) %>%
  select(
    Row_Type,
    SOC,
    PT,
    Treatment,
    N,
    Denominator,
    Percentage,
    Result
  )

# 10. Convert treatment groups into columns
T04 <- T04 %>%
  select(
    Row_Type,
    SOC,
    PT,
    Treatment,
    Result
  ) %>%
  tidyr::pivot_wider(
    names_from = Treatment,
    values_from = Result
  ) %>%
  mutate(
    Row_Type = as.character(Row_Type)
  )

# 11. Display T04
print(T04, n = 30)





# Check number of SOC rows
T04 %>%
  filter(Row_Type == "SOC") %>%
  nrow()

# Check number of PT rows
T04 %>%
  filter(Row_Type == "PT") %>%
  nrow()

# Inspect SOC-level results
T04 %>%
  filter(Row_Type == "SOC") %>%
  print(n = Inf)

# Inspect first 20 PT rows
T04 %>%
  filter(Row_Type == "PT") %>%
  print(n = 20)

# Confirm no missing treatment results
T04 %>%
  summarise(
    Missing_Placebo = sum(is.na(Placebo)),
    Missing_Low = sum(is.na(`Xanomeline Low Dose`)),
    Missing_High = sum(is.na(`Xanomeline High Dose`)),
    Missing_Total = sum(is.na(Total))
  )



# ============================================================
# T05: SEVERITY AND SERIOUSNESS VALIDATION
# ============================================================

# 1. Severity by treatment
adae %>%
  filter(trtemfl == "Y") %>%
  group_by(trta, aesev) %>%
  summarise(
    AE_records = n(),
    Subjects = n_distinct(usubjid),
    .groups = "drop"
  ) %>%
  arrange(aesev, trta)

# 2. Seriousness by treatment
adae %>%
  filter(trtemfl == "Y") %>%
  group_by(trta, aeser) %>%
  summarise(
    AE_records = n(),
    Subjects = n_distinct(usubjid),
    .groups = "drop"
  ) %>%
  arrange(aeser, trta)

# 3. Check severity and seriousness combinations
adae %>%
  filter(trtemfl == "Y") %>%
  count(aesev, aeser, name = "AE_records")






# ============================================================
# T05: TEAEs BY SEVERITY AND SERIOUSNESS
# ============================================================

# 1. Prepare treatment-emergent AE data
t05_data <- adae %>%
  filter(trtemfl == "Y") %>%
  transmute(
    usubjid,
    Treatment = trta,
    Severity = aesev,
    Seriousness = case_when(
      aeser == "N" ~ "NON-SERIOUS",
      aeser == "Y" ~ "SERIOUS",
      TRUE ~ NA_character_
    )
  )

# 2. Define reusable AE category summary function
ae_category_summary <- function(data, category_var,
                                category_order, section_label) {

  # Subject counts by treatment and category
  treatment_counts <- data %>%
    filter(!is.na(.data[[category_var]])) %>%
    distinct(usubjid, Treatment, Category = .data[[category_var]]) %>%
    count(Category, Treatment, name = "N")

  # Overall subject counts by category
  total_counts <- data %>%
    filter(!is.na(.data[[category_var]])) %>%
    distinct(usubjid, Category = .data[[category_var]]) %>%
    count(Category, name = "N") %>%
    mutate(Treatment = "Total")

  # Combine treatment and overall counts
  all_counts <- bind_rows(
    treatment_counts,
    total_counts
  )

  # Generate all category-treatment combinations
  complete_data <- tidyr::crossing(
    Category = category_order,
    Treatment = treatment_order
  ) %>%
    left_join(
      all_counts,
      by = c("Category", "Treatment")
    ) %>%
    mutate(
      N = coalesce(N, 0L)
    )

  # Add safety population denominators
  complete_data <- complete_data %>%
    left_join(
      t04_denominators,
      by = "Treatment"
    ) %>%
    mutate(
      Percentage = 100 * N / Denominator,
      Result = sprintf("%d (%.1f%%)", N, Percentage),
      Section = section_label
    )

  # Convert treatment groups into columns
  complete_data %>%
    select(
      Section,
      Category,
      Treatment,
      Result
    ) %>%
    tidyr::pivot_wider(
      names_from = Treatment,
      values_from = Result
    ) %>%
    mutate(
      Category = factor(
        Category,
        levels = category_order
      )
    ) %>%
    arrange(Category) %>%
    mutate(
      Category = as.character(Category)
    )
}

# 3. Severity summary
severity_order <- c(
  "MILD",
  "MODERATE",
  "SEVERE"
)

severity_table <- ae_category_summary(
  data = t05_data,
  category_var = "Severity",
  category_order = severity_order,
  section_label = "Severity"
)

# 4. Seriousness summary
seriousness_order <- c(
  "NON-SERIOUS",
  "SERIOUS"
)

seriousness_table <- ae_category_summary(
  data = t05_data,
  category_var = "Seriousness",
  category_order = seriousness_order,
  section_label = "Seriousness"
)

# 5. Combine severity and seriousness
T05 <- bind_rows(
  severity_table,
  seriousness_table
)

# 6. Display final T05
print(T05, n = Inf)









# ============================================================
# T05: QUALITY CONTROL
# ============================================================

# 1. Inspect complete table
print(T05, n = Inf)

# 2. Check severity subject counts independently
t05_data %>%
  filter(!is.na(Severity)) %>%
  distinct(usubjid, Treatment, Severity) %>%
  count(Severity, Treatment)

# 3. Check seriousness subject counts independently
t05_data %>%
  filter(!is.na(Seriousness)) %>%
  distinct(usubjid, Treatment, Seriousness) %>%
  count(Seriousness, Treatment)

# 4. Verify denominators
t04_denominators

# 5. Check missing results
T05 %>%
  summarise(
    Missing_Placebo = sum(is.na(Placebo)),
    Missing_Low = sum(is.na(`Xanomeline Low Dose`)),
    Missing_High = sum(is.na(`Xanomeline High Dose`)),
    Missing_Total = sum(is.na(Total))
  )






library(writexl)





# ============================================================
# EXPORT T03, T04 AND T05
# ============================================================

# 1. Define output directory
output_dir <- here::here("output", "tables")

# Create directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 2. Save individual CSV files
readr::write_csv(
  T03,
  file.path(output_dir, "T03_TEAE_Incidence.csv")
)

readr::write_csv(
  T04,
  file.path(output_dir, "T04_TEAE_by_SOC_PT.csv")
)

readr::write_csv(
  T05,
  file.path(output_dir, "T05_TEAE_Severity_Seriousness.csv")
)

# 3. Save all three tables in one Excel workbook
writexl::write_xlsx(
  list(
    T03_TEAE_Incidence = T03,
    T04_TEAE_by_SOC_PT = T04,
    T05_TEAE_Severity_Seriousness = T05
  ),
  path = file.path(output_dir, "AE_Summary_Tables.xlsx")
)

# 4. Confirm saved files
list.files(
  output_dir,
  pattern = "T0[345]|AE_Summary",
  full.names = TRUE
)


