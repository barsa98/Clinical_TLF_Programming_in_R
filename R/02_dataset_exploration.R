# ============================================================
# Script: 02_dataset_exploration.R
# Purpose: Explore ADaM datasets before TLF generation
# ============================================================

library(dplyr)

# 1. ADSL: Subject-level analysis

# Treatment group distribution

adam_data$adsl %>%
  count(trt01p, trt01pn)

# Population distribution

adam_data$adsl %>%
  summarise(
    Total_Subjects = n(),
    Safety_Population = sum(saffl == "Y", na.rm = TRUE),
    ITT_Population = sum(ittfl == "Y", na.rm = TRUE),
    Efficacy_Population = sum(efffl == "Y", na.rm = TRUE)
  )

# 2. ADAE: Adverse event analysis

adam_data$adae %>%
  count(trta, trtemfl)

# 3. ADLBC: Laboratory analysis

adam_data$adlbc %>%
  count(trtp, param)

# Check analysis flags

adam_data$adlbc %>%
  count(ablfl, anl01fl)



# Missing value summary for each ADaM dataset

missing_summary <- lapply(adam_data, function(df) {

  data.frame(
    Variable = names(df),
    Missing_Count = colSums(is.na(df)),
    Missing_Percentage = round(
      colMeans(is.na(df)) * 100, 2
    )
  )
})

missing_summary


# Investigate the subject count difference

# Subjects in SDTM DM but not ADSL

dm_subjects <- unique(sdtm_data$dm$usubjid)

adsl_subjects <- unique(adam_data$adsl$usubjid)

setdiff(dm_subjects, adsl_subjects)

# Subjects in ADSL but not SDTM DM

setdiff(adsl_subjects, dm_subjects)

# Subject count comparison

data.frame(
  Dataset = c("DM", "ADSL"),
  Unique_Subjects = c(
    length(dm_subjects),
    length(adsl_subjects)
  )
)



# ============================================================
# Subject-level reconciliation: DM vs ADSL
# ============================================================

library(dplyr)

# Identify subjects missing from ADSL

dm_not_adsl <- sdtm_data$dm %>%
  anti_join(
    adam_data$adsl,
    by = "usubjid"
  )

# Number of subjects

nrow(dm_not_adsl)

# Examine their treatment allocation in DM

dm_not_adsl %>%
  count(arm, armcd)

# Check whether they have AE records

dm_not_adsl %>%
  select(usubjid) %>%
  left_join(
    sdtm_data$ae %>%
      count(usubjid, name = "AE_records"),
    by = "usubjid"
  ) %>%
  mutate(AE_records = coalesce(AE_records, 0L))

# Check whether they have laboratory records

dm_not_adsl %>%
  select(usubjid) %>%
  left_join(
    sdtm_data$lbch %>%
      count(usubjid, name = "LB_records"),
    by = "usubjid"
  ) %>%
  mutate(LB_records = coalesce(LB_records, 0L))




# ADSL population flags

adam_data$adsl %>%
  count(saffl, ittfl, efffl, .drop = FALSE)

# Population distribution by treatment

adam_data$adsl %>%
  group_by(trt01p) %>%
  summarise(
    Total = n(),
    Safety = sum(saffl == "Y", na.rm = TRUE),
    ITT = sum(ittfl == "Y", na.rm = TRUE),
    Efficacy = sum(efffl == "Y", na.rm = TRUE),
    .groups = "drop"
  )

# ADAE: Treatment-emergent events

adam_data$adae %>%
  group_by(trta) %>%
  summarise(
    Total_Records = n(),
    Treatment_Emergent = sum(trtemfl == "Y", na.rm = TRUE),
    Non_Treatment_Emergent = sum(trtemfl == "N", na.rm = TRUE),
    Unique_Subjects = n_distinct(usubjid),
    .groups = "drop"
  )

# ADLBC: Analysis records

adam_data$adlbc %>%
  group_by(trtp) %>%
  summarise(
    Total_Records = n(),
    Baseline_Records = sum(ablfl == "Y", na.rm = TRUE),
    Analysis_Records = sum(anl01fl == "Y", na.rm = TRUE),
    .groups = "drop"
  )



library(dplyr)

# Week 24 treatment completion
adam_data$adsl %>%
  count(comp24fl, .drop = FALSE)

# Week 24 completion by treatment group
adam_data$adsl %>%
  group_by(trt01p) %>%
  summarise(
    Total = n(),
    Completed_Week24 = sum(comp24fl == "Y", na.rm = TRUE),
    .groups = "drop"
  )



names(adam_data$adsl) %>%
  grep("COMP|DISCON|DCDECOD|DCREAS", ., value = TRUE)

adam_data$adsl %>%
  count(dcdecod, dcreascd, .drop = FALSE)


names(adam_data$adsl) %>%
  grep("COMP|DISCON|DCDECOD|DCREAS", ., value = TRUE)


names(adam_data$adsl) %>%
  grep(
    "COMP|DISCON|DCDECOD|DCREAS",
    .,
    value = TRUE,
    ignore.case = TRUE
  )


# Compare Week 24 completion flag with disposition
adam_data$adsl %>%
  count(comp24fl, dcdecod, .drop = FALSE) %>%
  arrange(comp24fl, dcdecod)


adam_data$adsl %>%
  group_by(trt01p, dcdecod) %>%
  summarise(
    Subjects = n(),
    .groups = "drop"
  ) %>%
  arrange(trt01p, dcdecod)




library(dplyr)

# Disposition by treatment group
disposition_summary <- adam_data$adsl %>%
  group_by(trt01p) %>%
  summarise(
    Total = n(),
    AE_Discontinuation = sum(dcdecod == "ADVERSE EVENT", na.rm = TRUE),
    AE_Discontinuation_Pct = round(
      AE_Discontinuation / Total * 100, 1
    ),
    .groups = "drop"
  )

disposition_summary



disposition_by_group <- adam_data$adsl %>%
  group_by(trt01p, dcdecod) %>%
  summarise(
    Subjects = n(),
    .groups = "drop"
  ) %>%
  group_by(trt01p) %>%
  mutate(
    Percentage = round(Subjects / sum(Subjects) * 100, 1)
  ) %>%
  ungroup()

disposition_by_group



