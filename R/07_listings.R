
# ============================================================
# 07_LISTINGS.R
# Clinical TLF Programming in R
# ============================================================

library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(readr)

# ============================================================
# L01: SUBJECT DEMOGRAPHICS LISTING
# Source: ADSL
# Population: Safety Population
# ============================================================

L01 <- adsl %>%
  filter(saffl == "Y") %>%
  select(
    STUDYID = studyid,
    SITEID = siteid,
    USUBJID = usubjid,
    SUBJID = subjid,
    TREATMENT = trt01a,
    AGE = age,
    AGE_GROUP = agegr1,
    SEX = sex,
    RACE = race,
    ETHNICITY = ethnic,
    BMI = bmibl,
    HEIGHT = heightbl,
    WEIGHT = weightbl
  ) %>%
  arrange(TREATMENT, USUBJID)

# ============================================================
# L01: QUALITY CONTROL
# ============================================================

# Number of subjects
nrow(L01)

# Unique subject count
n_distinct(L01$USUBJID)

# Duplicate subject check
L01 %>%
  count(USUBJID) %>%
  filter(n > 1)

# Treatment distribution
L01 %>%
  count(TREATMENT)

# Missing values
colSums(is.na(L01))

# ============================================================
# L01: EXPORT
# ============================================================

output_dir <- here::here("output", "listings")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(
  L01,
  file.path(output_dir, "L01_Subject_Demographics.csv")
)




setdiff(
  c("studyid", "siteid", "usubjid", "trta",
    "aeterm", "aedecod", "aebodsys",
    "astdt", "astdtf", "astdy",
    "aendt", "aendy", "adurn", "aduru",
    "aesev", "aeser", "aerel", "aeacn",
    "aeout", "trtemfl"),
  names(adae)
)



L02 <- adae %>%
  filter(saffl == "Y") %>%
  select(
    STUDYID = studyid,
    SITEID = siteid,
    USUBJID = usubjid,
    TREATMENT = trta,
    AE_TERM = aeterm,
    PREFERRED_TERM = aedecod,
    SYSTEM_ORGAN_CLASS = aebodsys,
    START_DATE = astdt,
    START_DATE_FLAG = astdtf,
    START_DAY = astdy,
    END_DATE = aendt,
    END_DAY = aendy,
    DURATION = adurn,
    DURATION_UNIT = aduru,
    SEVERITY = aesev,
    SERIOUS = aeser,
    RELATIONSHIP = aerel,
    ACTION_TAKEN = aeacn,
    OUTCOME = aeout,
    TREATMENT_EMERGENT = trtemfl
  ) %>%
  arrange(TREATMENT, USUBJID, START_DATE, PREFERRED_TERM)




dim(L02)
head(L02)




# ============================================================
# L02: ADVERSE EVENTS LISTING
# Source: ADAE
# Population: Safety Population
# ============================================================

L02 <- adae %>%
  filter(saffl == "Y") %>%
  select(
    STUDYID = studyid,
    SITEID = siteid,
    USUBJID = usubjid,
    TREATMENT = trta,

    # Adverse event identification
    AE_TERM = aeterm,
    PREFERRED_TERM = aedecod,
    SYSTEM_ORGAN_CLASS = aebodsys,

    # Event timing
    START_DATE = astdt,
    START_DATE_FLAG = astdtf,
    START_DAY = astdy,
    END_DATE = aendt,
    END_DATE_FLAG = aendtf,
    END_DAY = aendy,
    DURATION = adurn,
    DURATION_UNIT = aduru,

    # Event characteristics
    SEVERITY = aesev,
    SERIOUS = aeser,
    RELATIONSHIP = aerel,
    ACTION_TAKEN = aeacn,
    OUTCOME = aeout,

    # Treatment-emergent classification
    TREATMENT_EMERGENT = trtemfl
  ) %>%
  arrange(TREATMENT, USUBJID, START_DATE, PREFERRED_TERM)


# ============================================================
# L02: QUALITY CONTROL
# ============================================================

# Total AE records
nrow(L02)

# Unique subjects
n_distinct(L02$USUBJID)

# AE records by treatment
L02 %>%
  count(TREATMENT)

# Treatment-emergent AE records
L02 %>%
  filter(TREATMENT_EMERGENT == "Y") %>%
  count(TREATMENT)

# Unique subjects with treatment-emergent AEs
L02 %>%
  filter(TREATMENT_EMERGENT == "Y") %>%
  summarise(N_Subjects = n_distinct(USUBJID))

# Serious AE records
L02 %>%
  filter(SERIOUS == "Y") %>%
  count(TREATMENT)

# Missing values
colSums(is.na(L02))


# ============================================================
# L02: EXPORT
# ============================================================

write_csv(
  L02,
  file.path(output_dir, "L02_Adverse_Events.csv")
)








# ============================================================
# L03: LABORATORY RESULTS LISTING
# Source: ADLBC
# Population: Safety Population
# ============================================================

L03 <- adlbc %>%
  filter(saffl == "Y") %>%
  select(
    STUDYID = studyid,
    USUBJID = usubjid,
    SUBJID = subjid,
    TREATMENT = trta,
    PARAMETER = param,
    PARAMETER_CODE = paramcd,
    VISIT = avisit,
    VISIT_NUMBER = avisitn,
    ANALYSIS_DATE = adt,
    ANALYSIS_DAY = ady,
    BASELINE_FLAG = ablfl,
    BASELINE_VALUE = base,
    LAB_VALUE = aval,
    CHANGE_FROM_BASELINE = chg,
    NORMAL_RANGE_INDICATOR = anrind,
    BASELINE_RANGE_INDICATOR = bnrind,
    ANALYSIS_FLAG = anl01fl
  ) %>%
  arrange(
    TREATMENT,
    USUBJID,
    PARAMETER_CODE,
    VISIT_NUMBER
  )








# Dimensions
dim(L03)

# Unique subjects
n_distinct(L03$USUBJID)

# Records by treatment
L03 %>%
  count(TREATMENT)

# Records by parameter
L03 %>%
  count(PARAMETER_CODE, PARAMETER)

# Records by visit
L03 %>%
  count(VISIT, VISIT_NUMBER)

# Duplicate subject-parameter-visit records
L03 %>%
  count(USUBJID, PARAMETER_CODE, VISIT_NUMBER) %>%
  filter(n > 1)

# Missing values
colSums(is.na(L03))




write_csv(
  L03,
  file.path(output_dir, "L03_Laboratory_Results.csv")
)



