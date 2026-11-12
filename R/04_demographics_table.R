
# ============================================================
# Script: 04_demographics_table.R
# Purpose: Review demographic and baseline variables
# Source: ADaM ADSL
# ============================================================

library(dplyr)

# 1. Load analysis dataset
adsl <- adam_data$adsl

# 2. Define treatment group order
treatment_order <- c(
  "Placebo",
  "Xanomeline Low Dose",
  "Xanomeline High Dose"
)

# 3. Confirm analysis population
adsl %>%
  summarise(
    Total_Subjects = n(),
    ITT_Subjects = sum(ittfl == "Y", na.rm = TRUE),
    Safety_Subjects = sum(saffl == "Y", na.rm = TRUE)
  )

# 4. Check treatment group distribution
adsl %>%
  count(trt01p)

# 5. Review continuous variables
adsl %>%
  summarise(
    Age_N = sum(!is.na(age)),
    Age_Mean = mean(age, na.rm = TRUE),
    Age_SD = sd(age, na.rm = TRUE),
    Age_Median = median(age, na.rm = TRUE),
    Age_Min = min(age, na.rm = TRUE),
    Age_Max = max(age, na.rm = TRUE),

    BMI_N = sum(!is.na(bmibl)),
    Height_N = sum(!is.na(heightbl)),
    Weight_N = sum(!is.na(weightbl))
  )

# 6. Review categorical variable levels
adsl %>%
  count(sex, .drop = FALSE)

adsl %>%
  count(race, .drop = FALSE)

adsl %>%
  count(ethnic, .drop = FALSE)

adsl %>%
  count(agegr1, .drop = FALSE)

# 7. Review categorical distributions by treatment
adsl %>%
  group_by(trt01p) %>%
  count(sex, .drop = FALSE)

adsl %>%
  group_by(trt01p) %>%
  count(race, .drop = FALSE)




# ============================================================
# 04_demographics_table.R
# Section 2: Demographics summary functions
# ============================================================

library(dplyr)
library(tidyr)

adsl <- adam_data$adsl

treatment_order <- c(
  "Placebo",
  "Xanomeline Low Dose",
  "Xanomeline High Dose"
)

adsl <- adsl %>%
  mutate(
    trt01p = factor(
      trt01p,
      levels = treatment_order
    )
  )








# Function: Continuous variable summary

continuous_summary <- function(data, variable, label) {

  # Summary by treatment group
  treatment_summary <- data %>%
    group_by(trt01p) %>%
    summarise(
      N = sum(!is.na(.data[[variable]])),
      Mean = mean(.data[[variable]], na.rm = TRUE),
      SD = sd(.data[[variable]], na.rm = TRUE),
      Median = median(.data[[variable]], na.rm = TRUE),
      Min = min(.data[[variable]], na.rm = TRUE),
      Max = max(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      Treatment = as.character(trt01p)
    ) %>%
    select(-trt01p)

  # Overall summary
  overall_summary <- data %>%
    summarise(
      N = sum(!is.na(.data[[variable]])),
      Mean = mean(.data[[variable]], na.rm = TRUE),
      SD = sd(.data[[variable]], na.rm = TRUE),
      Median = median(.data[[variable]], na.rm = TRUE),
      Min = min(.data[[variable]], na.rm = TRUE),
      Max = max(.data[[variable]], na.rm = TRUE)
    ) %>%
    mutate(Treatment = "Total")

  # Combine treatment and overall summaries
  bind_rows(treatment_summary, overall_summary) %>%
    mutate(Parameter = label, .before = 1)
}








# Function: Categorical variable summary


# Function: Categorical variable summary
# Includes zero-count categories for each treatment group

categorical_summary <- function(data, variable, label) {

  # Identify all categories present in the study
  categories <- data %>%
    filter(!is.na(.data[[variable]])) %>%
    pull(all_of(variable)) %>%
    as.character() %>%
    unique()

  # Count subjects by treatment and category
  category_counts <- data %>%
    filter(!is.na(.data[[variable]])) %>%
    transmute(
      Treatment = as.character(trt01p),
      Category = as.character(.data[[variable]])
    ) %>%
    count(Treatment, Category, name = "n")

  # Calculate non-missing denominators by treatment
  treatment_denominators <- data %>%
    group_by(trt01p) %>%
    summarise(
      Denominator = sum(!is.na(.data[[variable]])),
      .groups = "drop"
    ) %>%
    transmute(
      Treatment = as.character(trt01p),
      Denominator
    )

  # Create all treatment-category combinations
  treatment_summary <- tidyr::crossing(
    Treatment = treatment_order,
    Category = categories
  ) %>%
    left_join(
      category_counts,
      by = c("Treatment", "Category")
    ) %>%
    left_join(
      treatment_denominators,
      by = "Treatment"
    ) %>%
    mutate(
      n = coalesce(n, 0L),
      Percentage = if_else(
        Denominator > 0,
        n / Denominator * 100,
        0
      ),
      Result = sprintf("%d (%.1f%%)", n, Percentage)
    )

  # Calculate overall category counts
  overall_summary <- data %>%
    filter(!is.na(.data[[variable]])) %>%
    transmute(Category = as.character(.data[[variable]])) %>%
    count(Category, name = "n") %>%
    mutate(
      Denominator = sum(n),
      Percentage = n / Denominator * 100,
      Result = sprintf("%d (%.1f%%)", n, Percentage),
      Treatment = "Total"
    )

  # Combine treatment and overall results
  bind_rows(
    treatment_summary,
    overall_summary
  ) %>%
    mutate(
      Parameter = label,
      .before = 1
    )
}




# Continuous variables
age_summary <- continuous_summary(
  adsl, "age", "Age (years)"
)

bmi_summary <- continuous_summary(
  adsl, "bmibl", "BMI"
)

height_summary <- continuous_summary(
  adsl, "heightbl", "Height"
)

weight_summary <- continuous_summary(
  adsl, "weightbl", "Weight"
)

# Categorical variables
sex_summary <- categorical_summary(
  adsl, "sex", "Sex"
)

race_summary <- categorical_summary(
  adsl, "race", "Race"
)

ethnicity_summary <- categorical_summary(
  adsl, "ethnic", "Ethnicity"
)

age_group_summary <- categorical_summary(
  adsl, "agegr1", "Age group"
)

# Display results
print(age_summary)
print(bmi_summary)
print(height_summary)
print(weight_summary)

print(sex_summary)
print(race_summary)
print(ethnicity_summary)
print(age_group_summary)






# Recalculate categorical summaries

sex_summary <- categorical_summary(
  adsl, "sex", "Sex"
)

race_summary <- categorical_summary(
  adsl, "race", "Race"
)

ethnicity_summary <- categorical_summary(
  adsl, "ethnic", "Ethnicity"
)

age_group_summary <- categorical_summary(
  adsl, "agegr1", "Age group"
)

# Inspect results
print(race_summary, n = Inf)
print(sex_summary, n = Inf)
print(ethnicity_summary, n = Inf)
print(age_group_summary, n = Inf)






# ============================================================
# SECTION 3.1: FORMAT CONTINUOUS VARIABLES
# ============================================================


format_continuous <- function(summary_data, parameter_label) {

  summary_data %>%
    mutate(
      Treatment = as.character(Treatment),
      N = as.character(N),
      Mean_SD = sprintf("%.1f (%.2f)", Mean, SD),
      Median_Value = sprintf("%.1f", Median),
      Min_Max = sprintf("%.1f - %.1f", Min, Max)
    ) %>%
    select(
      Treatment,
      N,
      Mean_SD,
      Median_Value,
      Min_Max
    ) %>%
    tidyr::pivot_longer(
      cols = c(N, Mean_SD, Median_Value, Min_Max),
      names_to = "Statistic",
      values_to = "Value"
    ) %>%
    mutate(
      Parameter = parameter_label,
      Statistic = dplyr::recode(
        Statistic,
        "N" = "n",
        "Mean_SD" = "Mean (SD)",
        "Median_Value" = "Median",
        "Min_Max" = "Minimum - Maximum"
      )
    ) %>%
    select(Parameter, Statistic, Treatment, Value)
}


# Create continuous tables
age_table <- format_continuous(age_summary, "Age (years)")

bmi_table <- format_continuous(bmi_summary, "BMI (kg/m2)")

height_table <- format_continuous(height_summary, "Height (cm)")

weight_table <- format_continuous(weight_summary, "Weight (kg)")


# Combine continuous tables
continuous_table <- bind_rows(
  age_table,
  bmi_table,
  height_table,
  weight_table
)


# Inspect
print(continuous_table, n = Inf)




# ============================================================
# SECTION 3.2: FORMAT CATEGORICAL VARIABLES
# ============================================================

format_categorical <- function(summary_data, parameter_label,
                               category_order = NULL) {

  result <- summary_data %>%
    mutate(
      Treatment = as.character(Treatment),
      Category = as.character(Category)
    )

  # Apply category order when specified
  if (!is.null(category_order)) {
    result <- result %>%
      mutate(
        Category = factor(
          Category,
          levels = category_order
        )
      )
  }

  result %>%
    mutate(
      Statistic = as.character(Category),
      Parameter = parameter_label
    ) %>%
    select(
      Parameter,
      Statistic,
      Treatment,
      Value = Result
    )
}


# Format individual categorical summaries

sex_table <- format_categorical(
  sex_summary,
  "Sex",
  category_order = c("F", "M")
)

race_table <- format_categorical(
  race_summary,
  "Race",
  category_order = c(
    "AMERICAN INDIAN OR ALASKA NATIVE",
    "BLACK OR AFRICAN AMERICAN",
    "WHITE"
  )
)

ethnicity_table <- format_categorical(
  ethnicity_summary,
  "Ethnicity",
  category_order = c(
    "HISPANIC OR LATINO",
    "NOT HISPANIC OR LATINO"
  )
)

age_group_table <- format_categorical(
  age_group_summary,
  "Age group (years)",
  category_order = c("<65", "65-80", ">80")
)


# Combine categorical summaries

categorical_table <- bind_rows(
  sex_table,
  race_table,
  ethnicity_table,
  age_group_table
)


# Inspect categorical table

print(categorical_table, n = Inf)




# ============================================================
# SECTION 3.3: COMBINE CONTINUOUS AND CATEGORICAL SUMMARIES
# ============================================================

# Combine both summary sections
demographics_long <- bind_rows(
  continuous_table,
  categorical_table
)

# Check the combined dataset
dim(demographics_long)

print(demographics_long, n = 20)






# ============================================================
# SECTION 3.4: CREATE FINAL DEMOGRAPHICS TABLE
# ============================================================

treatment_order <- c(
  "Placebo",
  "Xanomeline Low Dose",
  "Xanomeline High Dose",
  "Total"
)

# Define parameter and category order
parameter_order <- c(
  "Age (years)",
  "Sex",
  "Race",
  "Ethnicity",
  "Age group (years)",
  "BMI (kg/m2)",
  "Height (cm)",
  "Weight (kg)"
)

demographics_table <- demographics_long %>%
  mutate(
    Treatment = factor(
      Treatment,
      levels = treatment_order
    ),
    Parameter = factor(
      Parameter,
      levels = parameter_order
    )
  ) %>%
  tidyr::pivot_wider(
    names_from = Treatment,
    values_from = Value
  ) %>%
  arrange(Parameter) %>%
  mutate(
    Statistic = factor(
      Statistic,
      levels = c(
        "n",
        "Mean (SD)",
        "Median",
        "Minimum - Maximum",
        "F",
        "M",
        "AMERICAN INDIAN OR ALASKA NATIVE",
        "BLACK OR AFRICAN AMERICAN",
        "WHITE",
        "HISPANIC OR LATINO",
        "NOT HISPANIC OR LATINO",
        "<65",
        "65-80",
        ">80"
      )
    )
  ) %>%
  arrange(Parameter, Statistic) %>%
  mutate(
    Parameter = as.character(Parameter),
    Statistic = as.character(Statistic)
  )

# Inspect final table
print(demographics_table, n = Inf)






# Check dimensions
dim(demographics_table)

# Check column names
names(demographics_table)

# Check treatment group totals
demographics_table %>%
  filter(Parameter == "Age (years)", Statistic == "n")

# Check missing values in treatment columns
demographics_table %>%
  summarise(
    across(
      c(
        Placebo,
        `Xanomeline Low Dose`,
        `Xanomeline High Dose`,
        Total
      ),
      ~ sum(is.na(.x))
    )
  )




# ============================================================
# SECTION 3.5: EXPORT DEMOGRAPHICS TABLE
# ============================================================

# Create output directory if it does not exist
output_dir <- here::here("output", "tables")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Export as CSV
readr::write_csv(
  demographics_table,
  file.path(output_dir, "T02_Demographics_Baseline.csv")
)

# Export as Excel
writexl::write_xlsx(
  demographics_table,
  file.path(output_dir, "T02_Demographics_Baseline.xlsx")
)

# Confirm saved files
list.files(output_dir)




