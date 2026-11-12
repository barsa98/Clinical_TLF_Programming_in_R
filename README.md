# Clinical TLF Programming in R

## Overview

This project demonstrates the development of clinical trial Tables, Listings, and Figures (TLFs) using R programming and existing SDTM and ADaM datasets.

The objective is to build a practical clinical programming workflow covering data exploration, population summaries, descriptive statistics, adverse event summaries, laboratory analyses, listings, graphical outputs, and quality control (QC).

The project uses example clinical trial datasets from the [`clinical_fd` repository](https://github.com/sas2r/clinical_fd), which contains mock clinical trial data based on PHUSE resources.

The programming work focuses on translating clinical analysis requirements into reproducible R scripts and generating structured outputs suitable for a clinical study reporting workflow.

## Project Objectives

- Explore and understand SDTM and ADaM datasets.
- Work with clinical trial analysis populations and treatment groups.
- Generate descriptive demographic and clinical summary tables.
- Summarize treatment-emergent adverse events (TEAEs).
- Perform laboratory value and change-from-baseline analyses.
- Develop subject-level clinical data listings.
- Create graphical summaries of safety and laboratory data.
- Implement programming QC checks and reconcile outputs against source datasets.
- Organize scripts and outputs into a reproducible project structure.

## Data Sources

The project uses example SDTM and ADaM datasets from the `clinical_fd` repository.

**Source repository:**
[https://github.com/sas2r/clinical_fd](https://github.com/sas2r/clinical_fd)

### Datasets used

| Dataset | Description                           | Records |
| ------- | ------------------------------------- | ------: |
| DM      | Demographics                          |     306 |
| AE      | Adverse Events                        |   1,191 |
| LBCH    | Laboratory Chemistry                  |  32,740 |
| ADSL    | Subject-Level Analysis Dataset        |     254 |
| ADAE    | Adverse Events Analysis Dataset       |   1,191 |
| ADLBC   | Laboratory Chemistry Analysis Dataset |  74,264 |

The project primarily uses ADSL, ADAE, and ADLBC for TLF development.

**Note:** The example datasets are not actual patient records from a clinical study conducted by the author. They are used for learning and portfolio development.

## Clinical Programming Workflow

The workflow follows the general sequence below:

1. Import SDTM and ADaM datasets.
2. Explore dataset structures, variables, and missing values.
3. Review study populations, treatment allocation, and subject disposition.
4. Generate demographic and clinical summary tables.
5. Generate adverse event and laboratory summary tables.
6. Develop subject-level listings.
7. Create graphical summaries.
8. Perform QC checks and validate outputs against source datasets.

## Tables, Listings, and Figures

### 1. Tables

| Table | Description                                             | Source dataset |
| ----- | ------------------------------------------------------- | -------------- |
| T01   | Subject Disposition and Analysis Population Summary     | ADSL           |
| T02   | Demographic and Baseline Characteristics                | ADSL           |
| T03   | Overall Treatment-Emergent Adverse Events               | ADAE, ADSL     |
| T04   | Adverse Events by System Organ Class and Preferred Term | ADAE           |
| T05   | Adverse Events by Severity and Seriousness              | ADAE           |
| T06   | Laboratory Values Summary                               | ADLBC          |
| T07   | Laboratory Change-from-Baseline Summary                 | ADLBC          |

**Table programming details**

- T01: Analysis population counts, treatment allocation, study completion, and discontinuation summaries.
- T02: Descriptive statistics for continuous demographic variables and frequency summaries for categorical variables.
- T03: Number and percentage of subjects experiencing at least one TEAE.
- T04: Subject incidence summaries by System Organ Class (SOC) and Preferred Term (PT).
- T05: Adverse event summaries by severity and seriousness.
- T06: Descriptive statistics for laboratory values across study visits.
- T07: Descriptive statistics for changes from baseline in laboratory values.

Continuous variables include N, mean, standard deviation, median, minimum, and maximum. Categorical variables are summarized using counts and percentages where applicable.

### 2. Listings

| Listing | Description                  | Source dataset |
| ------- | ---------------------------- | -------------- |
| L01     | Subject Demographics Listing | ADSL           |
| L02     | Adverse Events Listing       | ADAE           |
| L03     | Laboratory Results Listing   | ADLBC          |

**Listing programming details**

- L01: Subject identifiers, treatment assignments, demographics, and baseline characteristics.
- L02: Adverse event terms, SOC, start and end dates, severity, seriousness, relationship, action taken, and outcome.
- L03: Subject-level laboratory results, parameter information, visit details, baseline values, laboratory values, and changes from baseline.

### 3. Figures

| Figure | Description                                         | Source dataset |
| ------ | --------------------------------------------------- | -------------- |
| F01    | Adverse Event Incidence by Treatment Group          | ADAE, ADSL     |
| F02    | Mean Change from Baseline in ALT by Treatment Group | ADLBC          |

**Figure programming details**

**F01 – Adverse Event Incidence by Treatment Group**

A bar chart comparing the percentage of subjects with at least one treatment-emergent adverse event across the treatment groups.

The incidence percentage is calculated as:

TEAE incidence (%) = (Number of subjects with at least one TEAE / Safety population) × 100

**F02 – Mean Change from Baseline in ALT**

A line chart displaying mean changes from baseline in alanine aminotransferase (ALT) across study visits for the three treatment groups.

The figure includes:

- Mean change from baseline.
- Treatment group comparisons.
- Study visit progression.
- Error bars representing ±1 standard error (SE).

The figures are descriptive visualizations and are not intended to represent inferential statistical comparisons.

## Quality Control and Validation

QC is incorporated throughout the programming workflow rather than being limited to a final review.

The following checks are performed as applicable:

- Subject identifier uniqueness.
- Population and treatment group reconciliation.
- Duplicate record checks.
- Missing value assessment.
- Treatment-emergent adverse event count reconciliation.
- Subject-level incidence calculations.
- Laboratory parameter and visit-level record checks.
- Change-from-baseline calculations.
- Summary statistic verification.
- Figure data consistency with source analysis datasets.

A separate script, `09_tlf_qc.R`, is used for consolidated final validation.

### Population QC findings

The ADSL population checks produced the following results:

| Population             | Number of subjects |
| ---------------------- | -----------------: |
| Total subjects in ADSL |                254 |
| Safety population      |                254 |
| ITT population         |                254 |
| Efficacy population    |                234 |

Treatment allocation:

| Treatment group      | Subjects |
| -------------------- | -------: |
| Placebo              |       86 |
| Xanomeline Low Dose  |       84 |
| Xanomeline High Dose |       84 |
| **Total**            |  **254** |

Additional reconciliation findings:

- 118 subjects completed the Week 24 treatment phase.
- 110 subjects completed the study through Week 26.
- No duplicate subject identifiers were identified in ADSL.
- No missing subject identifiers were identified in ADSL.

## Project Structure

```text
Clinical_TLF_Programming_in_R/
│
├── data/
│   ├── raw/
│   │   ├── ae.csv
│   │   ├── dm.csv
│   │   ├── lbch.csv
│   │   ├── adae.csv
│   │   ├── adlbc.csv
│   │   └── adsl.csv
│   │
│   └── processed/
│
├── R/
│   ├── 01_import_datasets.R
│   ├── 02_dataset_exploration.R
│   ├── 03_population_analysis.R
│   ├── 04_demographics_table.R
│   ├── 05_ae_summary_tables.R
│   ├── 06_laboratory_tables.R
│   ├── 07_listings.R
│   ├── 08_figures.R
│   └── 09_tlf_qc.R
│
├── output/
│   ├── tables/
│   ├── listings/
│   └── figures/
│
├── reports/
│
├── README.md
└── Clinical_TLF_Programming_in_R.Rproj
```

## Tools and Technologies

- **Programming language:** R
- **IDE:** RStudio
- **Data manipulation:** dplyr, tidyr
- **Data import and export:** readr
- **Data visualization:** ggplot2
- **Project path management:** here
- **Version control:** Git and GitHub

## How to Run the Project

### Prerequisites

Install R and RStudio.

Install the main R packages:

```r
install.packages(c(
  "dplyr",
  "tidyr",
  "ggplot2",
  "readr",
  "here"
  "writexl"
))
```

Install any additional packages required by the Excel export code in the project scripts.

### Execution sequence

1. Clone or download the repository.
2. Open `Clinical_TLF_Programming_in_R.Rproj` in RStudio.
3. Place the source CSV datasets in `data/raw/`.
4. Run the scripts in numerical order, beginning with `01_import_datasets.R`.
5. Review the generated tables, listings, and figures in the corresponding `output/` folders.
6. Run `09_tlf_qc.R` for consolidated QC.

The scripts use project-relative paths through the `here` package to support reproducibility.

## Scope and Limitations

- This is an independent learning and portfolio project using example clinical trial datasets.
- The work demonstrates descriptive TLF programming rather than a complete clinical study reporting or regulatory submission process.
- The available datasets determine which analyses and listings can be developed.
- No concomitant medication listing is included because an ADCM dataset was not available.
- L03 is therefore developed as a laboratory results listing using ADLBC.
- The laboratory figure uses ALT as an illustrative laboratory parameter.
- The project does not claim formal validation under a regulated clinical programming environment.

## Project Status

The project includes development of seven summary tables, three subject-level listings, and two figures, with programming QC checks incorporated into the workflow.

Further improvements may include:

- Additional laboratory and safety figures.
- More extensive independent QC programming.
- Automated comparison of production and QC datasets.
- Standardized table and listing formatting.
- Additional clinical reporting outputs.

## Feedback and Suggestions

This project is part of my continued learning in clinical programming and statistical reporting using R.

I welcome feedback and suggestions from professionals working in:

- Clinical Programming
- Statistical Programming
- Clinical Data Management
- Clinical Data Science
- Biostatistics
- CDISC and clinical data standards

Suggestions on programming practices, clinical reporting conventions, QC approaches, and improvements to the workflow would be appreciated.

---

**GitHub:** [https://github.com/barsa98](https://github.com/barsa98)

*This project is intended for educational, skill-development, and portfolio demonstration purposes.*
