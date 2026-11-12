# ============================================================
# F01: ADVERSE EVENT INCIDENCE BY TREATMENT GROUP
# Source: ADAE + ADSL
# Population: Safety Population
# ============================================================

library(dplyr)
library(ggplot2)
library(here)

#  Prepare the data

# 1. Safety population and treatment groups
safety_pop <- adsl %>%
  filter(saffl == "Y") %>%
  select(
    USUBJID = usubjid,
    TREATMENT = trt01a
  ) %>%
  distinct()

# 2. Count subjects with at least one TEAE
teae_subjects <- adae %>%
  filter(
    saffl == "Y",
    trtemfl == "Y"
  ) %>%
  distinct(usubjid) %>%
  inner_join(
    safety_pop,
    by = c("usubjid" = "USUBJID")
  ) %>%
  count(TREATMENT, name = "n_teae")

# 3. Calculate safety population denominators
treatment_denominators <- safety_pop %>%
  count(TREATMENT, name = "N")

# 4. Calculate incidence percentage
F01_data <- treatment_denominators %>%
  left_join(teae_subjects, by = "TREATMENT") %>%
  mutate(
    n_teae = coalesce(n_teae, 0L),
    percentage = (n_teae / N) * 100,
    label = sprintf("%d/%d (%.1f%%)", n_teae, N, percentage)
  )

# Inspect results
F01_data




# Generate the figure


F01_plot <- ggplot(
  F01_data,
  aes(x = TREATMENT, y = percentage)
) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = label),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Adverse Event Incidence by Treatment Group",
    subtitle = "Subjects with at least one treatment-emergent adverse event",
    x = "Treatment Group",
    y = "Subjects with TEAE (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

F01_plot





# Export the figure

figure_dir <- here::here("output", "figures")

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

ggsave(
  filename = file.path(figure_dir, "F01_Adverse_Event_Incidence.png"),
  plot = F01_plot,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(figure_dir, "F01_Adverse_Event_Incidence.pdf"),
  plot = F01_plot,
  width = 9,
  height = 6
)








# ============================================================
# F02: MEAN CHANGE FROM BASELINE IN ALT
# Source: ADLBC
# Population: Safety Population
# ============================================================




# Prepare the laboratory data

F02_data <- adlbc %>%
  filter(
    saffl == "Y",
    paramcd == "ALT",
    anl01fl == "Y",
    !is.na(chg)
  ) %>%
  group_by(trta, avisit, avisitn) %>%
  summarise(
    N = n(),
    Mean_CHG = mean(chg, na.rm = TRUE),
    SD = sd(chg, na.rm = TRUE),
    SE = SD / sqrt(N),
    .groups = "drop"
  ) %>%
  arrange(trta, avisitn)

# Inspect the summary
F02_data



# Generate the figure

F02_plot <- ggplot(
  F02_data,
  aes(
    x = avisitn,
    y = Mean_CHG,
    color = trta,
    group = trta
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50"
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      ymin = Mean_CHG - SE,
      ymax = Mean_CHG + SE
    ),
    width = 0.3
  ) +
  scale_x_continuous(
    breaks = sort(unique(F02_data$avisitn)),
    labels = F02_data %>%
      distinct(avisitn, avisit) %>%
      arrange(avisitn) %>%
      pull(avisit)
  ) +
  labs(
    title = "Mean Change in ALT from Baseline",
    subtitle = "By treatment group and study visit",
    x = "Study Visit",
    y = "Mean Change from Baseline (ALT)",
    color = "Treatment Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

F02_plot




# Save the figure

ggsave(
  filename = file.path(figure_dir, "F02_ALT_Mean_Change.png"),
  plot = F02_plot,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  filename = file.path(figure_dir, "F02_ALT_Mean_Change.pdf"),
  plot = F02_plot,
  width = 10,
  height = 6
)


# Basic QC

# Check number of observations per treatment and visit
F02_data %>%
  select(trta, avisit, N)

# Check missing values
colSums(is.na(F02_data))

# Check duplicate treatment-visit combinations
F02_data %>%
  count(trta, avisitn) %>%
  filter(n > 1)




