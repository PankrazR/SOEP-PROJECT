########################
#                      #
#  SUMMARY STATISTICS  #
#                      #
########################

# ------------------------------------------------------------------------------
# Berechne zusammenfassende Statistiken für hourly_wages_2020 mit Quantilen
summary_hourly_wages <- dat_cleaned %>%
  filter(syear %in% seq(1990, 2020, by = 5)) %>%
  group_by(syear) %>%
  summarise(
    observations = n(),
    Mean = mean(hourly_wage_2020, na.rm = TRUE),
    Std_dev = sd(hourly_wage_2020, na.rm = TRUE),
    Min = min(hourly_wage_2020, na.rm = TRUE),
    Q1 = quantile(hourly_wage_2020, 0.25, na.rm = TRUE),
    Median = median(hourly_wage_2020, na.rm = TRUE),
    Q3 = quantile(hourly_wage_2020, 0.75, na.rm = TRUE),
    Max = max(hourly_wage_2020, na.rm = TRUE)
  )

# Erstelle die LaTeX-Tabelle
print(xtable(summary_hourly_wages, caption = "Summary Statistics for Hourly Wages (2020 Adjusted)"), 
      include.rownames = FALSE)

# ------------------------------------------------------------------------------

# Berechne zusammenfassende Statistiken für current_weekly_hrs_worked mit Quantilen
summary_hours_worked <- dat_cleaned %>%
  filter(syear %in% seq(1990, 2020, by = 5)) %>%
  group_by(syear) %>%
  summarise(
    observations = n(),
    Mean = mean(current_weekly_hrs_worked, na.rm = TRUE),
    Std_dev = sd(current_weekly_hrs_worked, na.rm = TRUE),
    Min = min(current_weekly_hrs_worked, na.rm = TRUE),
    Q1 = quantile(current_weekly_hrs_worked, 0.25, na.rm = TRUE),
    Median = median(current_weekly_hrs_worked, na.rm = TRUE),
    Q3 = quantile(current_weekly_hrs_worked, 0.75, na.rm = TRUE),
    Max = max(current_weekly_hrs_worked, na.rm = TRUE)
  )

# Erstelle die LaTeX-Tabelle
print(xtable(summary_hours_worked, caption = "Summary Statistics for Weekly Hours Worked"), 
      include.rownames = FALSE)

# ------------------------------------------------------------------------------

# Berechne zusammenfassende Statistiken für current_gross_labor_inc_2020 mit Quantilen
summary_gross_income <- dat_cleaned %>%
  filter(syear %in% seq(1990, 2020, by = 5)) %>%
  group_by(syear) %>%
  summarise(
    observations = n(),
    Mean = mean(current_gross_labor_inc_2020, na.rm = TRUE),
    Std_dev = sd(current_gross_labor_inc_2020, na.rm = TRUE),
    Min = min(current_gross_labor_inc_2020, na.rm = TRUE),
    Q1 = quantile(current_gross_labor_inc_2020, 0.25, na.rm = TRUE),
    Median = median(current_gross_labor_inc_2020, na.rm = TRUE),
    Q3 = quantile(current_gross_labor_inc_2020, 0.75, na.rm = TRUE),
    Max = max(current_gross_labor_inc_2020, na.rm = TRUE)
  )

# Erstelle die LaTeX-Tabelle
print(xtable(summary_gross_income, caption = "Summary Statistics for Gross Labor Income (2020 Adjusted)"), 
      include.rownames = FALSE)

# ------------------------------------------------------------------------------
