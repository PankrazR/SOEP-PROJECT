###################################
#                                 #
#  DATA PREPERATION AND CLEANING  #
#                                 #
###################################

# ------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(spatstat)
library(stats)
library(Hmisc)
library(readxl)
library(plm)
library(ineq)
library(knitr)
library(kableExtra)
library(xtable)


# ------------------------------------------------------------------------------
# Load data
# ------------------------------------------------------------------------------

dat1 <- read.csv("C:/SOEP-PROJECT/data/pequiv.csv")
dat2 <- read.csv("C:/SOEP-PROJECT/data/pgen.csv")

# ------------------------------------------------------------------------------
# Merge datasets by 'pid' and 'syear'
# ------------------------------------------------------------------------------

dat_merged <- merge(dat1, dat2, by = c("pid", "syear"))

# ------------------------------------------------------------------------------
# Rename important variables
# ------------------------------------------------------------------------------

dat_merged <- dat_merged %>%
  rename(
    age = d11101,
    gender = d11102ll,
    school_leaving_degree = pgpsbil,
    vocational_degree_received = pgpbbil01,
    college_degree = pgpbbil02,
    no_vocational_degree = pgpbbil03,
    employment_status_detailed = pgemplst,
    employment_status_simple = e11102,
    employment_level = e11103,
    current_gross_labor_inc = pglabgro,
    current_weekly_hrs_worked = pgvebzeit,
    occupational_position = pgstib,
    east_west = l11102
  )

# ------------------------------------------------------------------------------
# Filter out NAs and invalid values
# ------------------------------------------------------------------------------

dat_cleaned <- dat_merged %>%
  filter(current_gross_labor_inc > 0, current_weekly_hrs_worked > 0) %>%
  filter(gender != -1, east_west != -1) %>%
  filter(school_leaving_degree >= 0) %>%
  filter(occupational_position >= 0 | is.na(occupational_position))

# ------------------------------------------------------------------------------
# Filter data for the years 1990 to 2020
# ------------------------------------------------------------------------------

dat_cleaned <- dat_cleaned %>%
  filter(syear >= 1990 & syear <= 2020)

# ------------------------------------------------------------------------------
# Apply sample restrictions
# ------------------------------------------------------------------------------

dat_cleaned <- dat_cleaned %>%
  filter(age >= 20 & age <= 64) %>%
  filter(employment_status_simple == 1) %>%
  filter(!(occupational_position %in% c(10, 11, 12, 13, 110:150, 410:440)))

# ------------------------------------------------------------------------------
# Calculate hourly wage
# ------------------------------------------------------------------------------

dat_cleaned <- dat_cleaned %>%
  mutate(hourly_wage = current_gross_labor_inc / (current_weekly_hrs_worked * 4.33))

# ------------------------------------------------------------------------------
# Calculate median hourly wage and threshold
# ------------------------------------------------------------------------------

dat_cleaned <- dat_cleaned %>%
  group_by(syear) %>%
  mutate(
    median_hourly_wage = median(hourly_wage, na.rm = TRUE),
    threshold = 20 * 0.5 * median_hourly_wage
  ) %>%
  ungroup() %>%
  filter(current_gross_labor_inc >= threshold)

# ------------------------------------------------------------------------------
# Remove top 0.05% of the wage distribution
# ------------------------------------------------------------------------------

quantil <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(upper_threshold = quantile(current_gross_labor_inc, 0.995, na.rm = TRUE))

dat_cleaned <- dat_cleaned %>%
  left_join(quantil, by = "syear") %>%
  filter(current_gross_labor_inc <= upper_threshold)

# ------------------------------------------------------------------------------
# CPI adjustment for wages and income
# ------------------------------------------------------------------------------

cpi_2020 <- dat_cleaned %>%
  filter(syear == 2020) %>%
  summarise(cpi_2020 = mean(y11101, na.rm = TRUE)) %>%
  pull(cpi_2020)

dat_cleaned <- dat_cleaned %>%
  mutate(
    cpi_adjusted = (y11101 / cpi_2020),
    hourly_wage_2020 = hourly_wage / cpi_adjusted,
    current_gross_labor_inc_2020 = current_gross_labor_inc / cpi_adjusted
  )

# ------------------------------------------------------------------------------
# Calculate average hourly wage
# ------------------------------------------------------------------------------

avg_hourly_wage <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(avg_hourly_wage = mean(hourly_wage_2020, na.rm = TRUE))

dat_cleaned <- dat_cleaned %>%
  left_join(avg_hourly_wage, by = "syear") %>%
  mutate(log_hourly_wage_2020 = log(hourly_wage_2020))

# ------------------------------------------------------------------------------
# Calculate average log hourly wage
# ------------------------------------------------------------------------------

avg_hourly_log_wage <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(avg_hourly_log_wage = mean(log_hourly_wage_2020, na.rm = TRUE))

dat_cleaned <- dat_cleaned %>%
  left_join(avg_hourly_log_wage, by = "syear") %>%
  filter(log_hourly_wage_2020 >= 0)

# ------------------------------------------------------------------------------
# Clean and convert ISCO codes
# ------------------------------------------------------------------------------

isco_mapping <- read_excel("C:/SOEP-PROJECT/data/mapping.xlsx")

isco_mapping_clean <- isco_mapping %>%
  select(`ISCO-88 code`, `ISCO-08 code`) %>%
  rename(ISCO_88 = `ISCO-88 code`, ISCO_08 = `ISCO-08 code`) %>%
  distinct(ISCO_88, .keep_all = TRUE)

dat_cleaned <- dat_cleaned %>%
  mutate(
    e11105_v1 = ifelse(e11105_v1 <= 0, NA, e11105_v1),
    e11105_v2 = ifelse(e11105_v2 <= 0, NA, e11105_v2)
  ) %>%
  left_join(isco_mapping_clean, by = c("e11105_v1" = "ISCO_88")) %>%
  mutate(
    isco_converted = ifelse(!is.na(ISCO_08), ISCO_08, e11105_v1),
    isco_final = ifelse(!is.na(e11105_v2), e11105_v2, isco_converted),
    isco_08_first_digit = substr(isco_final, 1, 1)
  )

# ------------------------------------------------------------------------------
# Clean NACE codes and create unified variables
# ------------------------------------------------------------------------------

dat_cleaned <- dat_cleaned %>%
  mutate(
    pgnace = ifelse(pgnace <= 0, NA, pgnace),
    pgnace2 = ifelse(pgnace2 <= 0, NA, pgnace2),
    unified_industry = case_when(
      !is.na(pgnace2) ~ as.integer(pgnace2 %/% 10),
      is.na(pgnace2) & !is.na(pgnace) ~ as.integer(pgnace %/% 10)
    ),
    industry_group = case_when(
      unified_industry == 1 ~ "Agriculture, Forestry, and Fishing",
      unified_industry == 2 ~ "Mining and Quarrying",
      unified_industry %in% 3:5 ~ "Manufacturing and Utilities",
      unified_industry == 6 ~ "Construction",
      unified_industry == 7 ~ "Trade and Retail",
      unified_industry == 8 ~ "Transportation and Logistics",
      unified_industry == 9 ~ "Accommodation and Food Services",
      unified_industry == 10 ~ "Financial and Real Estate Services",
      unified_industry == 11 ~ "Professional, Technical, and Admin. Services",
      unified_industry %in% 12:14 ~ "Public Services, Health, and Education",
      unified_industry %in% 15:16 ~ "Arts, Recreation, and Other Services",
      TRUE ~ "Other/Unknown"
    )
  )

# ------------------------------------------------------------------------------

dat_cleaned <- dat_cleaned %>%
  mutate(
    log_hours_worked = log(current_weekly_hrs_worked),
    log_earnings_2020 = log(current_gross_labor_inc_2020)
  )

# ------------------------------------------------------------------------------

