#########################
#                       #
#  PERCENTILE ANALYSIS  #
#                       #
#########################

# ------------------------------------------------------------------------------

# Logarithmic transformation of hourly wages
dat_cleaned <- dat_cleaned %>%
  mutate(log_hourly_wage = log(hourly_wage_2020))

# Calculate weighted percentiles for log-transformed wages (1990-2020)
percentiles_weighted_log <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(
    p10 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.10, na.rm = TRUE),
    p25 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.25, na.rm = TRUE),
    p50 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.50, na.rm = TRUE),
    p75 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.75, na.rm = TRUE),
    p90 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.90, na.rm = TRUE),
    p95 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.95, na.rm = TRUE),
    p99 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.99, na.rm = TRUE)
  )

#-------------------------------------------------------------------------------

# Extract percentiles for the year 2005
percentiles_2005_log <- percentiles_weighted_log %>%
  filter(syear == 2005) %>%
  select(-syear)

# Normalize the percentiles relative to 2005
percentiles_normalized_log <- percentiles_weighted_log %>%
  mutate(across(starts_with("p"), ~ .x - percentiles_2005_log[[cur_column()]]))

#-------------------------------------------------------------------------------

# Convert to long format for visualization
percentiles_long_weighted_log <- percentiles_normalized_log %>%
  pivot_longer(cols = starts_with("p"), 
               names_to = "percentile", 
               values_to = "value")

# ------------------------------------------------------------------------------

# Visualization of normalized weighted percentiles for log wages (1990-2020)
ggplot(data = percentiles_long_weighted_log, aes(x = syear, y = value, color = percentile)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Normalized Weighted Percentiles of Log Hourly Wages (Relative to 2005, 1990-2020)",
       x = "Year",
       y = "Normalized Log Hourly Wage",
       color = "Percentile") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------

############################
#                          #
# GENDER SPECIFIC ANALYSIS #
#                          #
############################

#-------------------------------------------------------------------------------

# Percentiles for log wages for men (1990-2020)
percentiles_men_log <- dat_cleaned %>%
  filter(gender == 1) %>%
  group_by(syear) %>%
  summarise(
    p10 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.10, na.rm = TRUE),
    p25 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.25, na.rm = TRUE),
    p50 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.50, na.rm = TRUE),
    p75 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.75, na.rm = TRUE),
    p90 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.90, na.rm = TRUE),
    p95 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.95, na.rm = TRUE),
    p99 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.99, na.rm = TRUE)
  )

#-------------------------------------------------------------------------------

# Percentiles for log wages for women (1990-2020)
percentiles_women_log <- dat_cleaned %>%
  filter(gender == 2) %>%
  group_by(syear) %>%
  summarise(
    p10 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.10, na.rm = TRUE),
    p25 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.25, na.rm = TRUE),
    p50 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.50, na.rm = TRUE),
    p75 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.75, na.rm = TRUE),
    p90 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.90, na.rm = TRUE),
    p95 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.95, na.rm = TRUE),
    p99 = wtd.quantile(log_hourly_wage, weights = w11105, probs = 0.99, na.rm = TRUE)
  )

#-------------------------------------------------------------------------------

# Normalize percentiles for men relative to 2005
percentiles_2005_men_log <- percentiles_men_log %>%
  filter(syear == 2005) %>%
  select(-syear)

percentiles_normalized_men_log <- percentiles_men_log %>%
  mutate(across(starts_with("p"), ~ .x - percentiles_2005_men_log[[cur_column()]]))

#-------------------------------------------------------------------------------

# Normalize percentiles for women relative to 2005
percentiles_2005_women_log <- percentiles_women_log %>%
  filter(syear == 2005) %>%
  select(-syear)

percentiles_normalized_women_log <- percentiles_women_log %>%
  mutate(across(starts_with("p"), ~ .x - percentiles_2005_women_log[[cur_column()]]))

#-------------------------------------------------------------------------------

# Convert to long format for men (log wages, 2005-2020)
percentiles_long_men_log <- percentiles_normalized_men_log %>%
  pivot_longer(cols = starts_with("p"), 
               names_to = "percentile", 
               values_to = "value")

# Convert to long format for women (log wages, 2005-2020)
percentiles_long_women_log <- percentiles_normalized_women_log %>%
  pivot_longer(cols = starts_with("p"), 
               names_to = "percentile", 
               values_to = "value")

#-------------------------------------------------------------------------------

# Plot for men (log wages, 1990-2020)
ggplot(data = percentiles_long_men_log, aes(x = syear, y = value, color = percentile)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Normalized Percentiles of Log Hourly Wages for Men (Relative to 2005, 1990-2020)", 
       x = "Year", 
       y = "Normalized Log Hourly Wages") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# Plot for women (log wages, 1990-2020)
ggplot(data = percentiles_long_women_log, aes(x = syear, y = value, color = percentile)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Normalized Percentiles of Log Hourly Wages for Women (Relative to 2005, 1990-2020)", 
       x = "Year", 
       y = "Normalized Log Hourly Wages") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# ------------------------------------------------------------------------------

###########################
#                         #
#   PLOTS FOR THE PAPER   #
#                         #
###########################

#-------------------------------------------------------------------------------

# Plot 1: Normalized weighted percentiles of log hourly wages (1990-2020)
ggplot(data = percentiles_long_weighted_log, aes(x = syear, y = value, color = percentile)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(x = "", y = "", color = "Percentile") +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

# ------------------------------------------------------------------------------

# Plot 2: Normalized percentiles of log hourly wages for men (1990-2020)
ggplot(data = percentiles_long_men_log, aes(x = syear, y = value, color = percentile)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(x = "", y = "", color = "Percentile") +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

# ------------------------------------------------------------------------------

# Plot 3: Normalized percentiles of log hourly wages for women (1990-2020)
ggplot(data = percentiles_long_women_log, aes(x = syear, y = value, color = percentile)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(x = "", y = "", color = "Percentile") +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

# ------------------------------------------------------------------------------
