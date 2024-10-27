############################
#                          #
#  VARIANCE DECOMPOSITION  #
#                          #
############################

#-------------------------------------------------------------------------------

# Compute variance for log wages with weights
var_log_wage <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(var_log_wage = cov.wt(cbind(log_hourly_wage_2020), wt = w11105)$cov[1, 1])

# Compute variance for log hours worked with weights
var_log_hours <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(var_log_hours = cov.wt(cbind(log_hours_worked), wt = w11105)$cov[1, 1])

# Compute weighted covariance between log wages and log hours worked
cov_log_wage_hours <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(cov_log_wage_hours = cov.wt(cbind(log_hourly_wage_2020, log_hours_worked), wt = w11105)$cov[1, 2])

#-------------------------------------------------------------------------------

# Compute variance decomposition for log earnings with weights
var_log_earnings_decomposition <- var_log_wage %>%
  left_join(var_log_hours, by = "syear") %>%
  left_join(cov_log_wage_hours, by = "syear") %>%
  mutate(var_log_earnings = var_log_wage + var_log_hours + 2 * cov_log_wage_hours)

#-------------------------------------------------------------------------------

# Plot variance decomposition results (log earnings, wages, hours, covariance) with larger axis and legend text
ggplot(data = var_log_earnings_decomposition, aes(x = syear)) +
  geom_line(aes(y = var_log_earnings, color = "Variance of Log Earnings"), size = 0.5) +
  geom_point(aes(y = var_log_earnings, color = "Variance of Log Earnings"), size = 1) +
  geom_line(aes(y = var_log_wage, color = "Variance of Log Wages"), size = 0.5) +
  geom_point(aes(y = var_log_wage, color = "Variance of Log Wages"), size = 1) +
  geom_line(aes(y = var_log_hours, color = "Variance of Log Hours"), size = 0.5) +
  geom_point(aes(y = var_log_hours, color = "Variance of Log Hours"), size = 1) +
  geom_line(aes(y = 2 * cov_log_wage_hours, color = "Covariance Term"), size = 0.5) +
  geom_point(aes(y = 2 * cov_log_wage_hours, color = "Covariance Term"), size = 1) +
  labs(x = "Year", y = "Variance", color = "Measure") +
  scale_color_manual(values = c(
    "Variance of Log Earnings" = "blue", 
    "Variance of Log Wages" = "black",
    "Variance of Log Hours" = "red",
    "Covariance Term" = "orange")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14))

#-------------------------------------------------------------------------------

############################
#                          #
# GENDER SPECIFIC ANALYSIS #
#                          #
############################

#-------------------------------------------------------------------------------

# Compute variance for log wages by gender with weights
var_log_wage_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(var_log_wage = cov.wt(cbind(log_hourly_wage_2020), wt = w11105)$cov[1, 1])

# Compute variance for log hours worked by gender with weights
var_log_hours_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(var_log_hours = cov.wt(cbind(log_hours_worked), wt = w11105)$cov[1, 1])

# Compute weighted covariance between log wages and log hours worked by gender
cov_log_wage_hours_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(cov_log_wage_hours = cov.wt(cbind(log_hourly_wage_2020, log_hours_worked), wt = w11105)$cov[1, 2])

#-------------------------------------------------------------------------------

# Compute variance decomposition for log earnings by gender with weights
var_log_earnings_decomposition_gender <- var_log_wage_gender %>%
  left_join(var_log_hours_gender, by = c("syear", "gender")) %>%
  left_join(cov_log_wage_hours_gender, by = c("syear", "gender")) %>%
  mutate(var_log_earnings = var_log_wage + var_log_hours + 2 * cov_log_wage_hours)

#-------------------------------------------------------------------------------

# Plot variance decomposition results by gender with larger axis and legend text
ggplot(data = var_log_earnings_decomposition_gender, aes(x = syear)) +
  geom_line(aes(y = var_log_earnings, color = "Variance of Log Earnings"), size = 0.5) +
  geom_point(aes(y = var_log_earnings, color = "Variance of Log Earnings"), size = 1) +
  geom_line(aes(y = var_log_wage, color = "Variance of Log Wages"), size = 0.5) +
  geom_point(aes(y = var_log_wage, color = "Variance of Log Wages"), size = 1) +
  geom_line(aes(y = var_log_hours, color = "Variance of Log Hours"), size = 0.5) +
  geom_point(aes(y = var_log_hours, color = "Variance of Log Hours"), size = 1) +
  geom_line(aes(y = 2 * cov_log_wage_hours, color = "Covariance Term"), size = 0.5) +
  geom_point(aes(y = 2 * cov_log_wage_hours, color = "Covariance Term"), size = 1) +
  labs(x = "Year", y = "Variance", color = "Measure") +
  scale_color_manual(values = c(
    "Variance of Log Earnings" = "blue", 
    "Variance of Log Wages" = "black",
    "Variance of Log Hours" = "red",
    "Covariance Term" = "orange")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),) +
  facet_wrap(~ gender, labeller = as_labeller(c(`1` = "Men", `2` = "Women")))

#-------------------------------------------------------------------------------

###########################
#                         #
#   PLOTS FOR THE PAPER   #
#                         #
###########################

#-------------------------------------------------------------------------------

# Plot variance decomposition results (log earnings, wages, hours, covariance) with larger axis and legend text
ggplot(data = var_log_earnings_decomposition, aes(x = syear)) +
  geom_line(aes(y = var_log_earnings, color = "Variance of Log Earnings"), size = 0.5) +
  geom_point(aes(y = var_log_earnings, color = "Variance of Log Earnings"), size = 1) +
  geom_line(aes(y = var_log_wage, color = "Variance of Log Wages"), size = 0.5) +
  geom_point(aes(y = var_log_wage, color = "Variance of Log Wages"), size = 1) +
  geom_line(aes(y = var_log_hours, color = "Variance of Log Hours"), size = 0.5) +
  geom_point(aes(y = var_log_hours, color = "Variance of Log Hours"), size = 1) +
  geom_line(aes(y = 2 * cov_log_wage_hours, color = "Covariance Term"), size = 0.5) +
  geom_point(aes(y = 2 * cov_log_wage_hours, color = "Covariance Term"), size = 1) +
  labs(x = "", y = "", color = "") +
  scale_color_manual(values = c(
    "Variance of Log Earnings" = "blue", 
    "Variance of Log Wages" = "black",
    "Variance of Log Hours" = "red",
    "Covariance Term" = "orange")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 14))

#-------------------------------------------------------------------------------

ggplot(data = var_log_earnings_decomposition_gender, aes(x = syear)) +
  geom_line(aes(y = var_log_earnings, color = "Variance of Log Earnings"), size = 0.5) +
  geom_point(aes(y = var_log_earnings, color = "Variance of Log Earnings"), size = 1) +
  geom_line(aes(y = var_log_wage, color = "Variance of Log Wages"), size = 0.5) +
  geom_point(aes(y = var_log_wage, color = "Variance of Log Wages"), size = 1) +
  geom_line(aes(y = var_log_hours, color = "Variance of Log Hours"), size = 0.5) +
  geom_point(aes(y = var_log_hours, color = "Variance of Log Hours"), size = 1) +
  geom_line(aes(y = 2 * cov_log_wage_hours, color = "Covariance Term"), size = 0.5) +
  geom_point(aes(y = 2 * cov_log_wage_hours, color = "Covariance Term"), size = 1) +
  labs(x = "", y = "", color = "") +
  scale_color_manual(values = c(
    "Variance of Log Earnings" = "blue", 
    "Variance of Log Wages" = "black",
    "Variance of Log Hours" = "red",
    "Covariance Term" = "orange")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.text = element_text(size = 14),  # Größe des Legendentextes
    legend.title = element_text(size = 16)  # Größe des Legendentitels
  ) +
  facet_wrap(~ gender, labeller = as_labeller(c(`1` = "Men", `2` = "Women")))

#-------------------------------------------------------------------------------


  