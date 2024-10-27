###############################
#                             #
#  INEQUALITY MEASURES HOURS  #
#                             #
###############################

#-------------------------------------------------------------------------------

# Log transformation of hours worked
dat_cleaned <- dat_cleaned %>%
  mutate(log_hours_worked = log(current_weekly_hrs_worked))

# Calculate standard deviation of log hours worked
std_dev_log_hours <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(std_dev_log_hours = sqrt(wtd.var(log_hours_worked, weights = w11105, na.rm = TRUE)))

# Calculate P90-P50 for log hours worked
P90_P50_hours <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(P90_P50_hours = wtd.quantile(log_hours_worked, weights = w11105, probs = 0.90, na.rm = TRUE) - 
              wtd.quantile(log_hours_worked, weights = w11105, probs = 0.50, na.rm = TRUE))

# Calculate P50-P10 for log hours worked
P50_P10_hours <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(P50_P10_hours = wtd.quantile(log_hours_worked, weights = w11105, probs = 0.50, na.rm = TRUE) - 
              wtd.quantile(log_hours_worked, weights = w11105, probs = 0.10, na.rm = TRUE))

# Calculate the weighted share of part-time jobs
# Assuming "part-time" is defined as working fewer than 30 hours per week
share_part_time <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(share_part_time = 
              weighted.mean(current_weekly_hrs_worked < 30, w11105, na.rm = TRUE))

#-------------------------------------------------------------------------------

# Plot standard deviation of log hours worked
ggplot(data = std_dev_log_hours, aes(x = syear, y = std_dev_log_hours)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Standard Deviation of Log Hours Worked", 
       x = "Year", 
       y = "Standard Deviation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot weighted difference between P90 and P50 for log hours worked
ggplot(data = P90_P50_hours, aes(x = syear, y = P90_P50_hours)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Weighted Difference Between P90 and P50 for Log Hours Worked", 
       x = "Year", 
       y = "Difference (P90 - P50)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot weighted difference between P50 and P10 for log hours worked
ggplot(data = P50_P10_hours, aes(x = syear, y = P50_P10_hours)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Weighted Difference Between P50 and P10 for Log Hours Worked", 
       x = "Year", 
       y = "Difference (P50 - P10)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot weighted share of part-time jobs
ggplot(data = share_part_time, aes(x = syear, y = share_part_time)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Weighted Share of Part-Time Jobs (Less than 30 Hours/Week)", 
       x = "Year", 
       y = "Share of Part-Time Jobs") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------

############################
#                          #
# GENDER SPECIFIC ANALYSIS #
#                          #
############################

#-------------------------------------------------------------------------------

# Calculate standard deviation of log hours worked by gender
std_dev_log_hours_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(std_dev_log_hours = sqrt(wtd.var(log_hours_worked, weights = w11105, na.rm = TRUE)))

# Calculate P90-P50 for log hours worked by gender
P90_P50_hours_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(P90_P50_hours = wtd.quantile(log_hours_worked, weights = w11105, probs = 0.90, na.rm = TRUE) - 
              wtd.quantile(log_hours_worked, weights = w11105, probs = 0.50, na.rm = TRUE))

# Calculate P50-P10 for log hours worked by gender
P50_P10_hours_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(P50_P10_hours = wtd.quantile(log_hours_worked, weights = w11105, probs = 0.50, na.rm = TRUE) - 
              wtd.quantile(log_hours_worked, weights = w11105, probs = 0.10, na.rm = TRUE))

# Calculate the weighted share of part-time jobs by gender
# Assuming "part-time" is defined as working fewer than 30 hours per week
share_part_time_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(share_part_time = 
              weighted.mean(current_weekly_hrs_worked < 30, w11105, na.rm = TRUE))

#-------------------------------------------------------------------------------

# Plot standard deviation of log hours worked by gender
ggplot(data = std_dev_log_hours_gender, aes(x = syear, y = std_dev_log_hours, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Standard Deviation of Log Hours Worked by Gender", 
       x = "Year", 
       y = "Standard Deviation",
       color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot weighted difference between P90 and P50 for log hours worked by gender
ggplot(data = P90_P50_hours_gender, aes(x = syear, y = P90_P50_hours, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Weighted Difference Between P90 and P50 for Log Hours Worked by Gender", 
       x = "Year", 
       y = "Difference (P90 - P50)",
       color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot weighted difference between P50 and P10 for log hours worked by gender
ggplot(data = P50_P10_hours_gender, aes(x = syear, y = P50_P10_hours, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Weighted Difference Between P50 and P10 for Log Hours Worked by Gender", 
       x = "Year", 
       y = "Difference (P50 - P10)",
       color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot weighted share of part-time jobs by gender
ggplot(data = share_part_time_gender, aes(x = syear, y = share_part_time, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Weighted Share of Part-Time Jobs by Gender", 
       x = "Year", 
       y = "Share of Part-Time Jobs",
       color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------

###########################
#                         #
#   PLOTS FOR THE PAPER   #
#                         #
###########################

#-------------------------------------------------------------------------------

# Plot standard deviation of log hours worked
ggplot(data = std_dev_log_hours, aes(x = syear, y = std_dev_log_hours)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_text(size = 14))

# Plot weighted difference between P90 and P50 for log hours worked
ggplot(data = P90_P50_hours, aes(x = syear, y = P90_P50_hours)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_text(size = 14))

# Plot weighted difference between P50 and P10 for log hours worked
ggplot(data = P50_P10_hours, aes(x = syear, y = P50_P10_hours)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_text(size = 14))

# Plot weighted share of part-time jobs
ggplot(data = share_part_time, aes(x = syear, y = share_part_time)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_text(size = 14))

#-------------------------------------------------------------------------------

# Plot standard deviation of log hours worked by gender
ggplot(data = std_dev_log_hours_gender, aes(x = syear, y = std_dev_log_hours, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(name = "Gender", values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_text(size = 14))

# Plot weighted difference between P90 and P50 for log hours worked by gender
ggplot(data = P90_P50_hours_gender, aes(x = syear, y = P90_P50_hours, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(name = "Gender", values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_text(size = 14))

# Plot weighted difference between P50 and P10 for log hours worked by gender
ggplot(data = P50_P10_hours_gender, aes(x = syear, y = P50_P10_hours, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(name = "Gender", values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_text(size = 14))

# Plot weighted share of part-time jobs by gender
ggplot(data = share_part_time_gender, aes(x = syear, y = share_part_time, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(name = "Gender", values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_text(size = 14))

#-------------------------------------------------------------------------------



