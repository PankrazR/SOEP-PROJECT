##################################
#                                #
#  INEQUALITY MEASURES EARNINGS  #
#                                #
##################################

#-------------------------------------------------------------------------------

# Step 1: Log transformation of earnings using PGLABGRO
dat_cleaned <- dat_cleaned %>%
  mutate(log_earnings = log(current_gross_labor_inc_2020))

# Step 2: Calculate standard deviation of log earnings
std_dev_log_earnings <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(std_dev_log_earnings = sqrt(wtd.var(log_earnings, weights = w11105, na.rm = TRUE)))

# Step 3: Calculate P90-P50 for log earnings
P90_P50_earnings <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(P90_P50_earnings = wtd.quantile(log_earnings, weights = w11105, probs = 0.90, na.rm = TRUE) - 
              wtd.quantile(log_earnings, weights = w11105, probs = 0.50, na.rm = TRUE))

# Step 4: Calculate P50-P10 for log earnings
P50_P10_earnings <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(P50_P10_earnings = wtd.quantile(log_earnings, weights = w11105, probs = 0.50, na.rm = TRUE) - 
              wtd.quantile(log_earnings, weights = w11105, probs = 0.10, na.rm = TRUE))

# Step 5: Calculate the weighted share of earnings below 2/3 of the median
share_below_2_3_median_earnings <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(share_below_2_3_median_earnings = 
              weighted.mean(log_earnings < log(2/3 * exp(wtd.quantile(log_earnings, weights = w11105, probs = 0.50, na.rm = TRUE))), 
                            w11105, na.rm = TRUE))

#-------------------------------------------------------------------------------

# Plot Weighted Standard Deviation of Log Earnings
ggplot(data = std_dev_log_earnings, aes(x = syear, y = std_dev_log_earnings)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Weighted Standard Deviation of Log Earnings", 
       x = "Year", 
       y = "Standard Deviation") +
  theme_minimal()

# Plot Weighted Difference Between P90 and P50 for Log Earnings
ggplot(data = P90_P50_earnings, aes(x = syear, y = P90_P50_earnings)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Weighted Difference Between P90 and P50 for Log Earnings", 
       x = "Year", 
       y = "Difference (P90 - P50)") +
  theme_minimal()

# Plot Weighted Difference Between P50 and P10 for Log Earnings
ggplot(data = P50_P10_earnings, aes(x = syear, y = P50_P10_earnings)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Weighted Difference Between P50 and P10 for Log Earnings", 
       x = "Year", 
       y = "Difference (P50 - P10)") +
  theme_minimal()

# Plot Weighted Share of Jobs Paying Less than 2/3 of the Median for Earnings
ggplot(data = share_below_2_3_median_earnings, aes(x = syear, y = share_below_2_3_median_earnings)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Weighted Share of Jobs Paying Less than 2/3 of the Median for Earnings", 
       x = "Year", 
       y = "Share Below 2/3 of Median Earnings") +
  theme_minimal()
#-------------------------------------------------------------------------------

############################
#                          #
# GENDER SPECIFIC ANALYSIS #
#                          #
############################

#-------------------------------------------------------------------------------

# Step 2: Calculate standard deviation of log earnings by gender
std_dev_log_earnings_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(std_dev_log_earnings = sqrt(wtd.var(log_earnings, weights = w11105, na.rm = TRUE)))

# Step 3: Calculate P90-P50 for log earnings by gender
P90_P50_earnings_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(P90_P50_earnings = wtd.quantile(log_earnings, weights = w11105, probs = 0.90, na.rm = TRUE) - 
              wtd.quantile(log_earnings, weights = w11105, probs = 0.50, na.rm = TRUE))

# Step 4: Calculate P50-P10 for log earnings by gender
P50_P10_earnings_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(P50_P10_earnings = wtd.quantile(log_earnings, weights = w11105, probs = 0.50, na.rm = TRUE) - 
              wtd.quantile(log_earnings, weights = w11105, probs = 0.10, na.rm = TRUE))


# Step 5: Calculate the weighted share of earnings below 2/3 of the median by gender
share_below_2_3_median_earnings_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(share_below_2_3_median_earnings = 
              weighted.mean(log_earnings < log(2/3 * exp(wtd.quantile(log_earnings, weights = w11105, probs = 0.50, na.rm = TRUE))), 
                            w11105, na.rm = TRUE))

#-------------------------------------------------------------------------------

# Plot 1: Standard deviation of log earnings by gender
ggplot(data = std_dev_log_earnings_gender, aes(x = syear, y = std_dev_log_earnings, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Standard Deviation of Log Earnings by Gender", 
       x = "Year", 
       y = "Standard Deviation",
       color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot 2: Weighted difference between P90 and P50 for log earnings by gender
ggplot(data = P90_P50_earnings_gender, aes(x = syear, y = P90_P50_earnings, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Weighted Difference Between P90 and P50 for Log Earnings by Gender", 
       x = "Year", 
       y = "Difference (P90 - P50)",
       color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot 3: Weighted difference between P50 and P10 for log earnings by gender
ggplot(data = P50_P10_earnings_gender, aes(x = syear, y = P50_P10_earnings, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Weighted Difference Between P50 and P10 for Log Earnings by Gender", 
       x = "Year", 
       y = "Difference (P50 - P10)",
       color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot 4: Weighted share of earnings below 2/3 of the median by gender
ggplot(data = share_below_2_3_median_earnings_gender, aes(x = syear, y = share_below_2_3_median_earnings, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Share of Earnings Below 2/3 of Median by Gender", 
       x = "Year", 
       y = "Share Below 2/3 of Median Earnings",
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

# Plot Weighted Standard Deviation of Log Earnings
ggplot(data = std_dev_log_earnings, aes(x = syear, y = std_dev_log_earnings)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# Plot Weighted Difference Between P90 and P50 for Log Earnings
ggplot(data = P90_P50_earnings, aes(x = syear, y = P90_P50_earnings)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# Plot Weighted Difference Between P50 and P10 for Log Earnings
ggplot(data = P50_P10_earnings, aes(x = syear, y = P50_P10_earnings)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# Plot Weighted Share of Jobs Paying Less than 2/3 of the Median for Earnings
ggplot(data = share_below_2_3_median_earnings, aes(x = syear, y = share_below_2_3_median_earnings)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

#-------------------------------------------------------------------------------

# Plot 1: Standard deviation of log earnings by gender
ggplot(data = std_dev_log_earnings_gender, aes(x = syear, y = std_dev_log_earnings, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(x = "", y = "", color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# Plot 2: Weighted difference between P90 and P50 for log earnings by gender
ggplot(data = P90_P50_earnings_gender, aes(x = syear, y = P90_P50_earnings, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(x = "", y = "", color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# Plot 3: Weighted difference between P50 and P10 for log earnings by gender
ggplot(data = P50_P10_earnings_gender, aes(x = syear, y = P50_P10_earnings, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(x = "", y = "", color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# Plot 4: Weighted share of earnings below 2/3 of the median by gender
ggplot(data = share_below_2_3_median_earnings_gender, aes(x = syear, y = share_below_2_3_median_earnings, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(x = "", y = "", color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

#-------------------------------------------------------------------------------
