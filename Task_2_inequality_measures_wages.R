###############################
#                             #
#  INEQUALITY MEASURES WAGES  #
#                             #
###############################

#-------------------------------------------------------------------------------

# Calculate Weighted Standard Deviation of Log Hourly Wages
std_dev_log_wage <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(std_dev_log_wage = sqrt(wtd.var(log_hourly_wage_2020, weights = w11105, na.rm = TRUE)))

# Calculate the Weighted Difference Between P90 and P50
P90_P50 <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(P90_P50 = wtd.quantile(log_hourly_wage_2020, weights = w11105, probs = 0.90, na.rm = TRUE) - 
              wtd.quantile(log_hourly_wage_2020, weights = w11105, probs = 0.50, na.rm = TRUE))

# Calculate the Weighted Difference Between P50 and P10
P50_P10 <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(P50_P10 = wtd.quantile(log_hourly_wage_2020, weights = w11105, probs = 0.50, na.rm = TRUE) - 
              wtd.quantile(log_hourly_wage_2020, weights = w11105, probs = 0.10, na.rm = TRUE))

# Calculate the Weighted Share of Jobs Paying Less than 2/3 of the Median
share_below_2_3_median <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(share_below_2_3_median = 
              weighted.mean(log_hourly_wage_2020 < log(2/3 * exp(wtd.quantile(log_hourly_wage_2020, weights = w11105, probs = 0.50, na.rm = TRUE))), 
                            w11105, na.rm = TRUE))

#-------------------------------------------------------------------------------

# Plot Weighted Standard Deviation of Log Hourly Wages
ggplot(data = std_dev_log_wage, aes(x = syear, y = std_dev_log_wage)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Weighted Standard Deviation of Log Hourly Wages", 
       x = "Year", 
       y = "Standard Deviation") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot Weighted Difference Between P90 and P50
ggplot(data = P90_P50, aes(x = syear, y = P90_P50)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Weighted Difference Between P90 and P50", 
       x = "Year", 
       y = "Difference (P90 - P50)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot Weighted Difference Between P50 and P10
ggplot(data = P50_P10, aes(x = syear, y = P50_P10)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Weighted Difference Between P50 and P10", 
       x = "Year", 
       y = "Difference (P50 - P10)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot Weighted Share of Jobs Paying Less than 2/3 of the Median
ggplot(data = share_below_2_3_median, aes(x = syear, y = share_below_2_3_median)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(title = "Weighted Share of Jobs Paying Less than 2/3 of the Median", 
       x = "Year", 
       y = "Share") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------

############################
#                          #
# GENDER SPECIFIC ANALYSIS #
#                          #
############################

#-------------------------------------------------------------------------------

# Berechnung der Standardabweichung der logarithmierten Löhne nach Geschlecht
std_dev_log_wage_gender <- dat_cleaned %>%
  filter(syear >= 1990 & syear <= 2020) %>%
  group_by(syear, gender) %>%
  summarise(std_dev_log_wage = sqrt(wtd.var(log_hourly_wage_2020, weights = w11105, na.rm = TRUE)))

# Berechnung der P90 - P50 Differenz nach Geschlecht
P90_P50_gender <- dat_cleaned %>%
  filter(syear >= 1990 & syear <= 2020) %>%
  group_by(syear, gender) %>%
  summarise(P90_P50 = wtd.quantile(log_hourly_wage_2020, weights = w11105, probs = 0.90, na.rm = TRUE) - 
              wtd.quantile(log_hourly_wage_2020, weights = w11105, probs = 0.50, na.rm = TRUE))

# Berechnung der P50 - P10 Differenz nach Geschlecht
P50_P10_gender <- dat_cleaned %>%
  filter(syear >= 1990 & syear <= 2020) %>%
  group_by(syear, gender) %>%
  summarise(P50_P10 = wtd.quantile(log_hourly_wage_2020, weights = w11105, probs = 0.50, na.rm = TRUE) - 
              wtd.quantile(log_hourly_wage_2020, weights = w11105, probs = 0.10, na.rm = TRUE))

# Berechnung des Anteils der Jobs unter 2/3 des Medians nach Geschlecht
share_below_2_3_median_gender <- dat_cleaned %>%
  filter(syear >= 1990 & syear <= 2020) %>%
  group_by(syear, gender) %>%
  summarise(share_below_2_3_median = 
              weighted.mean(log_hourly_wage_2020 < log(2/3 * exp(wtd.quantile(log_hourly_wage_2020, weights = w11105, probs = 0.50, na.rm = TRUE))), 
                            w11105, na.rm = TRUE))

#-------------------------------------------------------------------------------

# Plot der Standardabweichung der logarithmierten Löhne nach Geschlecht
ggplot(data = std_dev_log_wage_gender, aes(x = syear, y = std_dev_log_wage, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Standard Deviation of Log Wages (by Gender)", 
       x = "Year", 
       y = "Standard Deviation") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), 
                     labels = c("Men", "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())  # Legendentitel entfernen

# Plot der P90 - P50 Differenz nach Geschlecht
ggplot(data = P90_P50_gender, aes(x = syear, y = P90_P50, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "P90 - P50 (by Gender)", 
       x = "Year", 
       y = "Difference (P90 - P50)") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), 
                     labels = c("Men", "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())  # Legendentitel entfernen

# Plot der P50 - P10 Differenz nach Geschlecht
ggplot(data = P50_P10_gender, aes(x = syear, y = P50_P10, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "P50 - P10 (by Gender)", 
       x = "Year", 
       y = "Difference (P50 - P10)") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), 
                     labels = c("Men", "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())  # Legendentitel entfernen

# Plot des Anteils der Jobs unter 2/3 des Medians nach Geschlecht
ggplot(data = share_below_2_3_median_gender, aes(x = syear, y = share_below_2_3_median, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "Share Below 2/3 of Median (by Gender)", 
       x = "Year", 
       y = "Share") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), 
                     labels = c("Men", "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())  # Legendentitel entfernen

#-------------------------------------------------------------------------------

###########################
#                         #
#   PLOTS FOR THE PAPER   #
#                         #
###########################

#-------------------------------------------------------------------------------

# Plot Weighted Standard Deviation of Log Hourly Wages (Ohne Überschrift, größere Achsenbeschriftungen)
ggplot(data = std_dev_log_wage, aes(x = syear, y = std_dev_log_wage)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(x = "", y = "") +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

# Plot Weighted Difference Between P90 and P50 (Ohne Überschrift, größere Achsenbeschriftungen)
ggplot(data = P90_P50, aes(x = syear, y = P90_P50)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(x = "", y = "") +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

# Plot Weighted Difference Between P50 and P10 (Ohne Überschrift, größere Achsenbeschriftungen)
ggplot(data = P50_P10, aes(x = syear, y = P50_P10)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(x = "", y = "") +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

# Plot Weighted Share of Jobs Paying Less than 2/3 of the Median (Ohne Überschrift, größere Achsenbeschriftungen)
ggplot(data = share_below_2_3_median, aes(x = syear, y = share_below_2_3_median)) +
  geom_line(color = "blue", size = 0.5) +
  geom_point(size = 1, color = "blue") +
  labs(x = "", y = "") +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

#-------------------------------------------------------------------------------

# Plot der Standardabweichung der logarithmierten Löhne nach Geschlecht (Ohne Überschrift, größere Achsenbeschriftungen)
ggplot(data = std_dev_log_wage_gender, aes(x = syear, y = std_dev_log_wage, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(x = "", y = "", color = "Gender") +  
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("Men", "Women")) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

# Plot der P90 - P50 Differenz nach Geschlecht (Ohne Überschrift, größere Achsenbeschriftungen)
ggplot(data = P90_P50_gender, aes(x = syear, y = P90_P50, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(x = "", y = "", color = "Gender") + 
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("Men", "Women")) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

# Plot der P50 - P10 Differenz nach Geschlecht (Ohne Überschrift, größere Achsenbeschriftungen)
ggplot(data = P50_P10_gender, aes(x = syear, y = P50_P10, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(x = "", y = "", color = "Gender") + 
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("Men", "Women")) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

# Plot des Anteils der Jobs unter 2/3 des Medians nach Geschlecht (Ohne Überschrift, größere Achsenbeschriftungen)
ggplot(data = share_below_2_3_median_gender, aes(x = syear, y = share_below_2_3_median, color = as.factor(gender))) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(x = "", y = "", color = "Gender") + 
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("Men", "Women")) +
  theme_minimal() + 
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14), 
        legend.text = element_text(size = 14))

#-------------------------------------------------------------------------------
