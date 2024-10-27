###############################
#                             #
#  EVOLUTION OF HOURLY WAGES  #
#                             #
###############################

# ------------------------------------------------------------------------------

# Calculate weighted average hourly wage by year
avg_hourly_wage_overall <- dat_cleaned %>%
  group_by(syear) %>%
  dplyr::summarize(avg_hourly_wage = weighted.mean(hourly_wage_2020, w11105, na.rm = TRUE))

# Plot the evolution of weighted average hourly wages (in 2020 Euros)
ggplot(avg_hourly_wage_overall, aes(x = syear, y = avg_hourly_wage)) +
  geom_line(color = "blue", size = 0.75) +
  geom_point(color = "black") +
  labs(title = "Evolution of Weighted Average Hourly Wages (2020 Euros)",
       x = "Year",
       y = "Average Hourly Wage (2020 Euros)") +
  theme_minimal()

# ------------------------------------------------------------------------------

# Calculate weighted average log hourly wage by year
avg_hourly_log_wage <- dat_cleaned %>%
  group_by(syear) %>%
  dplyr::summarize(avg_hourly_log_wage = weighted.mean(log(hourly_wage_2020), w11105, na.rm = TRUE))

# Plot the evolution of weighted log hourly wages
ggplot(avg_hourly_log_wage, aes(x = syear, y = avg_hourly_log_wage)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black") +
  labs(title = "Evolution of Weighted Log Average Hourly Wages (2020 Euros)",
       x = "Year",
       y = "Log Average Hourly Wage") +
  theme_minimal()

# ------------------------------------------------------------------------------

# Calculate weighted average hourly wage grouped by year and gender
avg_hourly_wage <- dat_cleaned %>%
  group_by(syear, gender) %>%
  dplyr::summarize(avg_hourly_wage = weighted.mean(hourly_wage_2020, w11105, na.rm = TRUE))

# Plot the evolution of weighted hourly wages (in 2020 Euros) for men and women separately
ggplot(avg_hourly_wage, aes(x = syear, y = avg_hourly_wage, color = as.factor(gender))) +
  geom_line(size = 0.5) + 
  geom_point() +
  labs(title = "Average Hourly Wage Over the Years by Gender (Weighted)", 
       x = "Year", 
       y = "Average Hourly Wage (in 2020 Euros)", 
       color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), 
                     labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ------------------------------------------------------------------------------

# Berechnung des logarithmierten durchschnittlichen Stundenlohns (weighted)
avg_log_hourly_wage <- dat_cleaned %>%
  group_by(syear, gender) %>%
  dplyr::summarize(avg_log_hourly_wage = weighted.mean(log(hourly_wage_2020), w11105, na.rm = TRUE))

# Plot der Entwicklung des logarithmierten durchschnittlichen Stundenlohns für Männer und Frauen
ggplot(avg_log_hourly_wage, aes(x = syear, y = avg_log_hourly_wage, color = as.factor(gender))) +
  geom_line(size = 0.5) + 
  geom_point() +
  labs(title = "Average Log Hourly Wage Over the Years by Gender (Weighted)", 
       x = "Year", 
       y = "Average Log Hourly Wage", 
       color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), 
                     labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#-------------------------------------------------------------------------------

###########################
#                         #
#   PLOTS FOR THE PAPER   #
#                         #
###########################

#-------------------------------------------------------------------------------

# Plot 1: Evolution of weighted average hourly wages (in 2020 Euros) without axis titles, with larger axis labels
ggplot(avg_hourly_wage_overall, aes(x = syear, y = avg_hourly_wage)) +
  geom_line(color = "blue", size = 0.75) +
  geom_point(color = "black") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# ------------------------------------------------------------------------------

# Plot 2: Evolution of weighted log average hourly wages without axis titles, with larger axis labels
ggplot(avg_hourly_log_wage, aes(x = syear, y = avg_hourly_log_wage)) +
  geom_line(color = "black", size = 0.75) +
  geom_point(color = "black") +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# ------------------------------------------------------------------------------

# Plot 3: Average hourly wage over the years by gender (weighted) without axis titles, with larger axis labels
ggplot(avg_hourly_wage, aes(x = syear, y = avg_hourly_wage, color = as.factor(gender))) +
  geom_line(size = 0.5) + 
  geom_point() +
  labs(x = "", y = "", color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# ------------------------------------------------------------------------------

# Plot 4: Average log hourly wage over the years by gender (weighted) without axis titles, with larger axis labels
ggplot(avg_log_hourly_wage, aes(x = syear, y = avg_log_hourly_wage, color = as.factor(gender))) +
  geom_line(size = 0.5) + 
  geom_point() +
  labs(x = "", y = "", color = "Gender") +
  scale_color_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# ------------------------------------------------------------------------------

