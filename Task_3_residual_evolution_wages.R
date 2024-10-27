# Berechnung der residualen Löhne für jedes Jahr separat
dat_cleaned <- dat_cleaned %>%
  group_by(syear) %>%
  mutate(residual_wages = residuals(
    lm(log(hourly_wage_2020) ~ gender + age * gender + school_leaving_degree + 
         factor(industry_group) + current_weekly_hrs_worked + employment_status_detailed + 
         factor(isco_08_first_digit) + east_west + poly(age, 2), 
       data = cur_data(), weights = w11105)
  )) %>%
  ungroup()

#-------------------------------------------------------------------------------

# Berechnung der Standardabweichung der residualen Löhne
std_dev_residual_log_wage <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(std_dev_residual_log_wage = sqrt(wtd.var(residual_wages, weights = w11105, na.rm = TRUE)))

# Vergleich der Standardabweichung von ursprünglichen vs. residualen Löhnen
inequality_comparison <- std_dev_log_wage %>%
  left_join(std_dev_residual_log_wage, by = "syear", suffix = c("_original", "_residual")) %>%
  pivot_longer(cols = starts_with("std_dev"), names_to = "type", values_to = "value")


# Plot der Standardabweichung (ursprünglich vs. residual)
ggplot(data = inequality_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "", 
       x = "Year", 
       y = "Standard Deviation") +
  scale_color_manual(values = c("std_dev_log_wage" = "blue",  # Ursprüngliche Löhne in Blau
                                "std_dev_residual_log_wage" = "red")) +  # Residuale Löhne in Rot
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())  # Legendentitel entfernen

#-------------------------------------------------------------------------------

# Berechnung der P90 - P50 Differenz für residuale Löhne
P90_P50_residual <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(P90_P50_residual = wtd.quantile(residual_wages, weights = w11105, probs = 0.90, na.rm = TRUE) - 
              wtd.quantile(residual_wages, weights = w11105, probs = 0.50, na.rm = TRUE))

# Vergleich von P90 - P50 (ursprünglich vs. residual)
P90_P50_comparison <- P90_P50 %>%
  left_join(P90_P50_residual, by = "syear", suffix = c("_original", "_residual")) %>%
  pivot_longer(cols = starts_with("P90_P50"), names_to = "type", values_to = "value")

# Plot der P90 - P50 Differenz (ursprünglich vs. residual)
ggplot(data = P90_P50_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "", 
       x = "Year", 
       y = "Difference (P90 - P50)") +
  scale_color_manual(values = c("P90_P50" = "blue",  # Ursprüngliche Löhne in Blau
                                "P90_P50_residual" = "red")) +  # Residuale Löhne in Rot
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())  # Legendentitel entfernen

#-------------------------------------------------------------------------------

# Berechnung der P50 - P10 Differenz für residuale Löhne
P50_P10_residual <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(P50_P10_residual = wtd.quantile(residual_wages, weights = w11105, probs = 0.50, na.rm = TRUE) - 
              wtd.quantile(residual_wages, weights = w11105, probs = 0.10, na.rm = TRUE))

# Vergleich von P50 - P10 (ursprünglich vs. residual)
P50_P10_comparison <- P50_P10 %>%
  left_join(P50_P10_residual, by = "syear", suffix = c("_original", "_residual")) %>%
  pivot_longer(cols = starts_with("P50_P10"), names_to = "type", values_to = "value")

# Plot der P50 - P10 Differenz (ursprünglich vs. residual)
ggplot(data = P50_P10_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "", 
       x = "Year", 
       y = "Difference (P50 - P10)") +
  scale_color_manual(values = c("P50_P10" = "blue",  # Ursprüngliche Löhne in Blau
                                "P50_P10_residual" = "red")) +  # Residuale Löhne in Rot
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())  # Legendentitel entfernen

#-------------------------------------------------------------------------------

# Berechnung des Anteils der Jobs unter 2/3 des Medians für residuale Löhne
share_below_2_3_median_residual <- dat_cleaned %>%
  group_by(syear) %>%
  summarise(share_below_2_3_median_residual = 
              weighted.mean(residual_wages < log(2/3 * exp(wtd.quantile(residual_wages, weights = w11105, probs = 0.50, na.rm = TRUE))), 
                            w11105, na.rm = TRUE))

# Vergleich des Anteils unter 2/3 des Medians (ursprünglich vs. residual)
share_below_2_3_median_comparison <- share_below_2_3_median %>%
  left_join(share_below_2_3_median_residual, by = "syear", suffix = c("_original", "_residual")) %>%
  pivot_longer(cols = starts_with("share_below_2_3_median"), names_to = "type", values_to = "value")

# Plot des Anteils unter 2/3 des Medians (ursprünglich vs. residual)
ggplot(data = share_below_2_3_median_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "", 
       x = "Year", 
       y = "Share") +
  scale_color_manual(values = c("share_below_2_3_median" = "blue",  # Ursprüngliche Löhne in Blau
                                "share_below_2_3_median_residual" = "red")) +  # Residuale Löhne in Rot
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())  # Legendentitel entfernen

#-------------------------------------------------------------------------------

############################
#                          #
# GENDER SPECIFIC ANALYSIS #
#                          #
############################

#-------------------------------------------------------------------------------

# Berechnung der residualen Löhne nach Geschlecht
dat_cleaned <- dat_cleaned %>%
  group_by(syear, gender) %>%
  mutate(residual_wages_gender = residuals(
    lm(log(hourly_wage_2020) ~ age + school_leaving_degree + 
         factor(industry_group) + current_weekly_hrs_worked + employment_status_detailed + 
         factor(isco_08_first_digit) + east_west + poly(age, 2), 
       data = cur_data(), weights = w11105)
  )) %>%
  ungroup()

# Berechnung der Standardabweichung der residualen Löhne nach Geschlecht
std_dev_residual_log_wage_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(std_dev_residual_log_wage = sqrt(wtd.var(residual_wages_gender, weights = w11105, na.rm = TRUE)))

# Vergleich der Standardabweichung von ursprünglichen vs. residualen Löhnen nach Geschlecht
std_dev_gender_comparison <- std_dev_log_wage_gender %>%
  left_join(std_dev_residual_log_wage_gender, by = c("syear", "gender"), suffix = c("_original", "_residual")) %>%
  pivot_longer(cols = starts_with("std_dev"), names_to = "type", values_to = "value")

# Plot der Standardabweichung (ursprünglich vs. residual, getrennt nach Geschlecht)
ggplot(data = std_dev_gender_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "", 
       x = "Year", 
       y = "Standard Deviation") +
  scale_color_manual(values = c("std_dev_log_wage" = "blue",  # Ursprüngliche Löhne
                                "std_dev_residual_log_wage" = "red")) +  # Residuale Löhne
  facet_wrap(~ gender, labeller = as_labeller(c(`1` = "Men", `2` = "Women"))) +  # Facet nach Geschlecht
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

#-------------------------------------------------------------------------------

# Berechnung der P90 - P50 Differenz der residualen Löhne nach Geschlecht
P90_P50_residual_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(P90_P50_residual = wtd.quantile(residual_wages_gender, weights = w11105, probs = 0.90, na.rm = TRUE) - 
              wtd.quantile(residual_wages_gender, weights = w11105, probs = 0.50, na.rm = TRUE))

# Vergleich von P90 - P50 (ursprünglich vs. residual nach Geschlecht)
P90_P50_gender_comparison <- P90_P50_gender %>%
  left_join(P90_P50_residual_gender, by = c("syear", "gender"), suffix = c("_original", "_residual")) %>%
  pivot_longer(cols = starts_with("P90_P50"), names_to = "type", values_to = "value")

# Plot der P90 - P50 Differenz (ursprünglich vs. residual, getrennt nach Geschlecht)
ggplot(data = P90_P50_gender_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "", 
       x = "Year", 
       y = "Difference (P90 - P50)") +
  scale_color_manual(values = c("P90_P50" = "blue",  # Ursprüngliche Löhne
                                "P90_P50_residual" = "red")) +  # Residuale Löhne
  facet_wrap(~ gender, labeller = as_labeller(c(`1` = "Men", `2` = "Women"))) +  # Facet nach Geschlecht
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

#-------------------------------------------------------------------------------

# Berechnung der P50 - P10 Differenz der residualen Löhne nach Geschlecht
P50_P10_residual_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(P50_P10_residual = wtd.quantile(residual_wages_gender, weights = w11105, probs = 0.50, na.rm = TRUE) - 
              wtd.quantile(residual_wages_gender, weights = w11105, probs = 0.10, na.rm = TRUE))

# Vergleich von P50 - P10 (ursprünglich vs. residual nach Geschlecht)
P50_P10_gender_comparison <- P50_P10_gender %>%
  left_join(P50_P10_residual_gender, by = c("syear", "gender"), suffix = c("_original", "_residual")) %>%
  pivot_longer(cols = starts_with("P50_P10"), names_to = "type", values_to = "value")

# Plot der P50 - P10 Differenz (ursprünglich vs. residual, getrennt nach Geschlecht)
ggplot(data = P50_P10_gender_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "", 
       x = "Year", 
       y = "Difference (P50 - P10)") +
  scale_color_manual(values = c("P50_P10" = "blue",  # Ursprüngliche Löhne
                                "P50_P10_residual" = "red")) +  # Residuale Löhne
  facet_wrap(~ gender, labeller = as_labeller(c(`1` = "Men", `2` = "Women"))) +  # Facet nach Geschlecht
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

#-------------------------------------------------------------------------------

# Berechnung des Anteils der Jobs unter 2/3 des Medians für residuale Löhne nach Geschlecht
share_below_2_3_median_residual_gender <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(share_below_2_3_median_residual = 
              weighted.mean(residual_wages_gender < log(2/3 * exp(wtd.quantile(residual_wages_gender, weights = w11105, probs = 0.50, na.rm = TRUE))), 
                            w11105, na.rm = TRUE))

# Vergleich des Anteils unter 2/3 des Medians (ursprünglich vs. residual nach Geschlecht)
share_below_2_3_median_gender_comparison <- share_below_2_3_median_gender %>%
  left_join(share_below_2_3_median_residual_gender, by = c("syear", "gender"), suffix = c("_original", "_residual")) %>%
  pivot_longer(cols = starts_with("share_below_2_3_median"), names_to = "type", values_to = "value")

# Plot des Anteils unter 2/3 des Medians (ursprünglich vs. residual, getrennt nach Geschlecht)
ggplot(data = share_below_2_3_median_gender_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  labs(title = "", 
       x = "Year", 
       y = "Share") +
  scale_color_manual(values = c("share_below_2_3_median" = "blue",  # Ursprüngliche Löhne
                                "share_below_2_3_median_residual" = "red")) +  # Residuale Löhne
  facet_wrap(~ gender, labeller = as_labeller(c(`1` = "Men", `2` = "Women"))) +  # Facet nach Geschlecht
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

#-------------------------------------------------------------------------------

###########################
#                         #
#   PLOTS FOR THE PAPER   #
#                         #
###########################

#-------------------------------------------------------------------------------

# Plot der P90 - P50 Differenz (ursprünglich vs. residual) für Männer
ggplot(data = subset(P90_P50_gender_comparison, gender == 1), aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("P90_P50" = "blue", "P90_P50_residual" = "red")) +
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# Plot der P90 - P50 Differenz (ursprünglich vs. residual) für Frauen
ggplot(data = subset(P90_P50_gender_comparison, gender == 2), aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("P90_P50" = "blue", "P90_P50_residual" = "red")) +
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

#-------------------------------------------------------------------------------

# Plot der P50 - P10 Differenz (ursprünglich vs. residual) für Männer
ggplot(data = subset(P50_P10_gender_comparison, gender == 1), aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("P50_P10" = "blue", "P50_P10_residual" = "red")) +
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# Plot der P50 - P10 Differenz (ursprünglich vs. residual) für Frauen
ggplot(data = subset(P50_P10_gender_comparison, gender == 2), aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("P50_P10" = "blue", "P50_P10_residual" = "red")) +
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

#-------------------------------------------------------------------------------

# Plot der Standardabweichung (ursprünglich vs. residual) für Männer
ggplot(data = subset(std_dev_gender_comparison, gender == 1), aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("std_dev_log_wage" = "blue", "std_dev_residual_log_wage" = "red")) +
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# Plot der Standardabweichung (ursprünglich vs. residual) für Frauen
ggplot(data = subset(std_dev_gender_comparison, gender == 2), aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("std_dev_log_wage" = "blue", "std_dev_residual_log_wage" = "red")) +
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

#-------------------------------------------------------------------------------

# Plot des Anteils unter 2/3 des Medians (ursprünglich vs. residual) für Männer
ggplot(data = subset(share_below_2_3_median_gender_comparison, gender == 1), aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("share_below_2_3_median" = "blue", "share_below_2_3_median_residual" = "red")) +
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

# Plot des Anteils unter 2/3 des Medians (ursprünglich vs. residual) für Frauen
ggplot(data = subset(share_below_2_3_median_gender_comparison, gender == 2), aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("share_below_2_3_median" = "blue", "share_below_2_3_median_residual" = "red")) +
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 14))

#-------------------------------------------------------------------------------

# Plot der Standardabweichung (ursprünglich vs. residual)
ggplot(data = inequality_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("std_dev_log_wage" = "blue",  # Ursprüngliche Löhne in Blau
                                "std_dev_residual_log_wage" = "red")) +  # Residuale Löhne in Rot
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14))  # Vergrößert die Achsentext- und Titelgröße

#-------------------------------------------------------------------------------

# Plot der P90 - P50 Differenz (ursprünglich vs. residual)
ggplot(data = P90_P50_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("P90_P50" = "blue",  # Ursprüngliche Löhne in Blau
                                "P90_P50_residual" = "red")) +  # Residuale Löhne in Rot
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14))  # Vergrößert die Achsentext- und Titelgröße

#-------------------------------------------------------------------------------

# Plot der P50 - P10 Differenz (ursprünglich vs. residual)
ggplot(data = P50_P10_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("P50_P10" = "blue",  # Ursprüngliche Löhne in Blau
                                "P50_P10_residual" = "red")) +  # Residuale Löhne in Rot
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14))  # Vergrößert die Achsentext- und Titelgröße
z
#-------------------------------------------------------------------------------

# Plot des Anteils unter 2/3 des Medians (ursprünglich vs. residual)
ggplot(data = share_below_2_3_median_comparison, aes(x = syear, y = value, color = type)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  scale_color_manual(values = c("share_below_2_3_median" = "blue",  # Ursprüngliche Löhne in Blau
                                "share_below_2_3_median_residual" = "red")) +  # Residuale Löhne in Rot
  theme_minimal() +
  labs(x = "", y = "", color = "") +
  theme(axis.title = element_text(size = 16), 
        axis.text = element_text(size = 14))  # Vergrößert die Achsentext- und Titelgröße

#-------------------------------------------------------------------------------

