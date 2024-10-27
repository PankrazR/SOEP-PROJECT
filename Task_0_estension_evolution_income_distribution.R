#######################################
#                                     #
#  EVOLUTION OF INCOME DISTRIBUTIONS  #
#                                     #
#######################################

# ------------------------------------------------------------------------------

# Calculations for all years
# Calculation of weighted mode and median for inflation-adjusted monthly income
income_distribution <- dat_cleaned %>%
  group_by(syear, gender) %>%
  summarise(
    # Weighted mode (estimation)
    mode_income = as.numeric(names(sort(tapply(current_gross_labor_inc_2020, w11105, sum), decreasing = TRUE)[1])),
    
    # Weighted median
    median_income = weighted.median(current_gross_labor_inc_2020, w11105, na.rm = TRUE)
  )

# Output results (mode and median for each year and gender)
print(income_distribution)

# ------------------------------------------------------------------------------

# Density plots for all years with weighting
density_plot_all <- function(data, year) {
  ggplot(data = data, aes(x = current_gross_labor_inc_2020, fill = as.factor(gender), weight = w11105)) +  # Add weighting
    geom_density(alpha = 0.5) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
    labs(
      title = paste("Monthly Income Distribution by Gender in", year, "(in 2020 Euros, Weighted)"),
      x = "Monthly Income (in Euros)",
      y = "Density",
      fill = "Gender"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# ------------------------------------------------------------------------------

# Generate density plots for all years
for (year in unique(dat_cleaned$syear)) {
  print(density_plot_all(dat_cleaned %>% filter(syear == year), year))
}

# ------------------------------------------------------------------------------

###########################
#                         #
#   PLOTS FOR THE PAPER   #
#                         #
###########################

#-------------------------------------------------------------------------------

# Dichteplots für ausgewählte Jahre mit Gewichtung
density_plot_selected_years <- function(data, years) {
  for (year in years) {
    plot <- ggplot(data = data %>% filter(syear == year), aes(x = current_gross_labor_inc_2020, fill = as.factor(gender), weight = w11105)) +
      geom_density(alpha = 0.5, bw = 200) +  # Bandbreite anpassen
      scale_x_continuous(labels = scales::comma) +
      scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = c("1" = "blue", "2" = "red"), labels = c("1" = "Men", "2" = "Women")) +
      labs(fill = "Gender") +  # Legendentitel anpassen
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12),  # Größe der Zahlen auf den Achsen erhöhen
        axis.title.x = element_blank(),  # Entfernt x-Achsentitel
        axis.title.y = element_blank(),  # Entfernt y-Achsentitel
        plot.title = element_blank(),  # Entfernt den Plot-Titel
        legend.position = c(0.85, 0.85),  # Legende oben rechts platzieren
        legend.title = element_blank()  # Entfernt den Legendentitel
      )
    
    print(plot)  # Plot anzeigen
  }
}

# ------------------------------------------------------------------------------

# Wähle die Jahre, für die du die Plots erstellen möchtest
selected_years <- c(1990, 2000, 2010, 2020)

# Generiere Dichteplots für die ausgewählten Jahre
density_plot_selected_years(dat_cleaned, selected_years)

# ------------------------------------------------------------------------------
