co2_df_scaled <- co2_df * 1000
View(co2_df_scaled)


correlation_matrix <- cor(co2_df_scaled, gdp_df)
View(correlation_matrix)

# Loop through the country names
for (country in country_names) {
  # Create a data frame for each country
  assign(paste0(country, "_data"), data.frame(
    Year = index(gdp_df),
    GDP = coredata(gdp_df)[, country],
    CO2 = coredata(co2_df_scaled)[, country]
  ))
}

ylim_values <- list(
  argentina = c(0, 20000),
  australia = c(0, 30000),
  austria = c(0, 20000),
  brazil = c(0, 10000),
  canada = c(0, 30000),
  chile = c(0, 10000),
  china = c(0, 20000),
  cuba = c(0, 10000),
  egypt = c(0, 6000),
  estonia = c(0, 40000),
  finland = c(0, 30000),
  germany = c(0, 30000),
  greece = c(0, 30000),
  india = c(0, 4000),
  iran = c(0, 20000),
  israel = c(0, 20000),
  italy = c(0, 20000),
  japan = c(0, 20000),
  mexico = c(0, 10000),
  morocco = c(0, 4000),
  namibia = c(0, 4000),
  newZealand = c(0, 20000),
  nigeria = c(0, 4000),
  peru = c(0, 4000),
  poland = c(0, 20000),
  portugal = c(0, 20000),
  qatar = c(0, 100000),
  russia = c(0, 20000),
  saudiArabia = c(0, 30000),
  southKorea = c(0, 30000),
  sudan = c(0, 4000),
  thailand = c(0, 10000),
  turkey = c(0, 10000),
  uk = c(0, 30000),
  us = c(0, 40000)
)

xlim_values <- list(
  argentina = c(0, 20000),
  australia = c(0, 70000),
  austria = c(0, 70000),
  brazil = c(0, 20000),
  canada = c(0, 70000),
  chile = c(0, 20000),
  china = c(0, 15000),
  cuba = c(0, 10000),
  egypt = c(0, 4000),
  estonia = c(0, 30000),
  finland = c(0, 60000),
  germany = c(0, 50000),
  greece = c(0, 40000),
  india = c(0, 4000),
  iran = c(0, 10000),
  israel = c(0, 60000),
  italy = c(0, 50000),
  japan = c(0, 50000),
  mexico = c(0, 15000),
  morocco = c(0, 4000),
  namibia = c(0, 8000),
  newZealand = c(0, 50000),
  nigeria = c(0, 4000),
  peru = c(0, 8000),
  poland = c(0, 20000),
  portugal = c(0, 30000),
  qatar = c(0, 100000),
  russia = c(0, 20000),
  saudiArabia = c(0, 30000),
  southKorea = c(0, 40000),
  sudan = c(0, 4000),
  thailand = c(0, 10000),
  turkey = c(0, 15000),
  uk = c(0, 60000),
  us = c(0, 80000)
)

for (country in country_names) {
  # Get the corresponding data frame for the country
  country_data <- get(paste0(country, "_data"))
  
  # Get the xlim and ylim values for the current country
  xlim_vals <- xlim_values[[country]]
  ylim_vals <- ylim_values[[country]]
  
  # Plotting
  plot(country_data$GDP, country_data$CO2, main = paste("Income vs CO2 Footprint", country),
       xlab = "GDP per capita (dollars)", ylab = "CO2 consumption (kg)",
       xlim = xlim_vals, ylim = ylim_vals, col = "blue", pch = 16, cex = 1.5)
  
  # Linear regression
  model <- lm(CO2 ~ GDP, data = country_data)
  cat(paste("Correlation Coeff for", country, ":", cor(country_data$GDP, country_data$CO2)))
  
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  
  abline(model, col = "red")
}


# Convert the vectors to data frames
data <- data.frame(gdp_means, co2_means)

# Create a binary variable indicating whether a country is high-income or low-income
data$income <- ifelse(data$gdp_means > 20000, "High Income", "Low Income")

# Subset the data for high-income and low-income countries
high_income_data <- data[data$income == "High Income", ]
low_income_data <- data[data$income == "Low Income", ]

# Perform the t-test
result <- t.test(high_income_data$co2_means, low_income_data$co2_means)

# Print the result
print(result)