## Regression With All

plot(GDPs, scaled_CO2s, main = "Income vs CO2 Footprint",
     xlab = "GDP per capita (dollars)", ylab = "CO2 consumption (kg)",
     xlim = c(0, 50000), ylim = c(0, 40000), col = "blue", pch = 16, cex = 1.5)


model <- lm(scaled_CO2s ~ GDPs)
cat(paste("Correlation Coeff:", cor(GDPs, scaled_CO2s)))

intercept <- coef(model)[1]
slope <- coef(model)[2]

abline(model, col = "red")

cat(paste("Regression Line With Outliers:", intercept, "+", slope, "x\n"))
summary(model)

## Regression Without Outliers

plot(GDPs_without, scaled_CO2s_without, main = "Income vs CO2 Footprint",
     xlab = "GDP per capita (dollars)", ylab = "CO2 consumption (kg)",
     xlim = c(0, 50000), ylim = c(0, 40000), col = "blue", pch = 16, cex = 1.5)


model <- lm(scaled_CO2s_without ~ GDPs_without)
cat(paste("Correlation Coeff:", cor(GDPs_without, scaled_CO2s_without)))

intercept <- coef(model)[1]
slope <- coef(model)[2]

abline(model, col = "red")

cat(paste("Regression Line Without Outliers:", intercept, "+", slope, "x\n"))
summary(model)