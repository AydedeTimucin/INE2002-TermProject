## The distribution with keys
sorted_dictCO2s

## Five number (it is actually six in here but anyways) summary
summary(scaled_CO2s)

## Bar plot
barplot(sorted_dictCO2s, names.arg = names(sorted_dictCO2s),
        col = "black", main = "Country Code vs CO2 Consumption(kg) per capita",
        ylab = "CO2 Consumption(kg) Per Capita", ylim=c(0, 40000), width = 0.5, yaxp=c(0, 40000, 10))

abline(h=mean(GDPs), col="green", lwd = 5, lty="dotted")
abline(h=median(GDPs), col="blue", lwd = 5, lty="dotted")
#abline(h=seq(0, 50000, 5000), lty="dotted")
legend("topleft", legend = c("median 6449.7", "mean 7575.5"), col = c("blue", "black"), lty = c(1, 1), lwd = c(2, 2))

## Box plot
boxplot(scaled_CO2s, horizontal = TRUE, xlab = "CO2 consumption(kg) per capita", ylab = "",
        ylim=c(0, 40000), xaxp=c(0, 40000, 5), col="black", range = 10)

axis(side = 1, at = c(2865.4, 9928.7, 6449.7, 342.6, 38541.2),
     labels = c("2865.4 (Q1)", "9928.7 (Q3)", "6449.7 (median)", "342.6 (min)", "38541.2 (max)"), tick = TRUE, lty = 1, padj = 1.5)

segments(x0 = 2865.4, y0 = 0, x1 = 2865.4, y1 = 100, lty = "dotted") # Q1
segments(x0 = 9928.7, y0 = 0, x1 = 9928.7, y1 = 100, lty = "dotted") # Q3
segments(x0 = 6449.7, y0 = 0, x1 = 6449.7, y1 = 100, lty = "dotted") # Median
segments(x0 = 342.6, y0 = 0, x1 = 342.6, y1 = 100, lty = "dotted") # Min
segments(x0 = 38541.2, y0 = 0, x1 = 38541.2, y1 = 100, lty = "dotted") # Max

## Normality Test

print(shapiro.test(scaled_CO2s))

## Confidence Interval For Mean

confidence_level <- 0.99

sample_mean <- mean(scaled_CO2s)
sample_sd <- sd(scaled_CO2s)

standard_error <- sample_sd / sqrt(length(scaled_CO2s))

df <- length(scaled_CO2s) - 1

t_value <- qt((1 - confidence_level) / 2, df)

margin_of_error <- t_value * standard_error

lower_bound <- sample_mean - abs(margin_of_error)
upper_bound <- sample_mean + abs(margin_of_error)

cat("Sample Mean:", sample_mean, "\n")
cat("Margin of Error:", margin_of_error, "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

## Confidence Interval For Std

confidence_level <- 0.99

n <- length(scaled_CO2s)

sample_variance <- var(scaled_CO2s)

df <- n - 1

chi_square_lower <- qchisq((1 - confidence_level) / 2, df)
chi_square_upper <- qchisq((1 + confidence_level) / 2, df)

lower_bound <- sqrt((df * sample_variance) / chi_square_upper)
upper_bound <- sqrt((df * sample_variance) / chi_square_lower)

cat("Sample Variance:", sample_variance, "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

## Hypothesis Testing For Mean

null_mean <- 4000

alpha <- 0.01

result <- t.test(scaled_CO2s, mu = null_mean, alternative = "greater")

print(result)

## Goodness of Fit Test

hist_data <- hist(scaled_CO2s, breaks = 3, plot = FALSE)

num_bins <- length(hist_data$breaks) - 1

total <- length(scaled_CO2s)
expected_proportions <- rep(1/num_bins, num_bins)

expected_frequencies <- expected_proportions * total

result <- chisq.test(hist_data$counts, p = expected_proportions)

print(result)

## Sign Test

deviations <- scaled_CO2s - 12000

deviations <- deviations[deviations != 0]

positive_count <- sum(deviations > 0)
negative_count <- sum(deviations < 0)

count_smaller <- min(positive_count, negative_count)

test_value = ((count_smaller + 0.5) -0.5*length(scaled_CO2s)) / (sqrt(length(scaled_CO2s))/2)
critical_value <- qnorm(1 - 0.05/2)

print(test_value)
print(critical_value)







