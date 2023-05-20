## The distribution with keys
sorted_dictGDPs

## Five number (it is actually six in here but anyways) summary
summary(GDPs)

## Bar plot
barplot(sorted_dictGDPs, names.arg = names(sorted_dictGDPs),
        col = "dark green", main = "Country Code vs GDP per capita", ylab = "GDP Per Capita", ylim=c(0, 50000),
        width = 0.5, yaxp=c(0, 50000, 10))

abline(h=mean(GDPs), col="black", lwd = 5, lty="dotted")
abline(h=median(GDPs), col="blue", lwd = 5, lty="dotted")
#abline(h=seq(0, 50000, 5000), lty="dotted")
legend("topleft", legend = c("median 9005.2", "mean 17006.5"), col = c("blue", "black"), lty = c(1, 1), lwd = c(2, 2))

## Box plot
boxplot(GDPs, horizontal = TRUE, xlab = "GDP per capita", ylab = "",
        ylim=c(0, 50000), xaxp=c(0, 50000, 5), col="dark green")

axis(side = 1, at = c(3941.2, 31085.4, 9005.2, 984.9, 47714.2),
     labels = c("3941.2 (Q1)", "31085.4 (Q3)", "9005.2 (median)", "984.9 (min)", "47714.2 (max)"), tick = TRUE, lty = 1, padj = 1.5)

segments(x0 = 3941.2, y0 = 0, x1 = 3941.2, y1 = 100, lty = "dotted") # Q1
segments(x0 = 31085.4, y0 = 0, x1 = 31085.4, y1 = 100, lty = "dotted") # Q3
segments(x0 = 9005.2, y0 = 0, x1 = 9005.2, y1 = 100, lty = "dotted") # Median
segments(x0 = 984.9, y0 = 0, x1 = 984.9, y1 = 100, lty = "dotted") # Min
segments(x0 = 47714.2, y0 = 0, x1 = 47714.2, y1 = 100, lty = "dotted") # Max

## Normality Test

print(shapiro.test(GDPs))

## Confidence Interval For Mean

confidence_level <- 0.95

sample_mean <- mean(GDPs)
sample_sd <- sd(GDPs)

standard_error <- sample_sd / sqrt(length(GDPs))

df <- length(GDPs) - 1

t_value <- qt((1 - confidence_level) / 2, df)

margin_of_error <- t_value * standard_error

lower_bound <- sample_mean - abs(margin_of_error)
upper_bound <- sample_mean + abs(margin_of_error)

cat("Sample Mean:", sample_mean, "\n")
cat("Margin of Error:", margin_of_error, "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

## Confidence Interval For Std

confidence_level <- 0.95

n <- length(GDPs)

sample_variance <- var(GDPs)

df <- n - 1

chi_square_lower <- qchisq((1 - confidence_level) / 2, df)
chi_square_upper <- qchisq((1 + confidence_level) / 2, df)

lower_bound <- sqrt((df * sample_variance) / chi_square_upper)
upper_bound <- sqrt((df * sample_variance) / chi_square_lower)

cat("Sample Variance:", sample_variance, "\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")

## Hypothesis Testing For Mean

null_mean <- 12000

alpha <- 0.05

result <- t.test(GDPs, mu = null_mean, alternative = "greater")

print(result)


## Goodness of Fit Test

hist_data <- hist(GDPs, breaks = 3, plot = FALSE)

num_bins <- length(hist_data$breaks) - 1

total <- length(GDPs)
expected_proportions <- rep(1/num_bins, num_bins)

expected_frequencies <- expected_proportions * total

result <- chisq.test(hist_data$counts, p = expected_proportions)

print(result)



