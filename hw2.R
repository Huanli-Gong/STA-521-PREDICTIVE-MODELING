data <- read.table("T32.1",header=FALSE)
age_adjusted_rate <- data[, 5]

qqnorm(age_adjusted_rate)
qqline(age_adjusted_rate)

year <- data[, 4]
melanoma_rate <- data[, 5]

plot(year, melanoma_rate, xlab = "Year", ylab = "Age-Adjusted Melanoma Rate", main = "Age-Adjusted Melanoma Rate in Connecticut")

sunspot_number <- data[, 7]
rescale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
melanoma_rate_rescaled <- rescale(melanoma_rate)
sunspot_number_rescaled <- rescale(sunspot_number)

plot(year, melanoma_rate_rescaled, pch = 16, col = "blue", xlab = "Year", ylab = "Rescaled Value", main = "Age-Adjusted Melanoma Rate and Sunspot Number")
points(year, sunspot_number_rescaled, pch = 4, col = "red")
legend("topright", legend = c("Melanoma Rate", "Sunspot Number"), col = c("blue", "red"), pch = c(16, 4), cex = 0.8)

data <- read.table("Brink.txt",header=TRUE)
data <- data[data$BRINK == 0, ]

model <- lm(formula = CON ~  CITY, data = data)
new_data <- data.frame(CITY = 7105) 
predicted_con <- predict(model, newdata = new_data, interval = "prediction", level = 0.95)

cat("95% Prediction Interval for the amount collected in month 20:\n")
cat("Predicted Amount:", predicted_con[1], "\n")
cat("L:", predicted_con[2], "\n")
cat("U:", predicted_con[3], "\n")