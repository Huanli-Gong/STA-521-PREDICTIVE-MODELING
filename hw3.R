library(readr)
library(caret)
library(dplyr)
library(boot)

data <- read_csv("AMZN.csv")
data$previous_Low <- lag(data$Low)
data <- data[-1, ]

cv <- trainControl(method = "cv", number = 10)
cv_results <- train(High ~ previous_Low + Open, data = data,method = "lm", trControl = cv)
PMSE <- cv_results$results$RMSE^2
cat("Predictive Mean Squared Error:", PMSE, "\n")

calculate_ratio <- function(data, indices) {
  subset_data <- data[indices, ]
  high_low_ratio <- median(subset_data$High / subset_data$Low)
  return (high_low_ratio)
}

boot <- boot(data, statistic = calculate_ratio, R = 10000)

pivot_ci <- boot.ci(boot, type = "basic")
bca_ci <- boot.ci(boot, type = "bca")

print("Pivot Bootstrap Confidence Intervals:")
print(pivot_ci)

print("Bias-Corrected Accelerated Bootstrap Confidence Intervals:")
print(bca_ci)
