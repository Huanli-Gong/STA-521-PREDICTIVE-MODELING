library(tree)
library(mlbench)
data(BostonHousing)

print(any(BostonHousing$age >= 48.6 & BostonHousing$age <= 48.8))


sample_indices <- sample(1:nrow(BostonHousing), nrow(BostonHousing) * 0.7)
training_data <- BostonHousing[sample_indices, ]
testing_data <- BostonHousing[-sample_indices, ]

cart_model <- tree(medv ~ ., data = training_data)
cv_cart_model <- cv.tree(cart_model)

plot(pruned_cart_model)
text(pruned_cart_model, pretty = 0)



library(randomForest)
rf_model <- randomForest(medv ~ ., data = training_data, importance = TRUE, proximity = TRUE)
var_importance <- importance(rf_model, type = 1) 
varImpPlot(rf_model, type = 1, main = "Variable Importance (RSS)")

# Sort the variable importance values in descending order
var_importance <- var_importance[order(var_importance, decreasing = TRUE), ]

# Create a bar plot to visualize variable importance
barplot(var_importance, main = "Variable Importance (RSS)")

# Identify the most important variable
most_important_variable <- rownames(var_importance)[1]


cart_predictions <- predict(pruned_cart_model, newdata = testing_data)
rf_predictions <- predict(rf_model, newdata = testing_data)

cart_mse <- mean((cart_predictions - testing_data$medv)^2)
rf_mse <- mean((rf_predictions - testing_data$medv)^2)

# (i) Report the MSE values for both models
cat("CART Model MSE:", cart_mse, "\n")
cat("Random Forest Model MSE:", rf_mse, "\n")

# (ii) Explain why one model might perform better than the other
if (cart_mse < rf_mse) {
  cat("The CART model performs better in terms of MSE.")
} else if (rf_mse < cart_mse) {
  cat("The Random Forest model performs better in terms of MSE.")
} else {
  cat("Both models have similar MSE values.")
}

variable_importance = vector("numeric", length = ncol(testing_data) - 1)
# Permutation feature importance
for (i in 1:(ncol(testing_data) - 1)) {
  shuffled_data = testing_data
  shuffled_data[, i] = sample(shuffled_data[, i]) # Permute the variable
  # Calculate MSE after shuffling
  shuffled_predictions = predict(rf_model, newdata = shuffled_data)
  shuffled_mse = mean((shuffled_predictions - shuffled_data$medv)^2)
  # Compute importance
  variable_importance[i] = rf_mse - shuffled_mse
}
# Top 3 important variables after permutation
variable_rank = order(variable_importance, decreasing = T)[1:3]
cat("Top 3 Important Variables from Permutation Feature Importance:", colnames(testing_data)[variable_rank])


variable_importance = vector("numeric", length = ncol(testing_data) - 1)
for (i in 1:(ncol(testing_data) - 1)) {
  shuffled_data = testing_data
  shuffled_data[, i] = sample(shuffled_data[, i])
  shuffled_predictions = predict(rf_model, newdata = shuffled_data)
  shuffled_mse = mean((shuffled_predictions - shuffled_data$medv)^2)
  variable_importance[i] = shuffled_mse-unshuffled_mse
}
variable_rank = order(variable_importance, decreasing = T)[1:3]
cat("Top 3 Important Variables from Permutation Feature Importance:", colnames(testing_data)[variable_rank])