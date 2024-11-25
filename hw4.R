library(mgcv)
library(npreg)
library(boot)
library(faraway)
library(splines)
data(ozone)
for (j in 1:ncol(ozone))
{
  for (i in 1:nrow(ozone)) {
    if (is.na(ozone[i,j])){
      ozone[i,j]=mean(ozone[,j],na.rm=TRUE)
    }
  }
}

knots.temp <- c(ozone$temp[100], ozone$temp[200])
knots.dpg <- c(ozone$dpg[100], ozone$dpg[200])

model <- glm(O3 ~ bs(temp, knots = knots.temp, degree = 2) + 
                     bs(dpg, knots = knots.dpg, degree = 2), data = ozone)
predicted_values <- predict(model, type = "response")
plot(ozone$O3, predicted_values, xlab = "Observed Ozone", ylab = "Predicted Ozone", 
     main = "Observed vs. Predicted Ozone")


abline(0, 1, col = "red")
points(ozone$O3, predicted_values, col = "blue")

cv_results <- cv.glm(ozone, model, K = 10)

num_folds <- 10
mse_values <- numeric(num_folds)
indices <- sample(1:num_folds, nrow(ozone), replace = TRUE)

for (fold in 1:num_folds) {
  train_data <- ozone[indices != fold, ]
  test_data <- ozone[indices == fold, ]
  knots.temp <- c(ozone$temp[100], ozone$temp[200])
  knots.dpg <- c(ozone$dpg[100], ozone$dpg[200])
  model <- glm(O3 ~ bs(temp, knots = knots.temp, degree = 2) + 
                 bs(dpg, knots = knots.dpg, degree = 2), data = ozone)
  predictions <- predict(model, newdata = test_data)
  mse_values[fold] <- mean((test_data$O3 - predictions)^2)
}

pmse <- mean(mse_values)
print(paste("Estimated PMSE:", pmse))



library(ISLR2)

data(Bikeshare)
model <- glm(bikers ~ temp + hum + hr + workingday, data = Bikeshare,family = 'poisson')

new_data <- data.frame(
  temp = ((65-32)*(5/9) + 8)/(39+8), 
  hum = 0.5, 
  hr = factor(11, levels = levels(Bikeshare$hr)), 
  workingday = 0
)

log_pred <- predict(model, new_data, se.fit=TRUE)

pred_val <- log_pred$fit
pred_se <- log_pred$se.fit
print(exp(pred_val))
log_L <- pred_val - qnorm(0.975) *pred_se
log_U <- pred_val + qnorm(0.975) *pred_se
print(c(exp(log_L),exp(log_U)))


