data(iris)
X <- iris[, 1:4]
pca <- prcomp(X, center = TRUE, scale. = TRUE)
print(pca$rotation[, 1])

response_var <- ifelse(iris$Species == "setosa", 1, 0)
n_components <- 1
pca_data <- pca$x[, 1:n_components]
model <- glm(response_var ~ pca_data, family = binomial)
probabilities <- predict(model, newdata = as.data.frame(pca_data), type = "response")
predicted_species <- ifelse(probabilities > 0.5, 1, 0)
print(sum(probabilities)/length(predicted_species))
