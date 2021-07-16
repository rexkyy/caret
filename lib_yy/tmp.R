# Test 'KernelKnn' package
library(KernelKnn)
library(caret)

dataset <- iris

y <- dataset$Species
x <- dataset[, c(1, 2)]
train_control <- trainControl(method = "none")
# trControl <- train_control

method <- "knn"
# metric <- "Accuracy"
