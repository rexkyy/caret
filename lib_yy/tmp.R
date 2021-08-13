# KernelKnn Toy Example
### kernel knn only
#1. Response variable should transfered to numeric value in classification
#2. Minkowski power is equal to k. (this is default)
#3. y should be correct format according to the analysis method(classification/regression)
#4. regression: TRUE ( y should be also continuous value, levels should be NULL, method = minkowski)

data(iris)
m <- dim(iris)[1]
val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m)) 
iris.learn <- iris[-val,]
iris.valid <- iris[val,]
library(kknn)
iris.kknn <- kknn(Species~., iris.learn, iris.valid,
                  k = 7,
                  distance = 2,
                  kernel = "biweight")

yy <- as.numeric(iris.learn$Species)
iris.kernelknn <- KernelKnn(iris.learn[-ncol(iris.learn)],
                            TEST_data = iris.valid[-ncol(iris.valid)],
                            y = yy,
                            k = 7,
                            weights_function = "biweight",
                            regression = FALSE,
                            Levels = unique(yy))
apply(iris.kknn$CL, 1, unique)

# need values
#1. fitted value
#2. predicted/observed value
#3. confidence interval
#4. training error
trainData <- cbind(training_x, training_y)
mf <- model.frame(as.formula("training_y ~ ."), data = trainData)
cl <- model.response(mf)
# W: Matrix of weights of the k nearest neighbors. kernel knn result
W <- kernelknn_res
# CL: Matrix of classes of the k nearest neighbors
# dmtmp: distance matrix?
C <- matrix(dmtmp$cl, nrow = p, ncol = k + 1)
CL <- matrix(cl[C], nrow = p, ncol = k)
# fitted.values: Vector of predictions
if (classification) {
        fitted.values <- apply(iris.kernelknn, 1, order, decreasing = TRUE)[1, ]
} else { # regression
        # fitted.values <- rowsums(W*CL) / pmax(rowsoms(W), 1e-06) # if regression
}



## knn using caret pckg example ------------
set.seed(400)
knn_modelinfo <- getModelInfo("knn")$knn
knn_modelinfo$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
        if(is.factor(y)) {
                out <- knn3(as.matrix(x), y, k = param$k, ...)
        } else {
                out  <- knnreg(as.matrix(x), y, k = param$k, ...)
        }
        return(out)
}
knn_modelinfo$predict <- function(modelFit, newdata, submodels = NULL) {
        
        print("modelFit!!!")
        print(summary(modelFit))
        if(modelFit$problemType == "Classification") {
                out <- predict(modelFit, newdata,  type = "class")
        } else {
                out <- predict(modelFit, newdata)
        }
        out
}
ctrl <- trainControl(
        method = "cv",
        number = ifelse(grepl("cv", resample_method), 2, 1),
        repeats = ifelse(grepl("[d_]cv$", resample_method), 1, NA),
        search = "grid",
        savePredictions = TRUE,
        classProbs = FALSE #,
        # verboseIter = TRUE
)

n <- 5
knnFit <- train(
        Class ~ .,
        data = train_set, # data using 'kernelknn_tmp' script trainingset
        method = knn_modelinfo,
        tuneGrid = data.frame(k = c(2, 3)),
        trControl = ctrl
)
sknnFit
#



## kknn using caret pckg example---------------
set.seed(250)
kknn_modelinfo <- getModelInfo("knn")$kknn
kknn_modelinfo$predict <- function(modelFit, newdata, submodels = NULL) {
        if(!is.data.frame(newdata)) {
                newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
        }
        out <- predict(modelFit, newdata)
        return(out)
}

kknn_modelinfo$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
        dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
        dat$.outcome <- y
        outs <- kknn::train.kknn(.outcome ~ ., data = dat,
                         kmax = param$kmax,
                         distance = param$distance,
                         kernel = as.character(param$kernel), ...)
        print(str(outs))
        return(outs)
}

kknn_modelinfo$grid <- function(x, y, len = NULL, search = "grid") {

        if(search == "grid") {
                out <- data.frame(
                        kmax = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0], 
                        distance = 2, 
                        kernel = "optimal")
        } else {
                by_val <- if(is.factor(y)) length(levels(y)) else 1
                kerns <- c("rectangular", "triangular", "epanechnikov", 
                           "biweight", "triweight", "cos", "inv", "gaussian")
                out <- data.frame(
                        kmax = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE),
                        distance = runif(len, min = 0, max = 3),
                        kernel = sample(kerns, size = len, replace = TRUE))
        }
        return(out)
}

ctrl <- trainControl(
        method = "cv",
        number = ifelse(grepl("cv", resample_method), 2, 1),
        repeats = ifelse(grepl("[d_]cv$", resample_method), 1, NA),
        search = "grid",
        savePredictions = TRUE,
        classProbs = FALSE,
        verboseIter = TRUE
)
n <- 5
kknnFit <- train(
        Class ~ .,
        data = train_set,
        method = kknn_modelinfo,
        tuneGrid = data.frame(kmax = c(2, 3),
                              distance = c(2, 2),
                              kernel = c("optimal", "optimal")),
        trControl = ctrl
)
kknnFit
#

## Customizing caret example for SVM -------------
# sigmas <- sigest(as.matrix(iris.learn[-ncol(iris.learn)]),
#                  na.action = na.omit, 
#                  scaled = TRUE)
# ksvm_res <- ksvm(x = as.matrix(iris.learn[-ncol(iris.learn)]),
#                  y = yy,
#                  kernel = "rbfdot",
#                  kpar = list(sigma = sigmas[1]),
#                  C = 0.25,
#                  prob.model = classProb)
# 
# ## create test and training set
# index <- sample(1:dim(spam)[1])
# spamtrain <- spam[index[1:floor(dim(spam)[1]/2)], ]
# spamtest <- spam[index[((ceiling(dim(spam)[1]/2)) + 1):dim(spam)[1]], ]
# 
# ## train a support vector machine
# filter <- ksvm(type~.,
#                data = spamtrain,
#                kernel="rbfdot",
#                kpar=list(sigma=0.05),
#                C=5,
#                cross=3)
# str(filter)
#








library(earth)
data(etitanic)
# using model.matrix
head(model.matrix(survived ~ ., data = etitanic))

# using dummyVars
dummies <- dummyVars(survived ~ ., data = etitanic)
head(predict(dummies, newdata = etitanic))

data(mdrr)
data.frame(table(mdrrDescr$nR11))

nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[,-nzv]
dim(filteredDescr)

descrCor <- cor(filteredDescr)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)

library(AppliedPredictiveModeling)
transparentTheme(trans = .4)

plotSubset <- data.frame(scale(mdrrDescr[, c("nC", "X4v")]))
xyplot(
        nC ~ X4v,
        data = plotSubset,
        groups = mdrrClass,
        auto.key = list(columns = 2)
)

transformed <- spatialSign(plotSubset)
transformed <- as.data.frame(transformed)
xyplot(
        nC ~ X4v,
        data = transformed,
        groups = mdrrClass,
        auto.key = list(columns = 2)
)


# Create matrix
mt <- matrix(1:10, ncol = 5)

# Print matrix
cat("Matrix:\n")
print(mt)

# Scale matrix with default arguments
cat("\nAfter scaling:\n")
scale(mt)


preProcValues2 <- preProcess(training, method = "BoxCox")
trainBC <- predict(preProcValues2, training)
testBC <- predict(preProcValues2, test)
preProcValues2

# testOutput, lev, method
data <- testOutput
lev <- lev
model <- method
if (is.character(data$obs))
        data$obs <- factor(data$obs, levels = lev)
postResample(data[, "pred"], data[, "obs"])


## caret using resample method "random forest"-----
fitControl <- trainControl(method = "none", number = 10, repeats = NA,
                           verboseIter = TRUE)
rf_fit <- train(Class ~ ., data = training, method = "rf", 
                trControl = fitControl)
#
