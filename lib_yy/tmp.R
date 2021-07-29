# Test 'KernelKnn' package
library(KernelKnn)
library(caret)
library(kknn)

dataset <- ionosphere

y <- dataset$class
x <- dataset[,-ncol(dataset)]
train_control <- trainControl(method = "none")

knn_tmp <- train(class ~ .,
                 data = dataset,
                 method = "kknn",
                 trControl = train_control)




# str(knn_tmp)
# # trControl <- train_control
#
# method <- "knn"
# # metric <- "Accuracy"
#
#
# library(kknn)
# data(iris)
# m <- dim(iris)[1]
# val <- sample(1:m, size = round(m/3), replace = FALSE, prob = rep(1/m, m))
# iris.learn <- iris[-val,]
# iris.valid <- iris[val,]
# iris.kknn <- kknn(Species ~., iris.learn, iris.valid, distance = 1,
#                   kernel = "triangular")
# summary(iris.kknn)
# fit <- fitted(iris.kknn)
# table(iris.valid$Species, fit)
# pcol <- as.character(as.numeric(iris.valid$Species))
# pairs(iris.valid[1:4], pch = pcol, col = c("green3", "red")
#       [(iris.valid$Species != fit)+1])
# data(ionosphere)
# ionosphere.learn <- ionosphere[1:200,]
# ionosphere.valid <- ionosphere[-c(1:200),]
# fit.kknn <- kknn(class ~ ., ionosphere.learn, ionosphere.valid)
# table(ionosphere.valid$class, fit.kknn$fit)
# (fit.train1 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15,
#                           kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1))
# table(predict(fit.train1, ionosphere.valid), ionosphere.valid$class)
# (fit.train2 <- train.kknn(class ~ ., ionosphere.learn, kmax = 15,
#                           kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2))
# table(predict(fit.train2, ionosphere.valid), ionosphere.valid$class)
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

#####
tab <- table(obs, pred)
match.names = FALSE

n <- sum(tab)
ni <- apply(tab, 1, sum)
nj <- apply(tab, 2, sum)
if (match.names && !is.null(dimnames(tab))) {
        lev <- intersect(colnames(tab), rownames(tab))
        p0 <- sum(diag(tab[lev, lev])) / n
        pc <- sum(ni[lev] * nj[lev]) / n ^ 2
} else {
        m <- min(length(ni), length(nj))
        p0 <- sum(diag(tab[1:m, 1:m])) / n
        pc <- sum((ni[1:m] / n) * (nj[1:m] / n))
}
n2 <- choose(n, 2)
rand <- 1 + (sum(tab ^ 2) - (sum(ni ^ 2) + sum(nj ^ 2)) / 2) / n2
nis2 <- sum(choose(ni[ni > 1], 2))
njs2 <- sum(choose(nj[nj > 1], 2))
crand <-
        (sum(choose(tab[tab > 1], 2)) - (nis2 * njs2) / n2) / ((nis2 + njs2) /
                                                                       2 - (nis2 * njs2) / n2)
list(
        diag = p0,
        kappa = (p0 - pc) / (1 - pc),
        rand = rand,
        crand = crand
)


##### Using Your Own Model in 'train'

lpSVM <- list(type = "Classification",
              library = "kernlab",
              loop = NULL)

# the parameters element
prm <- data.frame(
        parameter = c("C", "sigma"),
        clas = rep("numeric", 2),
        label = c("Cost", "Sigma")
)
lpSVM$parameters <- prm

# grid element
# 사용하고자 하는 함수의 grid를 만들면 됨.
# pass their own grid via train's tuneGrid option or they can use this code
# to create a default grid.
# train 안에 있는 tuneGrid를 사용하던지 만든 grid함수를 사용하던지.
# tuneGrid는 어떻게 작동되고 만든 grid함수는 어떻게 동작하는거지?
# 이거는 train 의 method$grid에 들어감.
svmGrid <- function(x, y, len = NULL, search = "grid") {
        library(kernlab)
        ## This produces low, middle and high values for sigma
        ## (i.e. a vector with 3 elements).
        sigmas <-
                kernlab::sigest(as.matrix(x),
                                na.action = na.omit,
                                scaled = TRUE)
        ## To use grid search:
        if (search == "grid") {
                out <- expand.grid(sigma = mean(as.vector(sigmas[-2])),
                                   C = 2 ^ ((1:len) - 3))
        } else {
                ## For random search, define ranges for the parameters then
                ## generate random values for them
                rng <- extendrange(log(sigmas), f = .75)
                out <-
                        data.frame(sigma = exp(runif(
                                len, min = rng[1], max = rng[2]
                        )),
                        C = 2 ^ runif(len, min = -5, max = 8))
        }
        out
}

lpSVM$grid <- svmGrid


# The 'fit' Element
svmFit <-
        function(x,
                 y,
                 wts,
                 param,
                 lev,
                 last,
                 weights,
                 classProbs,
                 ...) {
                kernlab::ksvm(
                        x = as.matrix(x),
                        y = y,
                        kernel = "rbfdot",
                        kpar = list(sigma = param$sigma),
                        C = param$C,
                        prob.model = classProbs,
                        ...
                )
        }

lpSVM$fit <- svmFit

# The 'predict' Element
svmPred <-
        function(modelFit,
                 newdata,
                 preProc = NULL,
                 submodels = NULL) {
                kernlab::predict(modelFit, newdata)
        }

lpSVM$predict <- svmPred

# the 'prob' element
svmProb <-
        function(modelFit,
                 newdata,
                 preProc = NULL,
                 submodels = NULL)
                kernlab::predict(modelFit, newdata, type = "probabilities")
lpSVM$prob <- svmProb

# The 'sort' element
svmSort <- function(x)
        x[order(x$C), ]
lpSVM$sort <- svmSort

# the 'level' element
lpSVM$levels <- function(x)
        kernlab::lev(x)


### fit the model
library(mlbench)
data(Sonar)

library(caret)
set.seed(998)
inTraining <-
        createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[inTraining, ]
testing  <- Sonar[-inTraining, ]

fitControl <- trainControl(method = "repeatedcv",
                           ## 10-fold CV...
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

set.seed(825)
Laplacian <- train(
        Class ~ .,
        data = training,
        method = lpSVM,
        preProc = c("center", "scale"),
        tuneLength = 8,
        trControl = fitControl
)
Laplacian

ggplot(Laplacian) + scale_x_log10()

##### example 2: something more complicated - LogitBoost\
install.packages("caTools")
library(caTools)

mod <- LogitBoost(as.matrix(x), y, nIter = 51)
lbGrid <- data.frame(nIter = seq(11, 51, by = 10))

loop <- data.frame(.nIter = 51)
submodels <- list(data.frame(nIter = seq(11, 41, by = 10)))
fullGrid <- data.frame(nIter = seq(11, 51, by = 10))

# Get the largest value of nIter to fit the "full" model
loop <- fullGrid[which.max(fullGrid$nIter), , drop = FALSE]
submodels <- fullGrid[-which.max(fullGrid$nIter), , drop = FALSE]

# This is needs to be encased in a list in case there are more
# than one tuning parameter
submodels <- list(submodels)
lbPred <-
        function(modelFit,
                 newdata,
                 preProc = NULL,
                 submodels = NULL) {
                ## This model was fit with the maximum value of nIter
                out <-
                        caTools::predict.LogitBoost(modelFit, newdata, type = "class")
                
                ## In this case, 'submodels' is a data frame with the other values of
                ## nIter. We loop over these to get the other predictions.
                if (!is.null(submodels)) {
                        ## Save _all_ the predictions in a list
                        tmp <- out
                        out <-
                                vector(mode = "list",
                                       length = nrow(submodels) + 1)
                        out[[1]] <- tmp
                        
                        for (j in seq(along = submodels$nIter)) {
                                out[[j + 1]] <- caTools::predict.LogitBoost(modelFit,
                                                                            newdata,
                                                                            nIter = submodels$nIter[j])
                                
                        }
                }
                out
        }

lbFuncs <- list(
        library = "caTools",
        loop = function(grid) {
                loop <- grid[which.max(grid$nIter), , drop = FALSE]
                submodels <-
                        grid[-which.max(grid$nIter), , drop = FALSE]
                submodels <- list(submodels)
                list(loop = loop, submodels = submodels)
        },
        type = "Classification",
        parameters = data.frame(
                parameter = 'nIter',
                class = 'numeric',
                label = '# Boosting Iterations'
        ),
        grid = function(x, y, len = NULL, search = "grid") {
                out <- if (search == "grid")
                        data.frame(nIter = 1 + ((1:len) * 10))
                else
                        data.frame(nIter = sample(1:500, size = len))
                out
        },
        fit = function(x,
                       y,
                       wts,
                       param,
                       lev,
                       last,
                       weights,
                       classProbs,
                       ...) {
                caTools::LogitBoost(as.matrix(x), y, nIter = param$nIter)
        },
        predict = function(modelFit,
                           newdata,
                           preProc = NULL,
                           submodels = NULL) {
                out <- caTools::predict.LogitBoost(modelFit, newdata, type = "class")
                if (!is.null(submodels)) {
                        tmp <- out
                        out <-
                                vector(mode = "list",
                                       length = nrow(submodels) + 1)
                        out[[1]] <- tmp
                        
                        for (j in seq(along = submodels$nIter)) {
                                out[[j + 1]] <- caTools::predict.LogitBoost(modelFit,
                                                                            newdata,
                                                                            nIter = submodels$nIter[j])
                        }
                }
                out
        },
        prob = NULL,
        sort = function(x)
                x
)
set.seed(825)
lb1 <- system.time(train(
        Class ~ .,
        data = training,
        method = lbFuncs,
        tuneLength = 3,
        trControl = fitControl
))
## Now get rid of the submodel parts
lbFunc2 <- lbFuncs
lbFunc2$predict <-
        function(modelFit,
                 newdata,
                 preProc = NULL,
                 submodels = NULL) {
                caTools::predict.LogitBoost(modelFit, newdata, type = "class")
        }

lbFunc2$loop <- NULL


set.seed(825)
lb2 <- system.time(train(
        Class ~ .,
        data = training,
        method = lbFunc2,
        tuneLength = 3,
        trControl = fitControl
))

bigGrid <- data.frame(nIter = seq(1, 151, by = 10))
results <- bigGrid
results$SpeedUp <- NA

for (i in 2:nrow(bigGrid)) {
        rm(lb1, lb2)
        set.seed(825)
        lb1 <- system.time(
                train(
                        Class ~ .,
                        data = training,
                        method = lbFuncs,
                        tuneGrid = bigGrid[1:i, , drop = FALSE],
                        trControl = fitControl
                )
        )
        
        set.seed(825)
        lb2 <- system.time(
                train(
                        Class ~ .,
                        data = training,
                        method = lbFunc2,
                        tuneGrid = bigGrid[1:i, , drop = FALSE],
                        trControl = fitControl
                )
        )
        results$SpeedUp[i] <- lb2[3] / lb1[3]
}

ggplot(results, aes(x = nIter, y = SpeedUp)) +
        geom_point() + geom_smooth(method = "lm") +
        xlab("LogitBoost Iterations") +
        ylab("Speed-Up")


##### Example 3: Nonstandard Formulas
library(mboost)
data("bodyfat", package = "TH.data")
mod <- mboost(DEXfat ~ btree(age) + bols(waistcirc) + bbs(hipcirc),
              data = bodyfat)
modelInfo <- list(
        label = "Model-based Gradient Boosting",
        library = "mboost",
        type = "Regression",
        parameters = data.frame(
                parameter = "parameter",
                class = "character",
                label = "parameter"
        ),
        grid = function(x, y, len = NULL, search = "grid")
                data.frame(parameter = "none"),
        loop = NULL,
        fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                ## mboost requires a data frame with predictors and response
                dat <-
                        if (is.data.frame(x))
                                x
                else
                        as.data.frame(x)
                dat$DEXfat <- y
                mod <- mboost(DEXfat ~ btree(age) + bols(waistcirc) + bbs(hipcirc),
                              data = dat)
        },
        predict = function(modelFit, newdata,
                           submodels = NULL) {
                if (!is.data.frame(newdata))
                        newdata <- as.data.frame(newdata)
                ## By default a matrix is returned; we convert it to a vector
                predict(modelFit, newdata)[, 1]
        },
        prob = NULL,
        predictors = function(x, ...) {
                unique(as.vector(variable.names(x)))
        },
        tags = c("Ensemble Model", "Boosting", "Implicit Feature Selection"),
        levels = NULL,
        sort = function(x)
                x
)

## Just use the basic formula method so that these predictors
## are passed 'as-is' into the model fitting and prediction
## functions.
set.seed(307)
library(caret)
mboost_resamp <- train(DEXfat ~ age + waistcirc + hipcirc, 
                       data = bodyfat,
                       method = modelInfo,
                       trContro = trainControl(method = "repeatedcv",
                                               repeats = 5)
)

##### Example 4: PLS Feature Extraction Pre-Processing
data(tecator)
set.seed(930)
colnames(absorp) <- paste("x", 1:ncol(absorp))

## We will model the protein content data
trainMeats <- createDataPartition(endpoints[, 3], p = 3/4)
absorpTrain <- absorp[trainMeats[[1]], ]
proteinTrain <- endpoints[trainMeats[[1]], 3]
absoprTest <- absorp[- trainMeats[[1]], ]
proteinTest <- endpoints[- trainMeats[[1]], 3]

pls_rf <- list(
        label = "PLS-RF",
        library = c("pls", "randomForest"),
        type = "Regression",
        ## Tune over both parameters at the same time
        parameters = data.frame(
                parameters = c("ncomp", "mtry"),
                class = c("numeric", "numeric"),
                label = c("#Components", "#Randomly Selected Predictors")
        ),
        grid = function(x, y, len = NULL, search = "grid") {
                if (search == "grid") {
                        grid <- expand.grid(ncomp = seq(1, min(ncol(x) - 1, len), by = 1),
                                            mtry = 1:len)
                } else {
                        grid <- expand.grid(ncomp = sample(1:ncol(x), size = len),
                                            mtry = sample(1:ncol(x), size = len))
                }
                ## We can't have mtry > ncomp
                grid <- subset(grid, mtry <= ncomp)
        },
        loop = NULL,
        fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                ## First fit the pls model, generate the training set scores,
                ## then attach what is needed to the random forest object to
                ## be used later
                
                ## plsr only has a formula interface so create one data frame
                dat <- x
                dat$y <- y
                pre <- plsr(y~ ., data = dat, ncomp = param$ncomp)
                scores <- predict(pre, x, type = "scores")
                colnames(scores) <- paste("score", 1:param$ncomp, sep = "")
                mod <- randomForest(scores,  y, mtry = param$mtry, ...)
                mod$projection <- pre$projection
                mod
        },
        predict = function(modelFit, newdata, submodels = NULL) {
                ## Now apply the same scaling to the new samples
                scores <- as.matrix(newdata) %*% modelFit$projection
                colnames(scores) <- paste("score", 1:ncol(scores), sep = "")
                scores <- as.data.frame(scores)
                ## Predict the random forest model
                predict(modelFit, scores)
        },
        prob = NULL,
        varImp = NULL,
        predictors = function(x, ...) rownames(x$projection),
        levels = function(x) x$obsLevels,
        sort = function(x) x[order(x[, 1]), ]
)

meatCtrl <- trainControl(method = "repeatedcv", repeats = 5)

## These will take a while for these data
set.seed(184)
plsrf <- train(x = as.data.frame(absorpTrain), y = proteinTrain,
               method = pls_rf,
               preProc = c("center", "scale"),
               tuneLength = 10,
               ntree = 1000,
               trControl = meatCtrl) #####
ggplot(plsrf, plotType = "level")

## How does random forest do on its own?
set.seed(184)
rfOnly <- train(absorpTrain, proteinTrain,
                method = "rf",
                tuneLength = 10,
                ntree = 1000,
                trControl = meatCtrl)
getTrainPerf(rfOnly)

## How does random forest do on its own?
set.seed(184)
plsOnly <- train(absorpTrain, proteinTrain,
                 method = "pls",
                 tuneLength = 20,
                 preProc = c("center", "scale"),
                 trControl = meatCtrl)
getTrainPerf(plsOnly)