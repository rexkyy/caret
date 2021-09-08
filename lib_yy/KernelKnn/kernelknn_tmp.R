########## KernelKnn with caret
#### model fit
library(KernelKnn)
library(caret)
library(mlbench) # don't need to use
# Sonar data
data("Sonar")
inTraining <- createDataPartition(Sonar$Class, p = 0.75, list = FALSE)
training <- Sonar[inTraining, ]
testing <- Sonar[-inTraining, ]

# BostonHousing data for Regression
# data("BostonHousing")
# inTraining_BH <- createDataPartition(BostonHousing$medv, p = 0.75, list = FALSE)
# training_BH <- BostonHousing[inTraining_BH, ]
# testing_BH <- BostonHousing[-inTraining_BH, ]


##### REx module Function Arguments #####
### Variable
res_var <- "Class"
qual_var <- NULL
quan_var <- colnames(Sonar)[1:10]
### result
df <- Sonar[, c(res_var, qual_var, quan_var)]
normalize <- FALSE #. preproc에 적용
### Analysis
analysis_type <- "classification" # "regression"
distance_method <- "euclidean" # "manhattan"
weight_method <- "uniform"
k <- 4 # c(2, 3) # 4
### Validation
resample_method <- "none" # "cv" # 
resample_iter <- 1
resample_fold <- 2
# resample_repeats <- NULL # 5
train_p <- 0.7
search_method <- "grid" # "grid" "random"
# resample_p <- NULL
optm_metric <- "accuracy"
### Output


##### 0. Data Preparing
y <- df[, res_var]
x <- df[, c(qual_var, quan_var), drop = FALSE]

##### 1. Data Split
set.seed(333) #. need?
#. confirm if data partition needed when using resampling method
train_idx <- createDataPartition(y, times = resample_iter, p = train_p, 
                                 list = TRUE, groups = min(5, length(y)))

#. Iteration with train data generated. default = 1
i <- 1
training_y <- y[train_idx[[i]]]
training_x <- x[train_idx[[i]], ]
# test_y <- y[-train_idx[[i]], ]
# test_x <- x[-train_idx[[i]], ]
#. need to check if there is two class in training and test set

##### 2. Customized Functions
# Required list: type, library, parameters, grid, fit, predict, prob
KkNN <- list(
  type = c("Classification"), # Regression
  
  library = "KernelKnn",
  
  parameters = data.frame(
    parameter = c("k", "distance", "weight"),
    class = c("numeric", "character", "character"),
    label = c("Neighbors", "Distance", "Weight"),
    stringsAsFactors = FALSE
  ),
  
  grid = function (x, y, len = NULL, search = search_method) {
    if (search == "grid") {
      out <- data.frame(
        k = k,
        distance = "euclidean", #rep(distance_method, length(k)),
        weight = "gaussian", #rep("gaussian", length(k))
        stringsAsFactors = FALSE
      )
    } else {
      stop("Error: random search is not yet implemented")
    }
    out
  },
  
  fit = function (x, y, wts, param, lev, last, weights = weight_method, 
                  classProb = ifelse(analysis_type == "classification", FALSE, TRUE),
                  #. need to check
                  dist_method = distance_method, ...) {
    y_level <- levels(y)
    y_vec <- as.numeric(y)
    
    kernknn_res <- KernelKnn::KernelKnn(
      data = as.matrix(x), 
      y = y_vec,
      k = param$k,
      method = param$distance,
      weights_function = param$weight,
      h = 1, #weight function bandwidth - default value should be ??
      regression = classProb,
      transf_categ_cols = FALSE,
      threads = 1,
      extrema = F,
      Levels = unique(y_vec)
    )
    colnames(kernknn_res) <- y_level
    
    # Reconstruct 'KernelKnn' result
    if (classProb) { 
      stop("Error: fitted.values for regression not yet implemented.")
      # fitted.values <- rowsums(W*CL) / pmax(rowsoms(W), 1e-06)
    } else { 
      fitted.values <- apply(kernknn_res, 1, order, decreasing = TRUE)[1, ]
      for (i in 1:length(y_level)) {
        fitted.values[which(fitted.values == i)] <- y_level[i]
      }
    }
    
    # dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
    # modForm <- caret:::smootherFormula(x)
    # if(is.factor(y)) {
    #   dat$.outcome <- ifelse(y == lev[1], 0, 1)
    # } else {
    #   dat$.outcome <- y
    # }
    # modelArgs <- list(formula = modForm,
    #                   data = dat,
    #                   k = param$k,
    #                   weights_function = param$weight,
    #                   dist_method = param$distance)
    
    out <- list(
      prob = kernknn_res,
      fitted.values = as.factor(fitted.values),
      problemType = analysis_type
    )
    
    return(out)
  },
  
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    if(modelFit$problemType == "Classification") {
      out <- predict(modelFit, newdata,  type = "class")
    } else {
      out <- predict(modelFit, newdata)
    }
    out
  },
  
  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    predict(modelFit, newdata, type = "probabilities")
  }
)

##### 2. Train Options
resample_method <- "cv"
fitControl <- trainControl(method = resample_method,
                           number = ifelse(grepl("cv", resample_method), resample_fold, 1),
                           repeats = ifelse(grepl("[d_]cv$", resample_method), 1, NA),
                           search = search_method,
                           savePredictions = TRUE,
                           classProbs = FALSE,
                           verboseIter = TRUE
                           )

##### 3. Train: Fit the model
set.seed(825)

# data type transpose
train_set <- cbind(training_x, training_y)
colnames(train_set)[ncol(train_set)] <- res_var #. could be iter
kernel_knn <- train(Class ~ ., 
                    data = train_set,
                    method = KkNN,
                    tuneGrid = data.frame(k = c(2, 3),
                                          distance = c("euclidean", "euclidean"),#rep("euclidean", length(k)),
                                          weight = c("gaussian","gaussian"),
                                          stringsAsFactors = FALSE), #rep("gaussian", length(k))),
                    trControl = fitControl)



###### model info TEST --------------
# multiple_param_model <- which(unlist(lapply(aa, function (x) { if(nrow(x$parameters) > 1) { 
#   TRUE
#   } else {FALSE} 
# })))
# 
# model_df <- cbind(1:length(multiple_param_model), as.matrix(multiple_param_model))
# multiple_param_model[i]
# 
# i <- 10
# aa[[multiple_param_model[i]]]
# #
# 
# aa[[multiple_param_model[i]]]$fit
# aa[[multiple_param_model[i]]]$predict
# 
# ### kknn example
# yy <- as.numeric(training_y)
# # KernelKnn(training_x, y = yy, k = 2, weights_function = "gaussian", Levels = unique(yy))
# kknncv <- KernelKnnCV(training_x, y = yy, k = 2, folds = 3, weights_function = "gaussian", Levels = unique(yy))
# kknncv$preds
# 