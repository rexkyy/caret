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
weight_method <- NULL # "uniform"
k <- c(2, 3) # 4
### Validation
resample_method <- "cv" # "none"
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
# Assume data cleaning already applied (eg. treating missing value or generating dummy var)
set.seed(333) #. need?
#. confirm if data partition needed when using resampling method
# test set을 정해놓는게 나은지? 여러가지를 test set으로 하는게 나은지?
train_idx <- createDataPartition(y, times = resample_iter, p = train_p, 
                                 list = TRUE, groups = min(5, length(y)))

#. Iterate with train data generated default = 1
i <- 1
training_y <- y[train_idx[[i]]] # data.frame format 고정 어떻게???
training_x <- x[train_idx[[i]], ]
# test_y <- y[-train_idx[[i]], ]
# test_x <- x[-train_idx[[i]], ]
#. need to check if there is two class in training and test set

##### 1. Customized Functions
KkNN <- list(
  type = c("Classification"), # Regression
  
  library = "KernelKnn",
  
  parameters = data.frame(
    parameter = c("k"),
    class = c("numeric"),
    label = c("Neighbors")
  ),
  
  grid = function (x, y, len, search = search_method) {
    if (search == "grid") { 
      #. 만약 UI에서 들어온 값을 그대로 쓰면 어떻게됨? 
      # grid search 나 random search 를 쓰면 UI는 어떻게 구성해야함?
      out <- data.frame(
        k = len
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
      method = dist_method, 
      weights_function = weights,
      h = 1, #weight function bandwidth
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
    out <- list(
      prob = kernknn_res,
      fitted.values = as.factor(fitted.values),
      problemType = analysis_type
    )
    print(summary(out))
    
    
    return(out)
  },
  
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) {
    # print("modelFit!!!!!")
    print(str(modelFit))
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
# number: either the number of folds or number of resampling iterations
# repeats: For repeated k-folds cross-validation only
# search: grid/random none은 없나? 입력값만 넣어서 할 수 있는 것
fitControl <- trainControl(method = resample_method,
                           number = ifelse(grepl("cv", resample_method), resample_fold, 1),
                           repeats = ifelse(grepl("[d_]cv$", resample_method), 1, NA),
                           search = search_method,
                           savePredictions = TRUE,
                           classProbs = FALSE,
                           verboseIter = TRUE
                           )

##### 3. Train: Fit the model
set.seed(825) # seed 설정 어디서 하는게 좋지? 어디서 또 해야하지?

# data type transpose
train_set <- cbind(training_x, training_y)
colnames(train_set)[ncol(train_set)] <- res_var #. could be iter
kernel_knn <- train(Class ~ ., 
                    data = train_set,
                    method = KkNN,
                    # preProc = c("center", "scale"),
                    tuneGrid = data.frame(k = k),
                    trControl = fitControl)

# kernel_knn #. what is predictor? in the result

#. kernel knn result 다듬어야함
# kernel_knn$results
#. tune은 어떤값이 들어가는거지?
# kernel_knn$bestTune