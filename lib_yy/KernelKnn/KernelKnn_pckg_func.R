function(data, TEST_data = NULL, y, k = 5, h = 1, method = "euclidean",
         weights_function = NULL, regression = F, transf_categ_cols = F,
         threads = 1, extrema = F, Levels = NULL) {
  categorical_data_present <- sapply(data, function(x) {
    is.factor(x) ||
      is.character(x)
  })
  if (sum(categorical_data_present) && !transf_categ_cols) {
    stop("Categorical columns present in data. These should be either converted to numeric or the function should be run with transf_categ_cols = TRUE")
  }
  if (!is.numeric(k) || is.null(k) || (k >= nrow(data)) ||
    k < 1) {
    stop("k must be of type integer, greater than 0 and less than nrow(train)")
  }
  if (abs(k - round(k)) > 0) {
    k <- round(k)
    warning("k is float and will be rounded to : ",
      call. = F, expr = k
    )
  }
  if (h == 0) {
    stop("h can be any number except for 0")
  }
  if (!is.character(method) || is.null(method) || !method %in%
    c(
      "euclidean", "manhattan", "chebyshev",
      "canberra", "braycurtis", "pearson_correlation",
      "simple_matching_coefficient", "minkowski",
      "hamming", "mahalanobis", "jaccard_coefficient",
      "Rao_coefficient"
    )) {
    stop("method must be of type character and one of 'euclidean', 'manhattan', 'chebyshev', 'canberra', 'braycurtis', 'pearson_correlation', 'simple_matching_coefficient',\n         'minkowski', 'hamming', 'mahalanobis', 'jaccard_coefficient', 'Rao_coefficient'")
  }
  if (is.null(y)) {
    stop("the response variable should be numeric")
  }
  if (is.integer(y)) {
    y <- as.numeric(y)
  }
  if (!is.numeric(y)) {
    stop("in both regression and classification the response variable should be numeric or integer and in classification it should start from 1")
  }
  if (!regression && is.null(Levels)) {
    stop("In classification give the unique values of y in form of a vector")
  }
  if (!regression && any(unique(y) < 1)) {
    stop("the response variable values should begin from 1")
  }
  if (any(is.na(data)) || any(is.na(y))) {
    stop("the data or the response variable includes missing values")
  }
  if (!is.null(TEST_data) && any(is.na(TEST_data))) {
    stop("the TEST_data includes missing values")
  }
  if (length(y) != nrow(data)) {
    stop("the size of the data and y differ")
  }
  if (extrema && k < 4) {
    stop("k must be greater than 3 if extrema = TRUE")
  }
  if (method %in% c(
    "simple_matching_coefficient", "jaccard_coefficient",
    "Rao_coefficient"
  ) && !sum(apply(data, 2, function(x) {
    all(unique(x) %in%
      c(0, 1))
  })) == ncol(data)) {
    stop("methods : 'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' require the data to be in binary form e.g 0,1")
  }
  if (!is.null(TEST_data) && method %in% c(
    "simple_matching_coefficient",
    "jaccard_coefficient", "Rao_coefficient"
  ) &&
    !sum(apply(TEST_data, 2, function(x) {
      all(unique(x) %in%
        c(0, 1))
    })) == ncol(TEST_data)) {
    stop("methods : 'simple_matching_coefficient', 'jaccard_coefficient' and 'Rao_coefficient' require the TEST_data to be in binary form e.g 0,1")
  }
  if (!is.null(TEST_data) && ncol(data) != ncol(TEST_data)) {
    stop("the number of columns in train and test data differ")
  }
  if (transf_categ_cols) {
    if (is.null(TEST_data)) {
      data <- func_categorical_preds(data)
    } else {
      tmp_dat <- func_categorical_preds(rbind(data, TEST_data))
      data <- tmp_dat[1:nrow(data), ]
      TEST_data <- tmp_dat[(nrow(data) + 1):nrow(tmp_dat), ]
    }
  }
  if (is.null(TEST_data)) {
    mat <- matrix(, nrow = 0, ncol = 0)
    if (!is.matrix(data)) {
      data <- as.matrix(data)
    }
    if (extrema) {
      k <- k + 2
    }
    index_train <- knn_index_dist_rcpp(data, mat,
      k = k, method = method,
      threads = threads
    )
    if (extrema) {
      index_train$train_knn_idx <- index_train$train_knn_idx[
        ,
        -c(1, k)
      ]
      index_train$train_knn_dist <- index_train$train_knn_dist[
        ,
        -c(1, k)
      ]
      k <- k - 2
    }
    out_train <- matrix(y[index_train$train_knn_idx],
      nrow = nrow(data),
      ncol = k
    )
    if (!regression) {
      if (is.null(weights_function)) {
        out <- func_tbl_dist(out_train, sort(Levels))
        colnames(out) <- paste0("class_", sort(Levels))
      } else if (is.function(weights_function)) {
        W <- FUNCTION_weights(
          index_train$train_knn_dist,
          weights_function
        )
        out <- func_tbl(out_train, W, sort(Levels))
      } else if (is.character(weights_function) && nchar(weights_function) > 1) {
        W <- FUN_kernels(
          weights_function, index_train$train_knn_dist,
          h
        )
        out <- func_tbl(out_train, W, sort(Levels))
      } else {
        stop("false input for the weights_function argument")
      }
    } else {
      if (is.null(weights_function)) {
        out <- rowMeans(out_train)
      } else if (is.function(weights_function)) {
        W <- FUNCTION_weights(
          index_train$train_knn_dist,
          weights_function
        )
        out <- rowSums(out_train * W)
      } else if (is.character(weights_function) && nchar(weights_function) >
        1) {
        W <- FUN_kernels(
          weights_function, index_train$train_knn_dist,
          h
        )
        out <- rowSums(out_train * W)
      } else {
        stop("false input for the weights_function argument")
      }
    }
    return(out)
  } else {
    if (!is.matrix(data)) {
      data <- as.matrix(data)
    }
    if (!is.matrix(TEST_data)) {
      TEST_data <- as.matrix(TEST_data)
    }
    if (extrema) {
      k <- k + 2
    }
    index <- knn_index_dist_rcpp(data, TEST_data,
      k = k, method = method,
      threads = threads
    )
    if (extrema) {
      index$test_knn_idx <- index$test_knn_idx[, -c(1, k)]
      index$test_knn_dist <- index$test_knn_dist[, -c(
        1,
        k
      )]
      k <- k - 2
    }
    out_test <- matrix(y[index$test_knn_idx], ncol = k)
    if (!regression) {
      if (is.null(weights_function)) {
        out_te <- func_tbl_dist(out_test, sort(Levels))
        colnames(out_te) <- paste0("class_", sort(Levels))
      } else if (is.function(weights_function)) {
        W_te <- FUNCTION_weights(
          index$test_knn_dist,
          weights_function
        )
        out_te <- func_tbl(out_test, W_te, sort(Levels))
      } else if (is.character(weights_function) && nchar(weights_function) >
        1) {
        W_te <- FUN_kernels(
          weights_function, index$test_knn_dist,
          h
        )
        out_te <- func_tbl(out_test, W_te, sort(Levels))
      } else {
        stop("false input for the weights_function argument")
      }
    } else {
      if (is.null(weights_function)) {
        out_te <- rowMeans(out_test)
      } else if (is.function(weights_function)) {
        W_te <- FUNCTION_weights(
          index$test_knn_dist,
          weights_function
        )
        out_te <- rowSums(out_test * W_te)
      } else if (is.character(weights_function) && nchar(weights_function) >
        1) {
        W_te <- FUN_kernels(
          weights_function, index$test_knn_dist,
          h
        )
        out_te <- rowSums(out_test * W_te)
      } else {
        stop("false input for the weights_function argument")
      }
    }
    return(out_te)
  }
}




#### kernelKnnCV
# function (data, y, k = 5, folds = 5, h = 1, method = "euclidean",
#           weights_function = NULL, regression = F, transf_categ_cols = F,
#           threads = 1, extrema = F, Levels = NULL, seed_num = 1)
#   # {
#   #   if (length(y) != nrow(data))
#   #     stop("the size of the data and y differ")
#   #   if (!is.logical(regression))
#   #     stop("the regression argument should be either TRUE or FALSE")
#   #   if (any(is.na(data)) || any(is.na(y)))
#   #     stop("the data or the response variable includes missing values")
#   #   if (is.null(y))
#   #     stop("the response variable should be numeric")
#   #   if (is.integer(y))
#   #     y = as.numeric(y)
# #   if (!is.numeric(y))
# #     stop("in both regression and classification the response variable should be numeric or integer and in classification it should start from 1")
# #   if (!regression && any(unique(y) < 1))
# #     stop("the response variable values should begin from 1")
# #   if (folds < 2)
# #     stop("the number of folds should be at least 2")
# #   start = Sys.time()
# #   if (regression) {
# #     set.seed(seed_num)
# #     n_folds = regr_folds(folds, y)
# #   }
# #   else {
# #     set.seed(seed_num)
# #     n_folds = class_folds(folds, as.factor(y))
# #   }
# #   if (!all(unlist(lapply(n_folds, length)) > 5))
# #     stop("Each fold has less than 5 observations. Consider decreasing the number of folds or increasing the size of the data.")
# #   tmp_fit = list()
# #   cat("\n")
# #   cat("cross-validation starts ..", "\n")
# #   pb <- txtProgressBar(min = 0, max = folds, style = 3)
# #   cat("\n")
# for (i in 1:folds) {
#   tmp_fit[[i]] = KernelKnn(data[unlist(n_folds[-i]), ],
#                            TEST_data = data[unlist(n_folds[i]), ], y[unlist(n_folds[-i])],
#                            k = k, h = h, method = method, weights_function = weights_function,
#                            regression = regression, transf_categ_cols = transf_categ_cols,
#                            threads = threads, extrema = extrema, Levels = Levels)
#   setTxtProgressBar(pb, i)
# }
# close(pb)
# cat("\n")
# end = Sys.time()
# t = end - start
# cat("time to complete :", t, attributes(t)$units, "\n")
# cat("\n")
# return(list(preds = tmp_fit, folds = n_folds))
# }
#



######################### kknn modelInfo #############
# $kknn
# $kknn$label
# [1] "k-Nearest Neighbors"
#
# $kknn$library
# [1] "kknn"
#
# $kknn$loop
# NULL
#
# $kknn$type
# [1] "Regression"     "Classification"
#
# $kknn$parameters
# parameter     class           label
# 1      kmax   numeric Max. #Neighbors
# 2  distance   numeric        Distance
# 3    kernel character          Kernel
#
# $kknn$grid
# function(x, y, len = NULL, search = "grid") {
#   if(search == "grid") {
#     out <- data.frame(kmax = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0],
#                       distance = 2,
#                       kernel = "optimal")
#   } else {
#     by_val <- if(is.factor(y)) length(levels(y)) else 1
#     kerns <- c("rectangular", "triangular", "epanechnikov", "biweight", "triweight",
#                "cos", "inv", "gaussian")
#     out <- data.frame(kmax = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE),
#                       distance = runif(len, min = 0, max = 3),
#                       kernel = sample(kerns, size = len, replace = TRUE))
#   }
#   out
# }
#
# $kknn$fit
# function(x, y, wts, param, lev, last, classProbs, ...) {
#   dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
#   dat$.outcome <- y
#   kknn::train.kknn(.outcome ~ ., data = dat,
#                    kmax = param$kmax,
#                    distance = param$distance,
#                    kernel = as.character(param$kernel), ...)
# }
#
# $kknn$predict
# function(modelFit, newdata, submodels = NULL) {
#   if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
#   predict(modelFit, newdata)
# }
#
# $kknn$levels
# function(x) x$obsLevels
#
# $kknn$tags
# [1] "Prototype Models"
#
# $kknn$prob
# function(modelFit, newdata, submodels = NULL) {
#   if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
#   predict(modelFit, newdata, type = "prob")
# }
#
# $kknn$sort
# function(x) x[order(-x[,1]),]


######## knn modelInfo ############
# $knn
# $knn$label
# [1] "k-Nearest Neighbors"
#
# $knn$library
# NULL
#
# $knn$loop
# NULL
#
# $knn$type
# [1] "Classification" "Regression"
#
# $knn$parameters
# parameter   class      label
# 1         k numeric #Neighbors
#
# $knn$grid
# function(x, y, len = NULL, search = "grid"){
#   if(search == "grid") {
#     out <- data.frame(k = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0])
#   } else {
#     by_val <- if(is.factor(y)) length(levels(y)) else 1
#     out <- data.frame(k = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE))
#   }
# }
#
# $knn$fit
# function(x, y, wts, param, lev, last, classProbs, ...) {
#   if(is.factor(y))
#   {
#     knn3(as.matrix(x), y, k = param$k, ...)
#   } else {
#     knnreg(as.matrix(x), y, k = param$k, ...)
#   }
# }
#
# $knn$predict
# function(modelFit, newdata, submodels = NULL) {
#   if(modelFit$problemType == "Classification")
#   {
#     out <- predict(modelFit, newdata,  type = "class")
#   } else {
#     out <- predict(modelFit, newdata)
#   }
#   out
# }
#
# $knn$predictors
# function(x, ...) colnames(x$learn$X)
#
# $knn$tags
# [1] "Prototype Models"
#
# $knn$prob
# function(modelFit, newdata, submodels = NULL)
#   predict(modelFit, newdata, type = "prob")
#
# $knn$levels
# function(x) levels(x$learn$y)
#
# $knn$sort
# function(x) x[order(-x[,1]),]
