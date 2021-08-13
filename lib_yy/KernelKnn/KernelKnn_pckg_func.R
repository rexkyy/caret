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

##### kknn raw function #####
function (formula = formula(train), train, test, na.action = na.omit(), 
          k = 7, distance = 2, kernel = "optimal", ykernel = NULL, 
          scale = TRUE, contrasts = c(unordered = "contr.dummy", 
                                      ordered = "contr.ordinal")) 
{
  if (is.null(ykernel)) 
    ykernel = 0
  weight.y = function(l = 1, diff = 0) {
    k = diff + 1
    result = matrix(0, l, l)
    diag(result) = k
    for (i in 1:(k - 1)) {
      for (j in 1:(l - i)) {
        result[j, j + i] = k - i
        result[j + i, j] = k - i
      }
    }
    result
  }
  kernel <- match.arg(kernel, c("rectangular", "triangular", 
                                "epanechnikov", "biweight", "triweight", 
                                "cos", "inv", "gaussian", "rank", 
                                "optimal"), FALSE)
  ca <- match.call()
  response = NULL
  old.contrasts <- getOption("contrasts")
  options(contrasts = contrasts)
  formula = as.formula(formula)
  mf <- model.frame(formula, data = train)
  mt <- attr(mf, "terms")
  mt2 <- delete.response(mt)
  cl <- model.response(mf)
  d <- sum(attr(mt, "order"))
  if (is.ordered(cl)) {
    response <- "ordinal"
    lev <- levels(cl)
  }
  if (is.numeric(cl)) 
    response <- "continuous"
  if (is.factor(cl) & !is.ordered(cl)) {
    response <- "nominal"
    lev <- levels(cl)
  }
  if (distance <= 0) 
    stop("distance must >0")
  if (k <= 0) 
    stop("k must >0")
  learn <- model.matrix(mt, mf)
  valid <- model.matrix(mt2, test)
  m <- dim(learn)[1]
  p <- dim(valid)[1]
  q <- dim(learn)[2]
  ind <- attributes(learn)$assign
  d.sd <- numeric(length(ind)) + 1
  we <- numeric(length(ind)) + 1
  d.sd = apply(learn, 2, stats::var)
  for (i in unique(ind)) {
    d.sd[ind == i] = sqrt(mean(d.sd[ind == i]))
    we[ind == i] = 1/sum(ind == i)
  }
  we[d.sd == 0] = 0
  d.sd[d.sd == 0] = 1
  if (scale) {
    learn <- sweep(learn, 2L, d.sd, "/", check.margin = FALSE)
    valid <- sweep(valid, 2L, d.sd, "/", check.margin = FALSE)
  }
  ord = order(we * apply(learn, 2, sd), decreasing = TRUE)
  we = we[ord]
  learn = learn[, ord, drop = FALSE]
  valid = valid[, ord, drop = FALSE]
  Euclid <- FALSE
  if (distance == 2) 
    Euclid <- TRUE
  if (Euclid) 
    dmtmp <- .C("dmEuclid", as.double(learn), as.double(valid), 
                as.integer(m), as.integer(p), as.integer(q), 
                dm = double((k + 1L) * p), 
                cl = integer((k + 1L) * p), 
                k = as.integer(k + 1), 
                as.double(distance), as.double(we), PACKAGE = "kknn")
  else dmtmp <- .C("dm", as.double(learn), as.double(valid), 
                   as.integer(m), as.integer(p), as.integer(q), 
                   dm = double((k + 1L) * p), 
                   cl = integer((k + 1L) * p), 
                   k = as.integer(k + 1), as.double(distance), 
                   as.double(we), PACKAGE = "kknn")
  D <- matrix(dmtmp$dm, nrow = p, ncol = k + 1)
  C <- matrix(dmtmp$cl, nrow = p, ncol = k + 1)
  maxdist <- D[, k + 1]
  maxdist[maxdist < 1e-06] <- 1e-06
  D <- D[, 1:k]
  C <- C[, 1:k] + 1
  CL <- matrix(cl[C], nrow = p, ncol = k)
  if (response != "continuous") {
    l <- length(lev)
    weightClass <- matrix(0, p, l)
  }
  if (response == "continuous") {
    weightClass <- NULL
  }
  W <- D/maxdist
  W <- pmin(W, 1 - (1e-06))
  W <- pmax(W, 1e-06)
  if (kernel == "rank") 
    W <- (k + 1) - t(apply(as.matrix(D), 1, rank))
  if (kernel == "inv") 
    W <- 1/W
  if (kernel == "rectangular") 
    W <- matrix(1, nrow = p, ncol = k)
  if (kernel == "triangular") 
    W <- 1 - W
  if (kernel == "epanechnikov") 
    W <- 0.75 * (1 - W^2)
  if (kernel == "biweight") 
    W <- dbeta((W + 1)/2, 3, 3)
  if (kernel == "triweight") 
    W <- dbeta((W + 1)/2, 4, 4)
  if (kernel == "cos") 
    W <- cos(W * pi/2)
  if (kernel == "triweights") 
    W <- 1
  if (kernel == "gaussian") {
    alpha = 1/(2 * (k + 1))
    qua = abs(qnorm(alpha))
    W = W * qua
    W = dnorm(W, sd = 1)
  }
  if (kernel == "optimal") {
    W = rep(optKernel(k, d = d), each = p)
  }
  W <- matrix(W, p, k)
  if (response != "continuous") {
    for (i in 1:l) {
      weightClass[, i] <- rowSums(W * (CL == lev[i]))
    }
    weightClass <- weightClass/rowSums(weightClass)
    colnames(weightClass) <- lev
  }
  if (response == "ordinal") {
    blub = length(lev)
    weightClass = weightClass %*% weight.y(blub, ykernel)
    weightClass <- weightClass/rowSums(weightClass)
    weightClass <- t(apply(weightClass, 1, cumsum))
    colnames(weightClass) <- lev
    fit <- numeric(p)
    for (i in 1:p) fit[i] <- min((1:l)[weightClass[i, ] >= 0.5])
    fit <- ordered(fit, levels = 1:l, labels = lev)
  }
  if (response == "nominal") {
    fit <- apply(weightClass, 1, order, decreasing = TRUE)[1, ]
    fit <- factor(fit, levels = 1:l, labels = lev)
    if (kernel == "rectangular" && k > 1) {
      blub <- apply(weightClass, 1, rank, ties.method = "max")
      indices = (1:p)[colSums(blub == l) > 1]
      blub = t(blub)
      nM = matrix(0, p, l)
      colnames(nM) = lev
      for (i in 1:l) nM[, i] = apply((CL == lev[i]) %*% diag(1:k), 1, max)
      nM = (blub == l) * nM
      nM[nM == 0] <- k + 1
      fitv = numeric(p)
      for (i in indices) fitv[i] = which(nM[i, ] == min(nM[i, ]))
      fit[indices] <- factor(fitv[indices], levels = 1:l, labels = lev)
    }
  }
  if (response == "continuous") 
    fit <- rowSums(W * CL)/pmax(rowSums(W), 1e-06)
  options(contrasts = old.contrasts)
  result <- list(fitted.values = fit, 
                 CL = CL, 
                 W = W, 
                 D = D, 
                 C = C, 
                 prob = weightClass, 
                 response = response, 
                 distance = distance, 
                 call = ca, 
                 terms = mt)
  class(result) = "kknn"
  result
}