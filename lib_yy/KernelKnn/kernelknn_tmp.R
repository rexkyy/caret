library(KernelKnn)
# Distance
# Widely used distance metrics are the 
# euclidean, manhattan, chebyshev, minkowski(default the order 'p' equals k) and hamming
# (canberra, braycurtis, mahalanobis)
# for binary data [0, 1]
# - Rao_coefficient, jaccard_coefficient, simple_matching_coefficient, pearson_correlation


# Weight (kernels) 왜 kernel이라고 하지?

data("ionosphere")
ionosphere1 <- apply(ionosphere, 2, function (x) length(unique(x)))
ionosphere1 <- ionosphere[, -2]

x <- scale(ionosphere1[, -ncol(ionosphere1)])
y <- ionosphere1[, ncol(ionosphere1)]

# convert y from factor to numeric in classification
y <- c(1:length(unique(y)))[match(ionosphere1$class, sort(unique(ionosphere1$class)))]

# random split in train-test
spl_train <- sample(1:length(y), round(length(y) * 0.75))
spl_test <- setdiff(1:length(y), spl_train)

str(spl_train)
str(spl_test)
# evaluation metric
acc <- function(y_true, preds) { # Accuracy
  
  out <- table(y_true, max.col(preds, ties.method = "random"))
  acc <- sum(diag(out)) / sum(out)
  acc
}

preds_TEST <- KernelKnn(x[spl_train, ], TEST_data = x[spl_test, ],
                        y[spl_train], k = 5,
                        method = "euclidean", weights_function = NULL,
                        regression = FALSE,
                        Levels = unique(y))
head(preds_TEST)  # why? probability??? not 1 or 0

### Two ways to use a kernel in the KernelKnn function
# First option
# choose one of the existing kernels (uniform, triangular, epanechnikov, 
# biweight, triweight, tricube, gaussian, cosine, logistic, silverman, inverse,
# gaussianSimple, exponential)
preds_TEST_tric <- KernelKnn(x[spl_train, ], TEST_data = x[spl_test, ],
                             y[spl_train], k = 10, method = 'canberra',
                             weights_function = 'tricube',
                             regression = FALSE,
                             Levels = unique(y))

# Second option
norm_kernel <- function(W) {
  W <- dnorm(W, mean = 0, sd = 1.0)
  W <- W / rowSums(W)
  return(W)
}

preds_TEST_norm <- KernelKnn(x[spl_train, ], TEST_data = x[spl_test, ],
                             y[spl_train], k = 10,
                             method = 'canberra', weights_function = norm_kernel,
                             regression = FALSE,
                             Levels = unique(y))

fit_cv_pair1 <- KernelKnnCV(x, y, k = 10, folds = 5, method = 'canberra',
                            weights_function = 'tricube', regression = FALSE,
                            Levels = unique(y), threads = 5) # threads option 은 뭐지?
str(fit_cv_pair1)

fit_cv_pair2 <- KernelKnnCV(x, y, k = 9, folds = 5, method = 'canberra',
                            weights_function = 'epanechnikov', regression = FALSE,
                            Levels = unique(y), threads = 5)
str(fit_cv_pair2)

acc_pair1 <- unlist(lapply(1:length(fit_cv_pair1$preds), 
                           function (x) acc(y[fit_cv_pair1$folds[[x]]],
                                            fit_cv_pair1$preds[[x]])))
acc_pair1

cat('accurcay for params_pair1 is :', mean(acc_pair1), '\n')

acc_pair2 <- unlist(lapply(1:length(fit_cv_pair2$preds), 
                           function (x) acc(y[fit_cv_pair2$folds[[x]]],
                                            fit_cv_pair2$preds[[x]])))
acc_pair2
cat('accuracy for params_pair2 is :', mean(acc_pair2), '\n')



################# KernelKnn with caret 
KkNN <- list(
  type = c("Classification"), # Regression
  library = "KernelKnn",
  loop = NULL, # WHAT??
  
  # parameter elements
  # State parameter elements and class label
  parameters = data.frame(
    parameter = c("k"),
    class = c("numeric"),
    label = c("#Neighbors")
  ),
  
  # grid element
  # 
  grid = function (x, y, len = NULL, search = "grid") {
    if (search == "grid") {
      out <- data.frame(
        k = (5:(2 * len + 4))[(5:(2 * len + 4)) %% 2 > 0],
        distance = "euclidean", # other method possible
        kernel = "optimal" # kernel 이 optimal은 뭐지?,
      )
    } else {
      by_val <- if (is.factor(y)) length(levels(y)) else 1
      out <- data.frame(k = sample(seq(1, floor(nrow(x) / 3), by = by_val), size = len, replace = TRUE))
    }
  }
  
  # fit element
  fit = function (x, y, wts, param, lev, last, weight, classProb, ...) {
    KernelKnn::KernelKnn(
      data = as.matrix(x), 
      y = y,
      k = param$k,
      method = param$distance,
      weights_function = NULL, # weight should be parameter? weight value in fit
      regression = FALSE,
      transf
    )
  }
)

# parameter elements
# prm <- data.frame(parameter = c("k", "distance", "kernel"),
#                   class = c("numeric", "character", "character"),
#                   label = c("#Neighbors", "Distance", "Kernel"))
prm <- data.frame(parameter = c("k"),
                  class = c("numeric"),
                  label = c("#Neighbors"))
KkNN$parameters <- prm

# grid element
# knnGrid <- function(x, y, len = NULL, search = "grid") {
#   if(search == "grid") {
#     out <- data.frame(kmax = (5:((2 * len)+4))[(5:((2 * len)+4))%%2 > 0], ## need to edit
#                       distance = 2,
#                       kernel = "optimal")
#   } else {
#     by_val <- if(is.factor(y)) length(levels(y)) else 1
#     kerns <- c("rectangular", "triangular", "epanechnikov", 
#                "biweight", "triweight", "cos", "inv", "gaussian")
#     out <- data.frame(kmax = sample(seq(1, floor(nrow(x)/3), by = by_val), size = len, replace = TRUE),
#                       distance = runif(len, min = 0, max = 3),
#                       kernel = sample(kerns, size = len, replace = TRUE))
#   }
#   out
# }
# 
# KkNN$grid <- knnGrid()

# fit element
KkNNFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  # x, y, kernel, kpar, C, prob.model
  dat <- if (is.data.frame(x)) x else as.data.frame(x, stringsAsFa)
  KernelKnn::KernelKnn(
    
  )
}
