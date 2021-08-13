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
#                    distance = param$di stance,
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
