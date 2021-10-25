rm(list = ls())
library(caret)
library(rpart)


# hyperparameter: maxdepth, cp
# cp를 고려하면 maxdepth는 고려하지 않아도 되고 그런건가? 어차피 정해지는것이니까?

# cart algorithm classification & regression 
decisiontree_slot <- list(
  type = c("Classification", "Regression"),
  
  library = "rpart"
)

parameters <- data.frame(parameter = c("cp"),
                         class = "numeric",
                         label = "Complexity Parameter",
                         stringsAsFactors = FALSE)

grid <- function(x, y, len = NULL, search = "grid") {
  dat <- if (is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
  dat$.outcome <- y
  
  initialFit <- rpart::rpart(.outcome ~ ., 
                             data = dat, 
                             control = rpart::rpart.control(cp = 0))$cptable
  
  initialFit <- initialFit[order(-initialFit[, "CP"]), , drop = FALSE]
  if (search == "grid") {
    if (nrow(initialFit) < len) {
      tuneSeq <- data.frame(cp = seq(min(initialFit[, "CP"]),
                                     max(initialFit[, "CP"]),
                                     length = len))
    } else {
      tuneSeq <- data.frame(cp = initialFit[1:len, "CP"])
    }
    colnames(tuneSeq) <- "cp"
  } else { # random search
    tuneSeq <- data.frame(cp = unique(sample(initialFit[, "CP"], size = len, replace = TRUE)))
  }
  
  return(tuneSeq)
}


fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
  # wts, weights 뭐지? 둘다 같은건가?
  
  cpValue <- if (!last) param$cp else 0
  theDots <- list(...)
  # rpart 에서 사용하는 rpart.control이 여기에 들어감
  if (any(names(theDots) == "control")) {
    theDots$control$cp <- cpValue
    theDots$control$xval <- 0
    ctl <- theDots$control
    theDots$control <- NULL
  } else {
    ctl <- rpart::rpart.control(cp = cpValue, xval = 0)
  }
  
  ## check to see if weights were passed in (and available)
  if (!is.null(wts)) {
    theDots$weights <- wts
  }
  modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                      data = if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE),
                      control = ctl),
                 theDots)
  modelArgs$data$.outcome <- y
  
  out <- do.call(rpart::rpart, modelArgs)
  if (last) out <- rpart::prune.rpart(out, cp = param$cp) # last 가 뭐지?
  
  return(out)
}


predict_fun <- function(modelFit, newdata, submodels = NULL) {
  
  if (!is.data.frame(newdata)) {
    newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  }
  
  
  cat(paste0("(predict function) modelFit$problemType: ", modelFit$problemType, "\n"))
  pType <- if (modelFit$problemType == "Classification") { "class" } else { "vector" }
  cat(paste0("(predict function) pType: ", pType, "\n"))
  
  out <- predict(modelFit, newdata, type = pType)
  
  # if (!is.null(submodels)) {
  #   tmp <- vector(mode = "list", legnth = nrow(submodels) + 1)
  #   tmp[[1]] <- out
  #   for (j in seq(along = submodels$cp)) {
  #     prunedFit <- rpart::prune.rpart(modelFit, cp = submodels$cp[j])
  #     tmp[[j + 1]] <- predict(prunedFit, newdata, type = pType)
  #   }
  #   out <- tmp
  # }
  return(out)
}


prob <- function(modelFit, newdata, submodels = NULL) {
  if (!is.data.frame(newdata)) {
    print("come in!")
    newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
    out <- predict(modelFit, newdata, type = "prob")
  }
  
  # if (!is.null(submodels)) {
  #   tmp <- vector(mode = "list", length = nrow(submodels) + 1)
  #   tmp[[1]] <- out
  #   for (j in seq(along = submodels$cp)) {
  #     prunedFit <- rpart::prune.rpart(modelFit, cp = submodels$cp[j])
  #     tmpProb <- predict(prunedFit, newdata, type = "prob")
  #     tmp[[j + 1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, drop = FALSE], 
  #                                   stringsAsFactors = FALSE)
  #   }
  #   out <- tmp
  # }
  return(out)
}

decisiontree_slot$parameters <- parameters
decisiontree_slot$grid <- grid
decisiontree_slot$fit <- fit
decisiontree_slot$predict <- predict_fun
decisiontree_slot$prob <- prob


# Initial Value ----------------
# Data import
data("PimaIndiansDiabetes2", package = "mlbench")
dataset <- na.omit(PimaIndiansDiabetes2)
# initial value
dep_var <- "diabetes"
indep_var <- c("glucose", "pressure", "insulin", "mass")
p_train <- 80
valid_method <- "cv"
search_method <- "grid"
analysis_type <- "Classification"
iter <- 2
fold <- 2
tune_len <- 5

# train control
decisiontree_control <- trainControl(
  method = "cv",
  number = ifelse(grepl("cv", valid_method), fold, 1),
  repeats = ifelse(grepl("[d_]cv$", valid_method), iter, NA),
  search = search_method,
  savePredictions = TRUE,
  classProbs = ifelse(analysis_type == "Classification", TRUE, FALSE),
  verboseIter = TRUE)

# Data split
train_idx <- createDataPartition(dataset[, dep_var], p = p_train/100)$Resample1
train_set <- dataset[train_idx, , drop = FALSE]
test_set <- dataset[-train_idx, , drop = FALSE]

# train
decisiontree_fit <- train(
  x = train_set[, indep_var],
  y = train_set[, dep_var],
  method = decisiontree_slot,
  tuneLength = tune_len,
  trControl = decisiontree_control,
  tuneGrid = NULL,
  preProcess = NULL,
  weights = NULL)






# parameter combination by splitting method
# what matters if splitting method differ?
# anova: no parameters / poisson: single parameter, default = 1
# exponential: same as poisson
# the priors must be positive and sum to 1





