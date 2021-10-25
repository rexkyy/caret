### rpart
$label
[1] "CART"

$library
[1] "rpart"

$type
[1] "Regression"     "Classification"

$parameters
parameter   class                label
1        cp numeric Complexity Parameter

$grid
function(x, y, len = NULL, search = "grid"){
  dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
  dat$.outcome <- y
  initialFit <- rpart::rpart(.outcome ~ .,
                             data = dat,
                             control = rpart::rpart.control(cp = 0))$cptable
  initialFit <- initialFit[order(-initialFit[,"CP"]), , drop = FALSE]
  if(search == "grid") {
    if(nrow(initialFit) < len) {
      tuneSeq <- data.frame(cp = seq(min(initialFit[, "CP"]),
                                     max(initialFit[, "CP"]),
                                     length = len))
    } else tuneSeq <-  data.frame(cp = initialFit[1:len,"CP"])
    colnames(tuneSeq) <- "cp"
  } else {
    tuneSeq <- data.frame(cp = unique(sample(initialFit[, "CP"], size = len, replace = TRUE)))
  }
  
  tuneSeq
}

$loop
function(grid) {
  grid <- grid[order(grid$cp, decreasing = FALSE),, drop = FALSE]
  loop <- grid[1,,drop = FALSE]
  submodels <- list(grid[-1,,drop = FALSE])
  list(loop = loop, submodels = submodels)
}

$fit
function(x, y, wts, param, lev, last, classProbs, ...) {
  cpValue <- if(!last) param$cp else 0
  theDots <- list(...)
  if(any(names(theDots) == "control"))
  {
    theDots$control$cp <- cpValue
    theDots$control$xval <- 0
    ctl <- theDots$control
    theDots$control <- NULL
  } else ctl <- rpart::rpart.control(cp = cpValue, xval = 0)
  
  ## check to see if weights were passed in (and availible)
  if(!is.null(wts)) theDots$weights <- wts
  
  modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                      data = if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE),
                      control = ctl),
                 theDots)
  modelArgs$data$.outcome <- y
  
  out <- do.call(rpart::rpart, modelArgs)
  
  if(last) out <- rpart::prune.rpart(out, cp = param$cp)
  out
}

$predict
function(modelFit, newdata, submodels = NULL) {
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  
  pType <- if(modelFit$problemType == "Classification") "class" else "vector"
  out  <- predict(modelFit, newdata, type=pType)
  
  if(!is.null(submodels))
  {
    tmp <- vector(mode = "list", length = nrow(submodels) + 1)
    tmp[[1]] <- out
    for(j in seq(along = submodels$cp))
    {
      prunedFit <- rpart::prune.rpart(modelFit, cp = submodels$cp[j])
      tmp[[j+1]]  <- predict(prunedFit, newdata, type=pType)
    }
    out <- tmp
  }
  out
}

$prob
function(modelFit, newdata, submodels = NULL) {
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  out <- predict(modelFit, newdata, type = "prob")
  
  if(!is.null(submodels))
  {
    tmp <- vector(mode = "list", length = nrow(submodels) + 1)
    tmp[[1]] <- out
    for(j in seq(along = submodels$cp))
    {
      prunedFit <- rpart::prune.rpart(modelFit, cp = submodels$cp[j])
      tmpProb <- predict(prunedFit, newdata, type = "prob")
      tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, drop = FALSE], stringsAsFactors = TRUE)
    }
    out <- tmp
  }
  out
}

$predictors
function(x, surrogate = TRUE, ...)  {
  out <- as.character(x$frame$var)
  out <- out[!(out %in% c("<leaf>"))]
  if(surrogate)
  {
    splits <- x$splits
    splits <- splits[splits[,"adj"] > 0,]
    out <- c(out, rownames(splits))
  }
  unique(out)
}

$varImp
function(object, surrogates = FALSE, competes = TRUE, ...) {
  if(nrow(object$splits)>0) {
    tmp <- rownames(object$splits)
    rownames(object$splits) <- 1:nrow(object$splits)
    splits <- data.frame(object$splits)
    splits$var <- tmp
    splits$type <- ""
    
    frame <- as.data.frame(object$frame, stringsAsFactors = TRUE)
    index <- 0
    for(i in 1:nrow(frame)) {
      if(frame$var[i] != "<leaf>") {
        index <- index + 1
        splits$type[index] <- "primary"
        if(frame$ncompete[i] > 0) {
          for(j in 1:frame$ncompete[i]) {
            index <- index + 1
            splits$type[index] <- "competing"
          }
        }
        if(frame$nsurrogate[i] > 0) {
          for(j in 1:frame$nsurrogate[i]) {
            index <- index + 1
            splits$type[index] <- "surrogate"
          }
        }
      }
    }
    splits$var <- factor(as.character(splits$var))
    if(!surrogates) splits <- subset(splits, type != "surrogate")
    if(!competes) splits <- subset(splits, type != "competing")
    out <- aggregate(splits$improve,
                     list(Variable = splits$var),
                     sum,
                     na.rm = TRUE)
  } else {
    out <- data.frame(x = numeric(), Variable = character())
  }
  allVars <- colnames(attributes(object$terms)$factors)
  if(!all(allVars %in% out$Variable)) {
    missingVars <- allVars[!(allVars %in% out$Variable)]
    zeros <- data.frame(x = rep(0, length(missingVars)),
                        Variable = missingVars)
    out <- rbind(out, zeros)
  }
  out2 <- data.frame(Overall = out$x)
  rownames(out2) <- out$Variable
  out2
}

$levels
function(x) x$obsLevels

$trim
function(x) {
  x$call <- list(na.action = (x$call)$na.action)
  x$x <- NULL
  x$y <- NULL
  x$where <- NULL
  x
}

$tags
[1] "Tree-Based Model"              "Implicit Feature Selection"   
[3] "Handle Missing Predictor Data" "Accepts Case Weights"         

$sort
function(x) x[order(x[,1], decreasing = TRUE),]

$rpart1SE
$rpart1SE$label
[1] "CART"

$rpart1SE$library
[1] "rpart"

$rpart1SE$type
[1] "Regression"     "Classification"

$rpart1SE$parameters
parameter     class     label
1 parameter character parameter

$rpart1SE$grid
function(x, y, len = NULL, search = "grid") data.frame(parameter = "none")

$rpart1SE$loop
NULL

$rpart1SE$fit
function(x, y, wts, param, lev, last, classProbs, ...) { 
  dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
  dat$.outcome <- y
  
  ## check to see if weights were passed in (and availible)
  if(!is.null(wts)){
    out <- rpart::rpart(.outcome ~ ., data = dat, ...)
  } else {
    out <- rpart::rpart(.outcome ~ ., data = dat, weights = wts, ...)
  }
  out           
}

$rpart1SE$predict
function(modelFit, newdata, submodels = NULL) {                  
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  
  out <- if(modelFit$problemType == "Classification") 
    predict(modelFit, newdata, type = "class") else 
      predict(modelFit, newdata)
  out
}

$rpart1SE$prob
function(modelFit, newdata, submodels = NULL) {
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  predict(modelFit, newdata, type = "prob")
}

$rpart1SE$predictors
function(x, surrogate = TRUE, ...)  {
  out <- as.character(x$frame$var)
  out <- out[!(out %in% c("<leaf>"))]
  if(surrogate)
  {
    splits <- x$splits
    splits <- splits[splits[,"adj"] > 0,]
    out <- c(out, rownames(splits))
  }
  unique(out)
}

$rpart1SE$varImp
function(object, surrogates = FALSE, competes = TRUE, ...) {
  tmp <- rownames(object$splits)
  rownames(object$splits) <- 1:nrow(object$splits)
  splits <- data.frame(object$splits)
  splits$var <- tmp
  splits$type <- ""
  
  frame <- as.data.frame(object$frame, stringsAsFactors = TRUE)
  index <- 0
  for(i in 1:nrow(frame)) {
    if(frame$var[i] != "<leaf>") {
      index <- index + 1
      splits$type[index] <- "primary"
      if(frame$ncompete[i] > 0) {
        for(j in 1:frame$ncompete[i]) {
          index <- index + 1
          splits$type[index] <- "competing"
        }
      }
      if(frame$nsurrogate[i] > 0) {
        for(j in 1:frame$nsurrogate[i]) {
          index <- index + 1
          splits$type[index] <- "surrogate"
        }
      }
    }
  }
  splits$var <- factor(as.character(splits$var))
  if(!surrogates) splits <- subset(splits, type != "surrogate")
  if(!competes) splits <- subset(splits, type != "competing")
  out <- aggregate(splits$improve,
                   list(Variable = splits$var),
                   sum,
                   na.rm = TRUE)
  
  allVars <- colnames(attributes(object$terms)$factors)
  if(!all(allVars %in% out$Variable)) {
    missingVars <- allVars[!(allVars %in% out$Variable)]
    zeros <- data.frame(x = rep(0, length(missingVars)),
                        Variable = missingVars)
    out <- rbind(out, zeros)
  }
  out2 <- data.frame(Overall = out$x)
  rownames(out2) <- out$Variable
  out2  
}

$rpart1SE$levels
function(x) x$obsLevels

$rpart1SE$trim
function(x) {
  x$call <- list(na.action = (x$call)$na.action)
  x$x <- NULL
  x$y <- NULL
  x$where <- NULL
  x
}

$rpart1SE$notes
[1] "This CART model replicates the same process used by the `rpart` function where the model complexity is determined using the one-standard error method. This procedure is replicated inside of the resampling done by `train` so that an external resampling estimate can be obtained."

$rpart1SE$tags
[1] "Tree-Based Model"              "Implicit Feature Selection"    "Handle Missing Predictor Data"
[4] "Accepts Case Weights"         

$rpart1SE$sort
function(x) x[order(x[,1], decreasing = TRUE),]


$rpart2
$rpart2$label
[1] "CART"

$rpart2$library
[1] "rpart"

$rpart2$type
[1] "Regression"     "Classification"

$rpart2$parameters
parameter   class          label
1  maxdepth numeric Max Tree Depth

$rpart2$grid
function(x, y, len = NULL, search = "grid") {
  dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
  dat$.outcome <- y
  initialFit <- rpart::rpart(.outcome ~ .,
                             data = dat,
                             control = rpart::rpart.control(cp = 0))$cptable
  initialFit <- initialFit[order(-initialFit[,"CP"]), "nsplit", drop = FALSE]
  initialFit <- initialFit[initialFit[,"nsplit"] > 0 & initialFit[,"nsplit"] <= 30, , drop = FALSE]
  
  if(search == "grid") {
    
    if(dim(initialFit)[1] < len) {
      cat("note: only", nrow(initialFit),
          "possible values of the max tree depth from the initial fit.\n",
          "Truncating the grid to", nrow(initialFit), ".\n\n")
      tuneSeq <-  as.data.frame(initialFit, stringsAsFactors = TRUE)
    } else tuneSeq <-  as.data.frame(initialFit[1:len,], stringsAsFactors = TRUE)
    colnames(tuneSeq) <- "maxdepth"
  } else {
    tuneSeq <- data.frame(maxdepth = unique(sample(as.vector(initialFit[,1]), 
                                                   size = len, replace = TRUE)))
  }
  tuneSeq
}

$rpart2$loop
function(grid) {
  grid <- grid[order(grid$maxdepth, decreasing = TRUE),, drop = FALSE]
  loop <- grid[1,,drop = FALSE]
  submodels <- list(grid[-1,,drop = FALSE])
  list(loop = loop, submodels = submodels)
}

$rpart2$fit
function(x, y, wts, param, lev, last, classProbs, ...) { 
  theDots <- list(...)
  if(any(names(theDots) == "control"))
  {
    theDots$control$maxdepth <- param$maxdepth
    theDots$control$xval <- 0 
    ctl <- theDots$control
    theDots$control <- NULL    
  } else ctl <- rpart::rpart.control(maxdepth = param$maxdepth, xval = 0)  
  
  ## check to see if weights were passed in (and availible)
  if(!is.null(wts)) theDots$weights <- wts    
  
  modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                      data = if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE),
                      control = ctl),
                 theDots)
  modelArgs$data$.outcome <- y
  
  out <- do.call(rpart::rpart, modelArgs)
  out
}

$rpart2$predict
function(modelFit, newdata, submodels = NULL) {
  ## Models are indexed by Cp so approximate the Cp for
  ## the value of maxdepth
  depth2cp <- function(x, depth)
  {
    out <- approx(x[,"nsplit"], x[,"CP"], depth)$y
    out[depth > max(x[,"nsplit"])] <- min(x[,"CP"]) * .99
    out
  }
  
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  
  pType <- if(modelFit$problemType == "Classification") "class" else "vector"
  out  <- predict(modelFit, newdata, type=pType)
  
  if(!is.null(submodels))
  {
    tmp <- vector(mode = "list", length = nrow(submodels) + 1)
    tmp[[1]] <- out
    cpValues <- depth2cp(modelFit$cptable, submodels$maxdepth)
    for(j in seq(along = cpValues))
    {
      prunedFit <- rpart::prune.rpart(modelFit, cp = cpValues[j])
      tmp[[j+1]]  <- predict(prunedFit, newdata, type=pType)
    }
    out <- tmp
  }
  out
}

$rpart2$prob
function(modelFit, newdata, submodels = NULL) {
  depth2cp <- function(x, depth)
  {
    out <- approx(x[,"nsplit"], x[,"CP"], depth)$y
    out[depth > max(x[,"nsplit"])] <- min(x[,"CP"]) * .99
    out
  }
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  out <- predict(modelFit, newdata, type = "prob")
  
  if(!is.null(submodels))
  {
    tmp <- vector(mode = "list", length = nrow(submodels) + 1)
    tmp[[1]] <- out
    cpValues <- depth2cp(modelFit$cptable, submodels$maxdepth)
    
    for(j in seq(along = cpValues))
    {
      prunedFit <- rpart::prune.rpart(modelFit, cp = cpValues[j])
      tmpProb <- predict(prunedFit, newdata, type = "prob")
      tmp[[j+1]] <- as.data.frame(tmpProb[, modelFit$obsLevels, drop = FALSE], stringsAsFactors = TRUE)
    }
    out <- tmp
  }                              
  out
}

$rpart2$predictors
function(x, surrogate = TRUE, ...) {
  out <- as.character(x$frame$var)
  out <- out[!(out %in% c("<leaf>"))]
  if(surrogate)
  {
    splits <- x$splits
    splits <- splits[splits[,"adj"] > 0,]
    out <- c(out, rownames(splits))
  }
  unique(out)
}

$rpart2$varImp
function(object, surrogates = FALSE, competes = TRUE, ...) {
  tmp <- rownames(object$splits)
  rownames(object$splits) <- 1:nrow(object$splits)
  splits <- data.frame(object$splits)
  splits$var <- tmp
  splits$type <- ""
  
  frame <- as.data.frame(object$frame, stringsAsFactors = TRUE)
  index <- 0
  for(i in 1:nrow(frame)) {
    if(frame$var[i] != "<leaf>") {
      index <- index + 1
      splits$type[index] <- "primary"
      if(frame$ncompete[i] > 0) {
        for(j in 1:frame$ncompete[i]) {
          index <- index + 1
          splits$type[index] <- "competing"
        }
      }
      if(frame$nsurrogate[i] > 0) {
        for(j in 1:frame$nsurrogate[i]) {
          index <- index + 1
          splits$type[index] <- "surrogate"
        }
      }
    }
  }
  splits$var <- factor(as.character(splits$var))
  if(!surrogates) splits <- subset(splits, type != "surrogate")
  if(!competes) splits <- subset(splits, type != "competing")
  out <- aggregate(splits$improve,
                   list(Variable = splits$var),
                   sum,
                   na.rm = TRUE)
  
  allVars <- colnames(attributes(object$terms)$factors)
  if(!all(allVars %in% out$Variable)) {
    missingVars <- allVars[!(allVars %in% out$Variable)]
    zeros <- data.frame(x = rep(0, length(missingVars)),
                        Variable = missingVars)
    out <- rbind(out, zeros)
  }
  out2 <- data.frame(Overall = out$x)
  rownames(out2) <- out$Variable
  out2  
}

$rpart2$levels
function(x) x$obsLevels

$rpart2$trim
function(x) {
  x$call <- list(na.action = (x$call)$na.action)
  x$x <- NULL
  x$y <- NULL
  x$where <- NULL
  x
}

$rpart2$tags
[1] "Tree-Based Model"              "Implicit Feature Selection"    "Handle Missing Predictor Data"
[4] "Accepts Case Weights"         

$rpart2$sort
function(x) x[order(x[,1]),]


$rpartCost
$rpartCost$label
[1] "Cost-Sensitive CART"

$rpartCost$library
[1] "rpart" "plyr" 

$rpartCost$type
[1] "Classification"

$rpartCost$parameters
parameter   class                label
1        cp numeric Complexity Parameter
2      Cost numeric                 Cost

$rpartCost$grid
function(x, y, len = NULL, search = "grid"){
  dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
  dat$.outcome <- y
  initialFit <- rpart::rpart(.outcome ~ .,
                             data = dat,
                             control = rpart::rpart.control(cp = 0))$cptable
  initialFit <- initialFit[order(-initialFit[,"CP"]), , drop = FALSE] 
  if(search == "grid") {
    if(nrow(initialFit) < len) {
      tuneSeq <- expand.grid(cp = seq(min(initialFit[, "CP"]), 
                                      max(initialFit[, "CP"]), 
                                      length = len),
                             Cost = 1:len)
    } else tuneSeq <-  data.frame(cp = initialFit[1:len,"CP"], Cost = 1:len)
    colnames(tuneSeq) <- c("cp", "Cost")
  } else {
    tuneSeq <- data.frame(cp = 10^runif(len, min = -8, max = -1),
                          Cost = runif(len, min = 1, max = 30))
  }
  
  tuneSeq
}

$rpartCost$loop
function(grid) {
  loop <- plyr::ddply(grid,  plyr::`.`(Cost), function(x) c(cp = min(x$cp)))
  submodels <- vector(mode = "list", length = nrow(loop))
  
  for(i in seq(along = submodels)) {
    larger_cp <- subset(grid, subset = Cost == loop$Cost[i] & cp > loop$cp[i])
    submodels[[i]] <- 
      data.frame(cp = sort(larger_cp$cp))
  }
  
  list(loop = loop, submodels = submodels)    
}

$rpartCost$fit
function(x, y, wts, param, lev, last, classProbs, ...) { 
  theDots <- list(...)
  if(any(names(theDots) == "control")) {
    theDots$control$cp <- param$cp
    theDots$control$xval <- 0 
    ctl <- theDots$control
    theDots$control <- NULL
  } else ctl <- rpart::rpart.control(cp = param$cp, xval = 0)   
  
  lmat <-matrix(c(0, 1, param$Cost, 0), ncol = 2)
  rownames(lmat) <- colnames(lmat) <- levels(y)
  if(any(names(theDots) == "parms")) {
    theDots$parms$loss <- lmat
  } else parms <- list(loss = lmat)
  
  ## check to see if weights were passed in (and availible)
  if(!is.null(wts)) theDots$weights <- wts    
  
  modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                      data = if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE),
                      parms = parms,
                      control = ctl),
                 theDots)
  modelArgs$data$.outcome <- y
  
  out <- do.call(rpart::rpart, modelArgs)
  out
}

$rpartCost$predict
function(modelFit, newdata, submodels = NULL) {
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  
  pType <- if(modelFit$problemType == "Classification") "class" else "vector"
  out  <- predict(modelFit, newdata, type = pType)
  
  if(!is.null(submodels)) {
    tmp <- vector(mode = "list", length = nrow(submodels) + 1)
    tmp[[1]] <- out
    for(j in seq(along = submodels$cp)) {
      prunedFit <- rpart::prune.rpart(modelFit, cp = submodels$cp[j])
      tmp[[j+1]]  <- predict(prunedFit, newdata, type=pType)
    }
    out <- tmp
  }
  out
}

$rpartCost$levels
function(x) x$obsLevels

$rpartCost$prob
NULL

$rpartCost$tags
[1] "Tree-Based Model"              "Implicit Feature Selection"    "Cost Sensitive Learning"      
[4] "Two Class Only"                "Handle Missing Predictor Data" "Accepts Case Weights"         

$rpartCost$sort
function(x) x[order(-x$cp, -x$Cost),]


$rpartScore
$rpartScore$label
[1] "CART or Ordinal Responses"

$rpartScore$library
[1] "rpartScore" "plyr"      

$rpartScore$type
[1] "Classification"

$rpartScore$parameters
parameter     class                label
1        cp   numeric Complexity Parameter
2     split character       Split Function
3     prune character      Pruning Measure

$rpartScore$grid
function(x, y, len = NULL, search = "grid"){
  dat <- if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE)
  dat$.outcome <- y
  initialFit <- rpart::rpart(.outcome ~ .,
                             data = dat,
                             control = rpart::rpart.control(cp = 0))$cptable
  initialFit <- initialFit[order(-initialFit[,"CP"]), , drop = FALSE] 
  if(search == "grid") {
    if(nrow(initialFit) < len) {
      tuneSeq <- expand.grid(cp = seq(min(initialFit[, "CP"]), 
                                      max(initialFit[, "CP"]), 
                                      length = len),
                             split = c("abs", "quad"),
                             prune = c("mr", "mc"))
    } else tuneSeq <-  expand.grid(cp = initialFit[1:len,"CP"],
                                   split = c("abs", "quad"),
                                   prune = c("mr", "mc"))
    colnames(tuneSeq)[1] <- "cp"
  } else {
    tuneSeq <- expand.grid(cp = unique(sample(initialFit[, "CP"], size = len, replace = TRUE)),
                           split = c("abs", "quad"),
                           prune = c("mr", "mc"))
  }
  
  tuneSeq
}

$rpartScore$fit
function(x, y, wts, param, lev, last, classProbs, ...) { 
  cpValue <- if(!last) param$cp else 0
  theDots <- list(...)
  if(any(names(theDots) == "control")) {
    theDots$control$cp <- cpValue
    theDots$control$xval <- 0 
    ctl <- theDots$control
    theDots$control <- NULL
  } else ctl <- rpart::rpart.control(cp = cpValue, xval = 0)
  
  ## check to see if weights were passed in (and availible)
  if(!is.null(wts)) theDots$weights <- wts    
  
  modelArgs <- c(list(formula = as.formula(".outcome ~ ."),
                      data = if(is.data.frame(x)) x else as.data.frame(x, stringsAsFactors = TRUE),
                      split = as.character(param$split),
                      prune = as.character(param$prune),
                      control = ctl),
                 theDots)
  modelArgs$data$.outcome <- as.numeric(y)
  
  out <- do.call(rpartScore::rpartScore, modelArgs)
  
  if(last) out <- rpart::prune.rpart(out, cp = param$cp)
  out
}

$rpartScore$predict
function(modelFit, newdata, submodels = NULL) {    
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata, stringsAsFactors = TRUE)
  out  <-  modelFit$obsLevels[predict(modelFit, newdata)]
  
  if(!is.null(submodels)) {
    tmp <- vector(mode = "list", length = nrow(submodels) + 1)
    tmp[[1]] <- out
    for(j in seq(along = submodels$cp)) {
      prunedFit <- rpart::prune.rpart(modelFit, cp = submodels$cp[j])
      tmp[[j+1]]  <- modelFit$obsLevels[predict(prunedFit, newdata)]
    }
    out <- tmp
  }
  out
}

$rpartScore$prob
NULL

$rpartScore$predictors
function(x, surrogate = TRUE, ...)  {
  out <- as.character(x$frame$var)
  out <- out[!(out %in% c("<leaf>"))]
  if(surrogate) {
    splits <- x$splits
    splits <- splits[splits[,"adj"] > 0,]
    out <- c(out, rownames(splits))
  }
  unique(out)
}

$rpartScore$varImp
function(object, surrogates = FALSE, competes = TRUE, ...) {
  allVars <- all.vars(object$terms)
  allVars <- allVars[allVars != ".outcome"]
  out <- data.frame(Overall = object$variable.importance,
                    Variable = names(object$variable.importance))
  rownames(out) <- names(object$variable.importance)
  
  if(!all(allVars %in% out$Variable)) {
    missingVars <- allVars[!(allVars %in% out$Variable)]
    zeros <- data.frame(Overall = rep(0, length(missingVars)),
                        Variable = missingVars)
    out <- rbind(out, zeros)
  }
  rownames(out) <- out$Variable
  out$Variable <- NULL
  out  
}

$rpartScore$levels
function(x) x$obsLevels

$rpartScore$trim
function(x) {
  x$call <- list(na.action = (x$call)$na.action)
  x$x <- NULL
  x$y <- NULL
  x$where <- NULL
  x
}

$rpartScore$tags
[1] "Tree-Based Model"              "Implicit Feature Selection"    "Handle Missing Predictor Data"
[4] "Accepts Case Weights"          "Ordinal Outcomes"             

$rpartScore$sort
function(x) x[order(x[,1], decreasing = TRUE),]