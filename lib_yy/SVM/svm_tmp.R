rm(list=ls())
library(e1071)
library(caret)

local_output <- paste0(getwd(), "/lib_yy/SVM/svm_tmp_html.html")
source(paste0(getwd(), "/lib_yy/rex_lib/graphics.R"), encoding = "UTF-8")
source(paste0(getwd(), "/lib_yy/rex_lib/common.R"), encoding = "UTF-8")
# sample dataset
sample_data <- read.csv("D://yeyoung/Data/import_data/6.csv")
sample_data <- sample_data[, c("hyp", "bweight", "lowbw", "gestwks", "sex", "matage")]
sample_data$lowbw <- as.factor(sample_data$lowbw)
sample_data$sex <- as.factor(sample_data$sex)

# Input values----------
dataset <- sample_data
#Variables Tab
dep_var <- "hyp"
conti_var <- c("gestwks", "matage", "bweight")
cat_var <- NULL #c("lowbw", "sex")
standardize <- FALSE

#Analysis Tab
analysis_type <- "C-classification" # eps-regression
kernel <- "polynomial" # "linear", "radial", "sigmoid"
search_method <- "grid" # NULL, "random", "custom"
tune_len <- 3
cost <- 1
gamma <- 1/length(c(conti_var, cat_var))
degree <- 3
hyperparam_default <- TRUE # true 이면 조율모수, 그리드 개수의 default값을 다시 넣어주는 작업 필요. 

#Validation
split_method <- "by_random" # "use_all", "by_variable"
split_var <- NULL
p_train <- 70
# p_test <- 100 - p_training 
valid_method <- "cv" # "none", "split",
p_valid <- 4
fold <- 3
iter <- 10 # 이거 사용하지 않는 valid method 가 선택되었을 때 default 값 어떻게 처리하지?

##### 1. 변수설명 #####
# 변수설정
# dataset: 데이터셋
# dep_var: 종속변수
# conti_var: 연속형 변수
# cat_var: 범주형 변수
# standardize: 연속형 변수 표준화

# 분석옵션
# analysis_type: 분석방법
# kernel: 커널함수
# search_method: 조율모수 탐색방법
# tune_len: 조율모수 개수
# cost: 커널함수 cost
# gamma: 커널함수 gamma
# degree: 커널함수 차수
# hyperparam_default: 기본값 설정

# 자료분할
# split_data: 자료분할 방법
# split_var: 자료 분할 변수
# p_training: 훈련 데이터셋 비율
# p_test: 테스트 데이터셋 비율
# valid_method: 검증방법
# fold: k-fold cv 선택시 분할 횟수
# iter: repeated k-fold cv 선택 시 반복 횟수
#




##### 2. 필수 패키지 #####
load.pkg(c("R2HTML", "e1071", "caret"))




##### 3. 필요 함수 #####




###### 4. HTML Output #####
# html.output <- capture.output({


##### 4.1 분석 제목 #####
R2HTML::HTML(R2HTML::as.title("Support Vector Machine (SVM)"), HR = 1, 
             file = local_output, append = FALSE)


##### 4.2 데이터 확인: Error & Warning message #####


#### 4.2.1 커널함수 설정 #####
#yy. 기본값으로 들어가도록 설정. 함수 default 값임? 이거 기본값임?
#기본값 TRUE일 때 어디서 코드 지정해야하지?
if (is.null(degree)) { degree <- 3 }
if (is.null(gamma)) { gamma <- 1 / (length(c(conti_var, cat_var))) }
if (is.null(cost)) { cost <- 1 }


#### 4.2.2 독립 변수 미 선택 시 #####
if (is.null(conti_var) & is.null(cat_var)) {
  err_var1 <- "<li> Error : At least 1 independent variable should be selected. Analysis has been stopped."
}


#### 4.2.3 연속형 변수 확인 #####
if (!is.null(conti_var)) {
  is_cat <- sapply(conti_var, function (x) { !is.numeric(dataset[, x]) })
  if (any(is_cat)) {
    warn_var1 <- paste0("<li> Warning : The type of variable '", paste(conti_var[is_cat], collapse = ", "),
                        "' is not numeric but selected as the quantitative variable. ",
                        "It was excluded from the analysis.")
    conti_var <- conti_var[!is_cat]
  }
  if (length(conti_var) == 0) { conti_var <- NULL }
}


#### 4.2.4 범주형 변수 확인 #####
if (!is.null(cat_var)) {
  is_num <- sapply(cat_var, function (x) { is.numeric(dataset[, x]) })
  is_char <- sapply(cat_var, function (x) { is.character(dataset[, x]) })
  if (any(is_num)) {
    warn_var2 <- paste0("<li> Warning : The type of variable '", paste(cat_var[is_num], collapse = ", "),
                        "' is numeric but selected as the qualitative variable. ",
                        "It was coerced into Character")
    for (i in cat_var[is_num]) { dataset[, i] <- factor(dataset[, i]) }
  }
  if (any(is_char)) {
    for (i in cat_var[is_char]) { dataset[, i] <- factor(dataset[, i]) }
  }
}


#### 4.2.5 분석방법 회귀 & 종속변수가 연속형이 아닌 변수 확인 #####
if (analysis_type == "eps-regression" & !is.numeric(dataset[, dep_var])) {
  err_var2 <- paste0("<li> Error : Inappropriate type of dependent variable. ", 
                     "(EPS-regression is supported only for numeric variable. ",
                     "Analysis has been stopped.")
}


#### 4.2.6 분석방법 분류 & 종속변수가 연속형 변수 확인 #####
if (analysis_type == "C-classification" & is.numeric(dataset[, dep_var])) {
  if (length(table(dataset[, dep_var])) >= 20) {
    err_var3 <- paste0("<li> Error : The selected dependent variable has more than 20 levels ",
                       "and is expected to be quantitative. Please check whether it is continuous. ",
                       "If it is qualitative, the number of its levels should be reduced. ",
                       "Analysis has been stopped.")
  }
}
#

#### 4.2.7 커널함수-cost 확인 #####
if (sum(cost <= 0) != 0 | is.character(cost)) {
  err_tunepar1 <- paste0("<li> Error : The cost should be numbers and greater than 0. ",
                         "Analysis has been stopped.") 
}


#### 4.2.8 커널함수-gamma 확인 #####
if (gamma <= 0 | is.character(gamma)) {
  err_tunepar2 <- paste0("<li> Error : The gamma should be numbers and greater than 0. ",
                         "Analysis has been stopped.") 
}


# 종속변수에 결측이 있을 시 제외하는 것으로 확인 (렉스홈페이지 분석모듈 설명)
# 예측은 가능하나 예측된 값으로 평가하는 것은 불가 -> 무슨말이지? 결측을 예측?
if (!sum(is.na(dataset[, dep_var])) == 0) {
  warn_var3 <- paste0("<li> Warning : Dependent variable have missing value. ",
                      "It was excluded from the analysis.") 
}


#### 독립변수가 모두 없는 경우 #####
if ((is.null(conti_var) & is.null(cat_var)) | (length(conti_var) == 0 & length(cat_var) == 0) |
    (length(conti_var) == 0 & is.null(cat_var)) | (is.null(conti_var) & length(cat_var) == 0)) {
  err_obs1 <- paste0("<li> Error : When observations with missing value was excluded in data, ",
                     "there are not enough observation. Analysis has been stopped.")
}


##### 4.3 자료분할 #####


#### 4.3.1 Data processing
# raw data 저장
raw_dat <- dataset
if (analysis_type == "C-classification") {
  dataset[, dep_var] <- as.factor(dataset[, dep_var])
}
for (c in cat_var) {
  dataset[, c] <- as.factor(dataset[, c])
}

# 변수정리, complete observation 만 사용하도록
obs_unchecked_data <- dataset #f_data
indep_var <- c(conti_var, cat_var)
selected_var <- c(dep_var, indep_var, split_var)
dataset <- dataset[complete.cases(dataset[, selected_var, drop = FALSE]), selected_var, drop = FALSE]
obs_checked_data <- dataset

if (nrow(dataset) == 0) {
  err_obs2 <- paste0("<li> Error : When observations with missing value was excluded in data, ",
                     "there are not enough observation. Analysis has been stopped.")
}

# 훈련 및 시험 데이터셋 정리
if (split_method == "by_random") { # "by_variable", "use_all"
  n_train <- round(nrow(dataset) * p_train / 100)
  if (p_train == 100 | n_train == nrow(dataset)) {
    err_obs3 <- paste0("<li> Error : No observations are assigned to the test data. ",
                       "Validation using test dataset is not supported in this analysis.")
  } else if (n_train == 0) {
    err_obs4 <- paste0("<li> Error : No observations are assigned to the training dataset. ",
                       "To secure sufficient number of observations for the training")
  } else {
    train_idx <- createDataPartition(dataset[, dep_var], p = p_train/100)$Resample1
    
    test_set <- dataset[-train_idx, , drop = FALSE]
    train_set <- dataset[train_idx, , drop = FALSE]
    
    #yy: createDataPartition 함수가 아래 해결해주지 않을까 함. 해당 함수에서 발생가능한 오류를 사전에 차단해야함.
    # if (nrow(raw_dat) != (nrow(train_set) + nrow(test_set))) {
    #   warn_obs1 <- paste0("<li> Warning : Observations with missing dependent variable ",
    #                       "were not assigned to training or test dataset.")
    # }
    train_set <- train_set[order(as.numeric(rownames(train_set))), , drop = FALSE]
    test_set <- test_set[order(as.numeric(rownames(test_set))), , drop = FALSE]
  }
} else if (split_method == "by_variable") {
  
} else { # "use_all"
  
}
# split 데이터 우선 저장: 
# validation set 을 사용하지 못하는 경우 경고문 출력 후 이 데이터 사용
s_train_set <- train_set


# 훈련 및 시험 데이터셋 정리가 끝난 후 데이터 한번 저장해야할듯.
# validation 을 할 수 있는 조건이 안되면 이떄 저장한 데이터를 사용하도록
# validation 이 check 되면 일단 데이터셋 나눔.

# 훈련자료를 훈련/검증 자료로 나눔
if (validation) { # validation <- TRUE
  if (!is.null(p_valid) & p_valid > 1) {
    p_valid_adj <- p_train / p_valid / 100
    valid_idx <- createDataPartition(s_train_set[, dep_var], p = p_valid_adj)$Resample1
    
    train_set <- s_train_set[-valid_idx, , drop = FALSE]
    valid_set <- s_train_set[valid_idx, , drop = FALSE]
    
    train_set <- train_set[order(as.numeric(rownames(train_set))), , drop = FALSE]
    valid_set <- valid_set[order(as.numeric(rownames(valid_set))), , drop = FALSE]
  } else {
    #yy: p_valid 값이 제대로 안들어온 경우
    # p_valid로 obs갯수를 계산하니 train 보다 많은 경우 warning message?
    # err_validops1 <- 
  }
}
# raw_dat: original dataset, train_set: training dataset, test_set: test dataset,
# valid_set: validation dataset

# test dataset check
if (!is.null(cat_var) & exists("test_set")) {
  for (c in cat_var) { # c <- cat_var[1]
    var_level <- unique(train_set[, c])
    n_test_org <- nrow(test_set)
    test_set <- test_set[test_set[, c] %in% var_level, ]
    n_test_chck <- nrow(test_set)
    if (n_test_org > n_test_chck) {
      new_level_test <- ifelse(!exists("new_level_test"), c, paste0(new_level_test, ", ", c)) 
    }
  }
  if (exists("new_level_test")) {
    warn_level1 <- paste0("<li> Warning : Unobserved level of '", new_level_test, 
                          "' in training dataset was observed in test dataset. ",
                          "Observations with new level were excluded from the analysis.")
  }
  if (nrow(test_set) == 0) {
    # 이 에러메세지 바꿔야할듯. 더 자세한 상황설명필요
    err_obs5 <- paste0("<li> Error : When observatoins with missing value was excluded in data, ",
                       "there are not enough observations. Analysis has been stopped." ) 
  }
}

# validation dataset check
if (!is.null(cat_var) & exists("valid_set")) {
  for (c in cat_var) { # c <- cat_var[1]
    var_level <- unique(train_set[, c])
    n_test_org <- nrow(valid_set)
    valid_set <- valid_set[valid_set[, c] %in% var_level, ]
    n_valid_chck <- nrow(valid_set)
    if (n_valid_org > n_valid_chck) {
      new_level_valid <- ifelse(!exists("new_level_valid"), c, paste0(new_level_valid, ", ", c))
    }
  }
  if (exists("new_level_valid")) {
    warn_level2 <- paste0("<li> Warning : Unobserved level of '", new_level_valid,
                          "' in training dataset was observed in validation dataset. ",
                          " Observations with new level were excluded from the analysis.")
  }
  if (nrow(valid_set) == 0) {
    # 이 에러메세지 바꿔야할듯. 더 자세한 상황설명필요
    err_obs6 <- paste0("<li> Error : When observations with missing value was excluded in data, ",
                       "there are not enough observations. Analysis has been stopped.")
  }
}

# 반응변수가 하나의 값으로만 이뤄졌을 경우
if (length(unique(train_set[, dep_var])) == 1) {
  err_obs7 <- paste0("<li> Error : Dependent variable with single value can not be applied. ",
                     "Analysis has been stopped.")
}

if (exists("err_var1") | exists("err_var2") | exists("err_var3") |
    exists("err_tunepar1") | exists("err_tunepar2") |
    exists("err_obs1") | exists("err_obs2") | exists("err_obs3") | 
    exists("err_obs4") | exists("err_obs5") | exists("err_obs6") | 
    exists("err_obs7")) {
  stop_analysis1 <- c()
  
  R2HTML::HTML(R2HTML::as.title("Warnings"), HR = 2, file = local_output)
  
  if (exists("err_var1")) { R2HTML::HTML(err_var1, file = local_output) }
  if (exists("err_var2")) { R2HTML::HTML(err_var2, file = local_output) }
  if (exists("err_var3")) { R2HTML::HTML(err_var3, file = local_output) }
  
  if (exists("err_tunepar1")) { R2HTML::HTML(err_tunepar1, file = local_output) }
  if (exists("err_tunepar2")) { R2HTML::HTML(err_tunepar2, file = local_output) }
  
  if (exists("err_obs1")) { R2HTML::HTML(err_obs1, file = local_output) }
  if (exists("err_obs2")) { R2HTML::HTML(err_obs2, file = local_output) }
  if (exists("err_obs3")) { R2HTML::HTML(err_obs3, file = local_output) }
  if (exists("err_obs4")) { R2HTML::HTML(err_obs4, file = local_output) }
  if (exists("err_obs5")) { R2HTML::HTML(err_obs5, file = local_output) }
  if (exists("err_obs6")) { R2HTML::HTML(err_obs6, file = local_output) }
  if (exists("err_obs7")) { R2HTML::HTML(err_obs7, file = local_output) }
}

# err_var1: 질적변수 및 양적변수 중 하나라도 선택되지 않는 경우 분석중단
# err_var2: 양적변수 형태 확인. 분석에서 강제 제외
# err_var3: ...


if (!exists("stop_analysis1")) {
  
  ### Analysis options ###
  # 분석옵션 설정 according to hyperparameter and kernel function
  
  svm_slot <- list(
    type = c("Classification", "Regression"),
    library = "e1071",
    predict = function(modelFit, newdata, submodels = NULL) {
      predict(modelFit, newdata)
    },
    prob = function(modelFit, newdata, submodels = NULL) {
      out <- predict(modelFit, newdata, probability = TRUE)
      attr(out, "probabilities")
    }
  )
  
  if (kernel == "linear") {
    parameters <- data.frame(parameters = c("cost"), 
                             class = c("numeric"),
                             label = c("Cost"), 
                             stringsAsFactors = FALSE)
    
    grid <- function(x, y, len = NULL, search = "grid") {
      if (search == "grid") {
        out <- data.frame(cost = 2^((1:len) - 3))
      } else {
        out <- data.frame(cost = 2^runif(len, min = -5, max = 10))
      }
      return(out)
    }
    
    fit <- function(x, y, wts, param, lev, last, classProbs,
                    kernel, analysis_type, standardize, ...) {
      out <- e1071::svm(x = as.matrix(x), y = y, scale = standardize, 
                        type = analysis_type, kernel = kernel,
                        cost = param$cost, coef0 = 0,
                        probability = classProbs, class.weights = NULL)
      return(out)
    }
  } else if (kernel == "polynomial") {
    parameters <- data.frame(parameters = c("cost", "degree", "gamma"),
                             class = c("numeric", "numeric", "numeric"),
                             label = c("Cost", "Degree", "Gamma"),
                             stringsAsFactors = FALSE)
    
    grid <- function(x, y, len = NULL, search = "grid") {
      if (search == "grid") {
        out <- expand.grid(cost = 2^((1:len) - 3), degree = 1:len, gamma = 1/4)
      } else {
        out <- data.frame(cost = 2^runif(len, min = -5, max = 10), 
                          degree = 1:len, gamma = 1/4)
      }
      return(out)
    }
    
    fit <- function(x, y, wts, param, lev, last, classProbs,
                    kernel, analysis_type, standardize, ...) {
      out <- e1071::svm(x = as.matrix(x), y = y, scale = standardize,
                        type = analysis_type, kernel = kernel,
                        cost = param$cost, degree = param$degree, 
                        gamma = param$gamma, coef0 = 0,
                        probability = classProbs, class.weights = NULL)
      return(out)
    }
  } else if (kernel == "radial") {
    parameters <- data.frame(parameters = c("cost", "gamma"), 
                             class = c("numeric", "numeric"),
                             label = c("Cost", "Gama"),
                             stringsAsFactors = FALSE)
    
    grid <- function(x, y, len = NULL, search = "grid") {
      if (search == "grid") {
        out <- expand.grid(cost = 2^((1:len) - 3), gamma = 1/4)
      } else {
        out <- data.frame(cost = 2^runif(len, min = -5, max = 10), gamma = 1/4)
      }
      return(out)
    }
    
    fit <- function(x, y, wts, param, lev, last, classProbs,
                    kernel, machine_type, standardize, ...) {
      out <- e1071::svm(x = as.matrix(x), y = y, scale = standardize,
                        type = analysis_type, kernel = kernel,
                        cost = param$cost, gamma = param$gamma, coef0 = 0,
                        probability = classProbs, class.weights = NULL)
      return(out)
    }
  }
  svm_slot$parameters <- parameters
  svm_slot$grid <- grid
  svm_slot$fit <- fit
  # 
  
  # 1. Train options------------
  # input value switch
  svm_control <- trainControl(
    method = valid_method,
    number = ifelse(grepl("cv", valid_method), fold, 1),
    repeats = ifelse(grepl("[d_]cv$", valid_method), iter, NA),
    search = search_method,
    savePredictions = TRUE,
    classProbs = TRUE,
    verboseIter = TRUE)
  
  dep_var_level <- levels(train_set[, dep_var])
  if (analysis_type == "C-classification" && !is.na(as.numeric(dep_var_level))) {
    for (l in dep_var_level) {
      levels(train_set$hyp)[levels(train_set$hyp) == l] <- paste0("X", l)
    }
  }
  
  svm_fit <- train(x = train_set[, indep_var],
                   y = train_set[, dep_var],
                   method = svm_slot,
                   tuneLength = tune_len,
                   trControl = svm_control,
                   preProcess = NULL,
                   weights = NULL,
                   # the dots
                   standardize = standardize, 
                   kernel = kernel, 
                   analysis_type = analysis_type)
  
  if (class(svm_fit) == "try-error") {
    stop_analysis2 <- c()
    R2HTML::HTML(R2HTML::as.title("Warnings"), HR = 2, file = local_output)
    R2HTML::HTML(svm_fit, file = local_output)
  }
}


if (!exists("stop_analysis1") & !exists("stop_analysis2")) {
  
  # Data Structure
  R2HTML::HTML(R2HTML::as.title("Data Structure"), HR = 2, file = local_output)
  
  DS <- DStr(raw_dat, c(dep_var, indep_var, split_var), 2)
  R2HTML::HTML(DS, file = local_output, align = "left")
  
  # Variable List
  R2HTML::HTML(R2HTML::as.title("Variable List"), HR = 2, file = local_output)
  VL <- VList("Dependent Variable", dep_var, raw_dat[, dep_var])
  if (!is.null(cat_var)) {
    for (i in 1:length(cat_var)) {
      VL <- rbind(VL, VList("Explanatory Variable (Qualitative)",
                            cat_var[i], raw_dat[, cat_var[i]]))
    }
  }
  if (!is.null(conti_var)) {
    for (i in 1:length(conti_var)) {
      VL <- rbind(VL, VList("Explanatory Variable (Quantitative)",
                            conti_var[i], raw_dat[, conti_var[i]]))
    }
  }
  if (!is.null(split_var)) {
    VL <- rbind(VL, VList("Split Variable", split_var, raw_dat[, split_var]))
  }
  
  R2HTML::HTML(VL, file = local_output, align = "left")
  R2HTML::HTML(R2HTML::as.title("Analysis Description"), HR = 2, file = local_output)
  if (analysis_type == "C-classification") { anal_method <- "C-classification" }
  if (analysis_type == "eps-regression") { anal_method <- "EPS-regression" }
  kern_method <- switch(kernel, 
                        linear = "Linear", polynomial = "Polynomial",
                        radial = "Radial", sigmoid = "Sigmoid")
  AD <- matrix(c("Fitted model", paste(dep_var, paste(indep_var, collapse = "+"), sep = "~")),
               nrow = 1, ncol = 2, byrow = TRUE)
  AD <- rbind(AD, matrix(c("Analysis method", anal_method), nrow = 1, ))
  tune_param <- lapply(parameters$parameters, 
                       function (x) { paste(c(x, "= ", get(x)), collapse = "") })
  AD <- rbind(AD, matrix(c("Kernel function", 
                           paste(c(kern_method, 
                                   " (", 
                                   paste(unlist(tune_param), collapse = ", "), 
                                   ")"), collapse = "")),
                         nrow = 1, ncol = 2, byrow = TRUE))
  AD <- rbind(AD, matrix(c("Search method", search_method),
                         nrow = 1, ncol = 2, byrow = TRUE))
  AD <- rbind(AD, matrix(c("Data partition", ifelse(split_method == "use_all", FALSE, TRUE)),
                         nrow = 1, ncol = 2, byrow = TRUE))
  if (split_method == "use_all") {
    AD <- rbind(
      AD, matrix(c("Data partition method", 
                   paste("[Using all data as training data] Train : 100% (n=", 
                         nrow(train_set), ")", sep = "")),
                 nrow = 1, ncol = 2, byrow = TRUE))
  } else if (split_method == "by_random") {
    AD <- rbind(
      AD, matrix(c("Data partition method",
                   paste("[Split by random] Train : ", p_train, "% (n=",
                         nrow(train_set), ") / Test : ", 100 - p_train, "% (n=",
                         nrow(test_set), ")", sep = "")),
                 nrow = 1, ncol = 2, byrow = TRUE))
  } else if (split_method == "by_variable") {
    if (!exists("test_set")) { # 이 경우는 어떤 경우지?
      AD <- rbind(
        AD, matrix(c("Data partition method", 
                     paste("[Split by variable] Train : 1 (n=", nrow(train_set), ")", sep = "")),
                   nrow = 1, ncol = 2, byrow = TRUE))
    } else {
      AD <- rbind(
        AD, matrix(c("Data partition method",
                     paste("[Split by variable] Train : 1 (n=", nrow(train_set), 
                           ") / Test : 2 (n=", nrow(test_set), ")", sep = "")),
                   nrow = 1, ncol = 2, byrow = TRUE))
    }
  }
  
  if (valid_method == "none") {
    AD <- rbind(
      AD, matrix(c("Validation method", "none"), nrow = 1, ncol = 2, byrow = TRUE))
  } else if (valid_method == "split") {
    # 이 경우를 holdout validation이라고 함
    AD <- rbind(
      AD, matrix(c("Validation method", 
                   paste("[Training data Split by random] Validation : ", p_train / p_valid, "% (n=",
                         nrow(valid_set), ") / Train : ", p_train - p_train / p_valid, "% (n=",
                         nrow(test_set), ")", sep = "")),
                 nrow = 1, ncol = 2, byrow = TRUE))
  } else if (valid_method == "cv") {
    AD <- rbind(
      AD, matrix(c("Validation method", paste(fold, "-fold cross validation", sep = "")),
                 nrow = 1, ncol = 2, byrow = TRUE))
  } else if (valid_method == "LOOCV") {
    AD <- rbind(
      AD, matrix(c("Validation method", "Leave-one-out cross validation"),
                 nrow = 1, ncol = 2, byrow = TRUE))
  } else if (valid_method == "LGOCV") {
    AD <- rbind(
      AD, matrix(c("Validation method", "LGOCV"), nrow = 1, ncol = 2, byrow = TRUE))
  }
  
  if (nrow(obs_unchecked_data) == nrow(obs_checked_data)) {
    R2HTML::HTML(AD, file = local_output, align = "left")
  } else {
    if (exists("test_Set") & exists("train_set")) {
      R2HTML::HTML(AD, file = local_output, align = "left",
                   caption = paste("<div style=text-align:left><small>",
                                   "* Number of missing at train / test dataset : ",
                                   nrow(obs_checked_data) - nrow(obs_unchecked_data),
                                   "</small>", sep = ""))
    } else {
      R2HTML::HTML(AD, file = local_output, align = "left",
                   caption = paste("<div style=text-align:left><small>",
                                   "* Number of missing at train : ",
                                   nrow(obs_checked_data) - nrow(obs_unchecked_data),
                                   "</small>", sep = ""))
    }
  }
  
  R2HTML::HTML(R2HTML::as.title("Results of Support Vector Machine (SVM)"),
               HR = 2, file = local_output)
  
  
  # resampling method써서 나오는 결과에 맞게 validation을 training set 분할해서 하는 경우
  # 도 짜놓기. 어떤결과를 어떻게 사용할 것인지에 따라 다름
  if (valid_method == "split") {
    # Not yet implemented
  }
  
  if (exists("test_set")) {
    predicted <- predict(svm_fit$finalModel, test_set[, indep_var])
  }
  
  if (analysis_type == "C-classification") {
    test_confmat <- try(confusionMatrix(predicted, as.factor(test_set[, dep_var])), silent = TRUE)
    test_confmat
  }
  
}

#####

# 0. Caret Customized Functions-------
# Required list: type, library, parameters, grid, fit, predict, prob
# polymonial: degree, coef0
# sigmoid: coef0, gamma
# radial: gamma
# linear:



# })