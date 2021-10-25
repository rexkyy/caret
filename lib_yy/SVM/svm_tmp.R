rm(list=ls())

# local_output <- paste0(getwd(), "/lib_yy/SVM/svm_tmp_html_reg.html")
local_output <- paste0(getwd(), "/lib_yy/SVM/svm_tmp_html_class.html")
source(paste0(getwd(), "/lib_yy/rex_lib/graphics.R"), encoding = "UTF-8")
source(paste0(getwd(), "/lib_yy/rex_lib/common.R"), encoding = "UTF-8")
# sample dataset
sample_data <- read.csv("D://yeyoung/Data/import_data/6.csv")
sample_data <- sample_data[, c("hyp", "bweight", "lowbw", "gestwks", "sex", "matage", "preterm")]
sample_data$lowbw <- as.factor(sample_data$lowbw)
sample_data$sex <- as.factor(sample_data$sex)
sample_data$hyp <- as.factor(sample_data$hyp)
sample_data$preterm <- as.factor(sample_data$preterm)

# Input values----------
dataset <- sample_data
#Variables Tab
dep_var <- "hyp" #"gestwks" #
conti_var <- c("matage", "bweight")
cat_var <- NULL #c("lowbw", "sex")
standardize <- FALSE

#Analysis Tab
analysis_type <- "C-classification" #"eps-regression" # 
kernel <- "polynomial" # "linear", "radial", "sigmoid"
search_method <- "grid" # NULL, "random", "custom"
tune_len <- 3
cost <- NULL #1
gamma <- NULL #1/length(c(conti_var, cat_var))
degree <- NULL #3
hyperparam_default <- FALSE # true 이면 조율모수, 그리드 개수의 default값을 다시 넣어주는 작업 필요. 

#Validation
partition_method <- "by_random" # "use_all", "by_variable"
partition_var <- NULL
p_train <- 80
# p_test <- 100 - p_training 
valid_method <- "cv" # "split", "none",
p_valid <- NULL #4
fold <- 4 # 4
iter <- 10 # 10 # 이거 사용하지 않는 valid method 가 선택되었을 때 default 값 어떻게 처리하지?

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
# partition_method: 자료분할 방법
# partition_var: 자료 분할 변수
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
R2HTML::HTML(R2HTML::as.title("Support Vector Machine (SVM)"), HR = 1, file = local_output, append = FALSE)


##### 4.2 데이터 확인: Error & Warning message #####


#### 4.2.1 커널함수 설정 #####
#yy. 기본값으로 들어가도록 설정. 함수 default 값임? 이거 기본값임?
#기본값 TRUE일 때 어디서 코드 지정해야하지?
# if (is.null(degree)) { degree <- 3 }
# if (is.null(gamma)) { gamma <- 1 / (length(c(conti_var, cat_var))) }
# if (is.null(cost)) { cost <- 1 }


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
if (search_method == "custom") {
  if (sum(cost <= 0) != 0 | is.character(cost)) {
    err_tunepar1 <- paste0("<li> Error : The cost should be numbers and greater than 0. ",
                           "Analysis has been stopped.") 
  }
  
  #### 4.2.8 커널함수-gamma 확인 #####
  if (gamma <= 0 | is.character(gamma)) {
    err_tunepar2 <- paste0("<li> Error : The gamma should be numbers and greater than 0. ",
                           "Analysis has been stopped.") 
  }
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
obs_unchecked_data <- dataset
indep_var <- c(conti_var, cat_var)
selected_var <- c(dep_var, indep_var, partition_var)
dataset <- dataset[complete.cases(dataset[, selected_var, drop = FALSE]), selected_var, drop = FALSE]
obs_checked_data <- dataset

if (nrow(dataset) == 0) {
  err_obs2 <- paste0("<li> Error : When observations with missing value was excluded in data, ",
                     "there are not enough observation. Analysis has been stopped.")
}

# 훈련 및 시험 데이터셋 정리
if (partition_method == "by_random") { # "by_variable", "use_all"
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
    
    train_set <- train_set[order(as.numeric(rownames(train_set))), , drop = FALSE]
    test_set <- test_set[order(as.numeric(rownames(test_set))), , drop = FALSE]
  }
} else if (partition_method == "by_variable") {
  
} else { # "use_all"
  train_set <- dataset
}
# partition 데이터 우선 저장: 
# validation set 을 사용하지 못하는 경우 경고문 출력 후 이 데이터 사용
s_train_set <- train_set


# 훈련 및 시험 데이터셋 정리가 끝난 후 데이터 한번 저장해야할듯.
# validation 을 할 수 있는 조건이 안되면 이떄 저장한 데이터를 사용하도록
# validation 이 check 되면 일단 데이터셋 나눔.

# 훈련자료를 훈련/검증 자료로 나눔
if (valid_method == "split") {
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


# CATEGORICAL VARIABLE CHECK
# test dataset level check 
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

# validation dataset level check
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
        out <- expand.grid(cost = 2^((1:len) - 3), degree = 1:2, 
                           gamma = c(0.2, 0.23, 0.25, 0.27, 0.3))
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
  svm_control <- trainControl(
    method = "cv", #valid_method,
    number = ifelse(grepl("cv", valid_method), fold, 1),
    repeats = ifelse(grepl("[d_]cv$", valid_method), iter, NA),
    search = search_method,
    savePredictions = TRUE,
    classProbs = ifelse(analysis_type == "C-classification", TRUE, FALSE),
    verboseIter = TRUE)
  
  # parameter combination
  if (search_method == "grid") {
    params <- svm_slot$grid(len = tune_len, search = search_method)
  } else if (search_method == "random") {
    params <- svm_slot$random(len = tune_len, search = search_method)
  } else { #custom
    params <- expand.grid(cost = cost, degree = degree, gamma = gamma)
  }
  
  # when to apply resampling method for training
  if (valid_method == "split") {
    svm_fit_list <- lapply(
      1:nrow(params),
      function (i) {
        res <- svm(x = train_set[, indep_var],
                   y = train_set[, dep_var],
                   type = analysis_type,
                   scale = standardize, 
                   kernel = kernel,
                   cost = params[i, ]$cost,
                   degree = params[i, ]$degree,
                   gamma = params[i, ]$gamma,
                   probability = TRUE)
        return(res)
      })
    
    # beta0 <- - svm_fit_list[[1]]$rho
    # svm_fit_list[[1]]$coefs * valid_set[, indep_var]
    
    if (exists("change_level")) {
      for (l in levels(valid_set[, dep_var])) {
        levels(valid_set[, dep_var])[levels(valid_set[, dep_var]) == l] <- paste0("X", l)
      }
    }
    valid_fitted <- sapply(svm_fit_list, 
                           function (x) { svm_slot$predict(x, valid_set[, indep_var]) })
    fitted_smmry <- apply(valid_fitted, 2, postResample, obs = valid_set[, dep_var])
    fit_result <- data.frame(params,
                             Accuracy = fitted_smmry[1, ],
                             Kappa = fitted_smmry[2, ])
    svm_fit <- list()
    acc_order <- order(fit_result$Accuracy, decreasing = TRUE)
    svm_fit$results <- fit_result[acc_order, ]
    svm_fit$bestTune <- fit_result[1, parameters$parameters]
    svm_fit$finalModel <- svm_fit_list[[acc_order[1]]]
    
  } else if (valid_method == "none") {
    svm_fit_list <- lapply(
      1:nrow(params),
      function (i) {
        res <- svm(x = train_set[, indep_var],
                   y = train_set[, dep_var],
                   type = analysis_type,
                   scale = standardize, 
                   kernel = kernel,
                   cost = params[i, ]$cost,
                   degree = params[i, ]$degree,
                   gamma = params[i, ]$gamma,
                   probability = TRUE)
        return(res)
      })
    
    train_fitted <- sapply(svm_fit_list, function (x) { x$fitted })
    fitted_smmry <- apply(train_fitted, 2, postResample, obs = train_set[, dep_var])
    fit_result <- data.frame(params, 
                             Accuracy = fitted_smmry[1, ],
                             Kappa = fitted_smmry[2, ])
    svm_fit <- list()
    acc_order <- order(fit_result$Accuracy, decreasing = TRUE)
    svm_fit$results <- fit_result[acc_order, ]
    svm_fit$bestTune <- fit_result[1, parameters$parameters]
    svm_fit$finalModel <- svm_fit_list[[acc_order[1]]]
    
  } else { # resampling by caret package
    
    # caret package allow to have reference category as letters
    dep_var_level <- levels(train_set[, dep_var])
    if (analysis_type == "C-classification" && !is.na(as.numeric(dep_var_level))) {
      change_level <- c()
      for (l in dep_var_level) {
        levels(train_set[, dep_var])[levels(train_set[, dep_var]) == l] <- paste0("X", l)
      }
    }
    load("lib_yy/SVM/svm_clssf_grid_polykern_4cv.RData")
    
    # TEMPORARY DEACTIVATED FOR ANALYSIS LOADING TIME
    # svm_fit <- train(x = train_set[, indep_var],
    #                  y = train_set[, dep_var],
    #                  method = svm_slot,
    #                  tuneLength = tune_len,
    #                  trControl = svm_control,
    #                  tuneGrid = params,
    #                  preProcess = NULL,
    #                  weights = NULL,
    #                  # the dots
    #                  standardize = standardize, 
    #                  kernel = kernel, 
    #                  analysis_type = analysis_type)
    
    if (class(svm_fit) == "try-error") {
      stop_analysis2 <- c()
      R2HTML::HTML(R2HTML::as.title("Warnings"), HR = 2, file = local_output)
      R2HTML::HTML(svm_fit, file = local_output)
    }
    
    if (exists("change_level")) {
      
      changed_level <- svm_fit$levels
      svm_fit$levels <- substring(changed_level, 2)
      origin_level <- as.character(svm_fit$levels)
      
      for (l in changed_level) {
        levels(train_set[, dep_var])[levels(train_set[, dep_var]) == l] <- substring(changed_level[changed_level == l], 2)
        levels(svm_fit$pred$pred)[levels(svm_fit$pred$pred) == l] <- substring(changed_level[changed_level == l], 2)
        levels(svm_fit$pred$obs)[levels(svm_fit$pred$obs) == l] <- substring(changed_level[changed_level == l], 2)
        levels(svm_fit$finalModel$fitted)[levels(svm_fit$finalModel$fitted) == l] <- substring(changed_level[changed_level == l], 2)
        levels(svm_fit$trainingData$.outcome)[levels(svm_fit$trainingData$.outcome) == l] <- substring(changed_level[changed_level == l], 2)
      }
      
      colnames(svm_fit$pred)[3:(3+length(changed_level)-1)] <- origin_level
      svm_fit$finalModel$levels <- origin_level
      svm_fit$finalModel$obsLevels <- svm_fit$levels
    }
  }
}

if (!exists("stop_analysis1") & !exists("stop_analysis2")) {
  
  # Data Structure
  R2HTML::HTML(R2HTML::as.title("Data Structure"), HR = 2, file = local_output)
  
  DS <- DStr(raw_dat, c(dep_var, indep_var, partition_var), 2)
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
  if (!is.null(partition_var)) {
    VL <- rbind(VL, VList("Partition Variable", partition_var, raw_dat[, partition_var]))
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
  AD <- rbind(
    AD, 
    matrix(c("Kernel function", paste(c(kern_method, 
                                   " (", 
                                   paste(parameters$label, collapse = ", "), 
                                   ")"), collapse = "")),
           nrow = 1, ncol = 2, byrow = TRUE))
  AD <- rbind(AD, matrix(c("Search method", search_method),
                         nrow = 1, ncol = 2, byrow = TRUE))
  AD <- rbind(AD, matrix(c("Data partition", ifelse(partition_method == "use_all", FALSE, TRUE)),
                         nrow = 1, ncol = 2, byrow = TRUE))
  if (partition_method == "use_all") {
    AD <- rbind(
      AD, matrix(c("Data partition method", 
                   paste("[Using all data as training data] Train : 100% (n=", 
                         nrow(train_set), ")", sep = "")),
                 nrow = 1, ncol = 2, byrow = TRUE))
  } else if (partition_method == "by_random") {
    AD <- rbind(
      AD, matrix(c("Data partition method",
                   paste("[Split by random] Train : ", p_train, "% (n=",
                         nrow(train_set), ") / Test : ", 100 - p_train, "% (n=",
                         nrow(test_set), ")", sep = "")),
                 nrow = 1, ncol = 2, byrow = TRUE))
  } else if (partition_method == "by_variable") {
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
    if (exists("test_set") & exists("train_set")) {
      R2HTML::HTML(AD, file = local_output, align = "left",
                   caption = paste("<div style=text-align:left><small>",
                                   "* Number of missing at train / test dataset : ",
                                   nrow(obs_unchecked_data) - nrow(obs_checked_data),
                                   "</small>", sep = ""))
    } else {
      R2HTML::HTML(AD, file = local_output, align = "left",
                   caption = paste("<div style=text-align:left><small>",
                                   "* Number of missing at train : ",
                                   nrow(obs_unchecked_data) - nrow(obs_checked_data),
                                   "</small>", sep = ""))
    }
  }
  
  R2HTML::HTML(R2HTML::as.title("Results of Support Vector Machine (SVM)"), HR = 2, file = local_output)
  
  if (valid_method == "split") {
    R2HTML::HTML(as.title("Validation using training dataset by random split"), HR = 3, file = local_output)
  } else if (valid_method == "none") {
    R2HTML::HTML(as.title("Training result"), HR = 3, file = local_output)
  } else {
    if (valid_method == "cv") {
      if (fold > nrow(train_set)) {
        warn_valid1 <- paste0("<li> Warning : The number of folds cannot be greater than ",
                              "the number of non-missing observations. (# of non-misssing ",
                              "observations: ", nrow(train_set), ", # of folds specified by ",
                              "user: ", fold, ", readjusted # of folds: ", nrow(train_set), ")")
        fold <- nrow(train_set)
      }
      R2HTML::HTML(as.title(paste0(fold, "-Fold Cross Validation")), HR = 3, file = local_output)
    } else if (valid_method == "LOOCV") {
      R2HTML::HTML(R2HTML::as.title("Leave-One-Out Cross Validation"), HR = 3, file = local_output)
    } else {
      # other resampling method not yet implemented
    }
  }
  
  # TEST SET EVALUDATION
  # 4. accuracy/gamma, degree, 
  # 5. train, test, validation : cv사용하면 각 fold별로 어떻게 그림을 그릴 수 있는지?
  
  # Confusion matrix for [TEST DATASET]
  if (exists("test_set")) {
    
    test_pred <- predict(svm_fit$finalModel, test_set[, indep_var])
    
    if (analysis_type == "C-classification") {
      test_conf <- try(confusionMatrix(test_pred, as.factor(test_set[, dep_var])), silent = TRUE)
      
      test_confmat <- test_conf$table
      colnames(test_confmat) <- rownames(test_confmat) <- paste("Class: ", colnames(test_confmat))
      
      test_eval_metric <- as.matrix(round(test_conf$overall, 4))
      test_eval_metric <- test_eval_metric[c("Accuracy", "AccuracyLower", "AccuracyUpper", "AccuracyNull",
                                             "AccuracyPValue", "Kappa", "McnemarPValue"), , drop = FALSE]
      rownames(test_eval_metric) <- c("Acc", "LCI_Acc", "UCI_Acc", "NIR", "P-value<sup>1</sup>",
                                      "Kappa", "P-value<sup>2</sup>")
      
      test_acc <- test_conf$byClass
      if (sum(nrow(test_confmat) == nrow(test_acc)) == 1) {
        test_acc <- format(round(t(as.matrix(test_acc)), 4), 
                           scientific = FALSE, digits = 4, nsmall = 4)
      } else {
        # 이부분 다시 보기 필요
        test_acc <- format(round(as.matrix(test_acc), 4), 
                           scientific = FALSE, digits = 4, nsmall = 4)
        colnames(test_acc)[1:ncol(test_acc)] <- rownames(test_confmat)[1:ncol(test_acc)]
      }
    } else { # For regression
      test_obs <- test_set[, dep_var]
      test_resid <- test_pred - test_obs
      
      test_mse <- mean(test_resid^2, na.rm = TRUE)
      test_rmse <- sqrt(test_mse)
      test_mae <- mean(abs(test_resid), na.rm = TRUE)
      test_mape <- mean(abs(test_resid / test_obs)) * 100
      test_rsq <- cor(test_pred, test_obs, use = "complete.obs")
    }
  }
  
  # VALIDATION 도 필요하나? train/validation 을 비교해야하나? train/test 를 비교해야하나?
  # 찾아봐야할듯
  
  # Confusion matrix for [TRAINING DATASET]
  train_pred <- predict(svm_fit$finalModel, train_set[, indep_var])
  if (analysis_type == "C-classification") {
    train_conf <- try(confusionMatrix(train_pred, as.factor(train_set[, dep_var])), silent = TRUE)
    
    train_confmat <- train_conf$table
    colnames(train_confmat) <- rownames(train_confmat) <- paste("Class: ", colnames(train_confmat))
    
    train_eval_metric <- as.matrix(round(train_conf$overall, 4))
    train_eval_metric <- train_eval_metric[c("Accuracy", "AccuracyLower", "AccuracyUpper", "AccuracyNull",
                                             "AccuracyPValue", "Kappa", "McnemarPValue"), , drop = FALSE]
    rownames(train_eval_metric) <- c("Acc", "LCI_Acc", "UCI_Acc", "NIR", "P-value<sup>1</sup>",
                                    "Kappa", "P-value<sup>2</sup>")
    train_acc <- train_conf$byClass
    if (sum(nrow(train_confmat) == nrow(train_acc)) == 1) {
      train_acc <- format(round(t(as.matrix(train_acc)), 4),
                          scientific = FALSE, digits = 4, nsmall = 4)
    } else {
      train_acc <- format(round(as.matrix(train_acc), 4),
                          scientific = FALSE, digits = 4, nsmall = 4)
      colnames(train_acc)[1:ncol(train_acc)] <- rownames(train_confmat)[1:ncol(train_acc)]
    }
  } else {
    train_obs <- train_set[, dep_var]
    train_resid <- train_pred - train_obs
    
    train_mse <- mean(train_resid^2, na.rm = TRUE)
    train_rmse <- sqrt(train_mse)
    train_mae <- mean(abs(train_resid), na.rm =  TRUE)
    train_mape <- mean(abs(train_resid / train_obs)) * 100
    train_rsq <- cor(train_pred, train_obs, use = "complete.obs")
  }
  
  # Aggregate training/test confusion matrix
  if (analysis_type == "C-classification") {
    R2HTML::HTML(as.title("Confusion Matrix"), HR = 4, file = local_output)
    
    if (exists("test_acc")) {
      confmat <- matrix(NA, nrow(test_confmat) + nrow(train_confmat) + 5, ncol(test_confmat) + 1)
      confmat[c(1, nrow(train_confmat) + 4), 1] <- c("<b>train dataset<b>", "<b>test dataset<b>")
      confmat[2, 2:(ncol(train_confmat) + 1)] <- colnames(train_confmat)
      confmat[3:(nrow(train_confmat) + 2), 1] <- rownames(train_confmat)
      confmat[3:(nrow(train_confmat) + 2), 2:(ncol(train_confmat) + 1)] <- train_confmat
      confmat[(nrow(train_confmat) + 2 + 3), 2:ncol(confmat)] <- colnames(test_confmat)
      confmat[(nrow(train_confmat) + 2 + 4):nrow(confmat), 1] <- rownames(test_confmat)
      confmat[(nrow(train_confmat) + 2 + 4):nrow(confmat), 2:ncol(confmat)] <- test_confmat
    } else {
      confmat <- matrix(NA, nrow(train_confmat) + 2, ncol(train_confmat) + 1)
      confmat[1, 1] <- "<b>train dataset<b>"
      confmat[2, 2:ncol(confmat)] <- colnames(train_confmat)
      confmat[3:nrow(confmat), 1] <- rownames(train_confmat)
      confmat[3:nrow(confmat), 2:ncol(confmat)] <- train_confmat
      confmat[is.na(confmat)] <- ""
    }
    
    R2HTML::HTML(confmat, file = local_output, align = "left", row.names = FALSE,
                 caption = paste0("<div style= 'text-align:left'> <small>* Row : Predicted class ",
                                  "<br>* Column : True class</small>"))
    R2HTML::HTML(as.title("Overall Statistics"), HR = 4, file = local_output)
    
    if (exists("test_eval_metric")) {
      eval_metric <- cbind(train_eval_metric, test_eval_metric)
      colnames(eval_metric) <- c("train dataset", "test dataset")
    } else {
      eval_metric <- train_eval_metric
      colnames(eval_metric) <- "train dataset"
    }
    
    R2HTML::HTML(eval_metric, file = local_output, align = "left", digits = 4, row.names = FALSE,
                 caption = paste0('<div style="text-align:left"> <small>* Acc = Accuracy ',
                                  '<br>* LCI_Acc = Lower Bound of 95% CI for Accuracy ',
                                  '<br>* UCI_Acc = Upper bound of 95% CI for Accuracy ',
                                  '<br>* NIR = No Information Rate <br>* p-value<sup>1</sup> = p-value for Acc > NIR ',
                                  '<br>* p-value<sup>2</sup> = p-value for Mcnemars test</small>'))
    R2HTML::HTML(as.title("Statistics by Class"), HR = 4, file = local_output)
    
    if (exists("test_acc")) {
      acc <- matrix(NA, nrow(test_acc) + nrow(train_acc) + 5, ncol(test_acc) + 1)
      acc[c(1, nrow(train_acc) + 4), 1] <- c("<b>train dataset<b>", "<b>test dataset<b>")
      acc[2, 2:(ncol(train_acc) + 1)] <- colnames(train_acc)
      acc[3:(nrow(train_acc) + 2), 1] <- rownames(train_acc)
      acc[3:(nrow(train_acc) + 2), 2:(ncol(train_acc) + 1)] <- train_acc
      acc[(nrow(train_acc) + 2 + 3), 2:ncol(acc)] <- colnames(test_acc)
      acc[(nrow(train_acc) + 2 + 4):nrow(acc), 1] <- rownames(test_acc)
      acc[(nrow(train_acc) + 2 + 4):nrow(acc), 2:ncol(acc)] <- test_acc
      acc[is.na(acc)] <- ""
    } else {
      acc <- matrix(NA, nrow(train_acc) + 2, ncol(train_acc) + 1)
      acc[1, 1] <- "<b>train dataset<b>"
      acc[2, 2:ncol(acc)] <- colnames(train_acc)
      acc[3:nrow(acc), 1] <- rownames(train_acc)
      acc[3:nrow(acc), 2:ncol(acc)] <- train_acc
      acc[is.na(acc)] <- ""
    }
    
    R2HTML::HTML(acc, file = local_output, align = "left", digits = 4)
    
    if (length(conti_var) == 2) {
      
      clssf_plot_df <- data.frame(test_set, pred = test_pred)
      
      REx_ANA_PLOT()
      ggp <- ggplot(data = clssf_plot_df, aes(conti_var[1], conti_var[2], colour = pred)) + 
        geom_point() + scale_colour_manual(values = c("red", "blue")) 
      ggp <- REx_GraphicsGOset(ggp)
      print(rexAnaImage <<- ggp)
      REx_ANA_PLOT_OFF("")
    }
    # ggp <- ggplot() + 
    #   geom_point(data = fine_grid, aes(x = matage, y = bweight, colour = pred), alpha = 0.25) + 
    #   stat_contour(data = fine_grid, aes(x = matage, y = bweight, z = as.integer(pred)),
    #                lineend = "round", linejoin = "round", linemitre = 1, size = 0.25, colour = "black") + 
    #   geom_point(data = train_set, aes(x = matage, y = bweight, colour = hyp, shape = hyp)) + 
    #   ggtitle("SVM decision boundaries for leaf length vs. leaf width") + 
    #   labs(x = "matage", y = "hyp", colour = "class", shape = "class") + 
    #   theme(plot.title = element_text(hjust = 0.5))
    
    
    # heatmap related to Accuracy
    heatmap_data <- svm_fit$results[svm_fit$results$degree == 1, c("cost", "gamma", "Accuracy")]
    REx_ANA_PLOT()
    ggp_hm <- ggplot(heatmap_data, aes(cost, gamma, fill = Accuracy)) + 
      ggtitle("Performance of Support Vector Classification via parameter combinations") + 
      geom_raster(interpolate = TRUE) + 
      scale_fill_gradient(low = "white", high = "blue", space = "Lab") + 
      theme_bw()
    ggp_hm <- REx_GraphicsGOset(ggp_hm)
    print(rexAnaImage <<- ggp_hm)
    REx_ANA_PLOT_OFF("")
    
  } else { # for regression
    eval_metric <- format(
      data.frame(MSE = train_mse, RMSE = train_rmse, 
                 MAE = train_mae, MAPE = train_mape, Rsquared = train_rsq),
      scientific = FALSE, digits = 4, nsmall = 4)
    rownames(eval_metric) <- "Train"
    if (exists("test_set")) {
      eval_metric <- rbind(
        eval_metric,
        format(data.frame(MSE = test_mse, RMSE = test_rmse,
                   MAE = test_mae, MAPE = test_mape, Rsquared = test_rsq),
        scientific = FALSE, digits = 4, nsmall = 4))
      rownames(eval_metric)[2] <- "Test"
    }
    
    R2HTML::HTML(t(eval_metric), file = local_output, align = "left", digits = 15)
    R2HTML::HTML(as.title("Comparision Plot (Train)"), HR = 5, file = local_output)
    
    REx_ANA_PLOT()
    ggp <- ggplot(data = data.frame(x = train_pred, y = train_obs), aes(x = x, y = y)) + 
      geom_point() + geom_abline(intercept = 0, slope = 1) + 
      labs(x = "Predicted", y = "Observed", 
           title = "Comparison between predicted values and observed values") + 
      theme_bw() + theme(panel.grid = element_blank())
    ggp <- REx_GraphicsGOset(ggp)
    print(rexAnaImage <<- ggp)
    REx_ANA_PLOT_OFF("")
    
    if (exists("test_set")) {
      R2HTML::HTML(as.title("Comparison Plot (Test)"), HR = 5, file = local_output)
      
      REx_ANA_PLOT()
      ggp_test <- ggplot(data = data.frame(x = test_pred, y = test_obs), aes(x = x, y = y)) +
        geom_point() + geom_abline(intercept = 0, slope = 1) + 
        labs(x = "Predicted", y = "Observed",
             title = "Comparison between predicted values and observed values") + 
        theme_bw() + theme(panel.grid = element_blank())
      ggp_test <- REx_GraphicsGOset(ggp_test)
      print(rexAnaImage <<- ggp_test)
      REx_ANA_PLOT_OFF("")
      
      # only for there is one independent variable. 
      REx_ANA_PLOT()
      ggp_svr <- ggplot() + 
        geom_point(aes(x = test_set[, indep_var[1]], y = test_set[, dep_var]), colour = "red") + 
        geom_smooth(aes(x = test_set[, indep_var[1]], y = test_pred), colour = "blue") +
        ggtitle("Support vector regression result plot when there is one independent variable") + 
        labs(x = indep_var[1], y = dep_var) + 
        theme_bw() + theme(panel.grid = element_blank())
      ggp_svr <- REx_GraphicsGOset(ggp_svr)
      print(rexAnaImage <<- ggp_svr)
      REx_ANA_PLOT_OFF("")
      
      
      # heatmap related to RMSE
      heatmap_data <- svm_fit$results[svm_fit$results$degree == 1, c("cost", "gamma", "RMSE")]
      REx_ANA_PLOT()
      ggp_hm <- ggplot(heatmap_data, aes(cost, gamma, fill = RMSE)) + 
        ggtitle("Performance of Support Vector Regression via parameter combinations") + 
        geom_raster(interpolate = TRUE) + 
        scale_fill_gradient(low = "blue", high = "white", space = "Lab") + 
        theme_bw()
      ggp_hm <- REx_GraphicsGOset(ggp_hm)
      print(rexAnaImage <<- ggp_hm)
      REx_ANA_PLOT_OFF("")
    }
  }
  
  
  # FINAL PARAMETER SUMMARY
  # final parameter information according to kernel function
  if (length(svm_fit) == 1 & all(class(svm_fit) == "try-error")) {
    R2HTML::HTML(R2HTML::as.title("Warnings"), HR = 4, file = local_output)
    # 추가로 더 짜야함
  } else {
    svm_res <- svm_fit$results
    eval_stat <- colnames(svm_res)[!colnames(svm_res) %in% parameters$parameters]
    res <- svm_res[, c(parameters$parameters, eval_stat)]
    colnames(res)[1:length(parameters$parameters)] <- parameters$label
    
    final_param <- unlist(lapply(parameters$label, function (x) { paste(c(x, " = "), collapse = "") }))
    final_param <- paste0(final_param, svm_fit$bestTune, collapse = ", ")
    
    if (analysis_type == "C-classification") {
      res_info <- paste("<li> <strong>Accuracy</strong> was used to select the",
                        " optimal model using the largest value. ",
                        "<br><li> The final value used for the model were ",
                        "<strong>", final_param, "</strong>.", sep = "")
    } else { # regression
      res_info <- paste("<li> <strong>RMSE</strong> was used to select the",
                        " optimal model using the smallest value. ",
                        "<br><li> The final value used for the model were ",
                        "<strong>", final_param, "</strong>.", sep = "")
    }
    R2HTML::HTML(as.title("Hyper parameters printed out top 5"), HR = 4, file = local_output)
    R2HTML::HTML(res[1:5, ], file = local_output, align = "left", digits = 4, row.names = FALSE)
    R2HTML::HTML(res_info, file = local_output, align = "left")
    if (exists("warn_valid")) { R2HTML::HTML(warn_valid, file = local_output) }
  }
  
  # Used R Packages
  R2HTML::HTML(R2HTML::as.title("Used R Packages"), HR = 2, file = local_output)
  pkg_list <- list(list("Support vector machine (SVM)", "svm", "e1071"))
  if (valid_method != "none") {
    pkg_list$validation <- list("Validation", c("trainControl", "train"), c("caret", "caret"))
    if (analysis_type == "C-classification") { 
      pkg_list$Partition <- list("Confusion matrix", "confusionMatrix", "caret")
    }
  }
  if (partition_method != "use_all") {
    pkg_list$predict <- list("Prediction using test set", "predict", "stats")
  }
  R2HTML::HTML(used.pkg(pkg.list = pkg_list), file = local_output)
}

# Time Track for Analysis
R2HTML::HTMLhr(file = local_output)
R2HTML::HTML(paste("Analysis is finished at ", Sys.time(), ". REx : SVM anlaysis", sep = ""), file = local_output)

# if (exists("O")) {
#   return(list(html = html.output, Output = O))
# } else {
#   return(html.output)
# }

# })
