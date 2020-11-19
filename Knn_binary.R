#Sample Code

library(tibble)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(caret)
library(dbplyr)


# read full data
hd = readr::read_csv("data/hd.csv")

# function to determine proportion of NAs in a vector
na_prop = function(x) {
  mean(is.na(x))
}

# check proportion of NAs in each column
sapply(hd, na_prop)

# create dataset without columns containing more than 33% NAs
hd_clean = na.omit(hd[, !sapply(hd, na_prop) > 0.33])

hd_clean <- as_data_frame(hd_clean)
hd_clean$num <- factor(hd_clean$num)
hd_clean$fbs <- factor(hd_clean$fbs)
hd_clean$exang <- factor(hd_clean$exang)
hd_clean$sex <- factor(hd_clean$sex)
hd_clean$fbs <- factor(hd_clean$fbs)
hd_clean$restecg <- factor(hd_clean$restecg)
hd_clean$cp <- factor(hd_clean$cp)


hd_clean$num = factor(dplyr::case_when(
  hd_clean$num == "v0" ~ "0",
  hd_clean$num == "v1" ~ "1",
  hd_clean$num == "v2" ~ "1",
  hd_clean$num == "v3" ~ "1",
  hd_clean$num == "v4" ~ "1"
))



# tst-trn split data
trn_idx = sample(nrow(hd_clean), size = 0.8 * nrow(hd_clean))
trn = hd_clean[trn_idx, ]
tst = hd_clean[-trn_idx, ]


k_val = seq(1, 101, by = 2)



calc_accuracy = function(actual, predicted) {
  mean(actual == predicted)
}


index_fold = caret::createFolds(trn$num, k = 5)

calc_accuracy_single_fold = function(index_value, k){

  #split trn into est and val

  est = trn[-index_value,]
  val = trn[index_value,]


  knn_mod = knn3(formula= num~.,data= est, k = k)


  pred = predict(knn_mod, val,type ="class")

  tp = sum(val$num == 1 & pred == 1)
  fp = sum(val$num == 0 & pred == 1)
  fn = sum(val$num == 1 & pred == 0)
  tn = sum(val$num == 0 & pred == 0)



  calc_accuracy(actual =val$num, predicted = pred )

}


calc_sensitivity_single_fold = function(index_value, k){

  #split trn into est and val

  est = trn[-index_value,]
  val = trn[index_value,]


  knn_mod = knn3(formula= num~.,data= est, k = k)


  pred = predict(knn_mod, val,type ="class")

  tp = sum(val$num == 1 & pred == 1)
  fp = sum(val$num == 0 & pred == 1)
  fn = sum(val$num == 1 & pred == 0)
  tn = sum(val$num == 0 & pred == 0)



  fnr = tp / (tp + fn)

}

calc_cv_accuracy_for_k = function(neighbours){
  fold_accuracy = sapply(index_fold, calc_accuracy_single_fold, k = neighbours)
  c(
    cv_accuracy = mean(fold_accuracy)
  )
}

cv_accuracy_results = sapply(k_val, calc_cv_accuracy_for_k)

(cv_accuracy_results)


plot(k_val,cv_accuracy_results)

calc_cv_sens_for_k = function(neighbours){
  fold_sens = sapply(index_fold, calc_sensitivity_single_fold, k = neighbours)
  c(
    cv_sens = mean(fold_sens)
  )
}

cv_sens_results = sapply(k_val, calc_cv_sens_for_k)

(cv_sens_results)


plot(k_val,cv_sens_results)


