install.packages("corrplot")
library(tseries)
library(zoo)
library(rio)
library(tibbletime)
library(dplyr)
library(imputeTS)
library(corrplot)
library(MASS)
install.packages("CRAN")

# Get training size for data set
get_train_size = function(train_percent, num_rows) {
  train_size = ceiling(train_percent * num_rows)
  return (train_size)
}

# Split the data in training and testing set
train_test_split = function(mefactors, index, train_size) {
  x_train = mefactors[1: train_size,]
  x_test = mefactors[seq(train_size + 1, nrow(mefactors)),]
  
  y_train = index[1 : train_size]
  y_test = index[seq(train_size + 1, nrow(mefactors))]
  
  return(list("x_train" = x_train, "x_test" = x_test, "y_train" = y_train, "y_test" = y_test))
}

# Function to reduce PCs and perform logistic regression on them
optim_logit_reg = function(data, pc_values, col_to_remove) {
  optim_pc_values = pc_values[,-col_to_remove]
  lreg = glm(data ~ optim_pc_values, family = "binomial")
  summary = summary(lreg) 
  return (list("new_pc_vals" = optim_pc_values, "summary" = summary, "reg_object" = lreg))
}

# Function to find the regression accuracy to see if further reduction in PCs is required
regression_accuracy = function(reg, index_compare, dataset_size) {
  vals = (reg$fitted.values > 0.5)
  tbl = table(vals, index_compare)
  accuracy = (tbl[1, ][1] + tbl[2, ][2]) / dataset_size
  return (accuracy)
}

cur_dir <- getwd()
load(paste(cur_dir, "/scrapped_data.RData", sep = ""))

set.seed(17)
# Extract the dependent variable and remove it from original dataframe - in our situation s&p500 index
sp500 = stock_vs_mefactors$`S&P Monthly`
mefactors = stock_vs_mefactors[,3:ncol(stock_vs_mefactors)]

# Used for logistic regression
index_ret = (lag(sp500) - sp500) / sp500

# Get the correlation matrrix and see how we can exploit the correlation
mefactors_cor = cor(mefactors, method = "pearson")
colnames(mefactors_cor) = c(seq(1:length(mefactors)))
rownames(mefactors_cor) = c(seq(1:length(mefactors)))
corrplot(mefactors_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 90)

############################################
# ...... Logistic Regression...... #
############################################
index_ret_logit = index_ret[-1]
index_binary = ((index_ret_logit > 0) + 1) - 1
mefactors_logit = mefactors[-1,]

train_size = get_train_size(0.7, nrow(mefactors_logit))
data_split = train_test_split(mefactors_logit, index_binary, train_size)
factors_train = data_split$x_train
factors_test = data_split$x_test
index_train = data_split$y_train
index_test = data_split$y_test

# Normalisation of independent
factor_mean = apply(factors_train, 2, FUN = mean)
factor_std = apply(factors_train, 2, FUN = sd)
for (i in 1:ncol(factors_train)) {
  factors_train[,i] = (factors_train[,i] - factor_mean[i]) / factor_std[i]
}
for (i in 1:ncol(factors_test)) {
  factors_test[,i] = (factors_test[,i] - factor_mean[i]) / factor_std[i]
}
factors_train = as.matrix(factors_train)
factors_test = as.matrix(factors_test)

# Get the PCs on factor training set and extract PCs upto 99 % of total variance explained
mefactors_pc = princomp(factors_train, cor = T)
req_pcs = length(mefactors_pc$sdev[cumsum((mefactors_pc$sdev)^2) / ncol(factors_train)<0.99])+1
pc = mefactors_pc$loadings[, 1 : req_pcs]

# Converting X into PC Values
pc_values = factors_train%*%pc

############################################
# .......... Logistic Regression ......... #
############################################
accuracies = c()
first_reg = glm(index_train ~ pc_values, family = "binomial")
summary(first_reg)
acc1 = regression_accuracy(first_reg, index_train, train_size)

# Remove PCs that have the highest p-value while looking up there accuracy
second_reg = optim_logit_reg(index_train, pc_values, 2)
second_reg$summary
acc2 = regression_accuracy(second_reg$reg_object, index_train, train_size)

third_reg = optim_logit_reg(index_train, second_reg$new_pc_vals, 4)
third_reg$summary
acc3 = regression_accuracy(third_reg$reg_object, index_train, train_size)

fourth_reg = optim_logit_reg(index_train, third_reg$new_pc_vals, 5)
fourth_reg$summary
acc4 = regression_accuracy(fourth_reg$reg_object, index_train, train_size)

fifth_reg = optim_logit_reg(index_train, fourth_reg$new_pc_vals, 9)
fifth_reg$summary
acc5 = regression_accuracy(fifth_reg$reg_object, index_train, train_size)

sixth_reg = optim_logit_reg(index_train, fifth_reg$new_pc_vals, 3)
sixth_reg$summary
acc6 = regression_accuracy(sixth_reg$reg_object, index_train, train_size)

seventh_reg = optim_logit_reg(index_train, sixth_reg$new_pc_vals, 6)
seventh_reg$summary
acc7 = regression_accuracy(seventh_reg$reg_object, index_train, train_size)

accuracies = c(accuracies, acc1, acc2, acc3, acc4, acc5, acc6, acc7)
max_acc = max(accuracies)

# Max accuraacy occurs after 4th fitting, so we use its PCs for testing
beta = fourth_reg$reg_object$coefficients[2 : length(fourth_reg$reg_object$coefficients)]
fourth_reg$summary

pc_chosen = pc[, -c(4, 5, 9)]
alpha = pc_chosen %*% beta

y_pred = exp(factors_test %*% alpha) / (1 + exp(factors_test %*% alpha))
pr_test = y_pred > 0.5
tbl = table(pr_test, index_test)
tbl
accuracy_test = (tbl[1, ][1] + tbl[2, ][2]) / length(index_test)
accuracy_test

# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
