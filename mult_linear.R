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


cur_dir <- getwd()
load(paste(cur_dir, "/scrapped_data.RData", sep = ""))

set.seed(17)
train_size = get_train_size(0.8, nrow(stock_vs_mefactors))

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

# Remove the data column from dataframe and split into train test dataframes
data_split = train_test_split(mefactors, sp500, train_size)
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

# Normalize independent variables
index_train = (index_train - mean(index_train)) / sd(index_train)
index_test = (index_test - mean(index_train)) / sd(index_train)

# Get the PCs on factor training set and extract PCs upto 99 % of total variance explained
mefactors_pc = princomp(factors_train, cor = T)
req_pcs = length(mefactors_pc$sdev[cumsum((mefactors_pc$sdev)^2) / ncol(factors_train)<0.99])+1
pc = mefactors_pc$loadings[, 1 : req_pcs]

# Converting X into PC Values
pc_values = factors_train%*%pc

############################################
# ...... Multiple Linear Regression ...... #
############################################
linear_reg<-lm(index_train~pc_values) 

# Calculating the beta and original alpha value
beta = linear_reg$coefficients[2 : length(linear_reg$coefficients)]
alpha = pc %*% beta

# Fit the regression to train set and compute its rss
index_fit = factors_train %*% alpha
rss_train = sum((index_train - index_fit)^2)

# Fit the regression to test set to predict and compute its rss
index_pred = factors_test %*% alpha
rss_test = sum((index_test - index_pred)^2)
rss_test

# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
