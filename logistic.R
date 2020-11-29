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

################################################
# ..........Related Trading Strategy ......... #
################################################
# Getting the Stock price of TQQQ and SQQQ
SPXL_price = get.hist.quote(instrument = "SPXL", start = "2012-03-02", end = "2020-10-02", quote = "Close", provider = "yahoo", compression = "m")
SPXS_price = get.hist.quote(instrument = "SPXS", start = "2012-03-02", end = "2020-10-02", quote = "Close", provider = "yahoo", compression = "m")
SPY_price = get.hist.quote(instrument = "SPY", start = "2012-03-02", end = "2020-10-02", quote = "Close", provider = "yahoo", compression = "m")
SPXL_price_vector<-as.vector(SPXL_price)
SPXS_price_vector<-as.vector(SPXS_price)
SPY_price_vector<-as.vector(SPY_price)
a<-(lag(SPXL_price_vector)-SPXL_price_vector) / SPXL_price_vector
b<-(lag(SPXS_price_vector)-SPXS_price_vector) / SPXS_price_vector
c<-(lag(SPY_price_vector)-SPY_price_vector) / SPY_price_vector
SPXL_price_monthly_return<-a[-1]
SPXS_price_monthly_return<-b[-1]
SPY_price_monthly_return<-c[-1]

##Calculating the performance of the strategy 1 
ret<-c()
for (i in 1:length(pr_test)){
  r<-ifelse(pr_test[i]==1,SPXL_price_monthly_return[i],SPXS_price_monthly_return[i])
  ret<-c(ret,r)
}
cum_performance_1<-cumprod(1+ret)

Period_return_1<-cum_performance_1[length(cum_performance_1)]-1
GM_Monthly_return_1<-(cum_performance_2[length(cum_performance_1)])^(1/length(cum_performance_1))-1

##Calculating the performance of the strategy 2
ret<-c()
for (i in 1:length(pr_test)){
  r<-ifelse(pr_test[i]==1,SPXL_price_monthly_return[i],0)
  ret<-c(ret,r)
}
cum_performance_2<-cumprod(1+ret)

Period_return_2<-cum_performance_2[length(cum_performance_2)]-1
GM_Monthly_return_2<-(cum_performance_1[length(cum_performance_2)])^(1/length(cum_performance_2))-1

##Calculating the performance of the strategy 3
ret<-c()
for (i in 1:length(pr_test)){
  r<-ifelse(pr_test[i]==1,SPXL_price_monthly_return[i],SPY_price_monthly_return[i])
  ret<-c(ret,r)
}
cum_performance_3<-cumprod(1+ret)

Period_return_3<-cum_performance_3[length(cum_performance_3)]-1
GM_Monthly_return_3<-(cum_performance_3[length(cum_performance_3)])^(1/length(cum_performance_3))-1

cum_performance_1_ts<-as.ts(cum_performance_1)
cum_performance_2_ts<-as.ts(cum_performance_2)
cum_performance_3_ts<-as.ts(cum_performance_3)


y_max<-max(max(cum_performance_1_ts),max(cum_performance_2_ts),max(cum_performance_3_ts))
y_min<-min(min(cum_performance_1_ts),min(cum_performance_2_ts),min(cum_performance_3_ts))

n<-"Strategies Performance"

pdf(n)

plot(cum_performance_1_ts ,ylim=c(y_min,y_max),xlab = 'Time(months)',ylab = 'Cumulative Performance', col='red')
title(n)
lines(cum_performance_2_ts,col='blue')
lines(cum_performance_3_ts,col='green')
legend("topleft",legend = c("SPXL and SPXS","SPXL only","SPXL and SPY"),col = c("red","blue","green"), fill = c("red","blue","green"))

dev.off()

df<-data_frame("SPXL and SPXS"=c(Period_return_1,GM_Monthly_return_1),
               "SPXL only"=c(Period_return_2,GM_Monthly_return_2),
               "SPXL and SPY"=c(Period_return_3,GM_Monthly_return_3))
df<-t(df)

colnames(df) = c("Period Return","GM Daily Return")



print(df)

write.xlsx(df,"Performance.xlsx")

# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
