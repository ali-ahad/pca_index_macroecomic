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

cur_dir <- getwd()
load(paste(cur_dir, "/scrapped_data.RData", sep = ""))

set.seed(17)
train_size = ceiling(0.8 * nrow(stock_vs_mefactors))

# Extract the dependent variable and remove it from original dataframe - in our situation s&p500 index
sp500 = stock_vs_mefactors$`S&P Monthly`
mefactors = stock_vs_mefactors[,3:ncol(stock_vs_mefactors)]

mefactors_cor = cor(mefactors, method = "pearson")
colnames(mefactors_cor) = c(seq(1:length(mefactors)))
rownames(mefactors_cor) = c(seq(1:length(mefactors)))

# The correlation plot shows there is some correlation between factors that we can exploit
corrplot(mefactors_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 90)

# Remove the data column from dataframe and split into train test dataframes
X = mefactors
y = sp500
X_train = X[1 : train_size,]
X_test = X[seq(train_size + 1, nrow(stock_vs_mefactors)),]
y_train = y[1 : train_size]
y_test = y[seq(train_size + 1, nrow(stock_vs_mefactors))]

# Normalisation of independent and dependent variables
X_mean = apply(X_train,2,FUN=mean)
X_std = apply(X_train,2,FUN=sd)
y_mean = mean(y_train)
y_std = sd(y_train)

for (i in 1:ncol(X_train)) {
  X_train[,i] = (X_train[,i] - X_mean[i])/ X_std[i]
}
for (i in 1:ncol(X_test)) {
  X_test[,i] = (X_test[,i] - X_mean[i])/ X_std[i]
}
y_train = (y_train - y_mean) / y_std
y_test = (y_test - y_mean) / y_std

X_train = as.matrix(X_train)
X_test = as.matrix(X_test)

##Doing the PCA on the X_train (train set of mafactors)
mefactors_pc = princomp(X_train,cor=T)

##Extracting PCs up to 90% of total variance 
n = length(mefactors_pc$sdev[cumsum((mefactors_pc$sdev)^2)/ncol(X_train)<0.95])+1
pc = mefactors_pc$loadings[,1:n]

##Converting X into PC values
PCValue = X_train%*%pc

##the regression on extracted PCs
fit1<-lm(y_train~PCValue)
beta=fit1$coefficients[2:length(fit1$coefficients)]

##Calculating the original alpha
alpha = pc%*%beta

##Fit the regression to train set
y_fit=X_train%*%alpha
##Computing the RSS on test set
sum((y_train-y_fit)^2)

#Prediction on test set
y_pred = X_test%*%alpha
##Computing the RSS on test set
sum((y_test-y_pred)^2)

# Orthogonality check
res = cor(mefactors_pc$scores, method = "pearson")
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 90)


# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
