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
y_ret = (lag(y) - y) / y

# Removing the first value of y_ret and first row 0f macroeconomic features
y_ret = y_ret[-1]
X = X[-1,]

y_binary = ((y_ret > 0) + 1) -1

train_size = ceiling(0.8 * nrow(X))
X_train = X[1 : train_size,]
X_test = X[seq(train_size + 1, nrow(X)),]
y_train = y_binary[1 : train_size]
y_test = y_binary[seq(train_size + 1, nrow(X))]

# Normalisation of dependent variables
X_mean = apply(X_train,2,FUN=mean)
X_std = apply(X_train,2,FUN=sd)

for (i in 1:ncol(X_train)) {
  X_train[,i] = (X_train[,i] - X_mean[i])/ X_std[i]
}
for (i in 1:ncol(X_test)) {
  X_test[,i] = (X_test[,i] - X_mean[i])/ X_std[i]
}

X_train = as.matrix(X_train)
X_test = as.matrix(X_test)

##Doing the PCA on the X_train (train set of mafactors)
mefactors_pc = princomp(X_train,cor=T)

##Extracting PCs up to 90% of total variance and converting X to PC value
n = length(mefactors_pc$sdev[cumsum((mefactors_pc$sdev)^2)/ncol(X_train)<0.99])+1
pc = mefactors_pc$loadings[,1:n]
PCValue = X_train%*%pc

# Perform logistic regression
lreg = glm(y_train~PCValue, family = "binomial")
summary(lreg)

# Removing PCValueComp.12 since it has the largest value p-value
PCValue2 = PCValue[,-12]
lreg2 = glm(y_train~PCValue2, family = "binomial")
summary(lreg2)

# Removing PCValueComp.2 since it has the largest value p-value
PCValue3 = PCValue2[,-2]
lreg3 = glm(y_train~PCValue3, family = "binomial")
summary(lreg3)

# Removing PCValueComp.3 since it has the largest value p-value
PCValue4 = PCValue3[,-2]
lreg4 = glm(y_train~PCValue4, family = "binomial")
summary(lreg4)

# Removing PCValueComp.5 since it has the largest value p-value
PCValue5 = PCValue4[,-3]
lreg5 = glm(y_train~PCValue5, family = "binomial")
summary(lreg5)

# Removing PCValueComp.6 since it has the largest value p-value
PCValue6 = PCValue5[,-3]
lreg6 = glm(y_train~PCValue6, family = "binomial")
summary(lreg6)

# Removing PCValueComp.6 since it has the largest value p-value
PCValue7 = PCValue6[,-6]
lreg7 = glm(y_train~PCValue7, family = "binomial")
summary(lreg7)

# Removing PCValueComp.6 since it has the largest value p-value
PCValue8 = PCValue7[,-3]
lreg8 = glm(y_train~PCValue8, family = "binomial")
summary(lreg8)

# Highest Accuracy
pr = (lreg3$fitted.values > 0.5)
tbl = table(pr, y_train)
accuracy = (tbl[1, ][1] + tbl[2, ][2]) / 104

