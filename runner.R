install.packages("corrplot")
library(tseries)
library(zoo)
library(rio)
library(tibbletime)
library(dplyr)
library(imputeTS)
library(corrplot)

cur_dir <- getwd()
load(paste(cur_dir, "/scrapped_data.RData", sep = ""))

set.seed(17)
train_size = ceiling(0.8 * nrow(stock_vs_mefactors))

# Remove the data column from dataframe and split into train test dataframes
stock_vs_mefactors = stock_vs_mefactors[,-1]
data_train = stock_vs_mefactors[1 : train_size,]
data_test = stock_vs_mefactors[seq(train_size + 1, nrow(stock_vs_mefactors)),]

# Extract the dependent variable and remove it from original dataframe - in our situation s&p500 index
sp500 = stock_vs_mefactors$`S&P Monthly`
mefactors = stock_vs_mefactors[,-1]

mefactors_cor = cor(mefactors, method = "pearson")
colnames(mefactors_cor) = c(seq(1:length(mefactors)))
rownames(mefactors_cor) = c(seq(1:length(mefactors)))

# The correlation plot shows there is some correlation between factors that we can exploit
corrplot(mefactors_cor, type = "upper", order = "hclust", tl.col = "black", tl.srt = 90)

# Normalisation of independent and dependent variables
mefactors_pc = prcomp(mefactors, center = TRUE, scale. = TRUE)
summary(mefactors_pc)

for (i in 1:ncol(mefactors)) {
  mefactors[,i] = (mefactors[,i] - mean(mefactors[,i])) / sd(mefactors[,i])
}
sp500 = (sp500 - mean(sp500)) / sd(sp500)

mefactors_pc = prcomp(mefactors, center = TRUE, scale. = TRUE)


# Orthogonality check
res = cor(mefactors_pc$scores, method = "pearson")
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 90)


# Clear console and environment
rm(list=ls())
cat("\014")  # ctrl+L
