setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")

library(caret)
source("DataPreprocess.R")

set.seed(464)

setwd("/home/gbakie/neu/stat-sp16/project/data")

news <- read.csv("Train.csv", header = TRUE)

news <- data_cleaning(news)

news <- correlation_cleaning(news)
obj <- target_transformation(news)
t_lambda <- obj$lambda
news <- obj$news

obj <- normalization(news)
news <- obj$news
news <- cat_encoding(news)

url <- news$url
news$url <- NULL

news <- cook_outliers_removal(news)

K = 10
folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)

rmses <- c()
for (i in 1:K) {
  news_train <- news[folds[[i]],]
  news_val <- news[-folds[[i]],]
  
  pred <- mean(news_train$shares)

  mse <- sum((pred - news_val$shares)**2) / nrow(news_val)
  rmses <- append(rmses, sqrt(mse))
}
mean(rmses)
sd(rmses)