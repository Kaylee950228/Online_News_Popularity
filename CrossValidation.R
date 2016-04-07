setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")

library(caret)
source("DataPreprocess.R")

set.seed(464)

#setwd("/Users/Darshan/Documents/Online_News_Popularity")
setwd("/home/gbakie/neu/stat-sp16/project/data")

news <- read.csv("Train.csv", header = TRUE)

news <- data_cleaning(news)
news <- correlation_cleaning(news)
news <- target_transformation(news)
obj <- normalization(news)
news <- obj$news
news <- cat_encoding(news)

url <- news$url
news$url <- NULL

K = 5

folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)

for (i in 1:K) {
  news_train <- news[folds[[i]],]
  news_val <- news[-folds[[i]],]
  
  null=lm(shares~1, data=news_train)
  full=lm(shares~., data=news_train)
  
  model <- step(null, scope=list(lower=null, upper=full), direction="forward", trace=0)
  
  print(model$call)
  pred <- predict(model, news_val)
  mse = sum((pred - news_val$shares)**2) / nrow(news_val)
  print(mse)
  
}