setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")

library(caret)
library(glmnet)
source("DataPreprocess.R")

set.seed(464)

#setwd("/Users/Darshan/Documents/Online_News_Popularity")
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

K = 5
alphas = seq(0,1,0.1)
lambdas = c(0.01,0.05,0.1,0.2,0.3)

folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)

for (alpha in alphas) {
  for (lambda in lambdas) {
    mses <- c()
    for (i in 1:K) {
      news_train <- news[folds[[i]],]
      news_val <- news[-folds[[i]],]
      
      X_train <- data.matrix(subset(news_train,select=-shares))
      y_train <- data.matrix(news_train$shares)
      X_val <- data.matrix(subset(news_val,select=-shares))
      y_val <- data.matrix(news_val$shares)
      
      model <- glmnet(X_train, y_train, family="gaussian", alpha=alpha, standardize=FALSE, lambda=lambda)
      
      #png(file="mygraphic.png",width=1600,height=1200,res=100)
      #plot(model, label=TRUE, asp=4.)
      #dev.off()
      #break
      
      pred <- predict(model, newx=X_val, s=lambda)
      pred <- target_inverse(pred, t_lambda)
      
      shares_val <- target_inverse(y_val, t_lambda)
      mse = sum((pred - shares_val)**2) / nrow(news_val)
      mses <- append(mses,mse)
    }
    mmse= mean(mses)
    smse= sd(mses)
    cat(sprintf("alpha = %f, lambda = %f, avg mse = %f, sd mse = %f\n", alpha, lambda, mmse,smse))
  }
}

# Best model: alpha = 0.100000, lambda = 0.050000, avg mse = 121579605.966527, sd mse = 90009954.966840