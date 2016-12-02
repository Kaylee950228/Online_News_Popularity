setwd("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/")
#setwd("/home/gbakie/neu/stat-sp16/project/data")

news <- read.csv("Train.csv", header = TRUE)

#setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")
setwd("/Users/Darshan/Documents/Online_News_Popularity")

library(caret)
library(glmnet)
source("DataPreprocess.R")

set.seed(464)

# run grid search with cross validation to select best values for lambda and alpha in elastic net
select_model <- function(news, t_lambda) {
  K = 10
  # alpha = 0 -> Ridge; alpha = 1 -> Lasso
  alphas = c(0,1)
  lambdas = c(1e-05, 1e-04, 1e-03, 1e-02, 0.1, 1.,10.)
  
  folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)
  # for each combination of parameters
  for (alpha in alphas) {
    for (lambda in lambdas) {
      rmses <- c()
      R2s <- c()
      
      # for each fold
      for (i in 1:K) {
        news_train <- news[folds[[i]],]
        news_val <- news[-folds[[i]],]
                
        X_train <- data.matrix(subset(news_train,select=-shares))
        y_train <- data.matrix(news_train$shares)
        X_val <- data.matrix(subset(news_val,select=-shares))
        y_val <- data.matrix(news_val$shares)
        
        model <- glmnet(X_train, y_train, family="gaussian", alpha=alpha, standardize=TRUE, 
                        lambda=lambda, nlambda=1)
        
      
        pred_train <- predict(model, newx=X_train, s=lambda)
        shares_train <- y_train
        
        # calculate R^2 in the fitted data
        ssto <- sum((shares_train - mean(shares_train))**2)
        sse <- sum((pred_train - shares_train)**2)
        R2 <- 1 - (sse/ssto)
        
        R2s <- append(R2s, R2)
        
        pred <- predict(model, newx=X_val, s=lambda)
        shares_val <- y_val
      
        sse <- sum((pred - shares_val)**2)
        rmse <- sqrt(sse / nrow(news_val))
        
        rmses <- append(rmses,rmse)
      }
      mrmse= mean(rmses)
      srmse= sd(rmses)
      mR2 = mean(R2s)
      cat(sprintf("alpha = %f, lambda = %f, avg rmse = %f, sd rmse = %f, avg R-2 = %f\n", 
                  alpha, lambda, mrmse,srmse,mR2))
    }
  }

  # Best LASSO model without outliers : 
  # alpha = 1.000000, lambda = 0.000010, avg rmse = 0.133560, sd rmse = 0.001593, avg R-2 = 0.172663  
  # Best LASSO model with outliers:
  # alpha = 1.000000, lambda = 0.000100, avg rmse = 0.164178, sd rmse = 0.005667, avg R-2 = 0.120495
  # Best RIDGE model without outliers:
  # alpha = 0.000000, lambda = 0.001000, avg rmse = 0.133558, sd rmse = 0.001585, avg R-2 = 0.172639
  # Best RIDGE model with outlier:
  # alpha = 0.000000, lambda = 0.001000, avg rmse = 0.164184, sd rmse = 0.005661, avg R-2 = 0.120518
}

#setwd("/Users/Darshan/Documents/Online_News_Popularity")
setwd("/home/gbakie/neu/stat-sp16/project/data")

news <- read.csv("Train.csv", header = TRUE)

news <- data_cleaning(news)
news <- correlation_cleaning(news)
obj <- target_transformation(news)
t_lambda <- obj$lambda
news <- obj$news

news <- cat_encoding(news)

url <- news$url
news$url <- NULL

#news <- cook_outliers_removal(news)

categorical_var <- c("data_channel_is_lifestyle", 
                     "data_channel_is_entertainment", "data_channel_is_bus", 
                     "data_channel_is_world", "data_channel_is_socmed", 
                     "data_channel_is_tech", "weekday_is_monday", "weekday_is_tuesday", 
                     "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", 
                     "weekday_is_saturday", "weekday_is_sunday")

news <- subset(news, select = setdiff(names(news),categorical_var))

select_model(news, t_lambda)
