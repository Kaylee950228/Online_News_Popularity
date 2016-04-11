setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")

library(caret)
library(glmnet)
source("DataPreprocess.R")

set.seed(464)

find_best_model <- function(news, t_lambda) {
  
  K = 10
  
  # alpha = 0 -> Ridge; alpha = 1 -> Lasso
  #alphas = seq(0,1,0.1)
  alphas = c(1)
  lambdas = c(1e-05, 1e-04, 1e-03, 1e-02, 0.1, 0)
  
  folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)
  
  for (alpha in alphas) {
    for (lambda in lambdas) {
      rmses <- c()
      R2s <- c()
      
      for (i in 1:K) {
        news_train <- news[folds[[i]],]
        news_val <- news[-folds[[i]],]
        #news_train <- outliers_removal(news_train)
                
        X_train <- data.matrix(subset(news_train,select=-shares))
        y_train <- data.matrix(news_train$shares)
        X_val <- data.matrix(subset(news_val,select=-shares))
        y_val <- data.matrix(news_val$shares)
        
        model <- glmnet(X_train, y_train, family="gaussian", alpha=alpha, standardize=FALSE, lambda=lambda)
        
        #png(file="mygraphic.png",width=1600,height=1200,res=100)
        #plot(model, label=TRUE, asp=4.)
        #dev.off()
        #break
        
        pred_train <- predict(model, newx=X_train, s="lambda.1se")
        #pred_train <- target_inverse(pred_train, t_lambda)
        #shares_train <- target_inverse(y_train, t_lambda)
        shares_train <- y_train
        
        ssto <- sum((shares_train - mean(shares_train))**2)
        sse <- sum((pred_train - shares_train)**2)
        R2 <- 1 - (sse/ssto)
        
        R2s <- append(R2s, R2)
        
        pred <- predict(model, newx=X_val, s=lambda)
        #pred <- target_inverse(pred, t_lambda)
        
        #shares_val <- target_inverse(y_val, t_lambda)
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
  
  # Best model: alpha = 0.100000, lambda = 0.050000, avg mse = 121579605.966527, sd mse = 90009954.966840
  # Best model R2: alpha = 0.000000, lambda = 0.010000, avg mse = 127964730.938037, sd mse = 91225556.448689, avg R-2 = 0.122139
}


#setwd("/Users/Darshan/Documents/Online_News_Popularity")
setwd("/home/gbakie/neu/stat-sp16/project/data")

news <- read.csv("Train.csv", header = TRUE)

#news <- outliers_removal(news)
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

categorical_var <- c("data_channel_is_lifestyle", 
                     "data_channel_is_entertainment", "data_channel_is_bus", 
                     "data_channel_is_world", "data_channel_is_socmed", 
                     "data_channel_is_tech", "weekday_is_monday", "weekday_is_tuesday", 
                     "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", 
                     "weekday_is_saturday", "weekday_is_sunday")

news <- subset(news, select = setdiff(names(news),categorical_var))

#find_best_model(news, t_lambda)

 
X_train <- data.matrix(subset(news,select=-shares))
y_train <- data.matrix(news$shares)

model <- glmnet(X_train, y_train, family="gaussian", alpha=1., standardize=FALSE)

pred <- predict(model, newx=X_train, s=0.05)
pred <- target_inverse(pred, t_lambda)

shares_val <- target_inverse(y_train, t_lambda)
res <- abs(pred - shares_val)
plot(pred, res, xlim=c(500,5000),ylim=c(0,50e+03))

news$residuals <- as.vector(res)
d <- ggplot(news, aes(cat_dow, residuals))
d + geom_point(alpha = 1/2) + stat_smooth(method=lm)