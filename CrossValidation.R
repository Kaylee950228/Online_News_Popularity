#setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")
setwd("/Users/Darshan/Documents/Online_News_Popularity")

library(caret)
source("DataPreprocess.R")

set.seed(464)

setwd("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/")
#setwd("/home/gbakie/neu/stat-sp16/project/data")

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

models <- list()
mses <- list()
for (i in 1:K) {
  news_train <- news[folds[[i]],]
  news_val <- news[-folds[[i]],]
  
  null=lm(shares~1, data=news_train)
  full=lm(shares~., data=news_train)
  
  model <- step(null, scope=list(lower=null, upper=full), direction="forward", trace=0)
  models[[i]] <- model

  pred <- predict(model, news_val)
  mse = sum((pred - news_val$shares)**2) / nrow(news_val)
  mses[[i]] <- mse
  
}

unique_coef <- c()

for(i in 1:length(models)){
  model_coef <- names(models[[i]]$coefficients)
  unique_coef <- unique(c(model_coef, unique_coef))
}

model_variables <- data.frame(matrix(NA,nrow=length(unique_coef),ncol=length(models)+1))
model_variables$X1 <- unique_coef

for(i in 1:length(models)){
  
  model_coef <- names(models[[i]]$coefficients)
  tf_coef <- unique_coef %in% model_coef
  var <- paste("X", toString(i+1), sep = "")
  model_variables[var] <- tf_coef
  
}