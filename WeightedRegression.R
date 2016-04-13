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

news_with_cat <- subset(news, select = categorical_var)

# Should be commented for the model with interaction terms
news <- subset(news, select = setdiff(names(news),categorical_var))

# Should be commented for the full dataset
#news <- cook_outliers_removal(news)

# Weight Regression Code started

ncvTest(lm(shares ~ .,data=news))
#m.unweighted <- lm(shares ~ ., data=news)
#news$res <- abs(m.unweighted$residuals)
#news$predict<-predict(m.unweighted, data=news)
#ggplot(data = news, aes(x=predict,y=res)) + geom_point() + stat_smooth() 
#+ xlab("Shares Prediction") + ylab("abs(residual)")
#news$res <- NULL
#news$predict <- NULL
#w <- predict(lm(abs(m.unweighted$res) ~ predict(m.unweighted, data=news)), data=news)
#w <- (w - min(w))/(max(w) - min(w))
## Code Ends
news$shares <- news$shares * w
K <- 10

folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)

models <- list()
rmses <- c()
R2s <- c()
for (i in 1:K) {
  
  news_train <- news[folds[[i]],]
  news_val <- news[-folds[[i]],]
  w <- w[folds[[i]]]
  
  m.unweighted <- lm(shares ~ ., data=news_train)
  w <- predict(lm(abs(m.unweighted$res) ~ predict(m.unweighted, data=news_train)), data=news_train)
  #w <- (w - min(w))/(max(w) - min(w))
  news_train$shares <- news_train$shares * w
  
  null=lm(shares~1, data=news_train)
  full=lm(shares~., data=news_train)
  
  #model <- lm(formula = shares ~ ., data = news_train, weights = 1/(w^2))
  model <- step(null, scope=list(lower=null, upper=full), direction="forward", trace=0)
  
  pred <- predict(model, news_val)
  
  #pred <- target_inverse(pred, lamda)
  #shares_val <- target_inverse(news_val$shares, lamda)
  #mse <- sum((pred - shares_val)**2) / nrow(news_val)
  
  mse <- sum((pred - news_val$shares)**2) / nrow(news_val)
  rmses <- append(rmses, sqrt(mse))
  
  R2s <- append(R2s, summary(model)$adj.r.squared)
  
  models[[i]] <- model
  
}

rmses
R2s
mean(rmses)
mean(R2s)

# Below code 

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

model_variables
rmses
R2s
mean(rmses)
mean(R2s)