setwd("/Users/Darshan/Documents/Online_News_Popularity")
source("DataPreprocess.R")

library(DAAG)
library(car)
library(leaps)

news_train <- read.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Train.csv", header = TRUE)
news_train <- data_cleaning(news_train)
news_train <- correlation_cleaning(news_train)
news_train <- target_transformation(news_train)
obj <- normalization(news_train)
news_train <- obj$news_train
news_train <- cat_encoding(news_train)


train_url <- news_train$url
news_train$url <- NULL
news_train$cat_dow <- NULL
categorical_var <- c("data_channel_is_lifestyle", 
                     "data_channel_is_entertainment", "data_channel_is_bus", 
                     "data_channel_is_world", "data_channel_is_socmed", 
                     "data_channel_is_tech", "weekday_is_monday", "weekday_is_tuesday", 
                     "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", 
                     "weekday_is_saturday", "weekday_is_sunday")

news_train <- subset(news_train, select = setdiff(names(news_train),categorical_var))

#leaps <- regsubsets(shares_transformed ~ 1, data=news_train)

#plot(leaps, scale="adjr2")

null=lm(shares~1, data=news_train)
full=lm(shares~., data=news_train)
#model <- step(null, scope=list(lower=null, upper=full), direction="forward")
model <- step(null, scope=list(upper=full), direction="both", data=news_train)

#shares.glm <- glm(shares_transformed ~ ., data = news_train)
#stepwise(shares.glm, direction='forward')
#fit <- lm(shares_transformed ~ num_hrefs + num_videos + num_imgs + num_self_hrefs, data=news_train)
#cv.lm(df=news_train, fit, m=10)