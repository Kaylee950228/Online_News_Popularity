suppressMessages(library(dplyr))
library(ggplot2)
library(reshape2)

setwd("/home/gbakie/neu/stat-sp16/project")
news_train <- read.csv("data/OnlineNewsPopularity.csv", header = TRUE)

train_url <- news_train$url
train_timedelta <- news_train$timedelta

news_train$timedelta <- NULL
news_train$url <- NULL

dow_cols = c("weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday",
              "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday",
              "weekday_is_sunday")

news_train$cat_dow <- 0

for (dow in dow_cols) {
    dow_idx = which(news_train[,dow] == 1)
    #print(dow_idx)
    news_train[dow_idx,"cat_dow"] <- which(dow_cols==dow)
}

news_train$cat_dow <- as.factor(news_train$cat_dow)
