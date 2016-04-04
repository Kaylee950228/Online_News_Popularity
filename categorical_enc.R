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

data_channel_cols = c("data_channel_is_lifestyle", "data_channel_is_entertainment", 
                      "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech",
                      "data_channel_is_world")


news_train$cat_dow <- 0
news_train$data_channel <- 0


for (dow in dow_cols) {
    dow_idx = which(news_train[,dow] == 1)
    #print(dow_idx)
    news_train[dow_idx,"cat_dow"] <- which(dow_cols==dow)
}

for (dc in data_channel_cols) {
  dc_idx = which(news_train[,dc] == 1)
  #print(dow_idx)
  news_train[dc_idx,"data_channel"] <- which(data_channel_cols==dc)
}


news_train$cat_dow <- as.factor(news_train$cat_dow)
news_train$data_channel <- as.factor(news_train$data_channel)

