suppressMessages(library(dplyr))
library(ggplot2)
library(reshape2)

news_train <- read.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Cleaned_Cor_Train.csv", header = TRUE)

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

data_channel_cols = c("data_channel_is_lifestyle", "data_channel_is_entertainment",
                       "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech",
                       "data_channel_is_world")

news_train$data_channel <- 0

for (channel in data_channel_cols) {
  channel_idx <- which(news_train[,channel] == 1)
  news_train[channel_idx,"data_channel"] <- which(data_channel_cols==channel)
}

news_train$data_channel <- as.factor(news_train$data_channel)

write.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Cleaned_Cor_Train.csv", row.names = FALSE, x = news_train)

#filter(news_train, data_channel_is_bus==0, data_channel_is_lifestyle==0, 
#       data_channel_is_entertainment==0, data_channel_is_socmed==0, 
#       data_channel_is_tech==0, data_channel_is_world==0)
