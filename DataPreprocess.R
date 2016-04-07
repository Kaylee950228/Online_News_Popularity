data_cleaning <- function(news){
  
  news$timedelta <- NULL  
  news <- news[news$n_tokens_content != 0,]
  news$n_non_stop_words <- NULL
  news$kw_min_min <- NULL
  
  news$is_weekend <- as.factor(news$is_weekend)
  
  return(news)
  
}

correlation_cleaning <- function(news){
  
  news$rate_negative_words <- NULL
  news$n_non_stop_unique_tokens <- NULL
  
  news$i_kw_max_avg_min <- news$kw_max_min + news$kw_avg_min
  news$i_kw_max_avg_avg <- news$kw_max_avg + news$kw_avg_avg
  
  news$kw_max_min <- NULL
  news$kw_avg_min <- NULL
  news$kw_max_avg <- NULL
  news$kw_avg_avg <- NULL
  
  return(news)
  
}

target_transformation <- function(news){
  
  library(car)
  p<-powerTransform(news$shares)
  shares_transformed <-bcPower(news$shares,p$lambda)
  news$shares <- shares_transformed
  return(news)
  
}

normalization <- function(news_train){
  
  library(dplyr)
  
  # All Column names
  column_names <- names(news_train)
  
  # Column names which needs to be ignored due to categorical and target feature
  
  ignored_column_names <- c("url", "timedelta", "data_channel_is_lifestyle",
                            "data_channel_is_entertainment", "data_channel_is_bus",
                            "data_channel_is_world", "data_channel_is_socmed",
                            "data_channel_is_tech", "weekday_is_monday", "weekday_is_tuesday",
                            "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday",
                            "weekday_is_saturday", "weekday_is_sunday", "is_weekend", "shares")
  
  needed_columns <- setdiff(column_names,ignored_column_names)
  
  # Normalized Train Data
  news_train_norm <- news_train %>% mutate_each_(funs(scale),vars=needed_columns)
  
  # Saving standard deviation of the columns which are normalized
  sd_values <- Map(sd, news_train[needed_columns])
  
  # Saving eman of the columns which are normalized
  mean_values <- Map(mean, news_train[needed_columns])
  
  return(list("sd_values"=sd_values, "mean_values"=mean_values, "news_train"=news_train_norm))
  
}

cat_encoding <- function(news){
  
  dow_cols = c("weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday",
               "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday",
               "weekday_is_sunday")
  
  news$cat_dow <- 0
  
  for (dow in dow_cols) {
    dow_idx = which(news[,dow] == 1)
    #print(dow_idx)
    news[dow_idx,"cat_dow"] <- which(dow_cols==dow)
  }
  
  news$cat_dow <- as.factor(news$cat_dow)
  
  data_channel_cols = c("data_channel_is_lifestyle", "data_channel_is_entertainment",
                        "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech",
                        "data_channel_is_world")
  
  news$data_channel <- 0
  
  for (channel in data_channel_cols) {
    channel_idx <- which(news[,channel] == 1)
    news[channel_idx,"data_channel"] <- which(data_channel_cols==channel)
  }
  
  news$data_channel <- as.factor(news$data_channel)
  
  return(news)
  
}

#setwd("/Users/Darshan/Documents/Online_News_Popularity")
#setwd("/home/gbakie/neu/stat-sp16/project/data")

#news_train <- read.csv("Train.csv", header = TRUE)
#news_train <- data_cleaning(news_train)
#news_train <- correlation_cleaning(news_train)
#news_train <- target_transformation(news_train)
#obj <- normalization(news_train)
#news_train <- obj$news_train
#news_train <- cat_encoding(news_train)

