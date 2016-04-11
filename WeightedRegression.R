setwd("/Users/Darshan/Documents/Online_News_Popularity")
source("DataPreprocess.R")

library(DAAG)
library(car)
library(leaps)
library(plyr)

news_train <- read.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Train.csv", header = TRUE)
news_train <- data_cleaning(news_train)
news_train <- correlation_cleaning(news_train)
return_obj <- target_transformation(news_train)
news_train <- return_obj$news
lamda <- return_obj$lambda
obj <- normalization(news_train)
news_train <- obj$news_train
news_train <- cat_encoding(news_train)

train_url <- news_train$url
news_train$url <- NULL

model <- lm(shares ~ i_kw_max_avg_avg + 
              num_hrefs * data_channel_is_socmed +
              self_reference_avg_sharess * weekday_is_sunday +
              kw_min_avg * is_weekend +
              num_keywords * data_channel_is_socmed * is_weekend +
              global_subjectivity * data_channel_is_socmed +
              num_imgs * is_weekend * data_channel_is_socmed +
              kw_min_max * data_channel_is_socmed +
              kw_min_max * data_channel_is_lifestyle +
              num_self_hrefs * is_weekend +
              abs_title_sentiment_polarity +
              average_token_length * data_channel_is_entertainment +
              abs_title_subjectivity +
              min_positive_polarity * data_channel_is_entertainment +
              i_kw_max_avg_min +
              n_tokens_title * data_channel_is_world +
              kw_max_max * weekday_is_saturday +
              title_sentiment_polarity +
              min_negative_polarity * data_channel_is_bus +
              title_subjectivity +
              kw_avg_max * data_channel_is_socmed * is_weekend +
              global_rate_negative_words +
              global_sentiment_polarity +
              self_reference_min_shares +
              n_unique_tokens +
              n_tokens_content * data_channel_is_bus +
              rate_positive_words +
              global_rate_positive_words, data = news_train)

ncvTest(model)

w <- predict(lm(abs(model$res) ~ news_train$i_kw_max_avg_avg + 
                  news_train$num_hrefs * news_train$data_channel_is_socmed +
                  news_train$self_reference_avg_sharess * news_train$weekday_is_sunday +
                  news_train$kw_min_avg * news_train$is_weekend +
                  news_train$num_keywords * news_train$data_channel_is_socmed * news_train$is_weekend +
                  news_train$global_subjectivity * news_train$data_channel_is_socmed +
                  news_train$num_imgs * news_train$is_weekend * news_train$data_channel_is_socmed +
                  news_train$kw_min_max * news_train$data_channel_is_socmed +
                  news_train$kw_min_max * news_train$data_channel_is_lifestyle +
                  news_train$num_self_hrefs * news_train$is_weekend +
                  news_train$abs_title_sentiment_polarity +
                  news_train$average_token_length * news_train$data_channel_is_entertainment +
                  news_train$abs_title_subjectivity +
                  news_train$min_positive_polarity * news_train$data_channel_is_entertainment +
                  news_train$i_kw_max_avg_min +
                  news_train$n_tokens_title * news_train$data_channel_is_world +
                  news_train$kw_max_max * news_train$weekday_is_saturday +
                  news_train$title_sentiment_polarity +
                  news_train$min_negative_polarity * news_train$data_channel_is_bus +
                  news_train$title_subjectivity +
                  news_train$kw_avg_max * news_train$data_channel_is_socmed * news_train$is_weekend +
                  news_train$global_rate_negative_words +
                  news_train$global_sentiment_polarity +
                  news_train$self_reference_min_shares +
                  news_train$n_unique_tokens +
                  news_train$n_tokens_content * news_train$data_channel_is_bus +
                  news_train$rate_positive_words +
                  news_train$global_rate_positive_words))


model.weighted <- lm(shares ~ i_kw_max_avg_avg + 
                       num_hrefs * data_channel_is_socmed +
                       self_reference_avg_sharess * weekday_is_sunday +
                       kw_min_avg * is_weekend +
                       num_keywords * data_channel_is_socmed * is_weekend +
                       global_subjectivity * data_channel_is_socmed +
                       num_imgs * is_weekend * data_channel_is_socmed +
                       kw_min_max * data_channel_is_socmed +
                       kw_min_max * data_channel_is_lifestyle +
                       num_self_hrefs * is_weekend +
                       abs_title_sentiment_polarity +
                       average_token_length * data_channel_is_entertainment +
                       abs_title_subjectivity +
                       min_positive_polarity * data_channel_is_entertainment +
                       i_kw_max_avg_min +
                       n_tokens_title * data_channel_is_world +
                       kw_max_max  +
                       title_sentiment_polarity +
                       min_negative_polarity * data_channel_is_bus +
                       title_subjectivity +
                       kw_avg_max * data_channel_is_socmed * is_weekend +
                       global_rate_negative_words +
                       global_sentiment_polarity +
                       self_reference_min_shares +
                       n_unique_tokens +
                       n_tokens_content * data_channel_is_bus +
                       rate_positive_words +
                       global_rate_positive_words, data = news_train, weights=1/(w^2))

summary(model.weighted)

