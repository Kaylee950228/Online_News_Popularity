setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")
#setwd("/Users/Darshan/Documents/Online_News_Popularity")

library(caret)
library(ggplot2)
source("DataPreprocess.R")

setwd("/home/gbakie/neu/stat-sp16/project/data")

# Load and preprocess train and test
news_train <- read.csv("Train.csv", header = TRUE)
news_test <- read.csv("Test.csv", header = TRUE)

news_train <- data_cleaning(news_train)
news_test <- data_cleaning(news_test)

news_train <- correlation_cleaning(news_train)
news_test <- correlation_cleaning(news_test)

return_obj <- target_transformation(news_train)
news_train <- return_obj$news
lambda <- return_obj$lambda

norm <- normalization(news_train)
sd_norm <- norm$sd_values
mean_norm <- norm$mean_values
news_train <- norm$news_train
news_test <- apply_normalization(news_test, mean_norm, sd_norm)

news_train <- cat_encoding(news_train)
news_test <- cat_encoding(news_test)

train_url <- news_train$url
news_train$url <- NULL
test_url <- news_test$url
news_test$url <- NULL

model <- lm(shares ~ data_channel +
              cat_dow +
              i_kw_max_avg_avg +
              self_reference_avg_sharess +
              i_kw_avg_max_max +
              num_hrefs +
              global_subjectivity +
              LDA_00 +
              LDA_01 +
              LDA_02 +
              num_self_hrefs +
              i_n_unique_tokens_content +
              i_title_sub_sent_polarity +
              abs_title_subjectivity +
              n_tokens_title +
              min_positive_polarity +
              num_imgs +
              average_token_length +
              title_sentiment_polarity, data=news_train)

target_news <- news_test$shares

pred <- predict(model, subset(news_test, select=-shares))
pred <- target_inverse(pred, lambda)

sse <- sum((pred - target_news)**2)
rmse <- sqrt(sse / nrow(news_test))

res <- (pred - target_news)
plot(pred,res)

