#setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")
setwd("/Users/Darshan/Documents/Online_News_Popularity")

library(caret)
library(ggplot2)
source("DataPreprocess.R")

#setwd("/home/gbakie/neu/stat-sp16/project/data")

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

categorical_var <- c("data_channel_is_lifestyle", 
                     "data_channel_is_entertainment", "data_channel_is_bus", 
                     "data_channel_is_world", "data_channel_is_socmed", 
                     "data_channel_is_tech", "weekday_is_monday", "weekday_is_tuesday", 
                     "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", 
                     "weekday_is_saturday", "weekday_is_sunday")

news_train <- subset(news_train, select = setdiff(names(news_train),categorical_var))

#news_train <- cook_outliers_removal(news_train)

set.seed(464)

B = 300
bootstrap <- function(formula, data) {
  n_rows <- nrow(data)
  
  models <- vector(mode="list", length=B)
  for (i in 1:B) {
    boot_idx <- sample(n_rows, n_rows, replace = TRUE)
    boot_data <- data[boot_idx, ]
    
    m <- lm(formula, data=boot_data)
    
    models[[i]] <- m
  }
  
  return(models)
}

# stepwise selection (with outliers)
predictors <- c("data_channel", "cat_dow", "i_kw_max_avg_avg",
                "self_reference_avg_sharess", "i_kw_avg_max_max",
                "num_hrefs", "global_subjectivity", "LDA_00", 
                "LDA_01", "LDA_02", "num_self_hrefs",
                "i_n_unique_tokens_content", "i_title_sub_sent_polarity",
                "abs_title_subjectivity", "n_tokens_title", "min_positive_polarity",
                "num_imgs", "average_token_length", "title_sentiment_polarity",
                "i_min_avg_negative_pol")

#stepwise selection (without outliers)
# predictors <- c("num_hrefs", "num_self_hrefs", "num_imgs",
#                 "self_reference_avg_sharess", "LDA_00", "LDA_02", "global_subjectivity",
#                 "global_rate_positive_words", "global_rate_negative_words", "min_positive_polarity",
#                 "max_negative_polarity", "title_sentiment_polarity", "abs_title_subjectivity",
#                 "i_n_unique_tokens_content", "i_rate_pos_gsent_polarity", "i_kw_max_avg_avg", 
#                 "i_kw_avg_max_max", "cat_dow", "data_channel", "i_title_sub_sent_polarity")

formula <- as.formula(paste("shares~", paste(predictors,collapse="+")))

models <- bootstrap(formula ,news_train)
pred <- matrix(nrow = nrow(news_test), ncol=B)

for (i in 1:length(models)) {
  m <- models[[i]]
  pred[,i] <- target_inverse(predict(m, subset(news_test,select=-shares)),
                             lambda)
}

#pred <- target_inverse(rowMeans(pred), lambda)

sse <- sum((rowMeans(pred) - news_test$shares)**2)
rmse <- sqrt(sse / nrow(news_test))
