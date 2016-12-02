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

# Can not apply transformation on weighted regression

return_obj <- target_transformation(news)
news <- return_obj$news
lamda <- return_obj$lambda

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

news <- subset(news, select = setdiff(names(news),categorical_var))

#news <- cook_outliers_removal(news)

# Final Model Results on K-fold
K <- 10
folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)

models <- list()
rmses <- c()
actual_scale_rmses <- c()
R2s <- c()

for (i in 1:K) {
  
  news_train <- news[folds[[i]],]
  news_val <- news[-folds[[i]],]
  
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
                title_sentiment_polarity + 
                i_min_avg_negative_pol, data=news_train)
  
  
  pred <- predict(model, news_val)
  
  mse <- sum((pred - news_val$shares)**2) / nrow(news_val)
  rmses <- append(rmses, sqrt(mse))
  
  pred <- target_inverse(pred, lamda)
  shares_val <- target_inverse(news_val$shares, lamda)
  actual_scale_mse <- sum((pred - shares_val)**2) / nrow(news_val)
  actual_scale_rmses <- append(actual_scale_rmses, actual_scale_mse)
  
  R2s <- append(R2s, summary(model)$adj.r.squared)
  
  models[[i]] <- model
  
}
rmses
actual_scale_rmses
R2s
mean(rmses)
sd(rmses)
mean(actual_scale_rmses)
sd(actual_scale_rmses)
mean(R2s)

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
     title_sentiment_polarity + 
     i_min_avg_negative_pol, data=news)
qqnorm(model$residuals) + qqline(model$residuals)