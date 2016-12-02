#setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")
setwd("/Users/Darshan/Documents/Online_News_Popularity")
library(leaps)
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

K <- 10

folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)

model.summaries <- list()
min.cp.index <- c()
max.r2.index <- c()
max.r2 <- c()
select.var.index <- c()

for (i in 1:K) {
  
  news_train <- news[folds[[i]],]
  news_val <- news[-folds[[i]],]
  
  model <- regsubsets(shares ~ num_hrefs +
                        num_self_hrefs +
                        num_imgs +
                        self_reference_avg_sharess +
                        LDA_00 +
                        LDA_02 +
                        global_subjectivity +
                        global_rate_positive_words +
                        global_rate_negative_words +
                        min_positive_polarity +
                        max_negative_polarity +
                        title_sentiment_polarity +
                        abs_title_subjectivity +
                        i_n_unique_tokens_content +
                        i_rate_pos_glob_sent_polarity +
                        i_kw_max_avg_avg +
                        i_kw_avg_max_max +
                        cat_dow +
                        data_channel + 
                        num_keywords +
                        LDA_04 +
                        num_videos +
                        average_token_length +
                        avg_positive_polarity +
                        i_min_avg_negative_pol +
                        LDA_01 +
                        LDA_03 +
                        i_title_subjectivity_sentiment_polarity, nbest = 1, nvmax = 38, 
                      data=news_train, force.in = c(1:29), 
                      method = "exhaustive", really.big = TRUE)
  
  summary.model <- summary(model)
  
  min.cp.index <- append(which.min(summary.model$cp), min.bic.index)
  max.r2.index <- append(which.max(summary.model$adjr2) ,max.r2.index)
  max.r2<- append(max(summary.model$adjr2),max.r2)
  
  model.summaries[[i]] <- summary.model
  
  select.var.index <- append(which.max(which(summary.model$which[1,])), select.var.index)
}

# Found four additional variable 
i_rate_pos_glob_sent_polarity
num_keywords
num_videos
i_min_avg_negative_pol
# i_rate_pos_glob_sent_polarity global_rate_negative_words avg_positive_polarity i_rate_pos_glob_sent_polarity