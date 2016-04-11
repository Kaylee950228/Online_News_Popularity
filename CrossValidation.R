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

news <- subset(news, select = setdiff(names(news),categorical_var))

K <- 10

folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)

models <- list()
mses <- c()
R2s <- c()

for (i in 1:K) {
  
  news_train <- news[folds[[i]],]
  news_val <- news[-folds[[i]],]
  
  null=lm(shares~1, data=news_train)
  full=lm(shares~., data=news_train)
  
  model <- step(null, scope=list(lower=null, upper=full), direction="both", trace=0)
  #model <- step(full, direction="backward", trace=0)
  
  pred <- predict(model, news_val)
  
  mse <- sum((pred - news_val$shares)**2) / nrow(news_val)
  
  mses <- append(mses, sqrt(mse))
  R2s <- append(R2s, summary(model)$adj.r.squared)
  
  models[[i]] <- model
  
}

unique_coef <- c()

for(i in 1:length(models)){
  model_coef <- names(models[[i]]$coefficients)
  unique_coef <- unique(c(model_coef, unique_coef))
}

model_variables <- data.frame(matrix(NA,nrow=length(unique_coef),ncol=length(models)+1))
model_variables$X1 <- unique_coef

for(i in 1:length(models)){
  
  model_coef <- names(models[[i]]$coefficients)
  tf_coef <- unique_coef %in% model_coef
  var <- paste("X", toString(i+1), sep = "")
  model_variables[var] <- tf_coef
  
}

model_variables
mses
R2s

summary(lm(shares ~ data_channel + i_kw_max_avg_avg + cat_dow + num_hrefs + 
             kw_max_max + self_reference_avg_sharess + LDA_02 + 
             global_subjectivity + LDA_00 + n_unique_tokens + num_self_hrefs + 
             abs_title_sentiment_polarity + n_tokens_title + abs_title_subjectivity
           + num_imgs + min_positive_polarity + average_token_length + 
             title_sentiment_polarity + num_keywords + LDA_01, data=news))

summary(lm(shares ~ data_channel + i_kw_max_avg_avg + cat_dow+num_hrefs + 
             kw_max_max + self_reference_avg_sharess + LDA_02 + 
             global_subjectivity + LDA_00 + n_unique_tokens + num_self_hrefs + 
             abs_title_sentiment_polarity + n_tokens_title + abs_title_subjectivity
           + num_imgs + min_positive_polarity + average_token_length + 
             title_sentiment_polarity + num_keywords + LDA_01 + title_subjectivity + 
             min_negative_polarity  + avg_negative_polarity, data=news))



model.summaries <- list()
min.bic.index <- c()
max.r2.index <- c()

select.var.index <- c()

for (i in 1:K) {
  
  news_train <- news[folds[[i]],]
  news_val <- news[-folds[[i]],]
  
  model <- regsubsets(shares ~ data_channel + i_kw_max_avg_avg + cat_dow+num_hrefs + 
                        kw_max_max + self_reference_avg_sharess + LDA_02 + 
                        global_subjectivity + LDA_00 + n_unique_tokens + num_self_hrefs + 
                        abs_title_sentiment_polarity + n_tokens_title + abs_title_subjectivity + 
                        num_imgs + min_positive_polarity + average_token_length + 
                        title_sentiment_polarity + num_keywords + LDA_01 + n_tokens_content + 
                        title_subjectivity + avg_negative_polarity + min_negative_polarity + 
                        i_kw_max_avg_min + global_sentiment_polarity + max_positive_polarity + 
                        max_negative_polarity + rate_positive_words + global_rate_negative_words + 
                        kw_min_max + kw_min_avg + rate_positive_words, 
                      nbest = 1, nvmax = 43,data=news_train, force.in = c(1:30), 
                      method = "exhaustive", really.big = TRUE)
  
  summary.model <- summary(model)
  min.bic.index <- append(which.min(summary.model$bic), min.bic.index)
  max.r2.index <- append(which.max(summary.model$adjr2) ,max.r2.index)
  model.summaries[[i]] <- summary.model
  
  select.var.index <- append(which.max(which(summary.model$which[2,])), select.var.index)
}

# Found four additional variable 

# title_subjectivity min_negative_polarity n_tokens_content avg_negative_polarity

# Exhaustive search on the variable which were not selected by all the fold in forward, backward
# and both directional step wise variable selection method
# columns <- c("data_channel", "i_kw_max_avg_avg", "cat_dow", "num_hrefs", "kw_max_max", 
#              "self_reference_avg_sharess", "LDA_02", "global_subjectivity", "LDA_00", 
#              "n_unique_tokens", "num_self_hrefs", "abs_title_sentiment_polarity", 
#              "n_tokens_title", "abs_title_subjectivity", "num_imgs", "min_positive_polarity", 
#              "average_token_length", "title_sentiment_polarity", "num_keywords","LDA_01")
# 
# seach_columns <- c("n_tokens_content","title_subjectivity","avg_negative_polarity",
#                    "min_negative_polarity","i_kw_max_avg_min","global_sentiment_polarity",
#                    "max_positive_polarity","max_negative_polarity","rate_positive_words",
#                    "global_rate_negative_words","kw_min_max","kw_min_avg","rate_positive_words")
# 
# ignore_columns <- setdiff(setdiff(setdiff(names(news), columns), seach_columns), "shares")
# 
# s.force.in <- c(17, 1, 16, 5, 14, 6, 15, 7, 10, 2, 4, 8,9,12,11,13,3,18,19,20,34,35,36,37,38,39,40,41,42)
# s.force.out <- c(43, 44, 45, 46, 47, 48, 49)

model <- regsubsets(shares ~ data_channel + i_kw_max_avg_avg + cat_dow+num_hrefs + 
                      kw_max_max + self_reference_avg_sharess + LDA_02 + 
                      global_subjectivity + LDA_00 + n_unique_tokens + num_self_hrefs + 
                      abs_title_sentiment_polarity + n_tokens_title + abs_title_subjectivity
                     + num_imgs + min_positive_polarity + average_token_length + 
                      title_sentiment_polarity + num_keywords + LDA_01 + n_tokens_content + 
                      title_subjectivity + avg_negative_polarity + min_negative_polarity + 
                      i_kw_max_avg_min + global_sentiment_polarity + max_positive_polarity + 
                      max_negative_polarity + rate_positive_words + global_rate_negative_words + 
                      kw_min_max + kw_min_avg + rate_positive_words, 
                    nbest = 1, nvmax = 43,data=news, force.in = c(1:30), 
                    method = "exhaustive", really.big = TRUE)

# for(column in columns){
#   s.force.in <- append(which(colnames(news)==column),  s.force.in)
# }
# 
# for(i.column in ignore_columns){
#   s.force.out <- append(which(colnames(news)==i.column), s.force.out)
# }
# regsubsets(shares~.,nbest = 1, nvmax = 4, force.in=s.force.in, force.out = s.force.out, data=news)