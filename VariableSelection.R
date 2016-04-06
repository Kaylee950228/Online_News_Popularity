setwd("/Users/Darshan/Documents/Online_News_Popularity")
source("DataPreprocess.R")

library(DAAG)
library(car)
library(leaps)
library(plyr)

news_train <- read.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Train.csv", header = TRUE)
news_train <- data_cleaning(news_train)
news_train <- correlation_cleaning(news_train)
news_train <- target_transformation(news_train)
obj <- normalization(news_train)
news_train <- obj$news_train
news_train <- cat_encoding(news_train)


train_url <- news_train$url
news_train$url <- NULL
#news_train$cat_dow <- NULL
categorical_var <- c("data_channel_is_lifestyle", 
                     "data_channel_is_entertainment", "data_channel_is_bus", 
                     "data_channel_is_world", "data_channel_is_socmed", 
                     "data_channel_is_tech", "weekday_is_monday", "weekday_is_tuesday", 
                     "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", 
                     "weekday_is_saturday", "weekday_is_sunday")

#news_train <- subset(news_train, select = setdiff(names(news_train),categorical_var))

#leaps <- regsubsets(shares_transformed ~ 1, data=news_train)

#plot(leaps, scale="adjr2")

null=lm(shares~1, data=news_train)
full=lm(shares~., data=news_train)

model <- step(null, scope=list(lower=null, upper=full), direction="forward")
#model <- step(null, scope=list(upper=full), direction="both", data=news_train)

#shares.glm <- glm(shares_transformed ~ ., data = news_train)
#stepwise(shares.glm, direction='forward')
#fit <- lm(shares_transformed ~ num_hrefs + num_videos + num_imgs + num_self_hrefs, data=news_train)
#cv.lm(df=news_train, fit, m=10)

prediction <- data.frame()
testsetCopy <- data.frame()

k = 5
news_train$id <- sample(1:k, nrow(news_train), replace = TRUE)
list <- 1:k

#Creating a progress bar to know the status of CV
progress.bar <- create_progress_bar("text")
progress.bar$init(k)

for (i in 1:k){
  # remove rows with id i from dataframe to create training set
  # select rows with id i to create test set
  
  trainingset <- subset(news_train, id %in% list[-i])
  testset <- subset(news_train, id %in% c(i))
  
  null=lm(shares~1, data=trainingset)
  full=lm(shares~., data=trainingset)
  
  mymodel <- step(null, scope=list(upper=full), direction="both", data=trainingset)
  
  temp <- as.data.frame(predict(mymodel, testset[,-1]))
  # append this iteration's predictions to the end of the prediction data frame
  prediction <- rbind(prediction, temp)
  
  # append this iteration's test set to the test set copy data frame
  # keep only the Sepal Length Column
  testsetCopy <- rbind(testsetCopy, as.data.frame(testset[,1]))
  
  progress.bar$step()
  
}

# add predictions and actual Sepal Length values
result <- cbind(prediction, testsetCopy[, 1])
names(result) <- c("Predicted", "Actual")
result$Difference <- abs(result$Actual - result$Predicted)

# As an example use Mean Absolute Error as Evalution 
summary(result$Difference)

summary(lm(shares ~ LDA_00 * data_channel_is_socmed + LDA_00 * data_channel_is_world + 
             LDA_01 * data_channel_is_world + LDA_02 * data_channel_is_tech , data=news_train))

summary(lm(shares ~ data_channel + cat_dow + LDA_00 * data_channel_is_socmed + LDA_00 * data_channel_is_world + 
             LDA_01 * data_channel_is_world + LDA_02 * data_channel_is_tech + i_kw_max_avg_avg + num_hrefs + 
             LDA_02 + self_reference_avg_sharess * weekday_is_sunday + kw_min_avg + num_keywords + 
             LDA_00 + global_subjectivity + num_imgs + kw_min_max + num_self_hrefs + 
             average_token_length + LDA_01 + abs_title_sentiment_polarity + 
             abs_title_subjectivity + min_positive_polarity + i_kw_max_avg_min + 
             n_tokens_title + kw_max_max + title_sentiment_polarity + 
             min_negative_polarity + title_subjectivity + kw_avg_max + 
             global_rate_negative_words + global_sentiment_polarity + 
             self_reference_min_shares + n_unique_tokens + n_tokens_content + 
             rate_positive_words + global_rate_positive_words, data=news_train))

summary(lm(shares ~ i_kw_max_avg_avg + 
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
             global_rate_positive_words, data = news_train))



i_kw_max_avg_avg + 
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
global_rate_positive_words

categorical_var <- c("data_channel_is_lifestyle", 
                     "data_channel_is_entertainment", "data_channel_is_bus", 
                     "data_channel_is_world", "data_channel_is_socmed", 
                     "data_channel_is_tech", "weekday_is_monday", "weekday_is_tuesday", 
                     "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday", 
                     "weekday_is_saturday", "weekday_is_sunday")
