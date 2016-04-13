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

news_with_cat <- subset(news, select = categorical_var)

news <- subset(news, select = setdiff(names(news),categorical_var))

news <- cook_outliers_removal(news)

K <- 10

folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)

models <- list()
rmses <- c()
R2s <- c()

## Code Ends

for (i in 1:K) {
  
  news_train <- news[folds[[i]],]
  news_val <- news[-folds[[i]],]
  
  null=lm(shares~1, data=news_train)
  full=lm(shares~., data=news_train)
  
  model <- step(null, scope=list(lower=null, upper=full), direction="both", trace=0)
  #model <- step(full, direction="backward", trace=0)
  
  pred <- predict(model, news_val)

  #pred <- target_inverse(pred, lamda)
  #shares_val <- target_inverse(news_val$shares, lamda)
  #mse <- sum((pred - shares_val)**2) / nrow(news_val)
  
  mse <- sum((pred - news_val$shares)**2) / nrow(news_val)
  rmses <- append(rmses, sqrt(mse))
  
  R2s <- append(R2s, summary(model)$adj.r.squared)
  
  models[[i]] <- model
  
}

# Below code 

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
rmses
R2s
mean(rmses)
mean(R2s)

# Best stepwise model with outliers
summary(lm(shares ~ data_channel +
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
             i_min_avg_negative_pol, data=news))

# Best stepwise model with outliers and interaction
summary(lm(shares ~ data_channel +
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
             n_tokens_title:weekday_is_tuesday +
             average_token_length:data_channel_is_entertainment + 
             num_hrefs:data_channel_is_socmed + 
             num_imgs:is_weekend:data_channel_is_socmed + 
             global_subjectivity:data_channel_is_socmed + 
             i_n_unique_tokens_content:data_channel_is_bus + 
             min_positive_polarity:data_channel_is_entertainment, data=news))

# Selected by subset selection by cp criteria (With outlier)
i_min_avg_negative_pol

# Selected by subset selection by cp criteria (Without outlier)
i_title_subjectivity_sentiment_polarity

# Interaction terms for dataset with outlier
n_tokens_title:weekday_is_tuesday
average_token_length:data_channel_is_entertainment
# Interaction terms common in both the dataset
num_hrefs:data_channel_is_socmed +
num_imgs:is_weekend:data_channel_is_socmed +
global_subjectivity:data_channel_is_socmed +
i_n_unique_tokens_content:data_channel_is_bus +
min_positive_polarity:data_channel_is_entertainment +
# Interaction terms for dataset without outlier
num_self_hrefs:is_weekend +
max_negative_polarity:is_weekend


# Best Model without Outlier 
summary(lm(shares ~ num_hrefs +
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
             i_rate_pos_gsent_polarity +
             i_kw_max_avg_avg +
             i_kw_avg_max_max +
             cat_dow +
             data_channel + 
             i_title_sub_sent_polarity, data=news))

# Best Model without Outlier and interaction
summary(lm(shares ~ num_hrefs +
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
             i_rate_pos_gsent_polarity +
             i_kw_max_avg_avg +
             i_kw_avg_max_max +
             cat_dow +
             data_channel + 
             i_title_sub_sent_polarity + 
             num_hrefs:data_channel_is_socmed +
             num_imgs:is_weekend:data_channel_is_socmed +
             global_subjectivity:data_channel_is_socmed +
             i_n_unique_tokens_content:data_channel_is_bus +
             min_positive_polarity:data_channel_is_entertainment + 
             num_self_hrefs:is_weekend +
             max_negative_polarity:is_weekend, data=news))

max_negative_polarity:is_weekend
# Exhuastive Subset selection on the remaning set of variables

# all the interaction
num_hrefs:data_channel_is_socmed
num_imgs:is_weekend:data_channel_is_socmed
num_keywords:data_channel_is_socmed
global_subjectivity:data_channel_is_socmed
avg_positive_polarity:data_channel_is_socmed
i_n_unique_tokens_content:data_channel_is_bus
average_token_length:data_channel_is_entertainment
min_positive_polarity:data_channel_is_entertainment
n_tokens_title:weekday_is_tuesday
num_self_hrefs:is_weekend
max_negative_polarity:is_weekend