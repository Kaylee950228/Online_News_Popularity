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

#news <- subset(news, select = setdiff(names(news),categorical_var))

news <- cook_outliers_removal(news)

K <- 10

folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)

models <- list()
rmses <- c()
R2s <- c()

# Weight Regression Code started

#ncvTest(lm(shares ~ .,data=news))
#m.unweighted <- lm(shares ~ ., data=news)
#news$res <- abs(m.unweighted$residuals)
#news$predict<-predict(m.unweighted, data=news)
#ggplot(data = news, aes(x=predict,y=res)) + geom_point() + stat_smooth() 
#+ xlab("Shares Prediction") + ylab("abs(residual)")
#news$res <- NULL
#news$predict <- NULL
#w <- predict(lm(abs(m.unweighted$res) ~ predict(m.unweighted, data=news)), data=news)

## Code Ends

for (i in 1:K) {
  
  news_train <- news[folds[[i]],]
  news_val <- news[-folds[[i]],]
  
  null=lm(shares~1, data=news_train)
  full=lm(shares~., data=news_train)
  
  #m.unweighted <- lm(shares ~ ., data=news_train)
  #w <- predict(lm(abs(m.unweighted$res) ~ predict(m.unweighted, data=news_train)), data=news_train)
  #model <- lm(formula = shares ~ ., data = news_train, weights = 1/(w^2))
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

# Best Model With outlier
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
             i_title_subjectivity_sentiment_polarity +
             abs_title_subjectivity +
             n_tokens_title +
             min_positive_polarity +
             num_imgs +
             average_token_length +
             title_sentiment_polarity + 
             I(n_tokens_title * weekday_is_tuesday) + 
             I(average_token_length * data_channel_is_entertainment) + 
             I(num_hrefs * data_channel_is_socmed) + 
             I(num_imgs * is_weekend * data_channel_is_socmed) + 
             I(global_subjectivity * data_channel_is_socmed) + 
             I(i_n_unique_tokens_content * data_channel_is_bus) +
             I(min_positive_polarity * data_channel_is_entertainment), data=news))

I(n_tokens_title * weekday_is_tuesday)
I(average_token_length * data_channel_is_entertainment)

I(num_hrefs * data_channel_is_socmed)
I(num_imgs * is_weekend * data_channel_is_socmed)
I(global_subjectivity * data_channel_is_socmed)
I(i_n_unique_tokens_content * data_channel_is_bus)
I(min_positive_polarity * data_channel_is_entertainment)

I(num_self_hrefs * is_weekend)


# Best Model with out Outlier 
summary(lm(shares ~ i_title_subjectivity_sentiment_polarity +
             data_channel +
             i_kw_max_avg_avg +
             cat_dow +
             self_reference_avg_sharess +
             num_hrefs +
             i_kw_avg_max_max +
             LDA_00 +
             num_self_hrefs +
             i_n_unique_tokens_content +
             global_subjectivity +
             LDA_02 +
             min_positive_polarity +
             num_imgs +
             LDA_04 +
             title_sentiment_polarity +
             max_negative_polarity +
             abs_title_subjectivity + 
             i_rate_pos_glob_sent_polarity + 
             global_rate_negative_words + 
             i_rate_pos_glob_sent_polarity, data=news))
I(max_negative_polarity * is_weekend)
# Exhuastive Subset selection on the remaning set of variables

model.summaries <- list()
min.bic.index <- c()
max.r2.index <- c()
max.r2 <- c()
select.var.index <- c()

for (i in 1:K) {
  
  news_train <- news[folds[[i]],]
  news_val <- news[-folds[[i]],]
  
  model <- regsubsets(shares ~ i_title_subjectivity_sentiment_polarity +
                        data_channel +
                        i_kw_max_avg_avg +
                        cat_dow +
                        self_reference_avg_sharess +
                        num_hrefs +
                        i_kw_avg_max_max +
                        LDA_00 +
                        num_self_hrefs +
                        i_n_unique_tokens_content +
                        global_subjectivity +
                        LDA_02 +
                        min_positive_polarity +
                        num_imgs +
                        LDA_04 +
                        title_sentiment_polarity +
                        max_negative_polarity +
                        abs_title_subjectivity + 
                        i_rate_pos_glob_sent_polarity +
                        global_rate_negative_words +
                        global_rate_positive_words +
                        num_keywords +
                        num_videos +
                        average_token_length +
                        avg_positive_polarity +
                        i_min_avg_negative_pol +
                        LDA_01 +
                        LDA_03, nbest = 1, nvmax = 38, 
                      data=news_train, force.in = c(1:28), 
                      method = "exhaustive", really.big = TRUE)
  
  summary.model <- summary(model)
  
  min.bic.index <- append(which.min(summary.model$bic), min.bic.index)
  max.r2.index <- append(which.max(summary.model$adjr2) ,max.r2.index)
  max.r2<- append(max(summary.model$adjr2),max.r2)
  
  model.summaries[[i]] <- summary.model
  
  select.var.index <- append(which.max(which(summary.model$which[1,])), select.var.index)
}

# Found four additional variable 

# i_rate_pos_glob_sent_polarity global_rate_negative_words avg_positive_polarity i_rate_pos_glob_sent_polarity

# Final Model Results on K-fold
K <- 10
folds <- createFolds(news$shares, k = K, list=TRUE, returnTrain=TRUE)

models <- list()
rmses <- c()
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
                i_title_subjectivity_sentiment_polarity +
                abs_title_subjectivity +
                n_tokens_title +
                min_positive_polarity +
                num_imgs +
                average_token_length +
                title_sentiment_polarity, data=news_train)
  
  pred <- predict(model, news_val)
  
  #pred <- target_inverse(pred, lamda)
  #shares_val <- target_inverse(news_val$shares, lamda)
  #mse <- sum((pred - shares_val)**2) / nrow(news_val)
  
  mse <- sum((pred - news_val$shares)**2) / nrow(news_val)
  rmses <- append(rmses, sqrt(mse))
  
  R2s <- append(R2s, summary(model)$adj.r.squared)
  
  models[[i]] <- model
  
}
rmses
R2s
mean(rmses)
mean(R2s)

