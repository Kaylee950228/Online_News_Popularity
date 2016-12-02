#setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")
setwd("/Users/Darshan/Documents/Online_News_Popularity")

library(caret)
library(ggplot2)
source("DataPreprocess.R")

set.seed(464)

B = 300
bootstrap <- function(formula, data) {
  n_rows <- nrow(data)
  
  models <- vector(mode="list", length=B)
  for (i in 1:B) {
    # sample the same number of points with replacement
    boot_idx <- sample(n_rows, n_rows, replace = TRUE)
    boot_data <- data[boot_idx, ]
    
    m <- lm(formula, data=boot_data)
    
    models[[i]] <- m
  }
  
  return(models)
}


# stepwise selection (with outliers)
# predictors <- c("data_channel", "cat_dow", "i_kw_max_avg_avg",
#                 "self_reference_avg_sharess", "i_kw_avg_max_max",
#                 "num_hrefs", "global_subjectivity", "LDA_00", 
#                 "LDA_01", "LDA_02", "num_self_hrefs",
#                 "i_n_unique_tokens_content", "i_title_sub_sent_polarity",
#                 "abs_title_subjectivity", "n_tokens_title", "min_positive_polarity",
#                 "num_imgs", "average_token_length", "title_sentiment_polarity",
#                 "i_min_avg_negative_pol")

# # stepwise selection (without outliers)
predictors <- c("num_hrefs", "num_self_hrefs", "num_imgs",
  "self_reference_avg_sharess", "LDA_00", "LDA_02", "global_subjectivity",
  "global_rate_positive_words", "global_rate_negative_words", "min_positive_polarity",
  "max_negative_polarity", "title_sentiment_polarity", "abs_title_subjectivity",
  "i_n_unique_tokens_content", "i_rate_pos_gsent_polarity", "i_kw_max_avg_avg", 
  "i_kw_avg_max_max", "cat_dow", "data_channel", "i_title_sub_sent_polarity")


formula <- as.formula(paste("shares~", paste(predictors,collapse="+")))

#setwd("/home/gbakie/neu/stat-sp16/project/data")
setwd("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/")

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

news <- cook_outliers_removal(news)

# number of coefficients in the model
N_COEF <- 31

# get the coefficients values from each model
coef <- matrix(nrow = B, ncol=N_COEF)
models <- bootstrap(formula, news)
for (i in 1:length(models)) {
  for (j in 2:N_COEF) {
    coef[i,j] <- coef(models[[i]])[[j]]
  }
}

# train a model on the full dataset
full_model <- lm(formula, data=news)
full_coef <- vector(mode="list", length=N_COEF)
predictor_names <- names(full_model$coefficients)[2:N_COEF]

# get the coefficients of the full model
for (i in 2:N_COEF) {
  full_coef[[i]] <- coef(full_model)[[i]]
}

# calculate coefficients confidence intervals
coef_max <- vector(mode="list", length=N_COEF)
coef_min <- vector(mode="list", length=N_COEF)
for (i in 2:N_COEF) {
  b_star_upper <- qnorm(0.975, mean=mean(coef[,i]), sd=sd(coef[,i]))
  b_star_lower <- qnorm(0.025, mean=mean(coef[,i]), sd=sd(coef[,i]))

  d1 <- full_coef[[i]] - b_star_upper
  d2 <- b_star_lower - full_coef[[i]]
  
  coef_max[[i]] <-  full_coef[[i]] - d2
  coef_min[[i]] <- full_coef[[i]] + d1
  
  cat(sprintf("predictor: %s, lower_value = %f, upper_value = %f\n", 
              predictor_names[i], coef_min[[i]], coef_max[[i]]))  
}

# plot the coefficient and their confidence interval
results = data.frame(name=predictor_names, coef=unlist(full_coef), max=unlist(coef_max), min=unlist(coef_min))

ggplot(results, aes(x = name, y = coef)) +
  geom_point(size = 1) +
  labs(x = "Predictor", y = "Estimated coefficient") +
  geom_errorbar(aes(ymax = max, ymin = min),width=0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10, face="bold"))

# prediction for all the models
pred <- matrix(nrow = nrow(news), ncol=B)
for (i in 1:length(models)) {
  m <- models[[i]]
  pred[,i] <- predict(m, subset(news,select=-shares))
}

sse <- sum((rowMeans(pred) - news$shares)**2)
rmse <- sqrt(sse / nrow(news))

news$res <- (news$shares - rowMeans(pred))
news$pred <- rowMeans(pred)

y <- quantile(news$res, c(0.25, 0.75))
x <- qnorm(c(0.25, 0.75))
slope <- diff(y)/diff(x)
int <- y[1L] - slope * x[1L]

p3<- ggplot(news, aes(sample=res)) + 
  stat_qq() +geom_abline(slope = slope, intercept = int) + 
  ylab("Bootstrap (Without Outliers)") + 
  theme(axis.title=element_text(size=9,face="bold"))