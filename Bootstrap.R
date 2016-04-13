setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")
#setwd("/Users/Darshan/Documents/Online_News_Popularity")

library(caret)
library(ggplot2)
source("DataPreprocess.R")

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

# stepwise selection
predictors <- c("data_channel", "cat_dow", "i_kw_max_avg_avg",
                "self_reference_avg_sharess", "i_kw_avg_max_max",
                "num_hrefs", "global_subjectivity", "LDA_00", 
                "LDA_01", "LDA_02", "num_self_hrefs",
                "i_n_unique_tokens_content", "i_title_sub_sent_polarity",
                "abs_title_subjectivity", "n_tokens_title", "min_positive_polarity",
                "num_imgs", "average_token_length", "title_sentiment_polarity",
                "i_min_avg_negative_pol")

formula <- as.formula(paste("shares~", paste(predictors,collapse="+")))

setwd("/home/gbakie/neu/stat-sp16/project/data")

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

N_COEF <- 31

coef <- matrix(nrow = B, ncol=N_COEF)
models <- bootstrap(formula, news)
for (i in 1:length(models)) {
  for (j in 2:N_COEF) {
    coef[i,j] <- coef(models[[i]])[[j]]
  }
}

full_model <- lm(formula, data=news)
full_coef <- vector(mode="list", length=N_COEF)
predictor_names <- names(full_model$coefficients)[2:N_COEF]

for (i in 2:N_COEF) {
  full_coef[[i]] <- coef(full_model)[[i]]
}


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

results = data.frame(name=predictor_names, coef=unlist(full_coef), max=unlist(coef_max), min=unlist(coef_min))

ggplot(results, aes(x = name, y = coef)) +
  geom_point(size = 1) +
  labs(x = "Predictor", y = "Estimated coefficient") +
  geom_errorbar(aes(ymax = max, ymin = min),width=0.1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=10, face="bold"))

pred <- matrix(nrow = nrow(news), ncol=B)
for (i in 1:length(models)) {
  m <- models[[i]]
  pred[,i] <- predict(m, subset(news,select=-shares))
}

sse <- sum((rowMeans(pred) - news$shares)**2)
rmse <- sqrt(sse / nrow(news))