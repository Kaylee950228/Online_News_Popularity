setwd("/home/gbakie/neu/stat-sp16/project/Online_News_Popularity")
#setwd("/Users/Darshan/Documents/Online_News_Popularity")

source("DataPreprocess.R")


set.seed(352)

B = 400
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


predictors <- c("data_channel", "cat_dow", "i_kw_max_avg_avg",
                "self_reference_avg_sharess", "i_kw_avg_max_max",
                "num_hrefs", "global_subjectivity", "LDA_00", 
                "LDA_01", "LDA_02", "num_self_hrefs",
                "i_n_unique_tokens_content", "i_title_subjectivity_sentiment_polarity",
                "abs_title_subjectivity", "n_tokens_title", "min_positive_polarity",
                "num_imgs", "average_token_length", "title_sentiment_polarity")

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

N_COEF <- 30

coef <- matrix(nrow = B, ncol=N_COEF)
models <- bootstrap(formula, news)
for (i in 1:length(models)) {
  for (j in 1:N_COEF) {
    coef[i,j] <- coef(models[[i]])[[j]]
  }
}

full_coef <- vector(mode="list", length=30)
full_model <- lm(formula, data=news)
for (i in 1:N_COEF) {
  full_coef[[i]] <- coef(full_model)[[i]]
}


for (i in 1:N_COEF) {
  b_star_upper <- qnorm(0.975, mean=mean(coef[,i]), sd=sd(coef[,i]))
  b_star_lower <- qnorm(0.025, mean=mean(coef[,i]), sd=sd(coef[,i]))

  d1 <- full_coef[[i]] - b_star_upper
  d2 <- b_star_lower - full_coef[[i]]
  
  upper_value <-  full_coef[[i]] - d2
  lower_value <- full_coef[[i]] + d1
  
  cat(sprintf("lower_value = %f, upper_value = %f\n", lower_value, upper_value))  
}

