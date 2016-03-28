news <- read.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/OnlineNewsPopularity.csv", header = TRUE)
set.seed(10)

news <- news[sample(1:nrow(news)),]

n_test_samples <- round(nrow(news) * 0.20)

news_test <- news[(1:n_test_samples),]
news_train <- news[((1+n_test_samples):nrow(news)),]

write.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Train.csv", row.names = FALSE, x = news_train)
write.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Test.csv", row.names = FALSE, x = news_test)

train_url <- news_train$url
target <- news_train$shares

news_train$timedelta <- NULL
news_train$url <- NULL
news_train$shares <- NULL

log_target <- log(target)
boxplot(log_target)
boxplot(target)

news_train <- subset(news_train, n_tokens_content!=0)
news_train$n_non_stop_words <- NULL
write.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Cleaned_Train.csv", row.names = FALSE, x = news_train)

