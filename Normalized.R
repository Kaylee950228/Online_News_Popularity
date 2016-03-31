library(dplyr)

# Load Training Data
news_train <- read.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Cleaned_Train.csv", header = TRUE)

# All Column names
column_names <- names(news_train)

# Column names which needs to be ignored due to categorical and target feature

ignored_column_names <- c("url", "timedelta", "data_channel_is_lifestyle",
                          "data_channel_is_entertainment", "data_channel_is_bus",
                          "data_channel_is_world", "data_channel_is_socmed",
                          "data_channel_is_tech", "weekday_is_monday", "weekday_is_tuesday",
                          "weekday_is_wednesday", "weekday_is_thursday", "weekday_is_friday",
                          "weekday_is_saturday", "weekday_is_sunday", "is_weekend", "shares")

needed_columns <- setdiff(column_names,ignored_column_names)

# Normalized Train Data
news_train_norm <- news_train %>% mutate_each_(funs(scale),vars=needed_columns)

# Saving standard deviation of the columns which are normalized
sd_values <- Map(sd, news_train[needed_columns])
write.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Train_SD_Values.csv", row.names = FALSE, x = sd_values)

# Saving eman of the columns which are normalized
mean_values <- Map(mean, news_train[needed_columns])
write.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Train_Mean_Values.csv", row.names = FALSE, x = mean_values)

# Saving Normalized Train Data
write.csv("/Users/Darshan/Documents/CS 7280 Stats/Project/Data/Normalized_Train.csv", row.names = FALSE, x = news_train_norm)