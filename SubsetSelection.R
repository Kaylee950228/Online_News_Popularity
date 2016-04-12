
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