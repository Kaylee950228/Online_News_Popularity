library(ggplot2)
library(grid)
library(gridExtra)

# 95% confidence interval with 9 degrees of freedom
t <- qt(0.95,9)

models_w_outlier <- data.frame(model=c("Stepwise", 
                             "Stepwise w/Interactions", 
                             "LASSO", "RIDGE","Baseline"),
                     rmse=c(0.164091, 0.1637224,0.164178, 0.164184,0.174777), 
                     sd=c(t*0.00660206, t*0.006687491, t*0.005667, t*0.00561,t*0.005497))

p1 <- ggplot(models, aes(x = model, y = rmse)) + 
  geom_point(size = 1) + 
  geom_errorbar(aes(ymax = rmse+sd, ymin = rmse-sd), width=0.1, size=1) + 
  ylim(0.15, 0.20) + 
  xlab("Various Models") + 
  ylab("Root Mean Square Error") + geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10, face = "bold")) +
  geom_text(hjust = 0, nudge_x = 0.05)

models_wo_outlier <- data.frame(model=c("Stepwise", 
                                       "Stepwise w/Interactions", 
                                       "LASSO", "RIDGE","Baseline"),
                               rmse=c(0.1335844, 0.1333038, 0.13356, 0.133558,0.146609), 
                               sd=c(t*0.001188368, t*0.001266149, t*0.001593, t*0.001585,t*0.00125299))

p2 <- ggplot(models_wo_outlier, aes(x = model, y = rmse, label=rmse)) + 
  geom_errorbar(aes(ymax = rmse+sd, ymin = rmse-sd), width=0.1, size=1, colour="red") + 
  geom_point(shape=21, size=2, fill="black",show.legend=TRUE) + 
  ylim(0.125, 0.16) + 
  xlab("Various Models") + 
  ylab("Root Mean Square Error") + geom_line() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10, face = "bold")) +
  geom_text(hjust = 0, nudge_x = 0.05)

grid.arrange(p1, p2, ncol = 2, top = "")