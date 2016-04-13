
require(ggplot2)

# 95% confidence interval with 9 degrees of freedom
t <- qt(0.95,9)

models <- data.frame(model=c("LASSO", "ADAP LASSO"),
                     rmse=c(0.133560, 0.1363652), 
                     sd=c(t*0.001593, t*0.001106626))

ggplot(models, aes(x = model, y = rmse)) +
  geom_point(size = 1) +
  geom_errorbar(aes(ymax = rmse+sd, ymin = rmse-sd),width=0.1, size=1) + 
  ylim(0.10, 0.20)