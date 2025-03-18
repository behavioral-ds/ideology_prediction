library(ggplot2)

set.seed(1234)
wdata = data.frame(
  ideology = factor(rep(c("Left", "Neutral"), each=200)),
  score = c(rnorm(200, 55), rnorm(200, 58)))

ggdensity(wdata, x = "score",
          add = "mean", rug = TRUE,
          color = "ideology", fill = "ideology") +
  # stat_compare_means(comparisons = list(c('Left','Neutral')))+ # Add pairwise comparisons p-value
  # stat_compare_means(label.y = 50)+
  theme(axis.line = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank())

# Choose the mean as 2.5 and standard deviation as 0.5.
y <- dnorm(x, mean = 2.5, sd = 0.5)

# Give the chart file a name.
png(file = "dnorm.png")

plot(x,y)