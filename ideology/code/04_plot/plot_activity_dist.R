library(dplyr)
library(ggplot2)
library(data.table)

library(ggpubr)


df <- fread('~/Data/opinion_dynamics/02c_stance_detection/data/03_processed/activity_profile.csv')



library(poweRlaw)
m = displ$new(df$counts)
est = estimate_xmin(m)

m$setXmin(est[[2]])
m$setPars(est[[3]])
m
plot(m)
lines(m, col=2)

p <- ggplot(df, create_aes(list(x = "counts")))

p <- p +
  geom_exec(stat_ecdf, data = df,
            color = datasets)

ggplot(df, create_aes(list(x = "counts")))

plot <- ggecdf(df, x = "counts",
       color = "dataset", 
       yscale = "log10", xscale = "log10", orientation= "reverse")

ggpar(plot, yscale = "log10")

df %>%
  ggplot(aes(x=(counts))) +
  # geom_density(aes(x=counts,color=dataset)) +
  # stat_ecdf(aes(x=counts, color=dataset),geom = "step") +
  geom_line(aes(y = 1 - ..y.., color=dataset), stat='ecdf') +
  theme_cowplot()+
  coord_trans(x="log2", y="log2")

df %>%
  ggplot() +
  geom_density(aes(x=counts,color=dataset)) +
  # stat_ecdf(aes(x=counts, color=dataset),geom = "step") +
  theme_cowplot()+
  coord_trans(x="log2")
