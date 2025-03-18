library(ggplot2)
library(cowplot)
library(dplyr)
library(ggh4x)

data_pr_lr <- fread('~/Data/opinion_dynamics/02c_stance_detection/data/04_results/bias_analysis/qanda_precision_lr.csv', col.names = c('Features', 'Hashtags', 'Left-Right MPP', 'Politician Endorsers', 'Party Followers'))
data_rec_lr <- fread('~/Data/opinion_dynamics/02c_stance_detection/data/04_results/bias_analysis/qanda_recall_lr.csv', col.names = c('Features', 'Hashtags', 'Left-Right MPP', 'Politician Endorsers', 'Party Followers'))

data_pr_fr <- fread('~/Data/opinion_dynamics/02c_stance_detection/data/04_results/bias_analysis/qanda_precision_fr.csv', col.names = c('Features', 'Far-Right MPP', 'MBFC MPP'))
data_rec_fr <- fread('~/Data/opinion_dynamics/02c_stance_detection/data/04_results/bias_analysis/qanda_recall_fr.csv', col.names = c('Features', 'Far-Right MPP', 'MBFC MPP'))

data_pr <- cbind(data_pr_lr, data_pr_fr[,2:3])
data_pr$metric_type <- 'Precision'
data_rec <- cbind(data_rec_lr, data_rec_fr[,2:3])
data_rec$metric_type <- 'Recall'




data <- rbind(data_rec,data_pr) %>%
  pivot_longer(!c(Features, metric_type), names_to = "Proxy", values_to = "Metric") %>%
  mutate(Proxy = fct_reorder(Proxy, Metric, .fun = mean)) %>%
  mutate(Metric = as.numeric(Metric))

mean_data <- data %>%
  group_by(metric_type, Proxy) %>%
  summarise(Metric = mean(Metric))


ggplot()+
  geom_bar(data=data, mapping=aes(fill = Features, y=Proxy, x = Metric),position=position_dodge2(), stat='identity') +
  geom_point(data=mean_data, mapping=aes(y=Proxy, x = Metric), shape=10) +
  # geom_boxplot(data=data, mapping=aes(y=Proxy, x = Metric)) +
  theme_cowplot() +
  scale_fill_brewer() +
  # scale_y_reverse()+
  # coord_flip() +
  facet_wrap(~metric_type, scales='free_x')+
  facetted_pos_scales(x = list(scale_x_reverse(limits=c(1.0,0.5)),scale_x_continuous(limits=c(0.5,1.0)))) +
  labs(x='', y='') +
  theme(legend.position = 'bottom',
        legend.direction = "horizontal",
        legend.margin = margin(0),
        legend.justification=c(-0.3,-0.1))+
  guides(fill=guide_legend(nrow=3, byrow=TRUE))
