library(data.table)
library(ggcorrplot)
library(corrplot)
library(patchwork)

for (dataset in c('qanda')){#},'ausvotes','socialsense','riot', 'parler')) {
  cross_gt_performance = as.matrix(read.csv(paste0('/data/roram/opinion_dynamics/02b_stance_detection/code/03_cross_ground_truth/',dataset,'_cross_gt_auc.csv'), row.names=1))
  cross_gt_overlap = as.matrix(read.csv(paste0('/data/roram/opinion_dynamics/02b_stance_detection/code/03_cross_ground_truth/',dataset,'_cross_gt_f1.csv'), row.names=1)) 
  p1 <- ggcorrplot(cross_gt_overlap, lab = TRUE) + 
    scale_fill_gradient2(limit = c(0,1), low = "blue", high =  "red", mid = "white", midpoint = 0.5) +
    theme_cowplot()+
    theme(
      legend.position=c(0.7,0.3),
      legend.direction = "vertical",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
    ) +
    # theme_cowplot()+
    labs(x="", y="",subtitle = paste(dataset,'F1-macro'))
  p2 <- ggcorrplot(cross_gt_performance, lab = TRUE) + 
    scale_fill_gradient2(limit = c(0,1), low = "blue", high =  "red", mid = "white", midpoint = 0.5) +
    theme_cowplot()+
    theme(
      axis.title.x = element_text(),
      axis.title.y = element_text(angle = 90),
      legend.position=c(0.7,0.3),
      legend.direction = "vertical",
      legend.title = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
    ) +
    # theme_cowplot()+
    labs(x='Tested On', y='Trained On', subtitle = paste(dataset,'ROC-AUC via LGBM'))
  plot(p1)
  plot(p2)
}

cross_dataset_performance = as.matrix(read.csv(paste0('/data/roram/opinion_dynamics/02b_stance_detection/code/03_cross_ground_truth/cross_dataset_auc.csv'), row.names=1))
ggcorrplot(cross_dataset_performance, lab = TRUE) + 
  scale_fill_gradient2(limit = c(0.5,1), low = "white", high =  "red", mid = "white", midpoint = 0.5) +
  theme_cowplot()+
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    # legend.position=c(0.7,0.3),
    # legend.direction = "vertical",
    # legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  ) +
  # theme_cowplot()+
  labs(x='Tested On', y='Trained On', subtitle = paste(dataset,'ROC-AUC via LGBM'))
