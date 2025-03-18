library(data.table)
library(ggcorrplot)
library(ggcorrplot)
library(patchwork)
library(stringr)

cross_gt_performance = as.matrix(read.csv(paste0('/data/roram/opinion_dynamics/02c_stance_detection/data/04_results/qanda_cross_gt_auc.csv'), row.names=1))
cross_dataset_performance = as.matrix(read.csv(paste0('/data/roram/opinion_dynamics/02c_stance_detection/data/04_results/cross_dataset_auc.csv'), row.names=1))


row.names(cross_gt_performance) <- c("Hashtags", "Left-Right MPP", "Politician Endorsers","Party Followers", "Far-Right MPP", "MBFC MPP")
colnames(cross_gt_performance) <- c("Hashtags", "Left-Right MPP", "Politician Endorsers","Party Followers", "Far-Right MPP", "MBFC MPP")

cross_gt_performance <- cross_gt_performance[c(1,4,3,2,5,6),c(1,4,3,2,5,6)]

cross_gt_plot <- ggcorrplot(cross_gt_performance, lab = TRUE) + 
  scale_fill_gradient2(limit = c(0,1), low = "blue", high =  "red", mid = "white", midpoint = 0.5) +
  theme_cowplot()+
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    legend.position=c(0.7,0.3),
    legend.direction = "vertical",
    legend.title = element_blank(),
    # axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.6)
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 5)) +
  # theme_cowplot()+
  labs(x='Tested On', y='Trained On',
       # subtitle = 'Qanda ROC-AUC via LGBM',
       )
cross_gt_plot

cross_gt_performance[c(1,2,3,4),c(1,2,3,4)]
row.names(cross_gt_performance) <- c("Hashtags", "Left-Right Media", "Politician Endorsers","Party Followers", "Far-Right Media", "MBFC")
colnames(cross_gt_performance) <- c("Hashtags", "Left-Right Media", "Politician Endorsers","Party Followers", "Far-Right Media", "MBFC")
cross_gt_plot_lr <- ggcorrplot(cross_gt_performance[c(1,2,3,4),c(1,2,3,4)], lab = TRUE) + 
  scale_fill_gradient2(limit = c(0,1), low = "blue", high =  "red", mid = "white", midpoint = 0.5) +
  theme_cowplot()+
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    # legend.position=c(0.7,0.3),
    legend.direction = "vertical",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  ) +
  # theme_cowplot()+
  labs(x='Tested On', y='Trained On',
       # subtitle = 'Qanda ROC-AUC via LGBM',
  )

cross_gt_performance[c(5,6),c(5,6)]
row.names(cross_gt_performance) <- c("Hashtags", "Left-Right Media", "Politician Endorsers","PARTY Party Followers", "Far-Right Media", "MBFC")
colnames(cross_gt_performance) <- c("Hashtags", "Left-Right Media", "Politician Endorsers","PARTY Party Followers", "Far-Right Media", "MBFC")
cross_gt_plot_fr <- ggcorrplot(cross_gt_performance[c(5,6),c(5,6)], lab = TRUE) + 
  scale_fill_gradient2(limit = c(0,1), low = "blue", high =  "red", mid = "white", midpoint = 0.5) +
  theme_cowplot()+
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    # legend.position=c(0.7,0.3),
    legend.direction = "vertical",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  ) +
  # theme_cowplot()+
  labs(x='Tested On', y='Trained On',
       # subtitle = 'Qanda ROC-AUC via LGBM',
  )

cross_dataset_plot <- ggcorrplot(cross_dataset_performance, lab = TRUE) + 
  scale_fill_gradient2(limit = c(0.5,1), low = "white", high =  "red", mid = "white", midpoint = 0.5) +
  theme_cowplot()+
  theme(
    axis.title.x = element_text(),
    axis.title.y = element_text(angle = 90),
    # legend.position=c(0.7,0.3),
    # legend.direction = "vertical",
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  ) +
  # theme_cowplot()+
  labs(x='Tested On', y='Trained On',
       # subtitle = paste(dataset,'ROC-AUC via LGBM'),
       )

pdf("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/main_plots/cross_gt_performance_corrplot.pdf", width = 6, height = 6 )
cross_gt_plot
dev.off()

pdf("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/main_plots/cross_gt_performance_lr_corrplot.pdf", width = 6, height = 6 )
cross_gt_plot_lr
dev.off()

pdf("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/main_plots/cross_gt_performance_fr_corrplot.pdf", width = 6, height = 6 )
cross_gt_plot_fr
dev.off()

pdf("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/main_plots/cross_dataset_performance_corrplot.pdf", width = 6, height = 6 )
cross_dataset_plot
dev.off()
