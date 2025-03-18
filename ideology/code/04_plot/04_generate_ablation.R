library(data.table)
library(dplyr)

manual_val <- fread('~/Data/opinion_dynamics/02c_stance_detection/data/04_results/ablation_qanda_manual_val_auc.csv')
ablation <- fread('~/Data/opinion_dynamics/02c_stance_detection/data/04_results/ablation_qanda_cv.csv')

kab <- knitr::kable(ablation,
                    booktabs=T, label="tab:ablation_table", format = 'latex')
writeLines(kab, '/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/main_plots/ablation_table.tex')


manual_val_plot <- as.matrix(manual_val,rownames=1)[c('use'),c(1,2,3,5),drop=F] %>%
  ggcorrplot(lab = TRUE) +
  scale_fill_gradient2(limit = c(0.5,1), low = "white", high =  "red", mid = "white", midpoint = 0.5) +
  theme_cowplot()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    # axis.title.y = element_text(angle = 90),
    # legend.position=c(0.7,0.3),
    legend.direction = "vertical",
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
  ) +
  labs(y='Ground Truth',
       # subtitle = 'Qanda ROC-AUC via LGBM',
  )

pdf("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/main_plots/manual_validation_plot.pdf", width = 6, height = 6 )
manual_val_plot
dev.off()
manual_val_plot
