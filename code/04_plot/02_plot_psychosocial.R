# Generate Plots

##### Mean Difference Plots
  
generate_plot <- function(dataset_name,cg_name,is_kl){
  kls_plot_data <- kls_global %>%
    filter(label != 'A') %>%
    filter(category_group == cg_name, dataset==dataset_name) %>%
    mutate(category = fct_reorder2(category, label,kl, .fun = function(l,k){
      max(if_else(l == 'FR',k,-Inf))
    })) %>%
    arrange(category) %>%
    mutate(category_level = as.numeric(as.factor(category)))
  
  mean_diffs_plot_data <- mean_diffs_global %>%
    filter(label != 'A') %>%
    filter(category_group == cg_name, dataset==dataset_name) %>%
    mutate(category = fct_reorder2(category, label,mean_diff, .fun = function(l,k){
      max(if_else(l == 'FR',k,-Inf))
    })) %>%
    arrange(category) %>%
    mutate(category_level = as.numeric(as.factor(category)))
  
  significances_plot_data <- significances_global %>%
    filter(group1 != 'A', group2 != 'A') %>%
    filter(cg == cg_name, dataset==dataset_name) %>%
    left_join((kls_plot_data %>% select(category,label, kl)),by=c('category'='category', 'group1'='label')) %>%
    rename(group1_mean_diff = kl) %>%
    left_join((kls_plot_data %>% select(category,label, kl)),by=c('category'='category', 'group2'='label')) %>%
    rename(group2_mean_diff = kl) %>%
    filter(p.signif %in% c('ns') | kruskal.p.signif == 'ns')  
  
  significances_plot_data <- significances_global %>%
    filter(group1 != 'A', group2 != 'A') %>%
    filter(cg == cg_name, dataset==dataset_name) %>%
    left_join((mean_diffs_plot_data %>% select(category,label, mean_diff)),by=c('category'='category', 'group1'='label')) %>%
    rename(group1_mean_diff = mean_diff) %>%
    left_join((mean_diffs_plot_data %>% select(category,category_level,label, mean_diff)),by=c('category'='category', 'group2'='label')) %>%
    rename(group2_mean_diff = mean_diff) %>%
    filter(p.signif %in% c('ns') | kruskal.p.signif == 'ns')
  
  v_adj <- 0.2
  ellipse_element <- if(nrow(significances_plot_data) > 0){
    geom_ellipse(data=significances_plot_data,
                 mapping=aes(x0=(group1_mean_diff+group2_mean_diff)/2, y0=category_level, a=(group1_mean_diff-group2_mean_diff)/2, b=v_adj, angle=0),#, a=3, b=1, angle=0),
                 alpha = 0.4)
  }else{
    geom_blank()
  }
  
  cols <- c( "L" = "blue","N" ="purple", "R" = "red", "FR" = "darkred")
  p1 <- ggplot() +
    geom_point(data = mean_diffs_plot_data,aes(x=mean_diff, y=category, group=label, color=label), alpha=0) +
    geom_text(data=significances_plot_data,
              mapping=aes(y=category, label=if_else(kruskal.p.signif=='ns','ns','')),x=0.05)+
    geom_segment(data=significances_plot_data,
                 mapping=aes(x=group1_mean_diff, xend=group2_mean_diff, y=category_level+v_adj, yend=category_level+v_adj),
                 alpha = 0.4, position=position_dodge2(width=0.9),
    )+
    geom_segment(data=significances_plot_data,
                 mapping=aes(x=group1_mean_diff, xend=group1_mean_diff, y=category_level+v_adj, yend=category_level),
                 alpha = 0.4, position=position_dodge2(width=0.9),
    )+
    geom_segment(data=significances_plot_data,
                 mapping=aes(x=group2_mean_diff, xend=group2_mean_diff, y=category_level+v_adj, yend=category_level),
                 alpha = 0.4, position=position_dodge2(width=0.9),
    )+
    geom_point(data = mean_diffs_plot_data,aes(x=mean_diff, y=category, group=label, color=label), alpha=1) +
    geom_path(data = mean_diffs_plot_data,aes(x=mean_diff, y=category, group=label, color=label), stat='identity',size=0.1) +
    theme_cowplot()+
    theme(legend.position = 'bottom') +
    scale_colour_manual(values = cols) +
    labs(
      # title=paste0(dataset_name," ",cg_name),
      x='Mean Difference (from neutral)',
      y='Moral Foundations Vice/Virtue',
      color='Ideological Group',
      # caption = paste0("The error bars indicate that the distribution means of the groups are not significantly different according to",'\n'," the Wilcox Rank Sign Test (alpha=5%) with Holm adjustment for family-wise error.")
    ) 
  
  p2 <- ggplot() +
    geom_point(data = kls_plot_data,aes(x=kl, y=category, group=label, color=label), alpha=0) +
    # geom_text(data=significances_plot_data,
    # mapping=aes(x = (group1_mean_diff + group2_mean_diff)/2, label = p.signif, y=category), alpha = 0.4, position=position_dodge2(width = 0.1))+
    geom_text(data=significances_plot_data,
              mapping=aes(y=category, label=if_else(kruskal.p.signif=='ns','ns','')),x=0.05)+
    geom_errorbarh(data=significances_plot_data,
                   mapping=aes(xmin = group1_mean_diff, xmax = group2_mean_diff, y=category), alpha = 0.4, position=position_dodge2(width=0.9), height=0.5)+
    geom_point(data = kls_plot_data,aes(x=kl, y=category, group=label, color=label), alpha=1) +
    geom_path(data = kls_plot_data,aes(x=kl, y=category, group=label, color=label), stat='identity',size=0.1) +
    theme_cowplot()+
    theme(legend.position = 'bottom') +
    scale_colour_manual(values = cols) +
    labs(
      # title=paste0(dataset_name," ",cg_name),
      # caption = "The error bars indicate that the distribution means of the groups are not significantly different according to the Wilcox Rank Sign Test with Holm adjustment for family-wise error.",
      x='Signed-KL Divergence (from neutral)',
      y='Grievance',
      color='Ideological Group',
    )
  
  if(is_kl){
    return(p2)
  }else{
    return(p1)
  }
}

plot1 <- generate_plot('ausvotes', 'grievance', T)
pdf("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/main_plots/grievance_ausvotes.pdf", width = 6, height = 6 )
plot1
dev.off()

pdf("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/main_plots/vice_virtue_qanda.pdf", width = 6, height = 6 )
generate_plot('qanda', 'mfd_v', F)
dev.off()

for (dataset in c('qanda','ausvotes','socialsense', 'riot', 'parler')) {
  # pdf(paste0("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/appendix/vice_virtue_",dataset,".pdf"), width = 6, height = 6 )
  generate_plot(dataset, 'mfd_v', F)
  # dev.off()
  
  # pdf(paste0("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/appendix/grievance_",dataset,".pdf"), width = 6, height = 6 )
  generate_plot(dataset, 'mfd_v', F)
  # dev.off()
}
dataset <- 'parler'
pdf(paste0("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/appendix/vice_virtue_",dataset,".pdf"), width = 6, height = 6 )
generate_plot(dataset, 'mfd_v', F)
dev.off()

pdf(paste0("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/appendix/grievance_",dataset,".pdf"), width = 6, height = 6 )
generate_plot(dataset, 'grievance', T)
dev.off()
