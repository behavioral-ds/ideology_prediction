# 01_collate_data
library(data.table)
library(dplyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)
library(forcats)
library(philentropy)
library(parallel)
library(ggpubr)
library(stringr)
library(ggforce)

kls_global <- data.frame()
mean_diffs_global <- data.frame()
significances_global <- data.frame()

# dataset = 'ausvotes'
for (dataset in c('qanda','ausvotes','socialsense', 'riot', 'parler')) {
  # print(dataset)
  labelled_data_dir = paste0('/home/roram/Data/opinion_dynamics/02c_stance_detection/data/04_results/labels/',dataset,'.csv')
  data = fread(labelled_data_dir,colClasses = 'character', header=T)
  #### Attach Grievance Data
  grievance_data_dir = paste0('/home/roram/Data/opinion_dynamics/grievancedictionary/results/',dataset,'_grevience.csv')
  grievance_data = fread(grievance_data_dir,colClasses = 'character')
  
  data <- data %>% inner_join(grievance_data, by='uid')
  
  ##### Attach MFD data
  mfd_data_dir = paste0('/home/roram/Data/opinion_dynamics/Moral_Foundation_FrameAxis/results/emfd_',dataset,'_results.csv')
  if(dataset == 'qanda'){
    r <- c(1,7:26)
  }else if(dataset == 'ausvotes' | dataset == 'parler' | dataset =='riot'){
    r <- c(1,6:25)
  }else{
    r <- c(1,5:24)
  }
  mfd_data = fread(mfd_data_dir,colClasses = 'character')[,..r]
  data <- data %>% left_join(mfd_data, by='uid')
  
  label_col <- which(colnames(data) == 'label')
  
  griev_start <- which(colnames(data) == 'deadline')
  griev_end <- which(colnames(data) == 'violence')
  
  mfd_start <- which(colnames(data) == 'violence')+1
  mfd_end <- ncol(data)
  
  mfd_vice_virture_range <- which(colnames(data) %in% colnames(data[,mfd_start:mfd_end] %>% select(ends_with('vice') | ends_with('virtue'))))
  mfd_bias_range <- which(colnames(data) %in% colnames(data[,mfd_start:mfd_end] %>% select(starts_with('bias'))))
  mfd_intensity_range <- which(colnames(data) %in% colnames(data[,mfd_start:mfd_end] %>% select(starts_with('intensity'))))
  category_groups <- rbind(data.frame(r=griev_start:griev_end,cg='grievance'), 
                           data.frame(r=mfd_vice_virture_range,cg='mfd_v'),
                           data.frame(r=mfd_bias_range,cg='mfd_bias'),
                           data.frame(r=mfd_intensity_range,cg='mfd_intensity'))
  
  r=category_groups$r
  category_groups$category <- colnames(data[,..r])
  
  data_long <- data[,c(2,..label_col,..r)] %>%
    gather('category', 'sentiment', -c(uid, label)) %>%
    mutate(sentiment=as.numeric(sentiment)) %>%
    filter(!is.na(sentiment)) %>%
    left_join(category_groups, by=c('category'='category'))
  
  source('/home/roram/Data/opinion_dynamics/02c_stance_detection/code/03_psychosocial_modelling/helper_functions.R')
  
  mean_diffs <- mcmapply(recover_mean_diff_from_neutral, colnames(data[,..r])) %>%
    t() %>%
    data.frame() %>%
    tibble::rownames_to_column("category")
  mean_diffs$category_group <- category_groups$cg
  mean_diffs$category_index <- category_groups$r
  mean_diffs <- mean_diffs %>%
    gather('label', 'mean_diff', -c(category,category_group,category_index)) %>%
    mutate(category = fct_reorder2(category, label,mean_diff, .fun = function(l,k){
      max(if_else(l == 'FR',k,-Inf))
    }))
  mean_diffs$dataset <- dataset
  mean_diffs_global <- rbind(mean_diffs_global, mean_diffs)
  
  significances <- rbindlist(mclapply(colnames(data[,..r]),recover_pairwise_significances))
  significances <- significances %>% left_join(category_groups, by = 'category')
  significances_global <- rbind(significances_global, significances)
  
  kls <- mcmapply(recover_divergence_from_neutral, colnames(data[,..r]), r=r)
  kls <- kls %>%
    t() %>%
    data.frame() %>%
    tibble::rownames_to_column("category")
  kls$category_group <- category_groups$cg
  kls$category_index <- category_groups$r
  kls_temp <- kls %>%
    gather('label', 'kl', -c(category,category_group,category_index)) %>%
    mutate(category = fct_reorder2(category, label,kl, .fun = function(l,k){
      max(if_else(l == 'FR',k,-Inf))
    }))
  kls_temp$dataset <- dataset
  kls_global <- rbind(kls_global, kls_temp)

}

rm(list=ls()[! ls() %in% c("kls_global","mean_diffs_global","significances_global")])
save.image(file='/home/roram/Data/opinion_dynamics/02c_stance_detection/code/03_psychosocial_modelling/divergence_analysis.RData')
