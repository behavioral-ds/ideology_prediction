
### Count Hypotheses wrt bias

datasets <- unique(mean_diffs_global$dataset)
individualisers <- c('fairness', 'care')
binders <- c('loyalty', 'authority', 'sanctity')
foundations <- c(individualisers,binders)

results_all <- matrix(0, nrow = length(foundations) ,ncol = length(datasets), dimnames = list(foundations,datasets))

for (mfd_type in c('bias','intensity')) {
  for (comparison_group in c('R', 'FR')) {
    results <- matrix(0, nrow = length(foundations) ,ncol = length(datasets), dimnames = list(foundations,datasets))
    for (ds in datasets){
      for (foundation in foundations) {
        left_score <- (mean_diffs_global %>% filter(category == paste0(mfd_type,'_',foundation), label=='L', dataset==ds))$mean_diff
        right_score <- (mean_diffs_global %>% filter(category == paste0(mfd_type,'_',foundation), label==comparison_group, dataset==ds))$mean_diff
        is_significant <- (significances_global %>% filter(category == paste0(mfd_type,'_',foundation), group1=='L'&group2==comparison_group|group1==comparison_group&group2=='L', dataset==ds))$kruskal.p.adj < 0.05
        direction_sign <- if(foundation %in% individualisers){1}else{-1}
        results[foundation, ds] = results[foundation, ds] + if((direction_sign*(left_score - right_score) >0) & is_significant){1}else{0}
      }
    }
    print(results)
    results_all <- results_all + results
  }
}
# addmargins(results_all, c(1,2))
kab <- knitr::kable(addmargins(results_all, c(1,2)),
                    booktabs=T, label="tab:hypothesis_table", format = 'latex')
writeLines(kab, '/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/main_plots/hypotheses_table.tex')


