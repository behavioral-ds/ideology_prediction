library(dictvectoR)
library(dplyr)
library(magrittr)
library(data.table)

library(ggplot2)
library(cowplot)
library(tidyverse)

library(ggpubr)
library(quanteda)
library(dictvectoR)

tw_data <- fread('~/Data/opinion_dynamics/02c_stance_detection/data/01_raw_data/qanda/qanda_emoji.csv', col.names = c('tid', 'rid' ,'qid', 'uid','name', 'bio', 'date', 'text_extended', 'text', 'hashtags', 'hashtags_extended', 'mentions', 'mentions_extended', 'urls_extended', 'urls'),colClasses = 'character') 
# %>%
#   filter(rid == '' | qid != '')

# tw_data <- tw_data %>%
#   group_by(uid) %>%
#   summarise(text = paste0(text, collapse = " ")) %>%
#   filter(trimws(text) != "")

tw_data %<>% clean_text(text_field = "text", replace_emojis=F, replace_numbers=F, remove_stopwords = F)

tw_data %<>% filter(trimws(text) != "")

dataset<-"qanda"
labelled_data_dir = paste0('/home/roram/Data/opinion_dynamics/02c_stance_detection/data/04_results/labels/',dataset,'.csv')
l_data = fread(labelled_data_dir,colClasses = 'character', header=T)

tw_data %<>% 
  left_join(l_data %>% select(uid, label))

CDS <- list(Catastrophizing = c('will fail', 'will go wrong', 'will end', 'will be impossible', 'will not happen', 'will be terrible', 'will be horrible', 'will be a catastrophe,', 'will be a disaster', 'will never end', 'will not end'),
            `Dichotomous Reasoning` = c('only', 'every', 'everyone', 'everybody', 'everything', 'everywhere', 'always', 'perfect', 'the best', 'all', 'not a single', 'no one', 'nobody,', 'nothing', 'nowhere', 'never', 'worthless', 'the worst', 'neither', 'nor', 'either or', 'black or white', 'ever'),
            `Disqualifying the Positive` = c('great but', 'good but', 'OK but', 'not that great', 'not that good', 'it was not', 'not all that', 'fine but', 'acceptable but', 'great yet,', 'good yet', 'OK yet', 'fine yet', 'acceptable yet'), 
            `Emotional Reasoning` = c('but I feel', 'since I feel', 'because I feel', 'but it feels', 'since it feels', 'because it feels', 'still feels'), 
            `Fortune-telling` = c('I will not', 'we will not', 'you will not', 'they will not', 'it will not', 'that will not', 'he will not', 'she will not'), 
            `Labeling and mislabeling` = c('I am a', 'he is a', 'she is a', 'they are a', 'it is a', 'that is a', 'sucks at', 'suck at', 'I never', 'he never', 'she never', 'you never', 'we never,', 'they never', 'I am an', 'he is an', 'she is an', 'they are an', 'it is an', 'that is an', 'a burden', 'a complete', 'a completely', 'a huge,', 'a loser', 'a major', 'a total', 'a totally', 'a weak', 'an absolute', 'an utter', 'a bad', 'a broken', 'a damaged', 'a helpless', 'a hopeless', 'an incompetent', 'a toxic', 'an ugly', 'an undesirable', 'an unlovable', 'a worthless', 'a horrible', 'a terrible'), 
            `Magnification and Minimization` = c('worst', 'best', 'not important', 'not count', 'not matter', 'no matter', 'the only thing', 'the one thing'), 
            `Mental Filtering` = c('I see only', 'all I see', 'all I can see', 'can only think', 'nothing good', 'nothing right', 'completely bad', 'completely wrong', 'only the bad', 'only the worst', 'if I just', 'if I only', 'if it just', 'if it only'), 
            Mindreading = c('everyone believes', 'everyone knows', 'everyone thinks', 'everyone will believe', 'everyone will know', 'everyone will think,', 'nobody believes', 'nobody knows', 'nobody thinks', 'nobody will believe', 'nobody will know', 'nobody will think', 'he believes,', 'he knows', 'he thinks', 'he does not believe', 'he does not know', 'he does not think', 'he will believe', 'he will know', 'he will think', 'he will not believe', 'he will not know', 'he will not think', 'she believes', 'she knows', 'she thinks', 'she does not believe,', 'she does not know', 'she does not think', 'she will believe', 'she will know', 'she will think', 'she will not believe', 'she will not know', 'she will not think', 'they believe', 'they know', 'they think', 'they do not believe', 'they do not know', 'they do not think,', 'they will believe', 'they will know', 'they will think', 'they will not believe', 'they will not know', 'they will not think', 'we believe', 'we know', 'we think', 'we do not believe', 'we do not know', 'we do not think', 'we will believe', 'we will know', 'we will think', 'we will not believe', 'we will not know', 'we will not think', 'you believe', 'you know', 'you think', 'you do not believe', 'you do not know,', 'you do not think', 'you will believe', 'you will know', 'you will think', 'you will not believe', 'you will not know', 'you will not think'), 
            Overgeneralizing = c('all of the time', 'all of them', 'all the time', 'always happens', 'always like', 'happens every time', 'completely', 'no one ever,', 'nobody ever', 'every single one of them', 'every single one of you', 'I always', 'you always', 'he always', 'she always', 'they always', 'I am always', 'you are always', 'he is always', 'she is always', 'they are always'), 
            Personalizing = c('all me', 'all my', 'because I', 'because my', 'because of my', 'because of me', 'I am responsible', 'blame me', 'I caused,', 'I feel responsible', 'all my doing', 'all my fault', 'my bad', 'my responsibility'), 
            `Should statements` = c('should', 'ought', 'must', 'have to', 'has to'))

CDS_dict <- dictionary(CDS)

text = corpus(tw_data$text, docvars = data.frame(ntoken = ntoken(tw_data$text)))

results = text %>%
  tokens() %>%
  tokens_wordstem() %>%  
  tokens_lookup(dictionary = CDS_dict, nomatch = "nomatch") %>%
  dfm() %>%
  dfm_weight(scheme = "boolean")

results = convert(results, to = "data.frame")

results <- cbind(tw_data[,c('uid','label','text')], results[,2:14])

library(boot)

# cds_key <- 'dichotomous reasoning'
# ideology_label <- 'L'

r <- results %>%
  pivot_longer(!c(uid,label,text), "CDS", "Value") %>%
  # filter(CDS  == cds_key) %>%
  as.data.table()

# ri <- r %>%
#   group_by(CDS,label) %>%
#   summarise(value = mean(value)) %>%
#   ungroup() %>%
#   pivot_wider(names_from = CDS, values_from = value) 
# 
# temp <- as.matrix(ri[2:14], rownames=TRUE)
# rownames(temp) <- c(ri$label)

boot.func <- function(d,i){
  print('tick')
  d2 <- d[i,]
  # temp <- d2 %>%
  #   group_by(label) %>%
  #   summarise(value = mean(value)) %>%
  #   deframe()
  # return(temp)
  
  ri <- d2 %>%
    group_by(CDS,label) %>%
    summarise(value = mean(value)) %>%
    ungroup() %>%
    pivot_wider(names_from = CDS, values_from = value) 
  
  temp <- as.matrix(ri[2:14], rownames=TRUE)
  rownames(temp) <- c(ri$label)
  return(temp)
}

set.seed(42)

library(parallel)
library(broom)
# library(progress)
replicates <- 100

# pb <- progress_bar$new(total = replicates + 1)
# X <- 1:1e7
# rbenchmark::benchmark(serial=sapply(X, function(x) log(sqrt(x))), parallel=mclapply(X, function(x) log(sqrt(x))), replications=10)
bootstrap_means <- boot(r,boot.func,R=replicates ,parallel = "multicore", ncpus=36)
Marr <- array(bootstrap_means$t, dim=c(replicates, 4, 13))
rownames(Marr) <- 1:replicates
colnames(Marr) <- rownames(bootstrap_means$t0)
# matrix.names(Marr) <- colnames(bootstrap_means$t0)
dimnames(Marr)[[3]] <- colnames(bootstrap_means$t0)

dt <- as.data.table(Marr)

colnames(dt) <- c('sample_id', 'label', 'CDS', 'value')

cols <- c( "L" = "blue","N" ="purple", "R" = "red", "FR" = "darkred")
dt %>%
  mutate(label = fct_relevel(label, 'L', 'N', 'R', 'FR'))%>%
  filter(CDS != 'nomatch')%>%
  ggplot(aes(y=value, x=label, color=label))+
  geom_boxplot()+
  geom_jitter(alpha=0.1)+
  # stat_compare_means()+
  facet_wrap(~CDS, scales = 'free')+
  scale_color_manual(values = cols) +
  labs(x='Ideology', y='CDS Prevalence') +
  theme_cowplot() +
  guides(color="none")

my_comparisons <- list(c("FR", "R"), c("FR", "L"), c("L", "R"), c("L", "FR") )

dt %>%
  filter(CDS == 'dichotomous reasoning')%>%
  mutate(label = fct_relevel(label, 'L', 'N', 'R', 'FR'))%>%
  ggplot(aes(y=value, x=label, color=label))+
  geom_jitter(aes(y=value, x=label, color=label), alpha=0.1)+
  geom_boxplot()+
  # facet_wrap(~CDS, scales = 'free')+
  stat_compare_means(label.y = 0.08)+
  # stat_compare_means(aes(label = after_stat(p.signif)), method = "t.test", ref.group = "N")+
  scale_color_manual(values = cols) +
  theme_cowplot() +
  labs(x='Ideology', y='Dichotomous Thinking CDS') +
  guides(color=FALSE)
