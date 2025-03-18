# devtools::install_github("thieled/dictvectoR")
library(dictvectoR)
library(dplyr)
library(magrittr)
library(data.table)

library(ggplot2)
library(cowplot)
library(tidyverse)


tw_data <- fread('~/Data/opinion_dynamics/02c_stance_detection/data/01_raw_data/qanda/qanda_emoji.csv', col.names = c('tid', 'rid' ,'qid', 'uid','name', 'bio', 'date', 'text_extended', 'text', 'hashtags', 'hashtags_extended', 'mentions', 'mentions_extended', 'urls_extended', 'urls'),colClasses = 'character') %>%
  filter(rid == '' | qid != '')
tw_data <- tw_data %>%
  group_by(uid) %>%
  summarise(text = paste0(text, collapse = " ")) %>%
  filter(trimws(text) != "")

tw_data %<>% clean_text(text_field = "text", replace_emojis=F, replace_numbers=F, remove_stopwords = F)

tw_data %<>% filter(trimws(text) != "")

texts <- prepare_train_data(tw_data, text_field = "text", seed = 42)

# clean_text(
#   df,
#   text_field = "text",
#   clean_field = "text",
#   tolower = T,
#   remove_punct = T,
#   simplify_punct = F,
#   replace_emojis = T,
#   replace_numbers = T,
#   remove_stopwords = F,
#   store_uncleaned = T,
#   count = T
# )

# Create local folder, set model file name
dir.create("~/ddr", showWarnings = FALSE)
model_file <- path.expand("~/ddr/ft_model_demo")

# Train a fasttext model using the twitter data
fastrtext::build_vectors(texts, 
                         model_file, 
                         modeltype = c("skipgram"),
                         dim = 70, epoch = 5, bucket = 5e+4, lr = 0.1,
                         maxn = 6,  minn = 4, minCount = 4,
                         verbose = 1, ws= 5)

# Load model:
model_path <- path.expand("~/ddr/ft_model_demo.bin")
model <- fastrtext::load_model(paste0(model_path))

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

for (k in names(CDS)) {
  print(k)
  # print(CDS[[k]])
  tw_data[[k]] <- cossim2dict(tw_data, CDS[[k]], model, replace_na = 'mean-sd')
}

dataset<-"qanda"
labelled_data_dir = paste0('/home/roram/Data/opinion_dynamics/02c_stance_detection/data/04_results/labels/',dataset,'.csv')
l_data = fread(labelled_data_dir,colClasses = 'character', header=T)

h_data <- tw_data[,c(1,2,6:17)] %>%
  pivot_longer(!c(uid,text), "CDS", "Value") %>%
  left_join(l_data %>% select(uid, label))

h_data %>%
  ggplot()+
  geom_density(aes(x=value, color=label)) +
  facet_wrap(~ CDS) +
  theme_cowplot()

h_data %>%
  ggplot()+
  geom_histogram(aes(x=value, color=label), bins=100) +
  facet_wrap(~ CDS) +
  theme_cowplot()

h_data %>%
  filter(CDS == 'Dichotomous Reasoning', label=='FR') %>%
  ggplot()+
  geom_histogram(aes(x=value, color=label), bins=100) +
  facet_wrap(~ label, scales = 'free') +
  theme_cowplot()

tep <- h_data %>%
  group_by(CDS, label) %>%
  summarise(m = mean(value))

temp<-h_data %>%
  filter(CDS == 'Dichotomous Reasoning', value>0.56,label=='FR') %>%
  arrange(value)

h_data %>%
  ggplot()+
  geom_boxplot(aes(x=label, y=`Dichotomous Reasoning`, color=label)) +
  theme_cowplot()


p <- ggboxplot(h_data, x = "Dichotomous Reasoning", y = "label",
               color = "Dichotomous Reasoning", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
               add = "jitter", shape = "Dichotomous Reasoning")
p

tw_data %>%
  dplyr::arrange(desc(dich_ddr)) %>%
  head(3) %>%
  dplyr::pull(text)

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

summary(bootstrap_means)

bootstrap_means_df <- tidy(bootstrap_means)
bootstrap_means$t[1,]

apply(Marr, c(2,3), median)

bootstrap_means$t0

bootstrap_means$t0
bootstrap_means$t0

ci <- boot.ci(boot.out=bootstrap_correlation,type=c('norm'), t=bootstrap_correlation$t, t0=bootstrap_correlation$t0)


data <- results[,lapply(.SD, mean), by=label, .SDcols=names(results)[4:16]] %>%
  pivot_longer(!label, "CDS", "Value")

# data <- results[,lapply(.SD, mean), by=uid, .SDcols=names(results)[4:16]] %>%
#   pivot_longer(!uid, "CDS", "Value") %>%
#   left_join(l_data %>% select(uid, label))

data %>%
  ggplot()+
  geom_density(aes(x=value, color=label)) +
  facet_wrap(~ CDS) +
  theme_cowplot()

h_data %>%
  ggplot()+
  geom_histogram(aes(x=value, color=label)) +
  facet_wrap(~ CDS) +
  theme_cowplot()  

data %>%
  filter(value > 1)

data %>%
  ggplot() +
  geom_boxplot(aes(y=value, color=label, x=label)) +
  facet_wrap(~ CDS, scale='free')+
  theme_cowplot()

temp <- data %>% filter(CDS == 'dichotomous reasoning')

temp %>%
  group_by(label) %>%
  summarise(m = mean(value))

mean(temp$value == 0)

temp %>%
  ggplot()+
  geom_density(aes(x=value, color=label)) +
  scale_x_log10()+
  theme_cowplot()

p <- ggboxplot(temp, x = "label", y = "value",
               color = "label",
               shape = "label")
p +
  scale_y_log10()
