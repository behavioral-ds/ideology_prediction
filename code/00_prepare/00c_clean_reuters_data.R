library(haven)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(cowplot)
library(stringr)

###### AUSTRALIA 2020
Reuters_2020_Australia <-
  read_sav("~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_2020/Reuters_2020 - Australia.sav")

# Brands Q5B

brand_vars <-
  names(Reuters_2020_Australia)

brand_vars <-
  brand_vars[grepl("^Q5B\\d",brand_vars)]

getLabel <- function(var) {
  return(attr(Reuters_2020_Australia[[var]], "label"))
}

# getLabel("Q1F")

brand.df <-
  data.frame(var = brand_vars,
             label = sapply(brand_vars, getLabel, simplify = T, USE.NAMES = F))

brand.df$domain <- c("bbc.com","cnn.com","nytimes.com","buzzfeed.com","vice.com","huffpost.com","dailymail.co.uk","nine.com.au","7news.com.au","news.com.au","abc.net.au","skynews.com.au","theage.com.au","smh.com.au","theconversation.com","crikey.com.au","10daily.com.au","theaustralian.com.au","dailytelegraph.com.au","heraldsun.com.au","couriermail.com.au","adelaidenow.com.au","afr.com","junkee.com","thesaturdaypaper.com.au",NA,"perthnow.com.au","brisbanetimes.com.au","thenewdaily.com.au","theguardian.com",NA,NA,NA,NA,NA,NA)

# Pol-orientation Q1F

library(tidyverse)

library(scales)

brand_mean <-  Reuters_2020_Australia[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(weight = read / sum(read)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::group_by(brand) %>%
  dplyr::summarise(mean_Q1F = weighted.mean(Q1F, weight = weight),
                   # mean_Q6 = weighted.mean(Q6_2016_Q6_2016_1_Q6_2016_grid, weight = weight),
                   p9_Q1F = quantile(Q1F, p = .75),
                   p1_Q1F = quantile(Q1F, p = .25),) %>%
  dplyr::arrange(mean_Q1F) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)],
                brand_domain = factor(brand_domain, levels = brand_domain, ordered = T))

library(ggridges)

temp <- Reuters_2020_Australia[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)]) %>%
  select(Q1F, id, brand_domain) 

temp %>%
  group_by(brand_domain) %>%
  count()

temp %>%
  ggplot() +
  geom_boxplot(aes(y=brand_domain, x=Q1F))+
  theme_cowplot()

library(forcats)

mean_publication_ideology <- mean(temp$Q1F)
sd_publication_ideology <- sd(temp$Q1F)

mean_overall_ideology <- mean((Reuters_2020_Australia %>% filter(Q1F < 8))$Q1F)
sd_overall_ideology <- sd((Reuters_2020_Australia %>% filter(Q1F < 8))$Q1F)

temp %>%
  group_by(brand_domain) %>%
  summarise(mean_ideology = mean(Q1F), sd=sd(Q1F))%>%
  mutate(brand_domain = forcats::fct_reorder(brand_domain, mean_ideology)) %>%
  ggplot(aes(y=brand_domain, x=mean_ideology)) + 
  geom_errorbar(aes(xmin=mean_ideology-sd, xmax=mean_ideology+sd), width=.1)+
  geom_point() +
  geom_vline(xintercept=mean_publication_ideology, linetype="longdash", color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology+sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology-sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_overall_ideology, linetype="longdash", color="#399283")+
  geom_vline(xintercept=mean_overall_ideology+sd_overall_ideology, linetype="dotted",color="#399283")+
  geom_vline(xintercept=mean_overall_ideology-sd_overall_ideology, linetype="dotted",color="#399283")+
  theme_cowplot() +
  labs(y='Publisher URL', x='Mean Ideology')

reuters_2020_au_news_lr.df <-
  brand_mean %>%
  dplyr::mutate(mean_Q1F = mean_Q1F - 4,
                domain = brand_domain) %>%
  dplyr::select(domain, lr = mean_Q1F)

reuters_2020_respondents.df <-
  Reuters_2020_Australia[c(brand.df$var, "Q1F")]

library(data.table)
fwrite(reuters_2020_au_news_lr.df, "~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_processed_data/reuters_2020_au_news_lr.csv")


##### AUSTRALIA 2021

Reuters_2021_Australia <- read_sav("~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_2021/Reuters DNR 2021 - Australia.sav")

# Brands Q5B

brand_vars <- names(Reuters_2021_Australia)

brand_vars <- brand_vars[grepl("^Q5B\\d",brand_vars)]

getLabel <- function(var) {
  return(attr(Reuters_2021_Australia[[var]], "label"))
}

# getLabel("Q1_aNEW")
# getLabel("Q1b_NEW")
# getLabel("Q1F")
# getLabel("Q6_2016_1")
# getLabel("Q6_2016_6")
# getLabel("Q6_2018_2")
# getLabel("Q6_2018_3")
# getLabel("Q6_2018_trust_rb_148")
# 
# getLabel2020("Q6_2016_Q6_2016_1_Q6_2016_grid")


brand.df <- data.frame(var = brand_vars, label = sapply(brand_vars, getLabel, simplify = T, USE.NAMES = F))

brand.df$domain <- c("bbc.com","cnn.com","nytimes.com","dailymail.co.uk","nine.com.au","7news.com.au","news.com.au","abc.net.au","skynews.com.au","theage.com.au","smh.com.au","theconversation.com","crikey.com.au","10daily.com.au","theaustralian.com.au","dailytelegraph.com.au","heraldsun.com.au","couriermail.com.au","adelaidenow.com.au","afr.com","thesaturdaypaper.com.au",NA,"perthnow.com.au","brisbanetimes.com.au","thenewdaily.com.au","theguardian.com",NA,NA,NA,NA,NA,NA)

# Pol-orientation Q1F

library(tidyverse)
library(scales)
library(ggridges)
library(forcats)
library(data.table)

brand_mean <-  Reuters_2021_Australia[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(weight = read / sum(read)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::group_by(brand) %>%
  dplyr::summarise(mean_Q1F = weighted.mean(Q1F, weight = weight),
                   p9_Q1F = quantile(Q1F, p = .75),
                   p1_Q1F = quantile(Q1F, p = .25),) %>%
  dplyr::arrange(mean_Q1F) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)],
                brand_domain = factor(brand_domain, levels = brand_domain, ordered = T))

temp <- Reuters_2021_Australia[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)]) %>%
  select(Q1F, id, brand_domain)

mean_publication_ideology <- mean(temp$Q1F)
sd_publication_ideology <- sd(temp$Q1F)

mean_overall_ideology <- mean((Reuters_2021_Australia %>% filter(Q1F < 8))$Q1F)
sd_overall_ideology <- sd((Reuters_2021_Australia %>% filter(Q1F < 8))$Q1F)

temp %>%
  group_by(brand_domain) %>%
  summarise(mean_ideology = mean(Q1F), sd=sd(Q1F))%>%
  mutate(brand_domain = forcats::fct_reorder(brand_domain, mean_ideology)) %>%
  ggplot(aes(y=brand_domain, x=mean_ideology)) + 
  geom_errorbar(aes(xmin=mean_ideology-sd, xmax=mean_ideology+sd), width=.1)+
  geom_point() +
  geom_vline(xintercept=mean_publication_ideology, linetype="longdash", color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology+sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology-sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_overall_ideology, linetype="longdash", color="#399283")+
  geom_vline(xintercept=mean_overall_ideology+sd_overall_ideology, linetype="dotted",color="#399283")+
  geom_vline(xintercept=mean_overall_ideology-sd_overall_ideology, linetype="dotted",color="#399283")+
  theme_cowplot() +
  labs(y='Publisher URL', x='Mean Ideology')

reuters_2021_au_news_lr.df <-
  brand_mean %>%
  dplyr::mutate(mean_Q1F = mean_Q1F - 4,
                domain = brand_domain) %>%
  dplyr::select(domain, lr = mean_Q1F)

fwrite(reuters_2021_au_news_lr.df, "~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_processed_data/reuters_2021_au_news_lr.csv")

##### US 2020

Reuters_2020_USA <- read_sav("~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_2020/Reuters_2020 - USA.sav")

# Brands Q5B

brand_vars <- names(Reuters_2020_USA)

brand_vars <- brand_vars[grepl("^Q5B\\d",brand_vars)]

getLabel <- function(var) {
  return(attr(Reuters_2020_USA[[var]], "label"))
}


brand.df <- data.frame(var = brand_vars, label = sapply(brand_vars, getLabel, simplify = T, USE.NAMES = F))

brand.df$domain <- c("bbc.com", "cnn.com", "nytimes.com", "buzzfeed.com", "vice.com", "theguardian.com", "huffpost.com", "dailymail.co.uk", "msn.com", "news.yahoo.com", "msnbc.com", "abcnews.go.com", "cbsnews.com", "foxnews.com", NA, "wsj.com", "usatoday.com", "latimes.com", NA, "washingtonpost.com", NA, "upworthy.com", NA, "vox.com", "aljazeera.com", "aol.com", "mashable.com", "ajplus.net", "npr.org", "newyorker.com", "slate.com", "democracynow.org", "vanityfair.com", "nationalgeographic.com", NA, NA, NA, "salon.com", NA, NA, NA, NA, NA, NA)

# Pol-orientation Q1F

library(tidyverse)
library(scales)
library(ggridges)
library(forcats)
library(data.table)

brand_mean <-  Reuters_2020_USA[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(weight = read / sum(read)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::group_by(brand) %>%
  dplyr::summarise(mean_Q1F = weighted.mean(Q1F, weight = weight),
                   p9_Q1F = quantile(Q1F, p = .75),
                   p1_Q1F = quantile(Q1F, p = .25),) %>%
  dplyr::arrange(mean_Q1F) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)],
                brand_domain = factor(brand_domain, levels = brand_domain, ordered = T))

temp <- Reuters_2020_USA[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)]) %>%
  select(Q1F, id, brand_domain)

mean_publication_ideology <- mean(temp$Q1F)
sd_publication_ideology <- sd(temp$Q1F)

mean_overall_ideology <- mean((Reuters_2020_USA %>% filter(Q1F < 8))$Q1F)
sd_overall_ideology <- sd((Reuters_2020_USA %>% filter(Q1F < 8))$Q1F)

temp %>%
  group_by(brand_domain) %>%
  summarise(mean_ideology = mean(Q1F), sd=sd(Q1F))%>%
  mutate(brand_domain = forcats::fct_reorder(brand_domain, mean_ideology)) %>%
  ggplot(aes(y=brand_domain, x=mean_ideology)) + 
  geom_errorbar(aes(xmin=mean_ideology-sd, xmax=mean_ideology+sd), width=.1)+
  geom_point() +
  geom_vline(xintercept=mean_publication_ideology, linetype="longdash", color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology+sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology-sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_overall_ideology, linetype="longdash", color="#399283")+
  geom_vline(xintercept=mean_overall_ideology+sd_overall_ideology, linetype="dotted",color="#399283")+
  geom_vline(xintercept=mean_overall_ideology-sd_overall_ideology, linetype="dotted",color="#399283")+
  theme_cowplot() +
  labs(y='Publisher URL', x='Mean Ideology')

reuters_2020_us_news_lr.df <-
  brand_mean %>%
  dplyr::mutate(mean_Q1F = mean_Q1F - 4,
                domain = brand_domain) %>%
  dplyr::select(domain, lr = mean_Q1F)

fwrite(reuters_2020_us_news_lr.df, "~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_processed_data/reuters_2020_us_news_lr.csv")

##### UK 2020
Reuters_2020_UK <- read_sav("~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_2020/Reuters_2020 - UK.sav")

# Brands Q5B

brand_vars <- names(Reuters_2020_UK)

brand_vars <- brand_vars[grepl("^Q5B\\d",brand_vars)]

getLabel <- function(var) {
  return(attr(Reuters_2020_UK[[var]], "label"))
}


brand.df <- data.frame(var = brand_vars, label = sapply(brand_vars, getLabel, simplify = T, USE.NAMES = F))

brand.df$domain <- c("bbc.com", "cnn.com", "nytimes.com", "buzzfeed.com", "vice.com", "theguardian.com", "huffpost.com", "dailymail.co.uk", "msn.com", "news.yahoo.com", "itv.com", "news.sky.com", "channel4.com", "channel5.com", "thesun.co.uk", "mirror.co.uk", "thetimes.co.uk", "telegraph.co.uk", "express.co.uk", "indy100.com", "ft.com", "metro.co.uk", "economist.com", "scotsman.com", "heraldscotland.com", "dailyrecord.co.uk", "walesonline.co.uk", "irishnews.com", "newsletter.co.uk", "belfasttelegraph.co.uk", "standard.co.uk", "ladbible.com", "inews.co.uk",  NA,  NA,  NA,  NA,  NA,  NA)

# Pol-orientation Q1F

library(tidyverse)
library(scales)
library(ggridges)
library(forcats)
library(data.table)

brand_mean <-  Reuters_2020_UK[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(weight = read / sum(read)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::group_by(brand) %>%
  dplyr::summarise(mean_Q1F = weighted.mean(Q1F, weight = weight),
                   p9_Q1F = quantile(Q1F, p = .75),
                   p1_Q1F = quantile(Q1F, p = .25),) %>%
  dplyr::arrange(mean_Q1F) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)],
                brand_domain = factor(brand_domain, levels = brand_domain, ordered = T))

temp <- Reuters_2020_UK[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)]) %>%
  select(Q1F, id, brand_domain)

mean_publication_ideology <- mean(temp$Q1F)
sd_publication_ideology <- sd(temp$Q1F)

mean_overall_ideology <- mean((Reuters_2020_UK %>% filter(Q1F < 8))$Q1F)
sd_overall_ideology <- sd((Reuters_2020_UK %>% filter(Q1F < 8))$Q1F)

temp %>%
  group_by(brand_domain) %>%
  summarise(mean_ideology = mean(Q1F), sd=sd(Q1F))%>%
  mutate(brand_domain = forcats::fct_reorder(brand_domain, mean_ideology)) %>%
  ggplot(aes(y=brand_domain, x=mean_ideology)) + 
  geom_errorbar(aes(xmin=mean_ideology-sd, xmax=mean_ideology+sd), width=.1)+
  geom_point() +
  geom_vline(xintercept=mean_publication_ideology, linetype="longdash", color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology+sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology-sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_overall_ideology, linetype="longdash", color="#399283")+
  geom_vline(xintercept=mean_overall_ideology+sd_overall_ideology, linetype="dotted",color="#399283")+
  geom_vline(xintercept=mean_overall_ideology-sd_overall_ideology, linetype="dotted",color="#399283")+
  theme_cowplot() +
  labs(y='Publisher URL', x='Mean Ideology')

reuters_2020_uk_news_lr.df <-
  brand_mean %>%
  dplyr::mutate(mean_Q1F = mean_Q1F - 4,
                domain = brand_domain) %>%
  dplyr::select(domain, lr = mean_Q1F)

fwrite(reuters_2020_uk_news_lr.df, "~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_processed_data/reuters_2020_uk_news_lr.csv")


##### Canada 2020
Reuters_2020_CA <- read_sav("~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_2020/Reuters_2020 - Canada.sav")

# Brands Q5B

brand_vars <- names(Reuters_2020_CA)

brand_vars <- brand_vars[grepl("^Q5B\\d",brand_vars)]

getLabel <- function(var) {
  return(attr(Reuters_2020_CA[[var]], "label"))
}


brand.df <- data.frame(var = brand_vars, label = sapply(brand_vars, getLabel, simplify = T, USE.NAMES = F))

brand.df$domain <- c("bbc.com", "cnn.com", "nytimes.com", "buzzfeed.com", "vice.com", "theguardian.com", "huffpost.com", "dailymail.co.uk", "msn.com", "news.yahoo.com", "msnbc.com", "foxnews.com", "wsj.com", "washingtonpost.com",  NA, "cbc.ca", "ctvnews.ca", "globalnews.ca", "citynews.ca", "theglobeandmail.com", "nationalpost.com", "macleans.ca", "economist.com", "thestar.com", "vancouversun.com",  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA, "rebelnews.com", "nationalobserver.com", "ipolitics.ca", "theconversation.com", "thelogic.co", "cp24.com",  NA,  NA,  NA,  NA,  NA,  NA)

# Pol-orientation Q1F

library(tidyverse)
library(scales)
library(ggridges)
library(forcats)
library(data.table)

brand_mean <-  Reuters_2020_CA[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(weight = read / sum(read)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::group_by(brand) %>%
  dplyr::summarise(mean_Q1F = weighted.mean(Q1F, weight = weight),
                   p9_Q1F = quantile(Q1F, p = .75),
                   p1_Q1F = quantile(Q1F, p = .25),) %>%
  dplyr::arrange(mean_Q1F) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)],
                brand_domain = factor(brand_domain, levels = brand_domain, ordered = T))

temp <- Reuters_2020_CA[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)]) %>%
  select(Q1F, id, brand_domain)

mean_publication_ideology <- mean(temp$Q1F)
sd_publication_ideology <- sd(temp$Q1F)

mean_overall_ideology <- mean((Reuters_2020_CA %>% filter(Q1F < 8))$Q1F)
sd_overall_ideology <- sd((Reuters_2020_CA %>% filter(Q1F < 8))$Q1F)

temp %>%
  group_by(brand_domain) %>%
  summarise(mean_ideology = mean(Q1F), sd=sd(Q1F))%>%
  mutate(brand_domain = forcats::fct_reorder(brand_domain, mean_ideology)) %>%
  ggplot(aes(y=brand_domain, x=mean_ideology)) + 
  geom_errorbar(aes(xmin=mean_ideology-sd, xmax=mean_ideology+sd), width=.1)+
  geom_point() +
  geom_vline(xintercept=mean_publication_ideology, linetype="longdash", color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology+sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology-sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_overall_ideology, linetype="longdash", color="#399283")+
  geom_vline(xintercept=mean_overall_ideology+sd_overall_ideology, linetype="dotted",color="#399283")+
  geom_vline(xintercept=mean_overall_ideology-sd_overall_ideology, linetype="dotted",color="#399283")+
  theme_cowplot() +
  labs(y='Publisher URL', x='Mean Ideology')

reuters_2020_ca_news_lr.df <-
  brand_mean %>%
  dplyr::mutate(mean_Q1F = mean_Q1F - 4,
                domain = brand_domain) %>%
  dplyr::select(domain, lr = mean_Q1F)

fwrite(reuters_2020_ca_news_lr.df, "~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_processed_data/reuters_2020_ca_news_lr.csv")


##### US 2021

Reuters_2021_USA <- read_sav("~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_2021/Reuters DNR 2021 - US.sav")

# Brands Q5B

brand_vars <- names(Reuters_2021_USA)

brand_vars <- brand_vars[grepl("^Q5B\\d",brand_vars)]

getLabel <- function(var) {
  return(attr(Reuters_2021_USA[[var]], "label"))
}


brand.df <- data.frame(var = brand_vars, label = sapply(brand_vars, getLabel, simplify = T, USE.NAMES = F))

brand.df$domain <- c("bbc.com", "cnn.com", "nytimes.com", "buzzfeed.com", "vice.com", "theguardian.com", "huffpost.com", "msn.com", "news.yahoo.com", "msnbc.com", "abcnews.go.com", "cbsnews.com", "foxnews.com",  NA, "wsj.com", "usatoday.com", "latimes.com",  NA, "washingtonpost.com",  NA,  NA, "vox.com", "aljazeera.com", "breitbart.com", "occupydemocrats.com", "npr.org", "newyorker.com", "slate.com",  NA,  NA, "people.com", "theblaze.com", "dailycaller.com", "oann.com", "newsmax.com", "theepochtimes.com", "axios.com", "nypost.com", "today.com",  NA,  NA,  NA,  NA,  NA,  NA)
# Pol-orientation Q1F

library(tidyverse)
library(scales)
library(ggridges)
library(forcats)
library(data.table)

brand_mean <-  Reuters_2021_USA[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(weight = read / sum(read)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::group_by(brand) %>%
  dplyr::summarise(mean_Q1F = weighted.mean(Q1F, weight = weight),
                   p9_Q1F = quantile(Q1F, p = .75),
                   p1_Q1F = quantile(Q1F, p = .25),) %>%
  dplyr::arrange(mean_Q1F) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)],
                brand_domain = factor(brand_domain, levels = brand_domain, ordered = T))

temp <- Reuters_2021_USA[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)]) %>%
  select(Q1F, id, brand_domain)

mean_publication_ideology <- mean(temp$Q1F)
sd_publication_ideology <- sd(temp$Q1F)

mean_overall_ideology <- mean((Reuters_2021_USA %>% filter(Q1F < 8))$Q1F)
sd_overall_ideology <- sd((Reuters_2021_USA %>% filter(Q1F < 8))$Q1F)

temp %>%
  group_by(brand_domain) %>%
  summarise(mean_ideology = mean(Q1F), sd=sd(Q1F))%>%
  mutate(brand_domain = forcats::fct_reorder(brand_domain, mean_ideology)) %>%
  ggplot(aes(y=brand_domain, x=mean_ideology)) + 
  geom_errorbar(aes(xmin=mean_ideology-sd, xmax=mean_ideology+sd), width=.1)+
  geom_point() +
  geom_vline(xintercept=mean_publication_ideology, linetype="longdash", color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology+sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology-sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_overall_ideology, linetype="longdash", color="#399283")+
  geom_vline(xintercept=mean_overall_ideology+sd_overall_ideology, linetype="dotted",color="#399283")+
  geom_vline(xintercept=mean_overall_ideology-sd_overall_ideology, linetype="dotted",color="#399283")+
  theme_cowplot() +
  labs(y='Publisher URL', x='Mean Ideology')

reuters_2021_us_news_lr.df <-
  brand_mean %>%
  dplyr::mutate(mean_Q1F = mean_Q1F - 4,
                domain = brand_domain) %>%
  dplyr::select(domain, lr = mean_Q1F)

fwrite(reuters_2021_us_news_lr.df, "~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_processed_data/reuters_2021_us_news_lr.csv")


##### UK 2021
Reuters_2021_UK <- read_sav("~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_2021/Reuters DNR 2021 - UK.sav")

# Brands Q5B

brand_vars <- names(Reuters_2021_UK)

brand_vars <- brand_vars[grepl("^Q5B\\d",brand_vars)]

getLabel <- function(var) {
  return(attr(Reuters_2021_UK[[var]], "label"))
}


brand.df <- data.frame(var = brand_vars, label = sapply(brand_vars, getLabel, simplify = T, USE.NAMES = F))

brand.df$domain <- c("bbc.com",  "cnn.com",  "nytimes.com",  "buzzfeed.com",  "vice.com",  "theguardian.com",  "huffpost.com",  "dailymail.co.uk",  "msn.com",  "news.yahoo.com",  "itv.com",  "news.sky.com",  "channel4.com",  "channel5.com",  "thesun.co.uk",  "mirror.co.uk",  "thetimes.co.uk",  "telegraph.co.uk",  "express.co.uk",  "indy100.com",  "ft.com",  "metro.co.uk",  "economist.com",  "scotsman.com",  "heraldscotland.com",  "dailyrecord.co.uk",  "walesonline.co.uk",  "irishnews.com",  "newsletter.co.uk",  "belfasttelegraph.co.uk",  "standard.co.uk",  "ladbible.com",  "inews.co.uk",  "rt.com",  "thecanary.co",  "tortoisemedia.com",   NA,   NA,   NA,   NA,   NA,   NA)

# Pol-orientation Q1F

library(tidyverse)
library(scales)
library(ggridges)
library(forcats)
library(data.table)

brand_mean <-  Reuters_2021_UK[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(weight = read / sum(read)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::group_by(brand) %>%
  dplyr::summarise(mean_Q1F = weighted.mean(Q1F, weight = weight),
                   p9_Q1F = quantile(Q1F, p = .75),
                   p1_Q1F = quantile(Q1F, p = .25),) %>%
  dplyr::arrange(mean_Q1F) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)],
                brand_domain = factor(brand_domain, levels = brand_domain, ordered = T))

temp <- Reuters_2021_UK[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)]) %>%
  select(Q1F, id, brand_domain)

mean_publication_ideology <- mean(temp$Q1F)
sd_publication_ideology <- sd(temp$Q1F)

mean_overall_ideology <- mean((Reuters_2021_UK %>% filter(Q1F < 8))$Q1F)
sd_overall_ideology <- sd((Reuters_2021_UK %>% filter(Q1F < 8))$Q1F)

temp %>%
  group_by(brand_domain) %>%
  summarise(mean_ideology = mean(Q1F), sd=sd(Q1F))%>%
  mutate(brand_domain = forcats::fct_reorder(brand_domain, mean_ideology)) %>%
  ggplot(aes(y=brand_domain, x=mean_ideology)) + 
  geom_errorbar(aes(xmin=mean_ideology-sd, xmax=mean_ideology+sd), width=.1)+
  geom_point() +
  geom_vline(xintercept=mean_publication_ideology, linetype="longdash", color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology+sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology-sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_overall_ideology, linetype="longdash", color="#399283")+
  geom_vline(xintercept=mean_overall_ideology+sd_overall_ideology, linetype="dotted",color="#399283")+
  geom_vline(xintercept=mean_overall_ideology-sd_overall_ideology, linetype="dotted",color="#399283")+
  theme_cowplot() +
  labs(y='Publisher URL', x='Mean Ideology')

reuters_2021_uk_news_lr.df <-
  brand_mean %>%
  dplyr::mutate(mean_Q1F = mean_Q1F - 4,
                domain = brand_domain) %>%
  dplyr::select(domain, lr = mean_Q1F)

fwrite(reuters_2021_uk_news_lr.df, "~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_processed_data/reuters_2021_uk_news_lr.csv")


##### Canada 2021
Reuters_2021_CA <- read_sav("~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_2021/Reuters DNR 2021 - Canada.sav")

# Brands Q5B

brand_vars <- names(Reuters_2021_CA)

brand_vars <- brand_vars[grepl("^Q5B\\d",brand_vars)]

getLabel <- function(var) {
  return(attr(Reuters_2021_CA[[var]], "label"))
}


brand.df <- data.frame(var = brand_vars, label = sapply(brand_vars, getLabel, simplify = T, USE.NAMES = F))

brand.df$domain <- c("bbc.com",  "cnn.com",  "nytimes.com",  "buzzfeed.com",  "vice.com",  "theguardian.com",  "huffpost.com",  "dailymail.co.uk",  "msn.com",  "news.yahoo.com",  "msnbc.com",  "foxnews.com",  "wsj.com",  "washingtonpost.com",   NA,  "cbc.ca",  "ctvnews.ca",  "globalnews.ca",  "citynews.ca",  "theglobeandmail.com",  "nationalpost.com",  "macleans.ca",  "economist.com",  "thestar.com",  "vancouversun.com",   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,   NA,  "rebelnews.com",  "ipolitics.ca",  "cp24.com",   NA,  "narcity.com",  "theprovince.com",  "theloopnewspaper.com",  "dailyhive.com",   NA,   NA,   NA,   NA,   NA,   NA)

# Pol-orientation Q1F

library(tidyverse)
library(scales)
library(ggridges)
library(forcats)
library(data.table)

brand_mean <-  Reuters_2021_CA[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(weight = read / sum(read)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::group_by(brand) %>%
  dplyr::summarise(mean_Q1F = weighted.mean(Q1F, weight = weight),
                   p9_Q1F = quantile(Q1F, p = .75),
                   p1_Q1F = quantile(Q1F, p = .25),) %>%
  dplyr::arrange(mean_Q1F) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)],
                brand_domain = factor(brand_domain, levels = brand_domain, ordered = T))

temp <- Reuters_2021_CA[c(brand.df$var, "Q1F")] %>%
  dplyr::mutate(id = 1:n()) %>%
  tidyr::pivot_longer(Q5B01:Q5B30, names_to = 'brand', values_to = 'read') %>%
  dplyr::filter(read > 0 & Q1F < 8) %>%
  dplyr::mutate(brand_domain = brand.df$domain[match(brand, brand.df$var)]) %>%
  select(Q1F, id, brand_domain)

mean_publication_ideology <- mean(temp$Q1F)
sd_publication_ideology <- sd(temp$Q1F)

mean_overall_ideology <- mean((Reuters_2021_CA %>% filter(Q1F < 8))$Q1F)
sd_overall_ideology <- sd((Reuters_2021_CA %>% filter(Q1F < 8))$Q1F)

temp %>%
  group_by(brand_domain) %>%
  summarise(mean_ideology = mean(Q1F), sd=sd(Q1F))%>%
  mutate(brand_domain = forcats::fct_reorder(brand_domain, mean_ideology)) %>%
  ggplot(aes(y=brand_domain, x=mean_ideology)) + 
  geom_errorbar(aes(xmin=mean_ideology-sd, xmax=mean_ideology+sd), width=.1)+
  geom_point() +
  geom_vline(xintercept=mean_publication_ideology, linetype="longdash", color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology+sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_publication_ideology-sd_publication_ideology, linetype="dotted",color="#c9245d")+
  geom_vline(xintercept=mean_overall_ideology, linetype="longdash", color="#399283")+
  geom_vline(xintercept=mean_overall_ideology+sd_overall_ideology, linetype="dotted",color="#399283")+
  geom_vline(xintercept=mean_overall_ideology-sd_overall_ideology, linetype="dotted",color="#399283")+
  theme_cowplot() +
  labs(y='Publisher URL', x='Mean Ideology')

reuters_2021_ca_news_lr.df <-
  brand_mean %>%
  dplyr::mutate(mean_Q1F = mean_Q1F - 4,
                domain = brand_domain) %>%
  dplyr::select(domain, lr = mean_Q1F)

fwrite(reuters_2021_ca_news_lr.df, "~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/reuters_processed_data/reuters_2021_ca_news_lr.csv")


###### Combine the reuters and allsides datasets

# allsides_mine <- fread("~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/all_allsides_media_bias.csv")

reuters <- rbind(
(reuters_2021_au_news_lr.df %>% mutate(year=2021, country='AU', weight=20)),
(reuters_2021_us_news_lr.df %>% mutate(year=2021, country='US', weight=2)),
(reuters_2021_uk_news_lr.df %>% mutate(year=2021, country='UK', weight=1)),
(reuters_2021_ca_news_lr.df %>% mutate(year=2021, country='CA', weight=1)),
(reuters_2020_au_news_lr.df %>% mutate(year=2020, country='AU', weight=20)),
(reuters_2020_us_news_lr.df %>% mutate(year=2020, country='US', weight=2)),
(reuters_2020_uk_news_lr.df %>% mutate(year=2020, country='UK', weight=1)),
(reuters_2020_ca_news_lr.df %>% mutate(year=2020, country='CA', weight=1))
)


allsides_official <- fread("~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/allsides_requested_data/AllSides-Media-Bias-Ratings_Publications_071822.csv") %>%
  filter(`allsides_media_bias_ratings/publication/source_type` == 'News Media') %>%
  filter(`allsides_media_bias_ratings/publication/source_url` != '') %>%
  filter(`allsides_media_bias_ratings/publication/media_bias_rating` != 'Mixed') %>%
  mutate(media_bias_rating = as.factor(`allsides_media_bias_ratings/publication/media_bias_rating`), url = `allsides_media_bias_ratings/publication/source_url`) %>%
  select(media_bias_rating, url) %>%
  mutate(stance = recode(media_bias_rating, 'Lean Left'=-0.5, 'Left'=-1, 'Center'=0.0, 'Lean Right'=0.5, 'Right'=1)) %>%
  mutate(url = str_extract(url, "(?<=://).*(?=(/*))"), url = paste0(url,'/'), url=str_replace_all(url, 'www.',''), domain=str_split(url, '/', simplify = T)[,1]) 
# %>%
#   group_by(domain) %>%
#   summarise(lr_stance_allsides = mean(stance))

scale_shift_to_allsides <- c()
for (nation in c('AU', 'CA', 'US', 'UK')) {
  res <- data.frame()
  for (x in seq(-1, 1, 0.01)) {
    aus_reuters <- reuters %>% filter(country==nation)
    res <- rbind(aus_reuters %>% inner_join(allsides_official,by='domain') %>%
                   mutate(score = ((lr+x)-stance)^2) %>%
                   summarise(x=x,score=sum(score)), res)
    
  }
  scale_shift_to_allsides[nation] <- res[which.min(res$score),]$x
  reuters <- reuters %>%
    mutate(lr = if_else(country==nation, lr+scale_shift_to_allsides[nation], lr))
}

# reuters <- reuters %>%
#   mutate(lr = if_else(country=='AU', lr+scale_shift_for_aus, lr)) %>%
#   mutate(lr = if_else(country=='US', lr+scale_shift_for_us, lr)) %>%
#   mutate(lr = if_else(country=='CA', lr+scale_shift_for_ca, lr)) %>%
#   mutate(lr = if_else(country=='UK', lr+scale_shift_for_UK, lr))
  
# res <- data.frame()
# for (x in seq(0.01, 1, 0.01)) {
#   res <- rbind(reuters %>% inner_join(allsides_official,by='domain') %>%
#                  mutate(score = ((lr+x)-stance)^2) %>%
#                  summarise(x=x,score=sum(score)), res)
# }
# scale_shift_to_allsides <- res[which.min(res$score),]$x

temp_reuters <- reuters %>%
  filter(!is.na(domain)) %>%
  # filter(country=='AU') %>%
  mutate(domain=as.character(domain)) %>%
  left_join(allsides_official %>%
              mutate(domain=as.character(domain),isallsides = T), by='domain') %>%
  mutate(domain=fct_reorder(domain,lr))

# fwrite(temp_reuters, "~/Data/opinion_dynamics/02b_stance_detection/URL_slants_shiny/temp_reuters.csv")

temp_reuters %>%
  ggplot() + 
  geom_point(aes(y=domain, x=(lr), color=country)) +
  geom_point(aes(y=domain, x=(stance)), shape=2) +
  theme_cowplot() +
  theme(axis.text.y = element_text(size=10))+
  labs(y='Publisher URL', x='Mean Ideology')

reuters <- reuters %>%
  filter(!is.na(domain)) %>%
  group_by(domain) %>%
  summarise(lr_stance = weighted.mean(lr,weight)) 

allsides_official <- allsides_official %>%
  group_by(domain) %>%
  summarise(lr_stance_allsides = mean(stance))

domain_data <- merge(reuters, allsides_official, all=T) %>%
  mutate(stance = coalesce(lr_stance, lr_stance_allsides)) %>%
  select(domain, stance) %>%
  arrange(stance)

domain_data %>% filter(domain %in% c('foxnews.com', 'breitbart.com', 'skynews.com.au', 'theage.com.au', 'thesaturdaypaper.com.au'))

# fwrite(domain_data, "~/Data/opinion_dynamics/02c_stance_detection/data/02_ground_truth_data/url_data/domain_allsides_reuters_lr.csv")


media_slant_plot <- reuters %>%
  mutate(domain=fct_reorder(domain, lr_stance)) %>%
  ggplot(aes(x=domain, y=lr_stance))+
  geom_bar(aes(fill=if_else(lr_stance<0,'#4285f4','#ff0000')),stat='identity') +
  geom_text(aes(x=domain, label=domain,hjust=if_else(lr_stance<0,-0.01,1.01)),y=0, angle=90)+
  theme_cowplot() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x = element_blank())+
  scale_fill_manual(values =c('#4285f4','#ff0000'))+
  labs(y='Ideology Score')+
  guides(fill="none")

media_slant_plot

# pdf("/home/roram/Data/opinion_dynamics/02c_stance_detection/data/05_plots/main_plots/reuters_media_slant.pdf", width = 9, height = 6 )
# media_slant_plot
# dev.off()
