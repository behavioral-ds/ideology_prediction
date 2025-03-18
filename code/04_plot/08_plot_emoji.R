library(ggplot2)
library(dplyr)
library(cowplot)

library(data.table)
library(tidyverse)

library(ggtext)
library(emojifont)

library(showtext)

font_add("OpenSansEmoji", "~/Data/opinion_dynamics/02c_stance_detection/code/04_plot/OpenSansEmoji/OpenSansEmoji.otf")
showtext_auto()

dataset<-"qanda"
flag_count_df <- fread("~/Data/opinion_dynamics/02c_stance_detection/data/01_raw_data/qanda/emoji_count.csv", colClasses = 'character')
labelled_data_dir = paste0('/home/roram/Data/opinion_dynamics/02c_stance_detection/data/04_results/labels/',dataset,'.csv')
data = fread(labelled_data_dir,colClasses = 'character', header=T)

j_data <- data %>%
  select(!V1) %>%
  left_join(flag_count_df %>% select(!V1), by=c('uid'))

j_data[,3:11] <- j_data[,3:11][,lapply(.SD, as.numeric)]

colnames(j_data) <- c("uid", "label", "australia", ("us"), ("un"), ("rainbow_flag"),("trans_falg"), ("rainbow"),  ("bomb"),  ("knife"), "coffee" )
emoji_text=element_text(family="OpenSansEmoji", size=20)
temp <- j_data[,2:9][,lapply(.SD, mean),by=label] %>%
  pivot_longer(!label, names_to = "Flag", values_to = "mean")

j_data[,2:9] %>%
  pivot_longer(!label, names_to = "Flag", values_to = "mean") %>%
  mutate(label = fct_relevel(label, "L", "N", "R", "FR")) %>%
  ggplot()+
  geom_boxplot(aes(x=label,y=mean, fill=label), position=position_dodge2()) +
  scale_y_log10()+
  theme_cowplot()

cols <- c( "L" = "blue","N" ="purple", "R" = "red", "FR" = "darkred")

temp %>%
  filter(Flag %in% c('australia', 'us', 'rainbow_flag')) %>%
  mutate(label = fct_relevel(label, "L", "N", "R", "FR")) %>%
  ggplot() +
  geom_bar(aes(x=label,y=mean, fill=label), stat='identity', position=position_dodge2()) +
  theme(axis.text.x = emoji_text) +
  scale_fill_manual(values = cols) +
  facet_wrap(~Flag, scale='free_y') +
  theme_cowplot() +
  labs(fill='Ideology', y="Mean Count", x="")+
  guides(fill='none')


temp %>%
  filter(Flag %in% c('australia', 'us', 'rainbow_flag')) %>%
  mutate(label = fct_relevel(label, "L", "N", "R", "FR")) %>%
  ggplot() +
  geom_boxplot(aes(x=label,y=mean, fill=label), stat='identity', position=position_dodge2()) +
  theme(axis.text.x = emoji_text) +
  scale_fill_manual(values = cols) +
  facet_wrap(~Flag, scale='free_y') +
  theme_cowplot() +
  labs(fill='Ideology', y="Mean Count", x="")+
  guides(fill='none')

j_data %>%
  group_by(label) %>%
  summarise(count= mean(count))

j_data %>%
  ggplot()+
  geom_boxplot(aes(x=count, y=label, group=label))+
  theme_cowplot() +
  scale_x_log10()


h_data <- j_data %>%
  mutate(label = relevel(as.factor(label), ref='N'))

# library(pscl)
# library(broom)
# mod.hurdle.aus <- hurdle(australia ~ label, data=h_data)
# df <- as_tibble(summary(mod.hurdle.aus)$coefficients$zero)
# df$emoji <- 'australia'
# 
# tidy(mod.hurdle.aus)
# 
# mod.hurdle.us <- hurdle(us ~ label, data=h_data)
# mod.hurdle.rainbow <- hurdle(rainbow_flag ~ label, data=h_data)
# 
# mod.hurdle.aus$coefficients$zero
# mod.hurdle.aus
# confint(mod.hurdle.aus,'zero',conf.level = 0.95)
# 
# coeftest(mod.hurdle, vcov = sandwich)
# hurdletest(mod.hurdle)
# 
# mod.zinf <- zeroinfl(australia ~ label, data=h_data)
# summary(mod.zinf)
# mod.hurdle$coefficients
# 
# log_data <- h_data %>%
#   mutate(australia = australia > 0)
# 
# mod.logistic <- glm(australia ~ label, data = log_data, family = "binomial")
# summary(mod.logistic)
# plot(australia ~ label, data=h_data)
# 

library(poissonreg)
library(tidymodels)
library(gtools)
tidymodels_prefer()

coefs_df <- data.frame()
for (emoji in c('australia', 'us', 'rainbow_flag', 'coffee')) {
  f <- as.formula(paste0(emoji,' ~ -1 + label'))
  mod.hurdle <- poisson_reg() %>% 
    set_engine("hurdle") %>% 
    fit(f, data=h_data)
  ci <- confint(mod.hurdle$fit)  %>%
    as_tibble( rownames='coefs')
  colnames(ci) <- c('coefs', 'lwr', 'upr')
  # %>%
  #   filter(grepl("zero",coefs))
  mod <- cbind(tidy(mod.hurdle, type="all"), ci) %>%
    mutate(emoji = emoji) 
  # %>%
  #   select(emoji, term, type, estimate, p.value, lwr, upr)
  coefs_df <- rbind(coefs_df, mod)
}

cols <- c( "L" = "blue","N" ="purple", "R" = "red", "FR" = "darkred")

coefs_df %>%
  mutate(stars = stars.pval(p.value)) %>%
  filter(!(emoji == 'rainbow_flag' & term == 'labelFR' & type == 'count')) %>%
  mutate(term = recode(term, labelL = "L", labelR = "R", labelFR = 'FR', `(Intercept)` ='N' ), term = fct_relevel(term, "N","L", "R", "FR")) %>%
  mutate(emoji = fct_relevel(emoji, 'australia', 'us', 'rainbow_flag'), emoji = recode(emoji, australia="Australia", us="United States", rainbow_flag="Rainbow Flag", coffee="Coffee")) %>%
  filter(type=='zero') %>% #, term != 'N') %>%
  ggplot() +
  # geom_point(aes(x=term, y=estimate)) + 
  geom_pointrange(aes(x=emoji, y=estimate, ymin=lwr, ymax=upr, group=term, color=term), position = position_dodge2(width=0.4)) +
  geom_hline(yintercept=0, linetype=2)+
  geom_text(aes(x=emoji, y=upr+0.1, label=stars,group=term, color=term), position = position_dodge2(width=0.4))+
  scale_color_manual(values = cols) +
  theme_cowplot() +
  labs(color='Ideology', x='', y='Logistic Regression Coef.') +
  theme(legend.position = 'bottom')

# Dotwhisker plot
coefs_df %>%
  mutate(stars = stars.pval(p.value)) %>%
  filter(!(emoji == 'rainbow_flag' & term == 'labelFR' & type == 'count')) %>%
  mutate(term = recode(term, labelL = "L", labelR = "R", labelFR = 'FR', labelN ='N' ), term = fct_relevel(term, "N","L", "R", "FR")) %>%
  mutate(emoji = fct_relevel(emoji, 'australia', 'us', 'rainbow_flag'), emoji = recode(emoji, australia="Australia", us="United States", rainbow_flag="Rainbow Flag", coffee="Coffee")) %>%
  filter(type=='zero') %>% #, term != 'N') %>%
  # mutate(estimate = exp(estimate), lwr=exp(lwr), upr=exp(upr)) %>%
  ggplot() +
  # geom_point(aes(x=term, y=estimate)) + 
  geom_pointrange(aes(x=emoji, y=estimate, ymin=lwr, ymax=upr, group=term, color=term), position = position_dodge2(width=0.4)) +
  geom_hline(yintercept=0, linetype=2)+
  geom_text(aes(x=emoji, y=upr+0.001, label=stars,group=term, color=term), position = position_dodge2(width=0.4))+
  scale_color_manual(values = cols) +
  theme_cowplot() +
  labs(color='Ideology', x='', y='Logistic Regression Coef.') +
  theme(legend.position = 'bottom')

# Odds Ratio
coefs_df %>%
  mutate(stars = stars.pval(p.value)) %>%
  filter(!(emoji == 'rainbow_flag' & term == 'labelFR' & type == 'count')) %>%
  mutate(term = recode(term, labelL = "L", labelR = "R", labelFR = 'FR', labelN ='N' ), term = fct_relevel(term, "N","L", "R", "FR")) %>%
  mutate(emoji = fct_relevel(emoji, 'australia', 'us', 'rainbow_flag'), emoji = recode(emoji, australia="Australia", us="United States", rainbow_flag="Rainbow Flag", coffee="Coffee")) %>%
  filter(type=='zero') %>% #, term != 'N') %>%
  mutate(estimate = exp(estimate), lwr=exp(lwr), upr=exp(upr)) %>%
  ggplot() +
  # geom_point(aes(x=term, y=estimate)) + 
  geom_pointrange(aes(x=emoji, y=estimate, ymin=lwr, ymax=upr, group=term, color=term), position = position_dodge2(width=0.6)) +
  # geom_hline(yintercept=0, linetype=2)+
  geom_text(aes(x=emoji, y=upr+0.001, label=stars,group=term, color=term), position = position_dodge2(width=0.6))+
  scale_color_manual(values = cols) +
  theme_cowplot() +
  labs(color='Ideology', x='', y='Odds Ratio') +
  theme(legend.position = 'bottom')

cols <- c( "L" = "blue","N (ref.)" ="purple", "R" = "red", "FR" = "darkred")
# Logistic Regression 
coefs_df %>%
  mutate(stars = stars.pval(p.value)) %>%
  filter(!(emoji == 'rainbow_flag' & term == 'labelFR' & type == 'count')) %>%
  mutate(term = recode(term, labelL = "L", labelR = "R", labelFR = 'FR', `(Intercept)` ='N (ref.)' ), term = fct_relevel(term, "N (ref.)","L", "R", "FR")) %>%
  mutate(emoji = fct_relevel(emoji, 'australia', 'us', 'rainbow_flag'), emoji = recode(emoji, australia="Australia", us="United States", rainbow_flag="Rainbow Flag", coffee="Coffee")) %>%
  filter(type=='zero') %>% #, term != 'N') %>%
  # mutate(estimate = exp(estimate), lwr=exp(lwr), upr=exp(upr)) %>%
  ggplot() +
  # geom_point(aes(x=term, y=estimate)) + 
  geom_pointrange(aes(x=emoji, y=estimate, ymin=lwr, ymax=upr, group=term, color=term), position = position_dodge2(width=0.4)) +
  geom_hline(yintercept=0, linetype=2)+
  geom_text(aes(x=emoji, y=upr+0.001, label=stars,group=term, color=term), position = position_dodge2(width=0.4))+
  scale_color_manual(values = cols) +
  theme_cowplot() +
  labs(color='Ideology', x='', y='Logistic Regression Coef.') +
  theme(legend.position = 'bottom')

# Odds Ratio
coefs_df %>%
  mutate(stars = stars.pval(p.value)) %>%
  filter(!(emoji == 'rainbow_flag' & term == 'labelFR' & type == 'count')) %>%
  mutate(term = recode(term, labelL = "L", labelR = "R", labelFR = 'FR', `(Intercept)` ='N (ref.)' ), term = fct_relevel(term, "N (ref.)","L", "R", "FR")) %>%
  mutate(emoji = fct_relevel(emoji, 'australia', 'us', 'rainbow_flag'), emoji = recode(emoji, australia="Australia", us="United States", rainbow_flag="Rainbow Flag", coffee="Coffee")) %>%
  filter(type=='zero') %>% #, term != 'N') %>%
  mutate(estimate = exp(estimate), lwr=exp(lwr), upr=exp(upr)) %>%
  ggplot() +
  # geom_point(aes(x=term, y=estimate)) + 
  geom_pointrange(aes(x=emoji, y=estimate, ymin=lwr, ymax=upr, group=term, color=term), position = position_dodge2(width=0.4)) +
  geom_hline(yintercept=0, linetype=2)+
  geom_text(aes(x=emoji, y=upr+0.001, label=stars,group=term, color=term), position = position_dodge2(width=0.4))+
  scale_color_manual(values = cols) +
  theme_cowplot() +
  labs(color='Ideology', x='', y='Odds Ratio') +
  theme(legend.position = 'bottom')

coefs_df %>%
  mutate(stars = stars.pval(p.value)) %>%
  filter(!(emoji == 'rainbow_flag' & term == 'labelFR' & type == 'count')) %>%
  mutate(term = recode(term, labelL = "L", labelR = "R", labelFR = 'FR', labelN ='N' ), term = fct_relevel(term, "N","L", "R", "FR")) %>%
  mutate(emoji = fct_relevel(emoji, 'australia', 'us', 'rainbow_flag'), emoji = recode(emoji, australia="Australia", us="United States", rainbow_flag="Rainbow Flag", coffee="Coffee")) %>%
  filter(type=='zero') %>% #, term != 'N') %>%
  # mutate(estimate = exp(estimate), lwr=exp(lwr), upr=exp(upr)) %>%
  ggplot() +
  # geom_point(aes(x=term, y=estimate)) + 
  geom_pointrange(aes(x=emoji, y=estimate, ymin=lwr, ymax=upr, group=term, color=term), position = position_dodge2(width=0.4)) +
  geom_hline(yintercept=0, linetype=2)+
  geom_text(aes(x=emoji, y=upr+0.001, label=stars,group=term, color=term), position = position_dodge2(width=0.4))+
  scale_color_manual(values = cols) +
  theme_cowplot() +
  labs(color='Ideology', x='', y='Odds Ratio') +
  theme(legend.position = 'bottom')

coefs_df %>%
  mutate(stars = stars.pval(p.value)) %>%
  filter(!(emoji == 'rainbow_flag' & term == 'labelFR' & type == 'count')) %>%
  mutate(term = recode(term, labelL = "L", labelR = "R", labelFR = 'FR', `(Intercept)` ='N' ), term = fct_relevel(term, "N","L", "R", "FR")) %>%
  mutate(type = recode(type, count="Count", zero="Zero")) %>%
  mutate(emoji = fct_relevel(emoji, 'australia', 'us', 'rainbow_flag'), emoji = recode(emoji, australia="Australia", us="United States", rainbow_flag="Rainbow Flag")) %>%
  # filter(type=='zero') %>%
  ggplot() +
  # geom_point(aes(x=term, y=estimate)) + 
  geom_pointrange(aes(x=emoji, y=estimate, ymin=lwr, ymax=upr, group=term, color=term), position = position_dodge2(width=0.4)) +
  geom_hline(yintercept=0, linetype=2)+
  geom_text(aes(x=emoji, y=upr+0.1, label=stars,group=term, color=term), position = position_dodge2(width=0.4))+
  scale_color_manual(values = cols) +
  theme_cowplot() +
  labs(color='Ideology', x='', y='Logistic Regression Coef.') +
  theme(legend.position = 'bottom') +
  facet_wrap(~ type)
