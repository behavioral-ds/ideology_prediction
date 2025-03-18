library(tidyverse)
library(dplyr)
library(data.table)
library(lubridate)


qanda <- fread("~/Data/opinion_dynamics/02c_stance_detection/data/01_raw_data/qanda/qanda.csv", col.names = c('tid', 'rid' ,'qid', 'uid', 'date', 'text_extended', 'text', 'hashtags', 'hashtags_extended', 'mentions', 'mentions_extended', 'urls_extended', 'urls'))
qanda$date <- as_datetime(as.POSIXct(qanda$date, format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT"))
qanda <- qanda %>%
  mutate(day = wday(as.Date(date)), episode = isoweek(as.Date(date)) - 5) %>%
  mutate(text_extended = na_if(text_extended, '') ,text = coalesce(text_extended, text)) %>%
  mutate(mentions_extended = na_if(mentions_extended, '') ,mentions = coalesce(mentions_extended, mentions)) %>%
  mutate(hashtags_extended = na_if(hashtags_extended, '') ,hashtags = coalesce(hashtags_extended, hashtags)) %>%
  mutate(urls_extended = na_if(urls_extended, '') ,urls = coalesce(urls_extended, urls))

qanda <- qanda %>%
  select(tid, rid, uid, date, text, hashtags, mentions, episode, urls)

fwrite(qanda,"~/Data/opinion_dynamics/02c_stance_detection/data/01_raw_data/qanda/qanda_episodes.csv")


# ```{python}
# import pandas as pd
# import re
# 
# qanda_df = pd.read_csv('./qanda_episodes.csv')
# 
# qanda_df['hashtags'] = qanda_df['hashtags'].fillna('').apply(lambda s: s.split(';;;'))
# 
# qanda_df['mentions'] = qanda_df['mentions'].fillna('').apply(lambda s: s.split(';;;'))
# 
# qanda_df['text'] = qanda_df['text'].apply(lambda t: re.sub(r'http\S+', '<URL>', t))
# 
# from keybert import KeyBERT
# kw_model = KeyBERT()
# 
# keywords = kw_model.extract_keywords(list(qanda_df['text'].values),keyphrase_ngram_range=(1, 2), stop_words='english')
# 
# 
# qanda_df['keywords'] = [[x[0] for x in kl] for kl in keywords]
# 
# import pickle as pk
# with open('./qanda_episodes.pk', 'wb') as wf:
#   pk.dump(qanda_df, wf)
# ```
