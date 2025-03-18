cwd=$(pwd)
cd ~/Data/opinion_dynamics/02c_stance_detection/data/01_raw_data/qanda/
jq -r '[.id_str, .user.id_str, .retweeted_status.id_str, .retweeted_status.user.id_str, ([.entities.hashtags[].text] | join(";;;")), (try ([.retweeted_status.extended_tweet.entities.hashtags[].text] | join(";;;")) catch null), (try ([.entities.user_mentions[].id_str] | join(";;;")) catch null), (try ([.retweeted_status.extended_tweet.entities.user_mentions[].id_str] | join(";;;")) catch null) ] | @csv' qanda-20200131-20201130-tweets.json > qanda_t_alt.csv &
jq -r '.retweeted_status | select(. != null) | [.id_str, .user.id_str, .retweeted_status.id_str, .retweeted_status.user.id_str, ([.entities.hashtags[].text] | join(";;;")), (try ([.retweeted_status.extended_tweet.entities.hashtags[].text] | join(";;;")) catch null), (try ([.entities.user_mentions[].id_str] | join(";;;")) catch null), (try ([.retweeted_status.extended_tweet.entities.user_mentions[].id_str] | join(";;;")) catch null) ] | @csv' qanda-20200131-20201130-tweets.json > qanda_r_alt.csv &
jq -r '.quoted_status | select(. != null) | [.id_str, .user.id_str, .retweeted_status.id_str, .retweeted_status.user.id_str, ([.entities.hashtags[].text] | join(";;;")), (try ([.retweeted_status.extended_tweet.entities.hashtags[].text] | join(";;;")) catch null), (try ([.entities.user_mentions[].id_str] | join(";;;")) catch null), (try ([.retweeted_status.extended_tweet.entities.user_mentions[].id_str] | join(";;;")) catch null) ] | @csv' qanda-20200131-20201130-tweets.json > qanda_q_alt.csv &
wait
cat qanda_t_alt.csv qanda_r_alt.csv qanda_q_alt.csv >> ~/Data/opinion_dynamics/02c_stance_detection/data/UUS_baseline/stance_detect/qanda_alt.csv
rm qanda_t_alt.csv
rm qanda_r_alt.csv
rm qanda_q_alt.csv
cd $cwd
