# jq -r '[.id_str, .retweeted_status.id_str ,.user.id_str, .created_at, .retweeted_status.extended_tweet.full_text, .text, ([.entities.hashtags[].text] | join(";;;")), (try ([.retweeted_status.extended_tweet.entities.hashtags[].text] | join(";;;")) catch null), (try ([.entities.user_mentions[].id_str] | join(";;;")) catch null), (try ([.retweeted_status.extended_tweet.entities.user_mentions[].id_str] | join(";;;")) catch null)] | @csv' qanda-20200131-20201130-tweets.json > qanda_extended.csv

# cwd=$(pwd)
# cd ~/Data/opinion_dynamics/02c_stance_detection/data/01_raw_data/riot/
# jq -r '.data[] | [.id, .conversation_id ,.author_id, .created_at, .text, (try ([.entities.urls[].expanded_url] | join(";;;")) catch null), (try ([.entities.urls[].unwound_url] | join(";;;")) catch null)] | @csv' riot.jsonl > riot.csv
# jq -r '.includes | .tweets[]? | [.id, .conversation_id ,.author_id, .created_at, .text, (try ([.entities.urls[].expanded_url] | join(";;;")) catch null), (try ([.entities.urls[].unwound_url] | join(";;;")) catch null)] | @csv' riot.jsonl >> riot.csv
# cd $cwd

cwd=$(pwd)
cd ~/Data/opinion_dynamics/02c_stance_detection/data/01_raw_data/riot/
jq -r '.data[] | [.id, .conversation_id ,.author_id, .created_at, .text, (try ([.entities.urls[].expanded_url] | join(";;;")) catch null), (try ([.entities.urls[].unwound_url] | join(";;;")) catch null), (try ([.entities.hashtags[].tag] | join(";;;")) catch null)] | @csv' riot.jsonl > riot.csv
jq -r '.includes | .tweets[]? | [.id, .conversation_id ,.author_id, .created_at, .text, (try ([.entities.urls[].expanded_url] | join(";;;")) catch null), (try ([.entities.urls[].unwound_url] | join(";;;")) catch null), (try ([.entities.hashtags[].tag] | join(";;;")) catch null)] | @csv' riot.jsonl >> riot.csv
cd $cwd
