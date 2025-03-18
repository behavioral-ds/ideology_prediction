cwd=$(pwd)
cd ~/Data/opinion_dynamics/02b_stance_detection/data/01_raw_data/qanda/
jq -r '[.id_str,.user.id_str, .user.description, .text] | @csv' qanda-20200131-20201130-tweets.json > qanda_descriptions.csv
cd $cwd
