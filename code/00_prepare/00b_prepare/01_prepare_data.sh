bash ./qanda_jq_command.sh
bash ./ausvotes_jq_command.sh
bash ./riot_jq_command.sh
singularity exec ~/Wrappers/rserver_container.sif Rscript qanda_add_episode_information.R

# mv ~/Data/opinion_dynamics/02b_stance_detection/data/01_raw_data/qanda/qanda_episodes.csv ~/Data/opinion_dynamics/02b_stance_detection/data/01_raw_data/qanda/data.csv

# mv ~/Data/opinion_dynamics/02b_stance_detection/data/01_raw_data/ausvotes/ausvotes.csv ~/Data/opinion_dynamics/02b_stance_detection/data/01_raw_data/ausvotes/data.csv
