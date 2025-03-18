alias nbx="jupyter nbconvert --execute"

bash ./00b_prepare/01_prepare_data.sh
nbx 00a_clean_raw_dataset.ipynb

