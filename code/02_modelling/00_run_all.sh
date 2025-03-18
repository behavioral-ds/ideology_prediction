alias nbx="jupyter nbconvert --execute"

# 00_run_all.sh
nbx 00d_generate_validation.ipynb
nbx 00e_hopkins.ipynb
# nbx 01_feature_abalation.ipynb
nbx 02_manual_validation.ipynb
nbx 03_cross_ground_truth.ipynb
nbx 04_cross_dataset.ipynb
nbx 05_generate_labels.ipynb
nbx 05b_generate_labels_for_emma.ipynb