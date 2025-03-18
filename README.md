# Practical Guidelines for Ideology Detection Pipelines and Psychosocial Applications

## Overview

Online ideology detection is crucial for downstream tasks such as countering ideologically motivated violent extremism and modeling opinion dynamics. However, two significant challenges arise in practitioners' deployment:

1. **Gold-standard training data** is prohibitively labor-intensive to collect and has limited reusability beyond its collection context (i.e., time, location, and platform).
2. To circumvent the cost of collecting labeled data, **ideological signals** (such as hashtags shared) are employed. Unfortunately, the annotation requirements and the context transferability of these signals remain largely unknown, and the bias they induce is unquantified.

This study provides guidelines for practitioners who require real-time detection of left, right, and extreme ideologies in large-scale online settings. We propose a framework for pipeline construction, describing ideology signals by their associated labor and context transferability. 

Our work evaluates many pipeline constructions, quantifies the biases associated with various ideological signals, and presents a pipeline that outperforms state-of-the-art methods, achieving an AUC ROC score of **0.95**. We demonstrate the capabilities of our pipeline on five datasets containing more than **1.12 million users**.

Additionally, we investigate whether findings in the **psychosocial literature**, developed for offline settings, apply in the online environment. We evaluate several psychosocial hypotheses at scale, which delineate ideologies in terms of morality, grievance, nationalism, and dichotomous thinking. Our results indicate that **right-wing ideologies** tend to use more **vice-moral language**, exhibit **more grievance-filled language**, show increased **black-and-white thinking patterns**, and have a greater association with **national flags**.

This research provides practitioners with guidelines for ideology detection and case studies for its application, fostering a safer and better-understood digital landscape.

## Authors

- **Rohit Ram** (University of Technology Sydney)  
- **Emma Thomas** (Flinders University)  
- **David Kernot** (Defence Science and Technology Group)  
- **Marian-Andrei Rizoiu** (University of Technology Sydney)

For further inquiries, please contact:

- Rohit Ram 
- Marian-Andrei Rizoiu

## Repository Structure

This repository contains the code and data for replicating our analysis and pipeline evaluation. It includes the following primary components:

### `ideology/code/00_prepare`
Scripts for data preprocessing and cleaning, including handling raw datasets and preparing them for feature extraction.

### `ideology/code/01_extract_features`
Scripts for extracting ideological features from raw data, including ground truth extraction, feature extraction, and emoji analysis.

### `ideology/code/02_modelling`
Code for the modeling phase, including validation generation, Hopkins tests, feature ablation, and inter-rater agreement analysis.

### `ideology/code/03_psychosocial_modelling`
Scripts to explore and model psychosocial factors such as morality, grievance, and nationalism in relation to ideologies.

### `ideology/code/04_plot`
Visualization tools to generate various plots, including correlation plots, bias plots, and activity distribution plots for ideologies.

## Usage Instructions

To run the pipeline, follow these steps:

1. **Preparation**: Start by cleaning the raw dataset using the scripts in `00_prepare`.
2. **Feature Extraction**: Extract relevant features using the scripts in `01_extract_features`.
3. **Modeling**: Use the scripts in `02_modelling` to train and validate your models.
4. **Psychosocial Analysis**: Analyze the psychosocial aspects of ideologies with the tools in `03_psychosocial_modelling`.
5. **Visualization**: Finally, visualize the results and generate plots using the `04_plot` scripts.

## Citation

If you use this repository or findings in your research, please cite our paper:

```{bibtex}
@inproceedings{ram2025ideology,
   author = {Rohit Ram and Emma Thomas and David Kernot and Marian-Andrei Rizoiu},
   booktitle = {ICWSM},
   title = {Practical Guidelines for Ideology Detection Pipelines and Psychosocial Applications},
   year = {2025},
}
```