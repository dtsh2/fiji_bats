Fiji bat–human interactions: analysis code

This release contains R code for analysing survey data on bat–human interactions in Fiji. The script cleans and summarises questionnaire responses, produces descriptive plots, and fits logistic regression models for past and current bat consumption, contact with bats, and bat preparation. Models include responses to the question on whether bats harm humans, age and gender, with comparisons of models with and without an age–gender interaction.

Data

The published data are available on Zenodo. Download the survey CSV and ensure it is named Fiji_Bat_Human_data.csv in your R working directory.

Running the analysis

Install the required R packages:

install.packages(c(
  "RColorBrewer", "tidyverse", "ggplot2", "skimr",
  "tidyr", "visreg", "reshape2", "broom"
))

Place the analysis script in the same directory as the data, set that directory as your R working directory, and run:

source("Fiji_Bat_Human_analysis(1).R")

Adjust the script filename above if it has been renamed in the release. Figures are saved as PNG files in the working directory. Model summaries and comparisons are printed to the console; contact-model odds ratios and confidence intervals are exported to logit_contact_harm_OR.csv.

Citation

Please cite the dataset using the citation provided on the Zenodo record when reusing the data.
