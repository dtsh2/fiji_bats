# Fiji bat–human interactions

This release contains questionnaire data and R code describing bat–human interactions in Fiji. The script summarises survey responses and fits logistic regression models for past and current bat consumption, contact with bats and bat preparation. Each model includes perceived harm from bats, age and gender. Models with and without an age–gender interaction are compared.

## Data and missing values

`Fiji_Bat_Human_data.csv` contains 209 survey rows and 24 columns. The original row order and column names are retained. The field `Person ID number` has repeated values and three missing values. Each of those three rows contains survey answers. Use rows as the unit when reproducing the counts below.

| Field(s) | Coding |
| --- | --- |
| `Bats_increasing`, `Bats_decreasing`, `Bats_same` | Originally a, b and c, respectively. Each is now an indicator. |
| `Leave_sick`, `Move_sick`, `Care_sick`, `Kill_sick`, `Give_sick`, `Eat_sick` | Originally a through f, respectively. Each is now an indicator. |
| `Other_sick` | Original free text. `NA` means no text was recorded in this field. A comment counts as an answer to the sick-bat question, including “Not sure”. |
| All other fields | Original recorded values. Empty fields, existing missing markers and standalone dashes in education or guano fields are written as `NA`. Qualified responses and dashes within text are retained. |

For the two sets of indicators, **1** means the option was marked. **0** means it was unmarked on a row with a recorded answer to that question. **NA** means the whole question had no recorded answer. The data do not distinguish the reasons a question was left unanswered. Multiple marks are retained. For example, a row marking both increasing and decreasing is coded `1,1,0`. A row containing only a sick-bat comment is coded zero for all six named actions and retains the comment.

Of the 209 rows, 201 have a trend response, including 13 marking both increasing and decreasing. The remaining eight have three missing trend indicators. For the sick-bat question, 195 rows have at least one mark or comment, including four with a comment alone. The remaining 14 have all six indicators and the comment missing. For percentages among respondents answering a question, use 201 or 195 as appropriate. Percentages across options can sum to more than 100%.

## Run the analysis

Unzip the archive and set the R working directory to the folder containing this README. The bundled CSV is ready to use. Skip `reformat_survey.R` unless you are recreating the CSV from the earlier letter-coded release.

Install the packages once. The publication figures use Times New Roman, which must also be installed on the computer. Then run the main script in R or RStudio.

```r
install.packages(c("RColorBrewer", "tidyverse", "ggplot2", "skimr", "tidyr",
                   "visreg", "reshape2", "broom", "patchwork", "ragg", "systemfonts"))
source("Fiji_Bat_Human_analysis.R")
```

The main script runs the descriptive analyses and regressions, then calls `Fiji_Bat_Human_RSOS_figures.R` automatically. To regenerate **only the five publication figures**, run `source("Fiji_Bat_Human_RSOS_figures.R")` in the same working directory. The figure script reads the CSV directly, preserving “Will eat” as a separate category in the figures even though the current-consumption model groups it with “Yes”.

The script imports the literal CSV value `NA` as an R missing value. Combined response labels retain multiple selections and sick-bat comments. For the catching and guano plots, qualified “No” responses are grouped under “No”, and “Will go - same amount” is grouped under “Will go” for catching. The guano plot retains the original analysis convention of treating the single `Mine_guano` response “a” as “No”. The CSV preserves that original entry.

Additional descriptive plots are saved as PNG files in the working directory. Model summaries and comparisons are printed to the console. Each regression uses rows with complete data for its outcome and predictors. Contact-model odds ratios and confidence intervals are written to `logit_contact_harm_OR.csv`.

## Publication figures

The figure script writes five PNGs and five LZW-compressed TIFFs to `RSOS_figures/` at 600 dpi, using 9-point Times New Roman text. Both formats use the same base filenames. Figure dimensions and font settings are near the top of the figure script.

| Base filename | Panels, in reading order | Width × height |
| --- | --- | --- |
| `contacts_gender` | Contact by age for females and males | 18 × 9 cm |
| `eating_age_current` | Past and current consumption by age | 18 × 9 cm |
| `eating_gender_current` | Current consumption by age for females and males | 18 × 9 cm |
| `eating` | Female past, female current, male past, male current | 18 × 15 cm |
| `Education_age` | Years of education by age for females and males | 18 × 12 cm |

Bars count survey rows. Age-based panels include the 198 rows with recorded age. Gender-specific panels also require recorded gender. Among eligible rows, missing contact, consumption or education responses appear as “Missing” when present. Each eligible row contributes once per period shown. The current-only gender figure uses the current response once per row.

## Reproduce the conversion from the original CSV

The bundled CSV is ready for analysis. To repeat the conversion from the earlier letter-coded file, run the following in a terminal, supplying the original file as input and an unused filename as output.

```sh
Rscript reformat_survey.R original_survey.csv Fiji_Bat_Human_data_reformatted.csv
```

## Citation

Please cite the dataset using the citation on its Dryad record when reusing the data.
