# BDHS 2022 Hypertension Unawareness

## Research question
What socioeconomic, demographic, and health-related factors are associated with
hypertension unawareness among adults aged 18+ with hypertension (measured high
blood pressure or current BP medication) in Bangladesh?

## Data
Bangladesh Demographic and Health Survey 2022, Household Member Recode (BDPR81FL).
Data are not included in this repository. Request access from the DHS Program
(https://dhsprogram.com) and place the file in the `data/` folder.

## Methods
- Outcome: hypertensive adults never told by a doctor or health worker that
  they have hypertension (unaware)
- Survey-weighted logistic regression (R `survey` package), with weights, PSU, strata
- Rao-Scott chi-square tests, joint Wald tests, interaction tests, sensitivity analysis

## How to run
Run the scripts in `scripts/` in order (01 to 04). Required R packages:
haven, dplyr, survey, gtsummary, car, pROC, flextable.

## Notes
Raw data must not be shared under the DHS data use terms.
