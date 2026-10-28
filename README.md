# Diabetes Health Indicators: Analysis & Predictive Modeling

Statistical analysis and prediction of diabetes status from the CDC **BRFSS 2015** survey
(253,680 respondents, 21 health indicators). Final project for *Statistical Data Processing*
(Group K, HCMUS). The original report is in [`docs/`](docs/NhomK_FinalProject_Report.pdf);
this repo is a cleaned-up, reproducible rewrite that also fixes several problems found in the
original analysis (see [What changed](#what-changed-from-the-original-report)).

## Layout

| File | Content |
|---|---|
| `R/helpers.R` | Data loading, splitting, SMOTE, metrics, Bootstrap / EL / permutation / IV functions |
| `01_eda.Rmd` | Class balance, BMI, prevalence tables, correlation |
| `02_bmi_inference.Rmd` | Bootstrap and Empirical Likelihood CIs for BMI |
| `03_hypothesis_tests.Rmd` | Chi-square and permutation tests with effect sizes |
| `04_feature_selection.Rmd` | Pearson, chi-square, IV/WOE |
| `05_logistic_multinomial.Rmd` | Logistic and multinomial logistic regression |
| `06_gam.Rmd` | GAM logistic (binary and one-vs-rest) |
| `07_rfe.Rmd` | Recursive feature elimination with Random Forest |
| `results/` | Output tables (`*.csv`) and rendered chapters (`reports/`) |
| `legacy/` | The two original `.Rmd` files, kept for reference only |

## Reproduce

```r
install.packages(c("tidyverse", "caret", "themis", "pROC", "emplik",
                   "mgcv", "nnet", "randomForest", "rmarkdown"))
```

```
Rscript run_all.R          # all chapters (~25 min), HTML if pandoc exists, else Markdown
Rscript run_all.R 01 04    # selected chapters
```

Developed with R 4.6.1. Chapter 02 is the slowest (empirical likelihood on 213k observations, ~6 min).

## Evaluation protocol

Stratified **70 / 15 / 15** split (train / validation / test), seed 123. SMOTE-NC is applied to the
training split only. Decision thresholds are chosen on the validation set (Youden's J); the test set is
used once for reporting. All numbers below are on the test set (n = 38,052, 15.9% positive).

## Key results

**BMI.** Mean BMI of diabetic respondents is 31.9 (95% CI 31.87 to 32.02, Bootstrap and EL agree) versus 27.7
for non-diabetic. The difference is 4.2 (CI 4.12 to 4.28); merging prediabetes lowers it to 4.06.

**Binary classification (diabetes or prediabetes vs none).** Majority-class baseline accuracy is 0.841.

| Model | Threshold | AUC | Sensitivity | Specificity | Precision |
|---|---|---|---|---|---|
| Logistic, 21 vars | 0.5 | 0.818 | 0.19 | 0.97 | 0.57 |
| Logistic, 21 vars | 0.139 (Youden) | 0.818 | 0.80 | 0.68 | 0.32 |
| Logistic, 11 vars | 0.133 (Youden) | 0.813 | 0.81 | 0.67 | 0.31 |
| Logistic + SMOTE, 21 vars | 0.461 (Youden) | 0.809 | 0.79 | 0.69 | 0.32 |
| GAM + SMOTE, 21 vars | 0.460 (Youden) | 0.809 | 0.78 | 0.70 | 0.33 |

- All models land at AUC 0.81 to 0.82. **SMOTE does not improve ranking quality**: it mostly shifts the
  threshold, which can be done directly by picking a threshold on the validation set.
- The 11-variable models lose almost nothing versus 21 variables.
- GAM adds nothing over logistic regression, so non-linearity (mainly BMI) is not the bottleneck.
- At threshold 0.5 on raw data the model finds only 19% of cases despite 85% accuracy.

**Three-class (none / prediabetes / diabetes).** Without balancing, recall for prediabetes is 0 and accuracy
(0.848) barely exceeds the baseline. With SMOTE, balanced accuracy is about 0.50 and prediabetes recall 0.32.
Prediabetes is essentially not separable from these indicators.

**Feature selection.** IV > 0.10 gives 11 variables: GenHlth, HighBP, BMI, Age, HighChol, DiffWalk, Income,
PhysHlth, HeartDiseaseorAttack, Education, PhysActivity. RFE (Kappa, 1% tolerance) selects 9 variables;
Kappa across subset sizes differs by less than one standard deviation, so the data support
"a small subset matches all 21", not one unique best subset.

These are predictive associations from cross-sectional survey data, not causal effects.

## What changed from the original report

1. **Label leakage removed.** The original 21-variable balanced multinomial model (reported 93.49%) had
   `Diabetes_binary`, a function of the target, among its predictors. Fixed: accuracy is 62.4%.
2. **Threshold tuning no longer uses the test set.** The original picked 0.3 and 0.6 by inspecting test results,
   and used a class weight of 5 for one logistic model but not its 11-variable counterpart, which explains their
   very different sensitivity. Now: one protocol, threshold from a validation split, no ad-hoc weights.
3. **Metrics beyond accuracy.** Every table includes the majority baseline, AUC, balanced accuracy.
4. **Permutation p-values** use R = 5000 and `(1 + k) / (R + 1)`, so they are never exactly 0; effect sizes
   (phi, Cohen's d) are reported next to p-values with BH correction.
5. **Ordinal EDA uses rates, not counts.** The original read "diabetes concentrates in high-income / high-education
   groups" off raw counts; the correct comparison is the disease rate per level.
6. **EL results.** The PDF printed the merged-group EL interval twice (sections 3.2.2 and 3.3.2); they now differ and a
   `stopifnot` guards against it.
7. **Education is kept**, consistent with the IV-based selection (the original text dropped it, then used it).
8. **One RFE configuration** (the original had two runs with different top-5 variables).

## Data

`dataset/diabetes_012_health_indicators_BRFSS2015.csv`: BRFSS 2015 diabetes health indicators
(`Diabetes_012`: 0 none, 1 prediabetes, 2 diabetes). `Diabetes_binary` is derived as `Diabetes_012 >= 1`.
Note: the committed copy of the CSV had been overwritten by a blank line in an earlier commit; it was
restored from the course folder.
