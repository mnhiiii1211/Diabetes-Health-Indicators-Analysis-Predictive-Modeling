# Diabetes-Health-Indicators-Analysis-Predictive-Modeling
Overview
This project analyzes over 250,000 health survey records from the CDC’s BRFSS dataset to investigate the relationship between various health indicators and diabetes risk. The primary goal is to develop high-performance predictive models to identify individuals at risk for early intervention.

Key Features & Methodology
Statistical Inference: Applied non-parametric methods, including Bootstrap and Empirical Likelihood (EL), to estimate 95% confidence intervals for BMI, confirming significant disparities between diabetic and non-diabetic groups.
Hypothesis Testing: Conducted Permutation Tests and Chi-square tests to validate the statistical significance of health behaviors and demographic factors.
Feature Engineering & Selection: Optimized model complexity using Information Value (IV), Weight of Evidence (WoE), and Recursive Feature Elimination (RFE) with Random Forest.
Imbalanced Data Handling: Utilized the SMOTE (Synthetic Minority Over-sampling Technique) to balance the target classes, ensuring robust model training.
Advanced Modeling: Developed and compared multiple classification models, including Logistic Regression, Multinomial Logistic, and Generalized Additive Models (GAM) with spline smoothing to capture non-linear trends.

Results
Successfully identified the Top 5 Critical Predictors: General Health (GenHlth), BMI, High Blood Pressure (HighBP), High Cholesterol, and History of Stroke.
The optimized model achieved an Accuracy of 86% and an AUC of 0.82, providing a reliable framework for large-scale health risk screening.

Technical Stack
Language: R Markdown
Key Libraries: tidyverse, caret, mgcv (for GAM), smotefamily (for SMOTE), rms, pROC
