# Results and Visual Outputs

## Overview
This directory contains key visualizations generated during exploratory data analysis (EDA) and model evaluation for predicting early discontinuation of adalimumab in patients with rheumatoid arthritis (RA).

These figures support the findings reported in the published study:
See full paper: ../docs/paper_citation.md

---

## Model Performance Overview

The machine learning models were evaluated using:
- AUC-ROC (discrimination ability)
- F1 Score (balance of precision and recall)
- Precision and Recall

### Key Finding
- **Elastic Net achieved the best performance on the test set**
  - AUC-ROC: **0.886**
  - F1 Score: **0.741**

Other strong models:
- Linear Discriminant Analysis (LDA)
- Support Vector Machine (SVM)

These results demonstrate that relatively interpretable models can perform competitively in real-world clinical prediction tasks. :contentReference[oaicite:0]{index=0}

---

## Figures

### 1. ROC Curve Comparison
`figure3_roc_grid.pdf`

- Displays ROC curves for multiple classification models
- Includes both:
  - Cross-validation performance
  - Test set performance
- Shows discriminative ability across thresholds

Interpretation:
- Elastic Net shows strong separation between high-risk and regular-risk groups
- XGBoost performs well in training but shows reduced generalizability (possible overfitting)

---

### 2. Feature Distribution (Bar Plots)
`supplementary_figure2_cat_bar.pdf`

- Compares categorical variables between:
  - High-risk group
  - Regular-risk group

Key differences:
- Higher proportion of:
  - New therapy initiation
  - Joint swelling
  - Comorbid conditions

Interpretation:
- These variables contribute to distinguishing high-risk patients

---

### 3. Feature Distribution (Density Plots)
`supplementary_figure3_density.pdf`

- Shows distribution of continuous variables:
  - Pain score
  - Morning stiffness
  - Age
  - BMI

Interpretation:
- High-risk patients show:
  - Higher pain scores
  - Longer duration of morning stiffness

These findings align with symptom burden as a key predictor. :contentReference[oaicite:1]{index=1}

---

### 4. Model Performance Comparison
`supplementary_figure1_b_metrics.pdf`

- Bar chart comparing:
  - AUC-ROC
  - F1 score
- Across different models

Interpretation:
- Highlights trade-offs between models
- Supports selection of Elastic Net as the final model

---

### 5. Correlation Heatmap
`supplementary_figure4_corrplot.pdf`

- Displays correlation among numeric predictors

Key observation:
- Low inter-feature correlation (all |r| < 0.4)

Interpretation:
- Confirms minimal multicollinearity
- Supports robustness of model input features :contentReference[oaicite:2]{index=2}

---

## Summary of Insights

- Symptom burden (pain, stiffness, swelling) is strongly associated with early discontinuation  
- Comorbidities and infection history contribute to risk  
- Elastic Net provides:
  - Strong predictive performance  
  - Clinical interpretability  

---

## Notes

- Figures are derived from retrospective specialty pharmacy data  
- Visualizations reflect both:
  - Exploratory analysis  
  - Model evaluation  

---

## Contact
For questions about results or visualizations, please contact the repository owner.
