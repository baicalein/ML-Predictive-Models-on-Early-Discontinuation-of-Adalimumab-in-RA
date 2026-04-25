# Results and Visual Outputs

## Overview
This directory contains key visualizations from exploratory data analysis (EDA) and model evaluation.

See full paper: ../docs/paper_citation.md

---

## Figures

---

### 1. ROC Curve Comparison

 [View Figure](figure3_roc_grid.pdf)

- ROC curves for all models (CV + test)
- Evaluates discriminative ability

Key Insight:
- Elastic Net shows strongest and most consistent separation
- XGBoost shows signs of overfitting

---

### 2. Model Performance Comparison

[View Figure](supplementary_figure1_bar_metrics.pdf)

- Compares AUC-ROC and F1 score across models

Key Insight:
- Elastic Net balances performance and generalizability
- Supports final model selection

---

### 3. Categorical Feature Comparison

[View Figure](supplementary_figure2_cat_bar.pdf)

- Bar plots comparing high-risk vs regular-risk groups

Key Insight:
- High-risk group shows:
  - More new therapy starts
  - More joint swelling
  - Higher comorbidity burden

---

### 4. Continuous Feature Distribution

[View Figure](supplementary_figure3_density.pdf)

- Density plots of continuous variables

Key Insight:
- High-risk patients have:
  - Higher pain scores
  - Longer morning stiffness

---

### 5. Correlation Heatmap

[View Figure](supplementary_figure4_corrplot.pdf)

- Correlation among predictors

Key Insight:
- Low multicollinearity (|r| < 0.4)
- Supports model stability

---

## Summary

- Elastic Net achieved best test performance:
  - AUC-ROC: **0.886**
  - F1 Score: **0.741**
- Symptom burden and comorbidities are key predictors
- Models show strong potential for clinical implementation
