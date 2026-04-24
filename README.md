![R](https://img.shields.io/badge/Language-R-blue)
![Machine Learning](https://img.shields.io/badge/Method-Machine%20Learning-orange)
![Healthcare](https://img.shields.io/badge/Domain-Healthcare-red)
![Rheumatoid Arthritis](https://img.shields.io/badge/Disease-RA-purple)
![Status](https://img.shields.io/badge/Status-Published-success)
![License](https://img.shields.io/badge/License-MIT-lightgrey)

# ML Predictive Models on Early Discontinuation of Adalimumab in RA

## Overview
This repository contains code supporting the study:

**"Predicting Early Discontinuation of Adalimumab in Patients with Rheumatoid Arthritis Using Machine Learning: A Specialty Pharmacy–Based Approach"**

The goal of this project is to develop machine learning models to identify patients at high risk of early discontinuation of adalimumab due to lack or loss of efficacy, enabling proactive pharmacist intervention.

---

## Objectives
- Predict early discontinuation (within 6 months) of adalimumab in RA patients  
- Use real-world specialty pharmacy data  
- Compare multiple machine learning models  
- Optimize classification threshold for clinical usability  

---

## Methods Summary
- Dataset: Retrospective specialty pharmacy data (N = 300)
- Outcome:
  - **High risk**: Discontinued within 6 months due to inefficacy  
  - **Regular risk**: Continued ≥ 6 months  

### Machine Learning Models
- Logistic Regression
- Linear Discriminant Analysis (LDA)
- Elastic Net
- Random Forest
- K-Nearest Neighbors (KNN)
- Support Vector Machine (SVM)
- XGBoost

### Key Techniques
- 80/20 Train-Test split
- 10-fold Cross Validation
- Bayesian hyperparameter tuning (for most models)
- Threshold optimization using **F1 score**

---

## Key Results
- **Best model (Test Set): Elastic Net**
  - AUC-ROC: **0.886**
  - F1 Score: **0.741**
- Strong performance also observed with:
  - LDA
  - SVM

Interpretation:
- Models effectively identify high-risk patients using routinely collected pharmacy data
- Supports targeted clinical intervention by pharmacists

---

## Repository Structure

```
├── datatable5_modified1.R # Main modeling script
├── requirements.R # Required R packages
├── data/ # (Not included – see below)
├── results/ # Outputs / figures
└── docs/ # Supporting documentation
```

---

## Data Availability
The dataset used in this study contains patient-level clinical and pharmacy data and **is not publicly available** due to privacy and institutional restrictions.

To run the code, you will need:
- A dataset with similar structure to `datatable5.xlsx`
- Variables including demographics, disease characteristics, PROs, and treatment data

---

## How to Run
1. Install required packages:
```r
source("requirements.R")
```
2. Place dataset file
```
datatable5.xlsx
```
3. Run the main script
```
source("datatable5_modified1.R")
```

## Dependencies

Main R packages used:

tidymodels
tidyverse
discrim
doParallel
ggcorrplot
xgboost
finetune
yardstick

(See `requirements.R` for full list)

## Publication

Yoon AH, Gedeck P, Oelofsen M.
**Predicting early discontinuation of adalimumab in patients with rheumatoid arthritis using machine learning: A specialty pharmacy–based approach.**
Journal of Managed Care & Specialty Pharmacy. 2026;32(3):336–347.

## Clinical Impact
* Enables early identification of patients at risk of treatment failure
* Supports pharmacist-led intervention strategies
* Improves medication efficiency and patient outcomes

## Future Work
* External validation on multi-site datasets
* Integration into specialty pharmacy workflow systems
* Model interpretability (e.g., SHAP)
* Prospective evaluation
## Contact
Angie H. Yoon, PharmD, MS, Healthdyne Specialty Pharmacy
## License
MIT License
