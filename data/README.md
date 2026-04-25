# Data Description

## Overview
The dataset used in this project consists of real-world specialty pharmacy data for patients with rheumatoid arthritis (RA) who initiated adalimumab therapy.

The data were used to develop machine learning models to predict early discontinuation due to lack or loss of efficacy.

---

## Data Availability
This dataset contains patient-level clinical and pharmacy data and **cannot be shared** due to HIPAA regulations and institutional privacy policies.

- No public dataset is provided  
- No sample or synthetic dataset is included  

---

## File Format
The original dataset used in this project was stored as:

```text
datatable5.xlsx
```
To run the analysis, users must provide their own dataset with a similar structure and variable definitions.

## Variables

The final model used 19 predictor variables collected at baseline (therapy initiation).

### Categorical Variables
* sex
* Humira (therapy type: new vs continuation)
* serology (at initial diagnosis)
* swell (presence of joint swelling)
* JointInjury (history of joint injury)
* FMA (fibromyalgia)
* Insomnia
* mental (depression)
* BoneHealth (osteoporosis/osteopenia)
* Infxn (infection history)
* Auto (other autoimmune disease)
* nonbio_prior (number of prior csDMARDs)
* nonbio_co (number of concomitant csDMARDs)
* comorbid (number of comorbidities)

### Continuous Variables
* age (years)
* BMI (body mass index)
* pain (0–10 numeric rating scale)
* Mstiff (morning stiffness, hours)
* RAdue (RA duration, years)

### Target Variable
`class`
* high → discontinued within 6 months due to lack or loss of efficacy
* regular → continued therapy ≥ 6 months
## Preprocessing (Summary)

Basic preprocessing steps include:

* Factor conversion for categorical variables
* Median imputation for numeric variables
* Mode imputation for categorical variables
* Removal of zero-variance predictors
* Feature engineering (e.g., derived QOL variable)

For full implementation details, see: `datatable5_modified1.R`

## Data Source

The dataset was derived from real-world specialty pharmacy data, including:

* Dispensing records
* Clinical management platform data
* Pharmacist-collected patient assessments

These data reflect routine clinical practice in specialty pharmacy settings.

## Notes
* Laboratory variables were excluded due to inconsistent formatting and missingness
* Patient-reported outcomes (e.g., pain, stiffness) were key predictors
* Dataset reflects patients with adequate adherence (PDC ≥ 80%)
## Contact

For questions about the dataset or collaboration opportunities, please contact the repository owner.
