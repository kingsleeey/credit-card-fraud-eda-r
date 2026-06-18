# Credit Card Fraud Detection: An Exploratory Data Analysis

An end-to-end exploratory data analysis (EDA) project investigating patterns in credit card fraud transactions across the western United States.

Using R, this project demonstrates how data auditing, cleaning, feature engineering, and visualization can uncover meaningful fraud insights before predictive modeling.

---

## Project Overview

Credit card fraud results in billions of dollars in losses annually. According to the report, global payment card fraud losses reached approximately **$33.4 billion in 2024**. 

This project analyzes transaction-level fraud data to answer practical business questions about fraudulent behavior.

The goal was not to build a production machine learning system, but to use statistical analysis and visualization to generate actionable insights.

---

## Research Questions

This analysis explored five questions:

1. Do fraudulent transactions involve larger amounts?
2. Which transaction categories experience the most fraud?
3. Does city population influence fraud likelihood?
4. Are fraud patterns geographically concentrated?
5. Is customer age associated with fraud victimization?

These questions guided the entire analysis. 

---

## Dataset

**Source:** Kaggle — Credit Card Fraud Data https://www.kaggle.com/datasets/neharoychoudhury/credit-card-fraud-data

Dataset characteristics:

| Metric              | Value                  |
| ------------------- | ---------------------- |
| Original Rows       | 14,446                 |
| Cleaned Rows        | 14,383                 |
| Original Variables  | 15                     |
| Final Variables     | 37                     |
| Fraud Cases         | 1,782                  |
| Fraud Rate          | 12.4%                  |
| Geographic Coverage | 13 Western U.S. States |
| Time Period         | Jan 2019 – Dec 2020    |

The dataset contained no missing values but required substantial cleaning. 

---

## Data Cleaning Pipeline

Twelve cleaning steps were performed:

* Fixed corrupted fraud labels
* Converted dates into proper formats
* Removed quotation artifacts from text variables
* Eliminated duplicate records
* Derived time-based variables
* Calculated customer age
* Removed unique transaction IDs
* Applied log transformations
* Engineered customer–merchant distance using the Haversine formula

The final dataset contained:

> **14,383 rows and 22 variables prior to feature engineering.** 

---

## Exploratory Data Analysis

The analysis uncovered several important findings:

### Class Imbalance

Only:

* 12.4% of transactions were fraudulent
* 87.6% were legitimate

This highlights why accuracy alone can be misleading.

---

### Transaction Amounts

Fraud data exhibited strong right skew:

* Mean transaction amount: **$122.72**
* Median: **$51.29**

Log transformation revealed distinct spending patterns.

---

### Geographic Insights

California accounted for the highest transaction volume among the 13 represented states.

Geographic distance between customers and merchants emerged as a potentially informative variable.

---

### Behavioral Patterns

Fraud patterns varied by:

* Transaction category
* Time of day
* Customer demographics
* Merchant distance

---

## Feature Engineering

New variables created included:

* `log_amt`
* `age`
* `trans_hour`
* `trans_day`
* `trans_month`
* `trans_year`
* `dist_km`

These transformed raw transaction records into richer analytical features.

---

## Key Findings

The analysis demonstrated that:

* Fraud detection datasets naturally exhibit severe class imbalance.
* Transaction amount distributions require transformation.
* Behavioral and geographic variables can provide meaningful fraud signals.
* Careful preprocessing is essential before any predictive modeling.

---

## Skills Demonstrated

This project showcases proficiency in:

* R Programming
* Exploratory Data Analysis
* Data Cleaning
* Feature Engineering
* Data Visualization
* Fraud Analytics
* Geospatial Calculations
* Business Communication

---

## Technologies Used

* R
* tidyverse
* ggplot2
* dplyr
* lubridate
* skimr
* naniar
* visdat
* janitor
* scales

---

## Running the Project

Clone the repository:

```bash
git clone https://github.com/yourusername/credit-card-fraud-eda-r.git
```

Install dependencies:

```r
install.packages(c(
  "tidyverse",
  "ggplot2",
  "dplyr",
  "lubridate",
  "skimr",
  "naniar",
  "visdat",
  "janitor",
  "scales"
))
```

Run:

```r
source("scripts/fraud_eda.R")
```

---

## Report

A complete analysis report is available:

📄 Final Report: 

---

## Future Improvements

Potential next steps include:

* Building classification models (Random Forest, XGBoost)
* Addressing class imbalance using SMOTE
* Evaluating precision, recall, and F1-score
* Developing an interactive fraud dashboard using Shiny

---

## Author

**Kingsley Egei**

---
