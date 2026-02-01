# Determinants of Economic Self-Sufficiency: A Structural Equation Modeling (SEM) Approach

## Project Overview
This project investigates the drivers of financial self-sufficiency among young adults in Germany and the UK. Using **Structural Equation Modeling (SEM)**, I analyzed how family background, institutional support, and personal work values influence current financial situations.

The analysis is based on the Cupesse dataset (N = 4,829).

## Key Techniques
* **Exploratory & Confirmatory Factor Analysis (EFA/CFA):** Validated a 5-factor measurement model.
* **Model Optimization:** Improved model fit (CFI: 0.906 -> 0.938) by analyzing Modification Indices and handling correlated errors.
* **Multi-group Analysis:** Tested for Measurement Invariance (Configural, Metric) across Germany and UK populations using `lavaan`.

## Results Highlight
* **Family Legacy Matters:** Financial situation at age 14 has a significant positive effect on current financial self-sufficiency (Std. Coef ~ 0.28).
* **Institutional Barriers:** "Lack of support in finding a job" is a key negative driver of financial stability.
* **Cross-National Validity:** The factor structure proved to be invariant across Germany and the UK (Metric Invariance supported).

## Visualization
![SEM Path Diagram](output/final_sem_horizontal_v3)
*(記得確認這裡的圖片路徑是否正確)*

## How to Run
1. Open `scripts/analysis.R` in RStudio.
2. Ensure the project root is set correctly (open as R Project).
3. Run the script to reproduce the CFA and SEM results.

## Project Origin & Contribution
This project originated from a university assignment (Multivariate Statistics at KU Leuven) completed by **Team 13**.

While the initial conceptualization was a group effort, my individual contributions to this repository include:

* **Analysis Validation:** Executed the complete statistical modeling workflow (**EFA, CFA, and SEM**) independently to cross-validate team results.
* **Code Integration:** Served as the **Lead R Script Maintainer** during the assignment, responsible for consolidating, cleaning, and debugging the team's code for the final submission.
* **Portfolio Optimization:** In this repository, I refactored the original script to improve readability and added publication-quality visualizations using `semPaths`.# Economic-Self-Sufficiency-SEM
