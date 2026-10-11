# Predicting GDP per Capita Across Income Groups

**Manuel Seick**  
Barcelona School of Economics — Foundations of Data Science Final Project, 2025

This repository contains the code and project materials for my individual research project on predicting GDP per capita across income groups using variables motivated by the augmented Solow growth model and a range of statistical-learning methods.

## Project overview

The project constructs a country-year panel covering 217 economies over 1990–2023 by combining World Bank World Development Indicators with Penn World Table data.

Starting from an augmented-Solow benchmark, the analysis compares OLS with LASSO, ridge regression, Random Forests, and k-nearest neighbours. The feature-engineered specifications additionally incorporate lagged GDP per capita, interaction terms, nonlinear transformations, and income-group interactions.

The project is predictive rather than causal. Out-of-sample performance is evaluated using a 70/30 train-test split. Across model classes, human capital and total factor productivity emerge as the most important predictors of GDP per capita.

## Repository structure

```text
.
├── README.md
├── code/
│   └── 01_analysis.R
├── data/
│   └── README.md
├── results/
│   └── README.md
└── paper/
    └── Predicting_GDP_Per_Capita_Across_Income_Groups.pdf
```

## Data

The analysis combines:

- **World Bank World Development Indicators (WDI)** for GDP, GNI, population, labour force, gross savings, and net primary income from abroad;
- **Penn World Table** for the human-capital index and total factor productivity; and
- **World Bank metadata** for country and regional classifications.

The raw source files are not redistributed in this repository. See [`data/README.md`](data/README.md) for the required filenames and source information.

## Empirical workflow

The R script performs the full analysis:

1. imports and merges the World Bank and Penn World Table source files;
2. diagnoses missingness and removes aggregate entities;
3. constructs the 1990–2023 country panel and income-group classification;
4. engineers augmented-Solow variables, lags, interactions, and nonlinear features;
5. estimates the baseline OLS model;
6. estimates LASSO and ridge models using baseline and engineered features;
7. estimates Random Forest and k-nearest-neighbour models;
8. compares out-of-sample RMSE across models; and
9. compares variable importance across the regularised and nonlinear models.

## Running the code

Set the working directory to the repository root and run:

```r
source("code/01_analysis.R")
```

Before running the script, place the required source files in the folders and under the filenames described in [`data/README.md`](data/README.md).

The package-installation commands at the top of the script are commented out. Any missing packages should be installed once before running the analysis.

## Reproducibility

This repository contains the R code used for the original project.

For public documentation, the script has only been adjusted for repository-relative file paths and package-installation handling. The substantive empirical specifications, feature construction, model choices, and evaluation procedures have not been rewritten.

The reorganised repository has not been rerun as a complete end-to-end package after these organisational changes.

## Software

The analysis was conducted in **R 4.5.2** using **RStudio 2025.09**.

The main packages used are `dplyr`, `tidyr`, `lubridate`, `glmnet`, `rpart`, `rpart.plot`, `randomForest`, `FNN`, `ggplot2`, `scales`, `worldbank`, `WDI`, `psych`, `reshape2`, and `stargazer`.

## Paper

The accompanying final project paper is available in [`paper/`](paper/).

## Authorship

This is an individual research project by Manuel Seick, completed at the Barcelona School of Economics in 2025.
