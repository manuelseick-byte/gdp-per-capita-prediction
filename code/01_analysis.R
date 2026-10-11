rm(list = ls())

########################### BEGIN SCRIPT #######################################
# Final Project – Data Science
# Solow Model with Income Groups, Feature Engineering & ML Models
###############################################################################

########################### (0) PACKAGES ######################################

# Repository paths (set the working directory to the repository root before running)
project_root <- getwd()
world_bank_dir <- file.path(project_root, "data", "World Bank")
pwt_dir <- file.path(project_root, "data", "PWT")

# Run these install lines ONCE manually if needed, then keep them commented out
# install.packages("randomForest")
# install.packages("FNN")
# install.packages("ggplot2")
# install.packages("worldbank")
# install.packages("WDI")
# install.packages("psych")
# install.packages("stargazer")

library(dplyr)
library(tidyr)
library(lubridate)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(FNN)
library(ggplot2)
library(scales)
library(worldbank)
library(WDI)
library(psych)
library(reshape2)
library(stargazer)

###############################################################################
################# (1) LOAD, READ AND MERGE MACRO DATA #########################
###############################################################################

# Define year columns (matches WDI CSVs)
year_cols <- as.character(1960:2024)

## 1.1 World Bank core series --------------------------------------------------

# 1) GDP (constant 2015 US$)
gdp_usd_raw <- read.csv(
  file.path(world_bank_dir, "API_NY.GDP.MKTP.KD_DS2_en_csv_v2_216051.csv"),
  skip = 4, check.names = FALSE
)

gdp_long <- gdp_usd_raw %>%
  select("Country Name", "Country Code", all_of(year_cols)) %>%
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "year",
    values_to = "gdp_const_usd"
  ) %>%
  mutate(year = as.integer(year))

# 2) GNI (Atlas method, current US$)
gni_atlas_raw <- read.csv(
  file.path(world_bank_dir, "API_NY.GNP.ATLS.CD_DS2_en_csv_v2_6564.csv"),
  skip = 4, check.names = FALSE
)

gni_long_atlas <- gni_atlas_raw %>%
  select("Country Name", "Country Code", all_of(year_cols)) %>%
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "year",
    values_to = "gni_const_usd"
  ) %>%
  mutate(year = as.integer(year))

# 3) Population
pop_raw <- read.csv(
  file.path(world_bank_dir, "API_SP.POP.TOTL_DS2_en_csv_v2_130083.csv"),
  skip = 4, check.names = FALSE
)

pop_long <- pop_raw %>%
  select("Country Name", "Country Code", all_of(year_cols)) %>%
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "year",
    values_to = "population"
  ) %>%
  mutate(year = as.integer(year))

# 4) Labour force
lf_raw <- read.csv(
  file.path(world_bank_dir, "API_SL.TLF.TOTL.IN_DS2_en_csv_v2_127995.csv"),
  skip = 4, check.names = FALSE
)

lf_long <- lf_raw %>%
  select("Country Name", "Country Code", all_of(year_cols)) %>%
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "year",
    values_to = "labour_force"
  ) %>%
  mutate(year = as.integer(year))

# 5) Gross savings (current US$)
gns_raw <- read.csv(
  file.path(world_bank_dir, "API_NY.GNS.ICTR.CD_DS2_en_csv_v2_224.csv"),
  skip = 4, check.names = FALSE
)

gns_long <- gns_raw %>%
  select("Country Name", "Country Code", all_of(year_cols)) %>%
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "year",
    values_to = "gross_savings_current_usd"
  ) %>%
  mutate(year = as.integer(year))

# 6) Net primary income from abroad (current US$)
npi_raw <- read.csv(
  file.path(world_bank_dir, "API_NY.GSR.NFCY.CD_DS2_en_csv_v2_6580.csv"),
  skip = 4, check.names = FALSE
)

npi_long <- npi_raw %>%
  select("Country Name", "Country Code", all_of(year_cols)) %>%
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "year",
    values_to = "net_primary_income_current_usd"
  ) %>%
  mutate(year = as.integer(year))

## 1.2 Merge all WDI series ----------------------------------------------------

macro_all <- gdp_long %>%
  full_join(gni_long_atlas,
            by = c("Country Name", "Country Code", "year")) %>%
  full_join(pop_long,
            by = c("Country Name", "Country Code", "year")) %>%
  full_join(lf_long,
            by = c("Country Name", "Country Code", "year")) %>%
  full_join(gns_long,
            by = c("Country Name", "Country Code", "year")) %>%
  full_join(npi_long,
            by = c("Country Name", "Country Code", "year"))

macro_all <- macro_all %>%
  rename(
    Country.Name = `Country Name`,
    Country.Code = `Country Code`
  )

## 1.3 PWT data: Human capital (hc) & TFP (rtfpna) -----------------------------

pwt_raw <- read.csv(
  file.path(pwt_dir, "2025-11-16T20-56_export.csv"),
  check.names = TRUE
)

# Identify year columns in PWT file
year_cols_pwt <- grep("^X?[0-9]{4}$", names(pwt_raw), value = TRUE)

pwt_long <- pwt_raw %>%
  pivot_longer(
    cols      = all_of(year_cols_pwt),
    names_to  = "year",
    values_to = "value"
  ) %>%
  mutate(year = as.integer(gsub("\\D", "", year)))

pwt_hc_tfp <- pwt_long %>%
  filter(Variable.code %in% c("hc", "rtfpna")) %>%
  select(ISO.code, Country, year, Variable.code, value) %>%
  pivot_wider(
    names_from  = Variable.code,
    values_from = value
  ) %>%
  rename(
    Country.Code        = ISO.code,
    Country.Name        = Country,
    human_capital_index = hc,
    A_t                 = rtfpna
  )

macro_all <- macro_all %>%
  left_join(
    pwt_hc_tfp %>%
      select(Country.Code, year, human_capital_index, A_t),
    by = c("Country.Code", "year")
  )

## 1.4 World Bank metadata: regions -------------------------------------------

data("WDI_data")
wb_countries <- WDI_data$country

wb_region <- wb_countries %>%
  select(iso3c, region) %>%
  rename(
    Country.Code = iso3c,
    wb_region    = region
  )

macro_all <- macro_all %>%
  left_join(wb_region, by = "Country.Code")

# Save merged macro data (optional)
write.csv(macro_all, "macro_all_merged.csv", row.names = FALSE)

###############################################################################
################# (2) MISSING DATA DIAGNOSTICS ################################
###############################################################################

## 2.1 Missing values per variable --------------------------------------------

df <- macro_all

na_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "variable",
    values_to = "na_count"
  ) %>%
  arrange(desc(na_count))

ggplot(na_summary, aes(x = reorder(variable, na_count), y = na_count)) +
  geom_col(fill = "#2C3E50") +
  coord_flip() +
  labs(
    x = "Variable",
    y = "Number of NA values"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10)
  ) +
  scale_y_continuous(labels = comma)

## 2.2 Completeness across countries ------------------------------------------

exclude_vars <- c("year", "wb_region", "Country.Name", "Country.Code")
vars_to_check <- setdiff(names(df), exclude_vars)

country_completeness <- df %>%
  group_by(Country.Code, Country.Name) %>%
  summarise(
    n_obs        = n(),
    completeness = 1 - mean(is.na(pick(all_of(vars_to_check)))),
    missingness  = mean(is.na(pick(all_of(vars_to_check)))),
    .groups      = "drop"
  ) %>%
  arrange(completeness)

ggplot(country_completeness, aes(x = completeness)) +
  geom_histogram(binwidth = 0.05, fill = "#2C3E50", color = "white") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Completeness (% of non-missing values)",
    y = "Number of Countries"
  ) +
  theme_minimal(base_size = 14)

## 2.3 Year-wise completeness for Solow variables -----------------------------

vars_needed <- c(
  "gdp_const_usd",
  "population",
  "labour_force",
  "human_capital_index",
  "A_t",
  "gross_savings_current_usd",
  "net_primary_income_current_usd"
)

availability_df <- macro_all %>%
  mutate(across(all_of(vars_needed), ~ !is.na(.)))

year_completeness <- availability_df %>%
  reframe(
    year         = year,
    complete_row = rowSums(across(all_of(vars_needed))) == length(vars_needed)
  ) %>%
  group_by(year) %>%
  summarise(
    completeness_rate = mean(complete_row),
    .groups           = "drop"
  )

ggplot(year_completeness, aes(x = year, y = completeness_rate)) +
  geom_line(color = "#2C3E50", size = 1.3) +
  geom_point(color = "#E74C3C", size = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Year",
    y = "Share of Countries with Complete Data (%)"
  ) +
  theme_minimal(base_size = 14)

## 2.4 Aggregates vs. countries -----------------------------------------------

non_country_obs <- macro_all %>%
  filter(wb_region == "Aggregates") %>%
  summarise(
    number_of_observations = n(),
    number_of_entities     = n_distinct(Country.Code)
  )

write.csv(non_country_obs, "non_country_observations.csv", row.names = FALSE)

###############################################################################
################# (3) FILTER TO COUNTRIES ONLY ################################
###############################################################################

macro_countries_only <- macro_all %>%
  filter(wb_region != "Aggregates")

write.csv(macro_countries_only, "macro_countries_only.csv", row.names = FALSE)

###############################################################################
################# (4) INCOME-GROUP CLASSIFICATION #############################
###############################################################################

# 4.1 Build base for 2023 classification --------------------------------------

income_2023 <- macro_countries_only %>%
  filter(year == 2023) %>%
  select(Country.Code, Country.Name,
         gni_const_usd, gdp_const_usd, population)

# 4.2 Last available GNI per capita (<= 2023) ---------------------------------

gni_last <- macro_countries_only %>%
  filter(
    year <= 2023,
    !is.na(gni_const_usd),
    !is.na(population),
    population > 0
  ) %>%
  arrange(Country.Code, year) %>%
  group_by(Country.Code, Country.Name) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(
    gni_pc_last   = gni_const_usd / population,
    year_last_gni = year
  ) %>%
  select(Country.Code, Country.Name, gni_pc_last, year_last_gni)

income_base <- income_2023 %>%
  left_join(gni_last, by = c("Country.Code", "Country.Name"))

# 4.3 Per-capita income with priority rule ------------------------------------

income_base <- income_base %>%
  mutate(
    gni_pc_2023 = ifelse(
      !is.na(gni_const_usd) & !is.na(population) & population > 0,
      gni_const_usd / population,
      NA_real_
    ),
    gdp_pc_2023 = ifelse(
      !is.na(gdp_const_usd) & !is.na(population) & population > 0,
      gdp_const_usd / population,
      NA_real_
    ),
    income_pc_for_class = case_when(
      !is.na(gni_pc_2023) ~ gni_pc_2023,
      is.na(gni_pc_2023) & !is.na(gdp_pc_2023) ~ gdp_pc_2023,
      is.na(gni_pc_2023) & is.na(gdp_pc_2023) & !is.na(gni_pc_last) ~ gni_pc_last,
      TRUE ~ 0
    )
  )

# 4.4 Apply thresholds: 4-group and 3-group -----------------------------------

income_base <- income_base %>%
  mutate(
    income_group_4 = case_when(
      income_pc_for_class <= 1035 ~ "Low income",
      income_pc_for_class >= 1036 & income_pc_for_class <= 4085 ~ "Lower-middle income",
      income_pc_for_class >= 4086 & income_pc_for_class <= 12615 ~ "Upper-middle income",
      income_pc_for_class > 12615 ~ "High income",
      TRUE ~ "Low income"
    ),
    income_group_3 = case_when(
      income_pc_for_class <= 1035 ~ "Low income",
      income_pc_for_class > 12615 ~ "High income",
      income_pc_for_class > 1035 & income_pc_for_class <= 12615 ~ "Middle income",
      TRUE ~ "Low income"
    )
  )

income_groups <- income_base %>%
  select(
    Country.Code, Country.Name,
    income_pc_for_class,
    gni_pc_2023, gdp_pc_2023, gni_pc_last, year_last_gni,
    income_group_4, income_group_3
  )

# 4.5 Merge classification back into country panel ----------------------------

macro_countries_only <- macro_countries_only %>%
  left_join(income_groups, by = c("Country.Code", "Country.Name"))

write.csv(income_groups, "income_groups.csv", row.names = FALSE)

# Countries classified as low income due to missing data
low_income_no_data <- income_groups %>%
  filter(
    income_group_3 == "Low income",
    is.na(gni_pc_2023),
    is.na(gdp_pc_2023),
    is.na(gni_pc_last)
  ) %>%
  select(Country.Code, Country.Name)

# Population by income group (3-group) in 2023
pop_by_income_3 <- macro_countries_only %>%
  filter(year == 2023) %>%
  group_by(income_group_3) %>%
  summarise(
    world_pop       = sum(population, na.rm = TRUE),
    world_pop_share = world_pop / sum(world_pop),
    .groups         = "drop"
  )

# Number of countries per income group (3-group)
countries_by_income_3 <- macro_countries_only %>%
  filter(year == 2023) %>%
  distinct(Country.Code, income_group_3) %>%
  count(income_group_3, name = "n_countries")

###############################################################################
################# (5) DATA CLEANING & FEATURE ENGINEERING #####################
###############################################################################

## 5.1 Restrict to 1990–2023 and basic filters --------------------------------

macro <- macro_countries_only %>%
  filter(year >= 1990, year <= 2023) %>%
  filter(population > 0, gdp_const_usd > 0)

## 5.2 Solow feature engineering ----------------------------------------------

macro_fe <- macro %>%
  arrange(Country.Code, year) %>%
  group_by(Country.Code) %>%
  mutate(
    gdp_pc = gdp_const_usd / population,
    gni_pc = gni_const_usd / population,
    log_gdp_pc = log(gdp_pc),
    savings_rate     = gross_savings_current_usd / gdp_const_usd,
    net_income_share = net_primary_income_current_usd / gdp_const_usd,
    labour_participation = labour_force / population,
    pop_growth = (population - lag(population)) / lag(population),
    log_hc = ifelse(human_capital_index > 0, log(human_capital_index), NA_real_),
    log_A_t = ifelse(A_t > 0, log(A_t), NA_real_)
  ) %>%
  ungroup()

solow_vars <- c(
  "log_gdp_pc", "savings_rate", "pop_growth",
  "log_hc", "log_A_t", "labour_participation", "net_income_share"
)

## 5.3 Country completeness (25% rule) + build macro_model with lags ----------

country_completeness_solow <- macro_fe %>%
  group_by(Country.Code, Country.Name) %>%
  summarise(
    completeness = 1 - mean(is.na(pick(all_of(solow_vars)))),
    .groups      = "drop"
  )

keep_countries <- country_completeness_solow %>%
  filter(completeness >= 0.25) %>%
  pull(Country.Code)

macro_model <- macro_fe %>%
  filter(
    Country.Code %in% keep_countries,
    !is.na(log_gdp_pc),
    !is.na(savings_rate),
    !is.na(pop_growth),
    !is.na(log_hc),
    !is.na(log_A_t)
  ) %>%
  arrange(Country.Code, year) %>%
  group_by(Country.Code) %>%
  # 10 lags of log GDP per capita (panel-consistent)
  mutate(
    across(
      log_gdp_pc,
      list(
        lag_1  = ~ lag(.x, 1),
        lag_2  = ~ lag(.x, 2),
        lag_3  = ~ lag(.x, 3),
        lag_4  = ~ lag(.x, 4),
        lag_5  = ~ lag(.x, 5),
        lag_6  = ~ lag(.x, 6),
        lag_7  = ~ lag(.x, 7),
        lag_8  = ~ lag(.x, 8),
        lag_9  = ~ lag(.x, 9),
        lag_10 = ~ lag(.x, 10)
      ),
      .names = "{.fn}"
    )
  ) %>%
  ungroup()

## 5.4 Extra feature engineering (interactions, nonlinear terms) --------------

# Interaction terms
macro_model$hc_x_tfp   <- macro_model$log_hc * macro_model$log_A_t
macro_model$save_x_pop <- macro_model$savings_rate * macro_model$pop_growth

# Quadratic terms
macro_model$save2 <- macro_model$savings_rate^2
macro_model$pop2  <- macro_model$pop_growth^2
macro_model$hc2   <- macro_model$log_hc^2
macro_model$tfp2  <- macro_model$log_A_t^2

# Income group interactions (using 3-group classification)
macro_model$middle_x_tfp <- ifelse(macro_model$income_group_3 == "Middle income", 1, 0) * macro_model$log_A_t
macro_model$high_x_hc    <- ifelse(macro_model$income_group_3 == "High income", 1, 0) * macro_model$log_hc

## 5.5 Autoregressive Solow model with 20 lags (extension) ---------------------

macro_ar <- macro_model %>%
  arrange(Country.Code, year) %>%
  group_by(Country.Code)

for (k in 1:20) {
  macro_ar <- macro_ar %>%
    mutate(!!paste0("log_gdp_pc_lag", k) := lag(log_gdp_pc, k))
}

macro_ar <- macro_ar %>% ungroup()

macro_ar20 <- macro_ar %>%
  filter(!is.na(log_gdp_pc_lag20))

lag_terms <- paste0("log_gdp_pc_lag", 1:20, collapse = " + ")

solow_terms <- paste(
  "savings_rate",
  "pop_growth",
  "log_hc",
  "log_A_t",
  "labour_participation",
  "net_income_share",
  sep = " + "
)

full_formula <- as.formula(
  paste("log_gdp_pc ~", lag_terms, "+", solow_terms)
)

solow_ar20 <- lm(full_formula, data = macro_ar20)
num_regressors <- length(coef(solow_ar20)) - 1

###############################################################################
################# (6) VISUALISATION OF ENGINEERED DATA ########################
###############################################################################

# 6.1 Correlation heatmap (Solow vars) ----------------------------------------

corr_matrix <- macro_model %>%
  select(all_of(solow_vars)) %>%
  cor(use = "pairwise.complete.obs")

melted_corr <- melt(corr_matrix)

ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color = "white") +
  scale_fill_gradient2(
    low = "#B2182B", high = "#2166AC", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "")

# 6.2 Distributions of key variables ------------------------------------------

ggplot(macro_model, aes(log_gdp_pc)) +
  geom_histogram(fill = "#2C3E50", bins = 40, color = "white") +
  labs(x = "log(GDP per capita)", y = "Count") +
  theme_minimal(base_size = 14)

ggplot(macro_model, aes(log_hc)) +
  geom_histogram(fill = "#4E79A7", bins = 40, color = "white") +
  labs(x = "log(HCI)", y = "Count") +
  theme_minimal(base_size = 14)

ggplot(macro_model, aes(log_A_t)) +
  geom_histogram(fill = "#F28E2B", bins = 40, color = "white") +
  labs(x = "log(A_t)", y = "Count") +
  theme_minimal(base_size = 14)

ggplot(macro_model, aes(savings_rate)) +
  geom_histogram(fill = "#E15759", bins = 40, color = "white") +
  labs(x = "Savings rate (Gross savings / GDP)", y = "Count") +
  theme_minimal(base_size = 14)

# 6.3 Scatterplots vs log GDP per capita --------------------------------------

ggplot(macro_model, aes(savings_rate, log_gdp_pc)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "Savings rate", y = "log(GDP per capita)") +
  theme_minimal(base_size = 14)

ggplot(macro_model, aes(log_hc, log_gdp_pc)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  labs(x = "log(HCI)", y = "log(GDP per capita)") +
  theme_minimal(base_size = 14)

ggplot(macro_model, aes(log_A_t, log_gdp_pc)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(x = "log(A_t)", y = "log(GDP per capita)") +
  theme_minimal(base_size = 14)

# 6.4 Distributions by income group -------------------------------------------

ggplot(macro_model, aes(x = income_group_3, y = log_gdp_pc)) +
  geom_boxplot(fill = "#1B9E77") +
  labs(x = "Income group", y = "log(GDP per capita)") +
  theme_minimal(base_size = 14)

ggplot(macro_model, aes(x = income_group_3, y = human_capital_index)) +
  geom_boxplot(fill = "#D95F02") +
  labs(x = "Income group", y = "Human Capital Index") +
  theme_minimal(base_size = 14)

###############################################################################
################# (7) BASELINE DATA + TRAIN/TEST SPLIT ########################
###############################################################################

solow_vars_base <- c(
  "savings_rate",
  "pop_growth",
  "log_hc",
  "log_A_t",
  "labour_participation",
  "net_income_share"
)

model_data_base <- macro_model %>%
  select(log_gdp_pc, all_of(solow_vars_base)) %>%
  drop_na()

set.seed(123)
n_base <- nrow(model_data_base)
train_idx_base <- sample(1:n_base, size = round(0.7 * n_base))

train_base <- model_data_base[train_idx_base, ]
test_base  <- model_data_base[-train_idx_base, ]

###############################################################################
################# (8) BASELINE SOLOW REGRESSION ###############################
###############################################################################

solow_baseline <- lm(
  log_gdp_pc ~ savings_rate + pop_growth + log_hc + log_A_t,
  data = train_base
)

summary(solow_baseline)

# Test RMSE (OLS baseline)
test_base$pred_ols <- predict(solow_baseline, newdata = test_base)
rmse_ols <- sqrt(mean((test_base$log_gdp_pc - test_base$pred_ols)^2))

# Actual vs. predicted (on test set)
ggplot(test_base, aes(x = log_gdp_pc, y = pred_ols)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(
    x = "Actual log(GDP per capita)",
    y = "Predicted log(GDP per capita)"
  ) +
  theme_minimal(base_size = 14)

# LaTeX regression table
stargazer(
  solow_baseline,
  type = "latex",
  title = "Baseline Solow Regression",
  dep.var.labels = "log GDP per capita",
  covariate.labels = c("Savings rate", "Population growth",
                       "log(Human capital)", "log(TFP)"),
  digits = 3
)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(solow_baseline)
par(mfrow = c(1, 1))

# Actual vs predicted for entire macro_model (for visualisation)
macro_model$pred_solow <- predict(solow_baseline, newdata = macro_model)

ggplot(macro_model, aes(x = log_gdp_pc, y = pred_solow)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(
    x = "Actual log(GDP per capita)",
    y = "Predicted log(GDP per capita)"
  ) +
  theme_minimal(base_size = 14)

###############################################################################
################# (9) PENALISED SOLOW MODELS – BASELINE FEATURES ##############
###############################################################################

x_base_train <- as.matrix(train_base %>% select(all_of(solow_vars_base)))
y_base_train <- train_base$log_gdp_pc

x_base_test  <- as.matrix(test_base %>% select(all_of(solow_vars_base)))
y_base_test  <- test_base$log_gdp_pc

# LASSO (baseline)
lasso_base <- cv.glmnet(
  x_base_train, y_base_train,
  alpha = 1,
  nfolds = 10,
  standardize = TRUE
)

lasso_base_pred <- predict(
  lasso_base,
  s    = lasso_base$lambda.min,
  newx = x_base_test
)

lasso_base_rmse <- sqrt(mean((y_base_test - lasso_base_pred)^2))

# Ridge (baseline)
ridge_base <- cv.glmnet(
  x_base_train, y_base_train,
  alpha = 0,
  nfolds = 10,
  standardize = TRUE
)

ridge_base_pred <- predict(
  ridge_base,
  s    = ridge_base$lambda.min,
  newx = x_base_test
)

ridge_base_rmse <- sqrt(mean((y_base_test - ridge_base_pred)^2))

###############################################################################
################# (10) PENALISED MODELS – ENGINEERED FEATURES #################
###############################################################################

engineered_vars <- c(
  solow_vars_base,               # baseline Solow variables
  paste0("lag_", 1:10),          # 10 lags of log GDP per capita
  "hc_x_tfp", "save_x_pop",      # interactions
  "save2", "pop2", "hc2", "tfp2",# quadratic terms
  "middle_x_tfp", "high_x_hc"    # income-group interactions
)

# Build engineered dataset WITHOUT dropping NAs yet
model_data_eng <- macro_model %>%
  select(log_gdp_pc, all_of(engineered_vars))

set.seed(123)
n_eng <- nrow(model_data_eng)
train_idx_eng <- sample(seq_len(n_eng), size = round(0.7 * n_eng))

train_eng <- model_data_eng[train_idx_eng, ]
test_eng  <- model_data_eng[-train_idx_eng, ]

# Drop NAs separately in train and test to avoid misalignment
train_eng <- train_eng %>% drop_na()
test_eng  <- test_eng %>% drop_na()

x_eng_train <- as.matrix(train_eng %>% select(all_of(engineered_vars)))
y_eng_train <- train_eng$log_gdp_pc

x_eng_test  <- as.matrix(test_eng %>% select(all_of(engineered_vars)))
y_eng_test  <- test_eng$log_gdp_pc

# LASSO on engineered features
lasso_eng <- cv.glmnet(
  x_eng_train, y_eng_train,
  alpha = 1,
  nfolds = 10,
  standardize = TRUE
)

lasso_eng_pred  <- predict(lasso_eng, s = lasso_eng$lambda.min, newx = x_eng_test)
lasso_eng_rmse  <- sqrt(mean((y_eng_test - lasso_eng_pred)^2))

# Ridge on engineered features
ridge_eng <- cv.glmnet(
  x_eng_train, y_eng_train,
  alpha = 0,
  nfolds = 10,
  standardize = TRUE
)

ridge_eng_pred <- predict(ridge_eng, s = ridge_eng$lambda.min, newx = x_eng_test)
ridge_eng_rmse <- sqrt(mean((y_eng_test - ridge_eng_pred)^2))

###############################################################################
################# (11) NON-LINEAR MODELS: RF & kNN (BASELINE) #################
###############################################################################

# We reuse train_base / test_base
x_train <- as.matrix(train_base %>% select(all_of(solow_vars_base)))
y_train <- train_base$log_gdp_pc

x_test  <- as.matrix(test_base %>% select(all_of(solow_vars_base)))
y_test  <- test_base$log_gdp_pc

# Random Forest
rf_model <- randomForest(
  x = x_train,
  y = y_train,
  ntree = 500,
  mtry = floor(sqrt(ncol(x_train))),
  importance = TRUE
)

rf_pred <- predict(rf_model, newdata = x_test)
rf_rmse <- sqrt(mean((y_test - rf_pred)^2))

varImpPlot(rf_model, main = "Random Forest Variable Importance")

# kNN (with scaling)
x_train_scaled <- scale(x_train)
train_mean <- attr(x_train_scaled, "scaled:center")
train_sd   <- attr(x_train_scaled, "scaled:scale")
x_test_scaled <- scale(x_test, center = train_mean, scale = train_sd)

k_values <- c(3, 5, 10, 20)
knn_rmse <- numeric(length(k_values))

for (i in seq_along(k_values)) {
  k <- k_values[i]
  knn_pred <- knn.reg(
    train = x_train_scaled,
    test  = x_test_scaled,
    y     = y_train,
    k     = k
  )$pred
  
  knn_rmse[i] <- sqrt(mean((y_test - knn_pred)^2))
}

knn_results <- data.frame(k = k_values, RMSE = knn_rmse)
best_k <- k_values[which.min(knn_rmse)]
min(knn_rmse)

###############################################################################
################# (12) MODEL COMPARISON TABLE #################################
###############################################################################

# Compile RMSE results
results <- data.frame(
  Model = c(
    "OLS (baseline)",
    "LASSO (baseline features)",
    "Ridge (baseline features)",
    "LASSO (engineered features)",
    "Ridge (engineered features)",
    "Random Forest (baseline features)",
    "kNN (baseline features, best k)"
  ),
  RMSE = c(
    rmse_ols,          # OLS test RMSE
    lasso_base_rmse,
    ridge_base_rmse,
    lasso_eng_rmse,
    ridge_eng_rmse,
    rf_rmse,
    min(knn_rmse)
  )
)

results

###############################################################################
################# (13) VARIABLE IMPORTANCE ACROSS MODELS ######################
###############################################################################

# Ridge importance (baseline features)
ridge_coef <- as.matrix(coef(ridge_base, s = ridge_base$lambda.min))[-1, ]
ridge_importance <- data.frame(
  variable         = solow_vars_base,
  importance_ridge = abs(ridge_coef)
)

# LASSO importance (baseline features)
lasso_coef <- as.matrix(coef(lasso_base, s = lasso_base$lambda.min))[-1, ]
lasso_importance <- data.frame(
  variable         = solow_vars_base,
  importance_lasso = abs(lasso_coef)
)

# Random Forest importance (baseline features)
rf_imp <- importance(rf_model, type = 1)
rf_importance <- data.frame(
  variable      = rownames(rf_imp),
  importance_rf = rf_imp[, 1]
)

# kNN permutation importance (baseline features)
knn_importance <- data.frame(
  variable        = solow_vars_base,
  importance_knn  = NA_real_
)

for (v in solow_vars_base) {
  x_test_permuted <- x_test_scaled
  col_idx <- which(colnames(x_test_permuted) == v)
  x_test_permuted[, col_idx] <- sample(x_test_permuted[, col_idx])
  
  perm_pred <- knn.reg(
    train = x_train_scaled,
    test  = x_test_permuted,
    y     = y_train,
    k     = best_k
  )$pred
  
  perm_rmse <- sqrt(mean((y_test - perm_pred)^2))
  knn_importance$importance_knn[knn_importance$variable == v] <-
    perm_rmse - min(knn_rmse)
}

importance_all <- ridge_importance %>%
  left_join(lasso_importance, by = "variable") %>%
  left_join(rf_importance,   by = "variable") %>%
  left_join(knn_importance,  by = "variable")

importance_norm <- importance_all
importance_norm[, -1] <- apply(importance_norm[, -1], 2, function(x) {
  if (max(x) == min(x)) {
    0
  } else {
    (x - min(x)) / (max(x) - min(x))
  }
})

importance_norm

###############################################################################
################# (14) VARIABLE IMPORTANCE PLOTTING ###########################
###############################################################################

importance_melted <- importance_norm %>%
  pivot_longer(
    cols = -variable,
    names_to  = "model",
    values_to = "importance"
  ) %>%
  mutate(
    model = dplyr::recode(
      model,
      importance_lasso = "LASSO",
      importance_ridge = "Ridge",
      importance_rf    = "Random Forest",
      importance_knn   = "kNN"
    ),
    model = factor(model,
                   levels = c("LASSO", "Ridge", "Random Forest", "kNN"))
  )

# Consistent colour palette
model_colors <- c(
  "LASSO"         = "#4E79A7",  # blue
  "Ridge"         = "#F28E2B",  # orange
  "Random Forest" = "#E15759",  # red
  "kNN"           = "#76B7B2"   # teal
)

ggplot(importance_melted,
       aes(x = reorder(variable, importance),
           y = importance,
           fill = model)) +
  geom_col(position = position_dodge(width = 0.75)) +
  coord_flip() +
  scale_fill_manual(values = model_colors) +
