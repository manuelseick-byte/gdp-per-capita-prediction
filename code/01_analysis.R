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

