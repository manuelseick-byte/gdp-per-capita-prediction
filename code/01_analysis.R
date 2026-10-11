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
