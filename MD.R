########################################################################################################################
######################################## Fluffsure Pet Insurance Pricing Model ######################################### 
################################################## Model Development ################################################### 
########################################################################################################################


### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# Title: Fluffsure Pet Insurance Pricing Model
# Script: Model Development
#
# Authors: Nicholas Wong, Vivian Chan, Nala Hong, Catherine Xie, Rachel Vy
# Date: September 2024
#
# Purpose: This script develops a pricing model for pet insurance, leveraging historical claims data provided by Fetch 
# Pet Health Pty Ltd. The model integrates both internal and external data to identify the factors influencing premiums,
# and aims to accurately and fairly price pet insurance for prospective customers.
#
# Dependencies: showtext, tidyverse, smotefamily, xgboost, caret, MLmetrics, MASS, car, lmtest, broom, patchwork, mgcv
#               glmnet, mlr, randomForest, parallelMap, parallel, mgcViz, grid, gridExtra
#
# Instructions: Set your working directory to the folder "UG23_reproducible_codes" in the provided folder and all the 
# code should run without any additions. Please also install the required font DMSans-Regular.ttf and ensure the path in
# font_add() matches the location on device. There are two .r files, an EDA.r and a MD.r. Please run the EDA.r file 
# first to ensure that the outputted .csv files are ready to be imported by the MD.r file.
# 
# Data Sources (Internal): 
# - Fetch Pet Health Pty Ltd: "UNSW_claims_data.csv", "UNSW_earned_data.csv"
#
# Data Sources (External): 
# - ABS (Geographical):  "POA_2021_AUST_GDA2020.shp", 
#                        "LGA_2024_AUST_GDA2020.shp", 
#                        "SA3_2021_AUST_GDA2020.shp"
#                        "POSTCODE_SA3_MAPPING.xls"
# - ABS (Socioeconomic): "Local Government Area, Indexes, SEIFA 2021.xlsx", 
#                        "ABS_SA_DATA.xlsx", 
#                        "INCOME_SA2-4.xlsx", 
#                        "63060DO003_202305.xlsx"
# - AKC (Breed):         "AKC_Breed_Data.xlsx"
# - HSVMA (Breed):       "Genetic_Disorders.xlsx"
# - ABS (Nature):        "LANDANDENVIRONMENT_SA2-4.xlsx"
# - NPI (Air Pollution): "Pollution.xlsx"
# - BOM (Temperature):   "mxtan.txt"
#
# Fonts: "DMSans-Regular.ttf"
# Download the font and ensure the path in font_add() matches the location on device
#
# Table of Contents:
# 1. Data Preprocessing
#    1.1. Preamble
#    1.2. Variable Encoding
#    1.3. Partitioning Data
#    1.4. Standardising Data
#    1.5. Feature Selection I
#    1.6. One-Hot Encoding
#    1.7. Synthetic Minority Oversampling (SMOTE)
#    1.8. Exploratory Data Analysis II
# 2. Frequency-Severity GLM
#    2.1. Poisson-Gamma Model
#    2.2. NegBin-LogNorm Model
#    2.3. QPoi-IGam Model
#    2.4. Feature Selection II
# 3. Generalised Additive Models
#    3.1. NegBin-LogNorm Model
#    3.2. Ensemble Methods
#    3.3. Hyperparameter Tuning
#    3.4. GBM with Best Parameters
# 4. Random Forest
#    4.1. Training Model
#    4.2. Feature Selection III
# 5. XGBoost Model
#    5.1. Training Model 
#    5.2. Hyperparameter Tuning II
# 6. Model Implementation
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# 1. Data Preprocessing ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.1. Preamble ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Required Packages
library(showtext)
library(tidyverse)
library(smotefamily)
library(xgboost)
library(caret)
library(MLmetrics)
library(MASS)
library(car)
library(lmtest)
library(broom)
library(patchwork)
library(mgcv)
library(glmnet)
library(mlr)
library(randomForest)
library(parallelMap)
library(parallel)
library(mgcViz)
library(grid)
library(gridExtra)

## Settings
options(scipen = 999)
font_add(family = "DM Sans", regular = "/Library/Fonts/DMSans-Regular.ttf")
showtext_auto()

## Data
EARNED_FULL_DF = read.csv("EARNED_FULL_DF.csv", header = TRUE)
PRICING <- read_csv("PRICING_DF.csv")
PRICING_DF <- read_csv("Raw Internal Data/New_Customers_Pricing_Output_File.csv")

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.2. Variable Encoding ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Data Types
EARNED_FULL_DF <- EARNED_FULL_DF %>%
  mutate(quote_date = as.Date(quote_date), 
         postcode = ifelse(nchar(postcode) == 3, paste0(0, postcode), postcode),
         median_dwelling_price_lga = as.numeric(median_dwelling_price_lga),
         across(c(pet_gender, 
                  pet_de_sexed, 
                  pet_de_sexed_age, 
                  pet_is_switcher, 
                  contribution, 
                  excess,
                  address_type_adj, 
                  suburb,
                  postcode,
                  state, 
                  pet_age_years,
                  number_of_breeds,
                  breed_type, 
                  breed_trait, 
                  breed_name_unique, 
                  breed_name_unique_concat, 
                  is_multi_pet_plan, 
                  quote_time_group, 
                  gender_de_sex, 
                  genetic_disorder_count, 
                  energy_level, 
                  lga_name, 
                  lga_code, 
                  lga_quintile, 
                  household_ideal, 
                  family_ideal, 
                  breed_group, 
                  breed_concat_group, 
                  lga_group), 
                as.factor)
  ) %>%
  dplyr::select(-X, -pet_age_months, -condition_category) %>%
  dplyr::select(-earned_units, -claim_count, -claim_paid, -total_claim_amount, everything(), 
                earned_units, claim_count, claim_paid, total_claim_amount) %>%
  rename(area = AREASQKM, dwelling_price = median_dwelling_price_lga, annual_temp = mean_annual_temp, 
         pm25 = log_pm25_per_sqkm, carbon_monoxide = log_carbon_monoxide_per_sqkm)

PRICING_DF <- PRICING_DF %>%
  filter(!nb_breed_trait %in% c('siamese', 'forest cat', 'spotted cats', 'persian', 'burmese'))

PRICING <- PRICING %>%
  mutate(quote_date = as.Date(quote_date), 
         postcode = ifelse(nchar(postcode) == 3, paste0(0, postcode), postcode),
         median_dwelling_price_lga = as.numeric(median_dwelling_price_lga),
         quote_month = as.numeric(quote_month), 
         across(c(pet_gender, 
                  pet_de_sexed, 
                  pet_de_sexed_age, 
                  pet_is_switcher, 
                  contribution, 
                  excess,
                  address_type_adj, 
                  suburb,
                  postcode,
                  state, 
                  pet_age_years,
                  number_of_breeds,
                  breed_type, 
                  breed_trait, 
                  breed_name_unique, 
                  breed_name_unique_concat, 
                  is_multi_pet_plan, 
                  quote_time_group, 
                  gender_de_sex, 
                  genetic_disorder_count, 
                  energy_level, 
                  lga_name, 
                  lga_code, 
                  lga_quintile, 
                  household_ideal, 
                  family_ideal, 
                  breed_group, 
                  breed_concat_group, 
                  lga_group), 
                as.factor)
  ) %>%
  dplyr::select(-...1, -pet_age_months) %>%
  dplyr::select(-Full_month_premium, everything(), Full_month_premium) %>%
  rename(area = AREASQKM, dwelling_price = median_dwelling_price_lga, annual_temp = mean_annual_temp, 
         pm25 = log_pm25_per_sqkm, carbon_monoxide = log_carbon_monoxide_per_sqkm)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.3. Partitioning Data ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Creating Training and Test Data
set.seed(4305)
train_index = createDataPartition(EARNED_FULL_DF$claim_count, p = 0.8, list = FALSE)
TRAIN = EARNED_FULL_DF %>% dplyr::slice(train_index)
TEST = EARNED_FULL_DF %>% dplyr::slice(-train_index)

## Capping Claim Counts
table(TRAIN$claim_count)
quantile(TRAIN$claim_count, 0.99)
quantile(TRAIN$claim_count / TRAIN$earned_units, 0.99)
summary(TRAIN %>% filter(claim_paid > 0) %>% pull(claim_paid))
TRAIN = TRAIN %>%
  mutate(claim_count = ifelse(claim_count > quantile(TRAIN$claim_count, 0.99)[[1]] & 
                                claim_count / earned_units > quantile(TRAIN$claim_count / TRAIN$earned_units, 0.99)[[1]], 
                              quantile(TRAIN$claim_count, 0.99)[[1]], claim_count), 
         claim_paid = ifelse(claim_paid > quantile(TRAIN$claim_paid, 0.998)[[1]], 
                             quantile(TRAIN$claim_paid, 0.998)[[1]], claim_paid))

table(TRAIN$claim_count)
summary(TRAIN %>% filter(claim_paid > 0) %>% pull(claim_paid))

## Decluttering Environment
rm(train_index)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.4. Standardising Data ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
columns_to_scale <- c(
  "owner_age_years", "avg_weight_kg", "avg_lifespan_yrs", "protected_area", "income_by_lga", "seifa_score", "area",
  "pm25", "carbon_monoxide", "population_density", "dwelling_price", "unemployment", "internet_access_proportion",
  "travelled_to_work_by_car", "worked_from_home", "avg_household_size", "married", "avg_children_per_family",
  "avg_cars_per_household", "own_property", "rent_property", "family_household_proportion", "pet_age", "income_by_age",
  "weighted_income", "pet_age_human_years", "age_difference"
)

TRAIN <- TRAIN %>%
  mutate(across(all_of(columns_to_scale), ~ as.numeric(scale(.))))
TEST <- TEST %>%
  mutate(across(all_of(columns_to_scale), ~ as.numeric(scale(.))))

PRICING <- PRICING %>%
  mutate(across(all_of(columns_to_scale), ~ as.numeric(scale(.))))


## Decluttering Environment
rm(columns_to_scale)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.5. Feature Selection I ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
TRAIN <- TRAIN %>%
  dplyr::select(-exposure_id, -suburb, -postcode, -pet_age_years, -breed_name_unique, -breed_name_unique_concat, 
                -quote_date, -gender_de_sex, -lga_name, -lga_code, -total_claim_amount, -average_breed_size, 
                -avg_cars_per_household, -travelled_to_work_by_car, -family_household_proportion, -own_property, 
                -carbon_monoxide, -pet_age_human_years, -income_by_age)
TEST <- TEST %>%
  dplyr::select(-exposure_id, -suburb, -postcode, -pet_age_years, -breed_name_unique, -breed_name_unique_concat, 
                -quote_date, -gender_de_sex, -lga_name, -lga_code, -total_claim_amount, -average_breed_size, 
                -avg_cars_per_household, -travelled_to_work_by_car, -family_household_proportion, -own_property, 
                -carbon_monoxide, -pet_age_human_years, -income_by_age)

PRICING <- PRICING %>%
  dplyr::select(-exposure_id, -suburb, -postcode, -pet_age_years, -breed_name_unique, -breed_name_unique_concat, 
                -quote_date, -gender_de_sex, -lga_name, -lga_code, -average_breed_size, 
                -avg_cars_per_household, -travelled_to_work_by_car, -family_household_proportion, -own_property, 
                -carbon_monoxide, -pet_age_human_years, -income_by_age)

## New Variable for whether there was a claim or not
TRAIN <- TRAIN %>%
  mutate(claim = ifelse(claim_count > 0, 1, 0))
TEST <- TEST %>%
  mutate(claim = ifelse(claim_count > 0, 1, 0))

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.6. One-Hot Encoding ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## OH - One Hot Encoding
TRAIN_OH = model.matrix(~ . -1, data = TRAIN %>% dplyr::select(-claim))
TEST_OH = model.matrix(~ . -1, data = TEST %>% dplyr::select(-claim))

## D-Matrices for xgBoost
FREQ_TRAIN_OH_X = model.matrix(~ . -1, data = TRAIN %>% dplyr::select(-earned_units, -claim_count, -claim_paid, -claim))
FREQ_TEST_OH_X = model.matrix(~ . -1, data = TEST %>% dplyr::select(-earned_units, -claim_count, -claim_paid, -claim))

FREQ_TRAIN_Y = TRAIN %>% dplyr::select(earned_units, claim_count, claim_paid, claim)
FREQ_TEST_Y = TEST %>% dplyr::select(earned_units, claim_count, claim_paid, claim)

FREQ_TRAIN_OH_D = xgb.DMatrix(data = FREQ_TRAIN_OH_X, label = FREQ_TRAIN_Y$claim_count, weight = TRAIN$earned_units)
FREQ_TEST_OH_D = xgb.DMatrix(data = FREQ_TEST_OH_X, label = FREQ_TEST_Y$claim_count)

SEV_TRAIN_OH_X = model.matrix(~ . - earned_units - claim_count + I(claim_count / earned_units) - 1,
                              data = TRAIN %>% filter(claim_paid > 0) %>% dplyr::select(-claim))
SEV_TEST_OH_X = model.matrix(~ . - earned_units - claim_count + I(claim_count / earned_units) - 1,
                             data = TEST %>% filter(claim_paid > 0) %>% dplyr::select( -claim))

SEV_TRAIN_Y = TRAIN %>% filter(claim_paid > 0) %>% dplyr::select(earned_units, claim_count, claim_paid, claim)
SEV_TEST_Y = TEST %>% filter(claim_paid > 0) %>% dplyr::select(earned_units, claim_count, claim_paid, claim)

SEV_TRAIN_OH_D = xgb.DMatrix(data = SEV_TRAIN_OH_X, label = SEV_TRAIN_Y$claim_paid)
SEV_TEST_OH_D = xgb.DMatrix(data = SEV_TEST_OH_X, label = SEV_TEST_Y$claim_paid)

## Removing Columns Based on Multicollinearity
TRAIN_OH <- TRAIN_OH[, !colnames(TRAIN_OH) %in% c("pet_genderfemale", "pet_de_sexed_ageNot desexed",
                                                  "breed_traitwater dog", "breed_traitunknown", "breed_traitcross",
                                                  "family_idealTRUE.TRUE.TRUE.FALSE")]
TEST_OH <- TEST_OH[, !colnames(TEST_OH) %in% c("pet_genderfemale", "pet_de_sexed_ageNot desexed",
                                               "breed_traitwater dog", "breed_traitunknown", "breed_traitcross",
                                               "family_idealTRUE.TRUE.TRUE.FALSE")]

colnames(TRAIN_OH) <- make.names(colnames(TRAIN_OH))
colnames(TEST_OH) <- make.names(colnames(TEST_OH))

# Testing for Multicollinearity
correlation_matrix <- cor(TRAIN_OH, use = "complete.obs")
high_correlation <- which(abs(correlation_matrix) > 0.9, arr.ind = TRUE)
high_correlation <- high_correlation[high_correlation[,1] != high_correlation[,2], ]
cbind(high_correlation, correlation = correlation_matrix[high_correlation])

## Pricing Data
PRICING = model.matrix(~ . -1, data = PRICING[, -which(names(PRICING) == "Full_month_premium")])
PRICING = PRICING[, !colnames(PRICING) %in% c("pet_genderfemale", "pet_de_sexed_ageNot desexed",
                                              "breed_traitwater dog", "breed_traitunknown", "breed_traitcross",
                                              "family_idealTRUE.TRUE.TRUE.FALSE")]
colnames(PRICING) <- make.names(colnames(PRICING))

## Decluttering Environment
rm(correlation_matrix, high_correlation)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.7. Synthetic Minority Oversampling (SMOTE) ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# # Defining the features and target
# features = as.data.frame(TRAIN_OH)
# target = as.factor(TRAIN$claim)
# 
# # Performing SMOTE
# smote_result = SMOTE(X = features, target = target, K = 5, dup_size = 5)
# smote_result_bl = BLSMOTE(X = features, target = target, K = 5)
# smote_result_ad = ADAS(X = features, target = target, K = 5)
# 
# # SMOTE
# TRAIN_SM = smote_result$data
# TRAIN_SM$claim = as.integer(TRAIN_SM$class)
# TRAIN_SM = TRAIN_SM %>% dplyr::select(-class)
# 
# # Borderline SMOTE
# TRAIN_BL = smote_result_bl$data
# TRAIN_BL$claim = as.integer(TRAIN_BL$class)
# TRAIN_BL = TRAIN_BL %>% dplyr::select(-class)
# 
# # ADASYN
# TRAIN_AD = smote_result_ad$data
# TRAIN_AD$claim = as.integer(TRAIN_AD$class)
# TRAIN_AD = TRAIN_AD %>% dplyr::select(-class)
# 
# # ADASYN with cap on samples
# X_synthetic <- smote_result_ad$syn_data[, -ncol(smote_result_ad$syn_data)]
# y_synthetic <- smote_result_ad$syn_data[, ncol(smote_result_ad$syn_data)]
# 
# size <- 1000
# 
# synthetic_sampled_indices <- sample(1:nrow(X_synthetic), size)
# 
# X_synthetic_sampled <- X_synthetic[synthetic_sampled_indices, ]
# y_synthetic_sampled <- as.numeric(y_synthetic[synthetic_sampled_indices])
# 
# X_final <- rbind(features, X_synthetic_sampled)
# y_final <- c(as.numeric(target)-1, y_synthetic_sampled)
# 
# Create the final TRAIN_AD dataset
# TRAIN_AD <- as.data.frame(X_final)
# TRAIN_AD$claim <- as.numeric(y_final)
# 
# # Checking distribution
# table(TRAIN_SM$claim)
# table(TRAIN_BL$claim)
# table(TRAIN_AD$claim)
# table(TRAIN$claim)
# 
# # Rounding columns
# no_round <- c("owner_age_years", "avg_weight_kg", "avg_lifespan_yrs", "income_by_lga", "seifa_score", "protected_area",
#              "area", "pm25", "population_density", "dwelling_price", "unemployment",
#              "internet_access_proportion", "worked_from_home", "avg_household_size",
#              "married", "avg_children_per_family", "rent_property", "weighted_income",
#              "age_difference", "earned_units", "claim_paid")
# 
# # Preparing the dataframes for modelling
# TRAIN_SM <- TRAIN_SM %>%
#  mutate(across(where(is.numeric) & !all_of(no_round), ~ round(.))) %>%
#  dplyr::select(-claim)
# TRAIN_BL <- TRAIN_BL %>%
#  mutate(across(where(is.numeric) & !all_of(no_round), ~ round(.))) %>%
#  dplyr::select(-claim)
# TRAIN_AD <- TRAIN_AD %>%
#  mutate(across(where(is.numeric) & !all_of(no_round), ~ round(.))) %>%
#  dplyr::select(-claim)
# 
# # Decluttering Environment
# rm(features, smote_result, smote_result_ad, smote_result_bl, no_round, target, X_final, X_synthetic,
#   X_synthetic_sampled, size, synthetic_sampled_indices, y_final, y_synthetic, y_synthetic_sampled, TRAIN_SM, TRAIN_BL)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.8. Exploratory Data Analysis II ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
cts_fct_plot <- function(dataframe, continuous_var, factor_var, se = TRUE, span = 0.5) {
  group_levels <- unique(as.character(dataframe[[factor_var]]))
  group_levels <- group_levels[!is.na(group_levels)]
  num_groups <- length(group_levels)
  
  # Define a palette of colors for up to 5 groups
  color_palette <- c("#00BFC4", "#F8766D", "#7CAE00", "#C77CFF", "#FF61CC")
  if (num_groups > length(color_palette)) stop("Number of factor levels exceeds the number of available colors.")
  
  # Assign colors to each group
  group_colors <- color_palette[1:num_groups]
  names(group_colors) <- group_levels
  group_colors["Overall"] <- "black"
  
  # Assign line types to each group
  line_types <- rep("solid", num_groups)
  names(line_types) <- group_levels
  line_types["Overall"] <- "dashed"
  
  # Frequency Plot
  frequency_plot <- ggplot(dataframe, aes_string(x = continuous_var, y = "claim_count / earned_units")) +
    geom_smooth(aes_string(colour = factor_var, linetype = factor_var), method = "loess", se = se, size = 1, span = span) +
    geom_smooth(aes(colour = "Overall", linetype = "Overall"), method = "loess", se = FALSE, size = 1, span = span) +
    scale_colour_manual(values = group_colors) +
    scale_linetype_manual(values = line_types) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = continuous_var, y = "Claim Frequency", colour = "Group", linetype = "Group",
         title = paste("Claim Frequency by", continuous_var, "and", factor_var)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          axis.line = element_line(linewidth = 1, colour = "grey80"),
          text = element_text(family = "DM Sans", size = 12))
  
  # Severity Plot
  severity_data <- dataframe[dataframe$claim_paid > 0, ]
  severity_plot <- ggplot(severity_data, aes_string(x = continuous_var, y = "claim_paid")) +
    geom_smooth(aes_string(colour = factor_var, linetype = factor_var), method = "loess", se = se, size = 1, span = span) +
    geom_smooth(aes(colour = "Overall", linetype = "Overall"), method = "loess", se = FALSE, size = 1, span = span) +
    scale_colour_manual(values = group_colors) +
    scale_linetype_manual(values = line_types) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = continuous_var, y = "Claim Severity", colour = "Group", linetype = "Group",
         title = paste("Claim Severity by", continuous_var, "and", factor_var)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16),
          axis.line = element_line(linewidth = 1, colour = "grey80"),
          text = element_text(family = "DM Sans", size = 12))
  
  # Merging Plots
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  print(frequency_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(severity_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
}
# 
# ## weighted_income (freq slightly linear, sev non-linear)
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "pet_gender", se = TRUE) #sev mild-weak
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "excess", se = TRUE) #freq yes sev moderate
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "contribution", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "pet_de_sexed", se = TRUE) #freq
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "address_type_adj", se = TRUE) #freq
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "quote_time_group", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "lga_quintile", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "breed_group", se = TRUE) #freq and sev for group 5
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "breed_type", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "gender_de_sex", se = TRUE) #sev
# cts_fct_plot(EARNED_FULL_DF, "weighted_income", "pet_is_switcher", se = TRUE) #freq
# 
# ## pet_age (freq + sev non-linear)
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "pet_gender", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "excess", se = FALSE) #freq yes 200 sev noise?
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "contribution", se = FALSE) #no
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "pet_de_sexed", se = FALSE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "address_type_adj", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "quote_time_group", se = FALSE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "energy_level", se = TRUE, span = 0.7) #freq 1 probs noise
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "lga_quintile", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "breed_group", se = FALSE, span = 0.7) #freq and sev for group 5
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "breed_type", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "gender_de_sex", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pet_age", "pet_is_switcher", se = TRUE) #no
# 
# ## avg_weight_kg (non-linear both)
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "pet_gender", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "excess", se = TRUE) #freq yes .
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "contribution", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "pet_de_sexed", se = TRUE) #Freq mild
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "address_type_adj", se = TRUE) #Freq noise probs
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "quote_time_group", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "energy_level", se = TRUE, span = 0.8) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "lga_quintile", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "breed_group", se = TRUE) #freq and sev for group 5
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "breed_type", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_weight_kg", "pet_is_switcher", se = TRUE) #sev + freq mild
# 
# ## avg_lifespan_yrs (non-linear both)
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "pet_gender", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "excess", se = TRUE) #freq yes .
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "contribution", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "pet_de_sexed", se = TRUE) #Freq mild
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "address_type_adj", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "quote_time_group", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "breed_group", se = TRUE, span = 0.6) #freq and sev for group 5
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "breed_type", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_lifespan_yrs", "pet_is_switcher", se = TRUE) #freq mild
# 
# ## seifa_score (linear freq, sev non-linear)
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "pet_gender", se = FALSE) #no
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "excess", se = TRUE) #freq yes
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "contribution", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "pet_de_sexed", se = TRUE) #Freq mild
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "address_type_adj", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "quote_time_group", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "breed_group", se = TRUE, span = 0.6) #freq and sev for group 5
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "breed_type", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "seifa_score", "pet_is_switcher", se = TRUE) #freq
# 
# ## population_density (linear freq, sev non-linear)
# cts_fct_plot(EARNED_FULL_DF, "population_density", "pet_gender", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "population_density", "excess", se = TRUE) #freq yes
# cts_fct_plot(EARNED_FULL_DF, "population_density", "contribution", se = TRUE, span = 0.7) #freq? Might be noise
# cts_fct_plot(EARNED_FULL_DF, "population_density", "pet_de_sexed", se = TRUE) #Freq
# cts_fct_plot(EARNED_FULL_DF, "population_density", "address_type_adj", se = TRUE) #freq, sev mild may be noise
# cts_fct_plot(EARNED_FULL_DF, "population_density", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "population_density", "quote_time_group", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "population_density", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "population_density", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "population_density", "breed_group", se = TRUE, span = 0.6) #freq and sev for group 5
# cts_fct_plot(EARNED_FULL_DF, "population_density", "breed_type", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "population_density", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "population_density", "pet_is_switcher", se = TRUE) #freq
# 
# ## dwelling_price (both non-linear)
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "pet_gender", se = FALSE) #no
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "excess", se = TRUE) #freq yes
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "contribution", se = TRUE, span = 0.7) #sev likely noise
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "pet_de_sexed", se = TRUE) #Freq weak
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "address_type_adj", se = TRUE) #freq (expected)
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "quote_time_group", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "breed_group", se = TRUE, span = 0.8) #very noisy freq and sev for 5
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "breed_type", se = TRUE, span = 0.7) #sev yes
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "dwelling_price", "pet_is_switcher", se = TRUE) #freq
# 
# ## Unemployment (linear freq, sev non-linear)
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "pet_gender", se = FALSE, span = 0.8) #no
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "excess", se = TRUE) #freq yes
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "contribution", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "pet_de_sexed", se = TRUE) #Freq moderate
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "address_type_adj", se = TRUE) #freq very weak
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "is_multi_pet_plan", se = TRUE) #freq very weak
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "quote_time_group", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "breed_group", se = TRUE, span = 0.8) #very noisy freq and sev for 5
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "breed_type", se = TRUE, span = 0.7) #sev yes
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "unemployment", "pet_is_switcher", se = TRUE) #both noisy
# 
# ## Internet Access (linear freq, sev non-linear)
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "pet_gender", se = TRUE, span = 0.8) #no
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "excess", se = TRUE) #freq yes
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "contribution", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "pet_de_sexed", se = TRUE) #Freq moderate
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "address_type_adj", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "quote_time_group", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "breed_group", se = TRUE, span = 0.8) #freq and sev for 5
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "breed_type", se = TRUE, span = 0.7) #sev yes
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "internet_access_proportion", "pet_is_switcher", se = TRUE) #no
# 
# ## Average Household Size (both non-linear)
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "pet_gender", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "excess", se = TRUE) #freq yes
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "contribution", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "pet_de_sexed", se = TRUE) #Freq weak
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "address_type_adj", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "quote_time_group", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "breed_group", se = TRUE, span = 0.8) #freq and sev for 5
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "breed_type", se = TRUE, span = 0.7) #sev yes
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "avg_household_size", "pet_is_switcher", se = TRUE) #freq weak
# 
# ## Married (both non-linear)
# cts_fct_plot(EARNED_FULL_DF, "married", "pet_gender", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "married", "excess", se = TRUE) #freq yes
# cts_fct_plot(EARNED_FULL_DF, "married", "contribution", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "married", "pet_de_sexed", se = TRUE) #Freq weak
# cts_fct_plot(EARNED_FULL_DF, "married", "address_type_adj", se = TRUE) #Freq weak
# cts_fct_plot(EARNED_FULL_DF, "married", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "married", "quote_time_group", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "married", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "married", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "married", "breed_group", se = TRUE, span = 0.9) #freq and sev for 5
# cts_fct_plot(EARNED_FULL_DF, "married", "breed_type", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "married", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "married", "pet_is_switcher", se = TRUE) #freq weak
# 
# ## income_by_lga (non-linear freq, sev non-linear)
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "pet_gender", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "excess", se = TRUE) #freq yes, sev yes
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "contribution", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "pet_de_sexed", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "address_type_adj", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "quote_time_group", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "breed_group", se = TRUE, span = 0.6) #freq yes for pink, sev yes prink/purple
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "breed_type", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "income_by_lga", "pet_is_switcher", se = TRUE) #yes
# 
# ## protected_area (non-linear freq, sev non-linear)
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "pet_gender", se = TRUE) #freq
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "excess", se = TRUE) #freq yes, sev yes
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "contribution", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "pet_de_sexed", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "address_type_adj", se = TRUE) #freq yes
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "quote_time_group", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "lga_quintile", se = TRUE, span = 0.6) #freq yes
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "breed_group", se = TRUE, span = 0.6) #freq yes for pink, sev yes prink/purple
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "breed_type", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "protected_area", "pet_is_switcher", se = TRUE) #yes freq
# 
# ## annual_temp (non-linear freq, sev non-linear)
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "pet_gender", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "excess", se = TRUE) #freq yes, sev yes
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "contribution", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "pet_de_sexed", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "address_type_adj", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "is_multi_pet_plan", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "quote_time_group", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "energy_level", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "breed_group", se = TRUE, span = 0.6) #freq yes for pink, sev yes prink/purple
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "breed_type", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "gender_de_sex", se = TRUE) #no
# cts_fct_plot(EARNED_FULL_DF, "annual_temp", "pet_is_switcher", se = TRUE) #yes
# 
# ## pm25 (non-linear freq, sev linear)
# cts_fct_plot(EARNED_FULL_DF, "pm25", "pet_gender", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pm25", "excess", se = TRUE, span=0.7) #freq yes, sev yes
# cts_fct_plot(EARNED_FULL_DF, "pm25", "contribution", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pm25", "pet_de_sexed", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pm25", "address_type_adj", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pm25", "is_multi_pet_plan", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pm25", "quote_time_group", se = TRUE, span = 0.7) # yes for purple
# cts_fct_plot(EARNED_FULL_DF, "pm25", "energy_level", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pm25", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "pm25", "breed_group", se = TRUE, span = 0.6) #fyes freq pink
# cts_fct_plot(EARNED_FULL_DF, "pm25", "breed_type", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pm25", "gender_de_sex", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "pm25", "pet_is_switcher", se = TRUE, span=0.7) #yes
# 
# ## carbon_monoxide (non-linear nonlinear, sev nonlinear)
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "pet_gender", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "excess", se = TRUE, span=0.7) #freq yes, sev yes
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "contribution", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "pet_de_sexed", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "address_type_adj", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "is_multi_pet_plan", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "quote_time_group", se = TRUE, span = 0.7) # yes for purple
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "energy_level", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "lga_quintile", se = TRUE, span = 0.6) #no
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "breed_group", se = TRUE, span = 0.6) #yes
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "breed_type", se = TRUE, span = 0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "gender_de_sex", se = TRUE, span=0.7) #no
# cts_fct_plot(EARNED_FULL_DF, "carbon_monoxide", "pet_is_switcher", se = TRUE, span=0.7) #yes

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# 2. Frequency-Severity GLM ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 2.1. Poisson-Gamma Model ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Building Full Model
freq_glm_poi = glm(claim_count ~ . - earned_units - claim_paid - claim, 
                   data = TRAIN,
                   family = poisson, 
                   weights = earned_units)
sev_glm_gam = glm(claim_paid ~ . - earned_units - claim, 
                  data = TRAIN %>% filter(claim_paid > 0), 
                  family = Gamma(link = "log"), 
                  control = glm.control(maxit = 100))

# Frequency Prediction
pred_freq = predict(freq_glm_poi, newdata = TEST, type = "response")
rmse_freq = RMSE(pred_freq, TEST$claim_count)
cat("Poisson Full Model RMSE:", rmse_freq, "\n")

# Severity Prediction
pred_sev = predict(sev_glm_gam, newdata = TEST %>% filter(claim_paid > 0), type = "response")
rmse_sev = RMSE(pred_sev, TEST %>% filter(claim_paid > 0) %>% pull(claim_paid))
cat("Gamma Full Model RMSE:", rmse_sev, "\n")

# Summary
summary(freq_glm_poi)
summary(sev_glm_gam)

## Decluttering Environment
rm(rmse_freq, rmse_sev, pred_freq, pred_sev)

## Checking Model Assumptions
# Poisson Dispersion
deviance = summary(freq_glm_poi)$deviance
df_residual = summary(freq_glm_poi)$df.residual

dispersion = deviance / df_residual

## Plotting Model Diagnostics - Poisson Distribution
fitted_values <- freq_glm_poi$fitted.values
deviance_residuals <- residuals(freq_glm_poi, type = "deviance")
standardized_residuals <- rstandard(freq_glm_poi)
leverage <- hatvalues(freq_glm_poi)

model_data_poi <- data.frame(
  fitted_values = fitted_values,
  residuals = deviance_residuals,
  standardized_residuals = standardized_residuals,
  leverage = leverage
)

# Plot 1: Residuals vs Fitted
plot1_poi <- ggplot(model_data_poi, aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 2: Normal Q-Q Plot
plot2_poi <- ggplot(model_data_poi, aes(sample = residuals)) +
  stat_qq(alpha = 0.25, size = 0.5) +
  stat_qq_line(size = 0.5, color = "#49A5F1") +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Residuals") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 3: Scale-Location Plot
plot3_poi <- ggplot(model_data_poi, aes(x = fitted_values, y = sqrt(abs(standardized_residuals)))) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Scale-Location Plot", x = "Fitted values", y = "√|Standardized Residuals|") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 4: Residuals vs Leverage
plot4_poi <- ggplot(model_data_poi, aes(x = leverage, y = residuals)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Leverage", x = "Leverage", y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"), 
        text=element_text(family="DM Sans", size=20))

# Combine the plots
combined_plot_poi <- (plot1_poi  | plot2_poi ) / (plot3_poi  | plot4_poi) +
  plot_annotation(title = "Model Diagnostics for Poisson Model",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 40, family="DM Sans")))

ggsave("Model Diagnostics Poisson.png", path = "Graphs and Figures/", combined_plot_poi, 
       width = 15, height = 15, units = "cm")


## Plotting Model Diagnostics - Gamma Distribution
fitted_values <- sev_glm_gam$fitted.values
deviance_residuals <- residuals(sev_glm_gam, type = "deviance")
standardized_residuals <- rstandard(sev_glm_gam)
leverage <- hatvalues(sev_glm_gam)

model_data_gam <- data.frame(
  fitted_values = fitted_values,
  residuals = deviance_residuals,
  standardized_residuals = standardized_residuals,
  leverage = leverage
)

# Plot 1: Residuals vs Fitted
plot1_gam <- ggplot(model_data_gam, aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 2: Normal Q-Q Plot
plot2_gam <- ggplot(model_data_gam, aes(sample = residuals)) +
  stat_qq(alpha = 0.25, size = 0.5) +
  stat_qq_line(size = 0.5, color = "#49A5F1") +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Residuals") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 3: Scale-Location Plot
plot3_gam <- ggplot(model_data_gam, aes(x = fitted_values, y = sqrt(abs(standardized_residuals)))) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Scale-Location Plot", x = "Fitted values", y = "√|Standardized Residuals|") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 4: Residuals vs Leverage
plot4_gam <- ggplot(model_data_gam, aes(x = leverage, y = residuals)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Leverage", x = "Leverage", y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"), 
        text=element_text(family="DM Sans", size=20))

# Combine the plots
combined_plot_gam <- (plot1_gam  | plot2_gam ) / (plot3_gam  | plot4_gam) +
  plot_annotation(title = "Model Diagnostics for Gamma Model",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 40, family="DM Sans")))

ggsave("Model Diagnostics Gamma.png", path = "Graphs and Figures/", combined_plot_gam, 
       width = 15, height = 15, units = "cm")

## Decluttering Environment
rm(plot1_gam, plot2_gam, plot3_gam, plot4_gam, combined_plot_gam, model_data_gam,
   plot1_poi, plot2_poi, plot3_poi, plot4_poi, combined_plot_poi, model_data_poi)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 2.2. NegBin-LogNorm Model ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Building Full Model
freq_glm_nb = glm.nb(formula = claim_count ~ . - earned_units - claim_paid - claim, 
                     data = TRAIN, weights = earned_units, init.theta = 0.421158032, link = log)

sev_glm_ln = glm(formula = log(claim_paid) ~ . -earned_units - claim_count - claim + I(claim_count / earned_units), 
                 family = gaussian(link = "identity"), data = TRAIN %>% filter(claim_paid > 0))

# Frequency Prediction
pred_freq = predict(freq_glm_nb, newdata = TEST, type = "response")
rmse_freq = RMSE(pred_freq, TEST %>% pull(claim_count))
cat("Negative Binomial Full Model RMSE:", rmse_freq, "\n")

# Severity Prediction
pred_sev = exp(predict(sev_glm_ln, newdata = TEST %>% filter(claim_paid > 0), type = "response"))
rmse_sev = RMSE(pred_sev, TEST %>% filter(claim_paid > 0) %>% pull(claim_paid))
cat("Log Normal Full Model RMSE:", rmse_sev, "\n")

# Summary
summary(freq_glm_nb)
summary(sev_glm_ln)

## Checking Model Assumptions
# Plotting Model Diagnostics - Negative Binomial
fitted_values <- freq_glm_nb$fitted.values
deviance_residuals <- residuals(freq_glm_nb, type = "deviance")
standardized_residuals <- rstandard(freq_glm_nb)
leverage <- hatvalues(freq_glm_nb)

model_data_nb <- data.frame(
  fitted_values = fitted_values,
  residuals = deviance_residuals,
  standardized_residuals = standardized_residuals,
  leverage = leverage
)

# Plot 1: Residuals vs Fitted
plot1_nb <- ggplot(model_data_nb, aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 2: Normal Q-Q Plot
plot2_nb <- ggplot(model_data_nb, aes(sample = residuals)) +
  stat_qq(alpha = 0.25, size = 0.5) +
  stat_qq_line(size = 0.5, color = "#49A5F1") +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Residuals") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 3: Scale-Location Plot
plot3_nb <- ggplot(model_data_nb, aes(x = fitted_values, y = sqrt(abs(standardized_residuals)))) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Scale-Location Plot", x = "Fitted values", y = "√|Standardized Residuals|") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 4: Residuals vs Leverage
plot4_nb <- ggplot(model_data_nb, aes(x = leverage, y = residuals)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Leverage", x = "Leverage", y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"), 
        text=element_text(family="DM Sans", size=20))

# Combine the plots
combined_plot_nb <- (plot1_nb  | plot2_nb ) / (plot3_nb  | plot4_nb) +
  plot_annotation(title = "Model Diagnostics for Negative Binomial Model",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 40, family="DM Sans")))

ggsave("Model Diagnostics NegBin.png", path = "Graphs and Figures/", combined_plot_nb, 
       width = 15, height = 15, units = "cm")


# Plotting Model Diagnostics - Log Normal
fitted_values <- sev_glm_ln$fitted.values
residuals <- residuals(sev_glm_ln, type = "deviance")
standardized_residuals <- rstandard(sev_glm_ln)
leverage <- hatvalues(sev_glm_ln)

model_data_ln <- data.frame(
  fitted_values = fitted_values,
  residuals = residuals,
  standardized_residuals = standardized_residuals,
  leverage = leverage
)

# Plot 1: Residuals vs Fitted
plot1_ln <- ggplot(model_data_ln, aes(x = fitted_values, y = residuals)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 2: Normal Q-Q Plot
plot2_ln <- ggplot(model_data_ln, aes(sample = residuals)) +
  stat_qq(alpha = 0.25, size = 0.5) +
  stat_qq_line(size = 0.5, color = "#49A5F1") +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Residuals") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 3: Scale-Location Plot
plot3_ln <- ggplot(model_data_ln, aes(x = fitted_values, y = sqrt(abs(standardized_residuals)))) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Scale-Location Plot", x = "Fitted values", y = "√|Standardized Residuals|") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"),
        text=element_text(family="DM Sans", size=20))

# Plot 4: Residuals vs Leverage
plot4_ln <- ggplot(model_data_ln, aes(x = leverage, y = residuals)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Leverage", x = "Leverage", y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title=element_text(hjust=0.5, size=24),
        axis.line=element_line(linewidth=0.5, colour="grey80"), 
        text=element_text(family="DM Sans", size=20))

# Combine the plots
combined_plot_ln <- (plot1_ln  | plot2_ln ) / (plot3_ln  | plot4_ln) +
  plot_annotation(title = "Model Diagnostics for Log-Normal Model",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 40, family="DM Sans")))

ggsave("Model Diagnostics LogNormal.png", path = "Graphs and Figures/", combined_plot_ln, 
       width = 15, height = 15, units = "cm")



## Decluttering Environment
rm(plot1_nb, plot2_nb, plot3_nb, plot4_nb, combined_plot_nb, model_data_nb,
   plot1_ln, plot2_ln, plot3_ln, plot4_ln, combined_plot_ln, model_data_ln)


## Comparing Models
# Calculate AIC for each model
aic_poi = AIC(freq_glm_poi)
aic_gam = AIC(sev_glm_gam)
aic_nb = AIC(freq_glm_nb)
aic_ln = AIC(sev_glm_ln)


# Display the AIC values for each model
cat("AIC for Poisson Frequency Model: ", aic_poi, "\n")
cat("AIC for Negative Binomial Frequency Model: ", aic_nb, "\n")
cat("AIC for Gamma Severity Model: ", aic_gam, "\n")
cat("AIC for Log-Normal Severity Model: ", aic_ln, "\n")

## Decluttering Environment
rm(aic_poi, aic_gam, aic_nb, aic_ln, rmse_freq, rmse_sev, pred_freq, pred_sev)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 2.4. Feature Selection II ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Backward Elimination
# freq_glm_nb_b = glm.nb(formula = claim_count ~ . - earned_units - claim_paid - 
#                          pet_gendermale - pet_de_sexedTRUE - stateWA - number_of_breeds5 - 
#                          number_of_breeds6 - breed_typepurebred - breed_traitsetter - genetic_disorder_count6 - area -
#                          lga_group3 - breed_group5 - breed_group3 - lga_quintile2 - stateTAS - genetic_disorder_count5 - 
#                          pet_de_sexed_ageNot.Sure - pet_de_sexed_age2..years - pet_de_sexed_age7.12.months - 
#                          quote_time_groupEvening..18.00...23.59. - lead_date_day - breed_concat_group8 - 
#                          family_idealFALSE.TRUE.TRUE.FALSE - breed_traitsighthound - breed_traitcollie.related - 
#                          breed_traitpinscher - breed_traitretriever - family_idealFALSE.TRUE.FALSE.FALSE - 
#                          family_idealTRUE.FALSE.FALSE.FALSE - breed_concat_group3 - breed_concat_group11, 
#                        data = data.frame(TRAIN_OH), weights = earned_units, link = log, 
#                        control = glm.control(maxit = 200))
# summary(freq_glm_nb_b)
# 
# sev_glm_ln_b = glm(formula = log(claim_paid) ~ . - earned_units - claim_count + I(claim_count / earned_units) - 
#                    pet_gendermale  - number_of_breeds5 - number_of_breeds6 - breed_traittraditional - 
#                    genetic_disorder_count6 - energy_level4 - pet_de_sexedTRUE - pet_de_sexed_age4.6.months - 
#                    pet_de_sexed_age7.12.months - pet_is_switcherTRUE - stateNSW - stateNT - stateQLD - 
#                    number_of_breeds3 - number_of_breeds4 - breed_typepurebred - breed_traitbull - breed_traitmastiff - 
#                    breed_traitpinscher - breed_traitretriever - breed_traitsetter - breed_traitsighthound - 
#                    breed_traitspitz.related - breed_traitterrier - breed_traitwhite.fluffy - is_multi_pet_planTRUE - 
#                    lead_date_day - quote_time_groupMorning..05.00...11.59. - lga_quintile4 - area - lga_quintile5 - 
#                    household_idealFALSE.FALSE.TRUE.TRUE - household_idealTRUE.FALSE.FALSE.TRUE - 
#                    household_idealTRUE.FALSE.TRUE.TRUE - household_idealTRUE.TRUE.TRUE.TRUE - 
#                    family_idealFALSE.FALSE.TRUE.FALSE - family_idealFALSE.TRUE.TRUE.FALSE - 
#                    family_idealTRUE.FALSE.FALSE.FALSE - family_idealTRUE.FALSE.TRUE.FALSE - quote_month - breed_group2 -
#                    breed_group3 - breed_group4 - breed_group5 - breed_concat_group2 - lga_group3 - 
#                    household_idealFALSE.TRUE.FALSE.TRUE, 
#                  family = gaussian(link = "log"), data = data.frame(TRAIN_OH) %>% filter(claim_paid > 0))
# summary(sev_glm_ln_b)

# # Frequency Prediction
# pred_freq = predict(freq_glm_nb_b, newdata = data.frame(TEST_OH), type = "response")
# rmse_freq = RMSE(pred_freq, TEST$claim_count)
# cat("NegBin Backward Selection Model RMSE:", rmse_freq, "\n")
# 
# # Severity Prediction
# pred_sev = exp(predict(sev_glm_ln_b, newdata = data.frame(TEST_OH) %>% filter(claim_paid > 0), type = "response"))
# rmse_sev = RMSE(pred_sev, TEST %>% filter(claim_paid > 0) %>% pull(claim_paid))
# cat("Gamma Backward Selection Model RMSE:", rmse_sev, "\n")

## LASSO Regression
x_train_nb <- model.matrix(freq_glm_nb)[, -1]  # Exclude intercept
y_train_nb <- TRAIN$claim_count
x_train_ln <- model.matrix(sev_glm_ln)[, -1]  # Exclude intercept
y_train_ln <- log(TRAIN$claim_paid[TRAIN$claim_paid > 0])

# Fit LASSO models
lasso_nb <- glmnet(x_train_nb, y_train_nb, alpha = 1)
lasso_ln <- glmnet(x_train_ln, y_train_ln, alpha = 1)

# Cross-validation for optimal lambda
cv_lasso_nb <- cv.glmnet(x_train_nb, y_train_nb, alpha = 1)
cv_lasso_ln <- cv.glmnet(x_train_ln, y_train_ln, alpha = 1)

# Best lambda
best_lambda_nb <- cv_lasso_nb$lambda.min
best_lambda_ln <- cv_lasso_ln$lambda.min

# Coefficients with best lambda
coef_nb = coef(lasso_nb, s = best_lambda_nb)
coef_ln = coef(lasso_ln, s = best_lambda_ln)

# Diagnostic Plots
plot(cv_lasso_nb)
plot(lasso_nb, xvar = "lambda", label = TRUE)
plot(cv_lasso_ln)
plot(lasso_ln, xvar = "lambda", label = TRUE)

# Frequency Prediction
pred_freq = predict(freq_glm_poi, newdata = TEST, type = "response")
rmse_freq = RMSE(pred_freq, TEST$claim_count)
cat("Poisson Full Model RMSE:", rmse_freq, "\n")

# Severity Prediction
pred_sev = predict(sev_glm_ln, newdata = TEST %>% filter(claim_paid > 0), type = "response")
rmse_sev = RMSE(pred_sev, TEST %>% filter(claim_paid > 0) %>% pull(claim_paid))
cat("Gamma Full Model RMSE:", rmse_sev, "\n")


rm(coef_ln, coef_nb, cv_lasso_ln, cv_lasso_nb, x_train_ln, x_train_nb, best_lambda_ln, best_lambda_nb, y_train_ln, y_train_nb)


### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# 3. Generalised Additive Models ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.1. NegBin-LogNorm Model ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# Frequency Model
# freq_gam_nb = bam(formula = claim_count ~
#                    pet_gendermale + pet_is_switcherTRUE +
#                    contribution90 + contribution100 + excess100 + excess200 + stateNSW +
#                    stateNT + stateSA +stateVIC + s(owner_age_years, k = 10) +
#                    number_of_breeds3 + breed_typedesignerbreed +
#                    breed_typeunnamed.cross + breed_traithound + breed_traitmastiff +
#                    breed_traitspaniel +
#                    breed_traitterrier +
#                    breed_traitwhite.fluffy + quote_time_groupLate.Night..00.00...04.59. +
#                    quote_time_groupMorning..05.00...11.59. + genetic_disorder_count2 + genetic_disorder_count3 +
#                    genetic_disorder_count4 + s(pet_age, k = 5, by = excess200) + s(avg_weight_kg, k = 7, by = excess200) +
#                    s(income_by_lga, k = 10) + lga_quintile3 + lga_quintile4 + seifa_score * contribution100 +
#                    s(area, k = 10) + s(population_density, k = 5, by = address_type_adjHouse) +
#                    s(dwelling_price, k = 10, pet_is_switcherTRUE) + s(unemployment, k = 10) +
#                    s(internet_access_proportion, k = 10) + s(avg_lifespan_yrs, k = 10, by = pet_de_sexedTRUE) +
#                    s(avg_household_size, k = 10) + s(avg_children_per_family, k = 10) +
#                    s(rent_property, k = 10) + s(protected_area, k = 10) +
#                    s(annual_temp, k = 10) + s(pm25, k = 10) + s(weighted_income, k = 5, by = excess200) +
#                    household_idealFALSE.FALSE.FALSE.TRUE + household_idealFALSE.FALSE.TRUE.TRUE +
#                    household_idealFALSE.TRUE.FALSE.TRUE + household_idealFALSE.TRUE.TRUE.TRUE +
#                    household_idealTRUE.FALSE.FALSE.TRUE + household_idealTRUE.FALSE.TRUE.TRUE +
#                    family_idealFALSE.FALSE.TRUE.FALSE +
#                    family_idealTRUE.FALSE.TRUE.FALSE + family_idealTRUE.TRUE.FALSE.FALSE +
#                    s(age_difference, k = 10) + quote_month + breed_group2 + breed_group4 + breed_group5 + breed_concat_group2 +
#                    breed_concat_group4 + breed_concat_group5 + breed_concat_group6 + breed_concat_group7 +
#                    breed_concat_group9 + breed_concat_group10 + lga_group4 + lga_group5 + lga_group7,
#                  family = nb(link = "log"),
#                  data = data.frame(TRAIN_OH), weights = earned_units, select = TRUE)

#freq_gam_nb = bam(formula = claim_count ~ 
#                    s(quote_month) + s(age_difference) + breed_concat_group6 + s(income_by_lga) + number_of_breeds2 + 
#                    breed_concat_group3 + excess100 + s(area) + s(annual_temp) + breed_group5 + address_type_adjHouse + 
#                    s(protected_area) + family_idealTRUE.TRUE.FALSE.FALSE + lga_group4 + s(dwelling_price, by = pet_is_switcherTRUE) + s(married) + 
#                    quote_time_groupLate.Night..00.00...04.59. + stateNSW + breed_traitterrier + breed_traitpointer + 
#                    contribution100 + energy_level3 + breed_traitretriever + breed_typedesignerbreed + 
#                    breed_concat_group8 + pet_de_sexedTRUE + lga_quintile5 + s(avg_household_size) + 
#                    breed_traitspitz.related + pet_de_sexed_age7.12.months + genetic_disorder_count5 + 
#                    breed_concat_group9 + s(weighted_income, k = 10, by = excess200) + s(pet_age, by = excess100, k = 20), 
#                  family = nb(link = "log"), 
#                  data = data.frame(TRAIN_OH), select = TRUE)


freq_gam_nb = bam(formula = claim_count ~ 
                    s(avg_weight_kg) + s(quote_month) + s(owner_age_years) + s(protected_area) + 
                    contribution90 + s(seifa_score) + s(area) +
                    s(avg_children_per_family) + s(annual_temp) + stateNSW + s(rent_property) + s(pm25) + 
                    s(internet_access_proportion) + lga_group7 + breed_group5 + 
                    family_idealTRUE.FALSE.FALSE.FALSE +
                    breed_traithound + contribution100 + s(dwelling_price, by = pet_is_switcherTRUE) + 
                    breed_concat_group9 + breed_typedesignerbreed +
                    s(worked_from_home) + breed_concat_group3 + breed_typepurebred + breed_traitpointer + 
                    stateTAS + breed_traitterrier + family_idealTRUE.TRUE.FALSE.FALSE + 
                    genetic_disorder_count2 + pet_gendermale + energy_level4 + 
                    breed_concat_group5 + number_of_breeds2 + breed_concat_group10 + breed_concat_group4 + 
                    breed_traitteckel + lga_group4 + s(avg_lifespan_yrs) + address_type_adjHouse +
                    s(pet_age, by = excess100, k = 3) + s(weighted_income, k = 5, by = excess200) + s(unemployment) + 
                    s(age_difference) + offset(log(earned_units)), 
                  family = nb(link = "log"), 
                  data = data.frame(TRAIN_OH), 
                  select = TRUE, 
                  control = gam.control(maxit = 100, epsilon = 1e-6))

## Model Diagnostics - NB GAM
residuals <- residuals(freq_gam_nb, type = "deviance")
fitted_values <- fitted(freq_gam_nb)
linear_predictors <- predict(freq_gam_nb, type = "link")

# 1. Response vs Fitted Values Plot
response_vs_fitted <- data.frame(
  fitted_values = fitted_values,
  response = freq_gam_nb$y  # Use the response variable from the model
)
plot1 <- ggplot(response_vs_fitted, aes(x = fitted_values, y = response)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "#49A5F1", linetype = "dashed") +
  labs(title = "Response vs Fitted Values", x = "Fitted values", y = "Response") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.line = element_line(linewidth = 0.5, colour = "grey80"), 
        text = element_text(family = "DM Sans", size = 8))

# 2. Histogram of Residuals Plot
plot2 <- ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.4) +
  labs(title = "Histogram of Deviance Residuals", x = "Residuals", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.line = element_line(linewidth = 0.5, colour = "grey80"), 
        text = element_text(family = "DM Sans", size = 8))

# 3. Residuals vs Linear Predictors Plot
residuals_vs_linear_pred <- data.frame(
  linear_predictors = linear_predictors,
  residuals = residuals
)
plot3 <- ggplot(residuals_vs_linear_pred, aes(x = linear_predictors, y = residuals)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#49A5F1") +
  labs(title = "Residuals vs Linear Predictors", x = "Linear Predictors", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.line = element_line(linewidth = 0.5, colour = "grey80"), 
        text = element_text(family = "DM Sans", size = 8))

# 4. Q-Q Plot of Deviance Residuals as grob
freq_gam_nb_Viz <- getViz(freq_gam_nb)
qq_plot <- qq.gamViz(freq_gam_nb_Viz, rep = 50, level = 0.9, 
                     a.qqpoi = list(shape = 1, size = 0.5, alpha = 0.25),
                     a.ablin = list(colour = "#49A5F1")) +
  labs(title = "Q-Q Plot of Deviance Residuals", x = "Theoretical Quantiles", y = "Deviance Residuals") +
  theme_grey() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.line = element_line(linewidth = 0.5, colour = "grey80"), 
        text = element_text(family = "DM Sans", size = 8))
qq_grob <- grid::grid.grabExpr(print(qq_plot))

# Arrange all four plots in a 2x2 grid using grid.arrange
combined_plot_nb_gam = grid.arrange(plot1, plot2, plot3, qq_grob, ncol = 2, nrow = 2)
ggsave("Model Diagnostics NegBin GAM.png", path = "Graphs and Figures/", combined_plot_nb_gam, 
       width = 15, height = 15, units = "cm")

# Frequency Prediction
pred_freq_test = predict(freq_gam_nb, newdata = data.frame(TEST_OH), type = "response")
RMSE(pred_freq_test, TEST$claim_count)

pred_freq_train = predict(freq_gam_nb, newdata = data.frame(TRAIN_OH), type = "response")
RMSE(pred_freq_train, TRAIN$claim_count)

gam.check(freq_gam_nb)
summary(freq_gam_nb)

# Severity Model (Gaussian) with smooth terms
# sev_gam_ln = gam(formula = log(claim_paid) ~
#                   pet_de_sexed_age2..years + contribution100 * excess100 +
#                   stateTAS + stateVIC + stateWA * number_of_breeds2 +
#                   breed_typeunnamed.cross + breed_traithound +
#                   quote_time_groupEvening..18.00...23.59. +
#                   genetic_disorder_count3 + genetic_disorder_count4 +
#                   energy_level2 + energy_level3 +
#                   lga_quintile3 + s(avg_weight_kg, by = pet_gendermale) +
#                   s(dwelling_price, k = 10) +
#                   s(avg_household_size, k = 10) +
#                   s(protected_area, k = 10) + s(pm25, k = 10) +
#                   s(weighted_income, k = 20) +
#                   family_idealTRUE.TRUE.FALSE.FALSE + breed_concat_group3 + breed_concat_group4 + breed_concat_group5 +
#                   breed_concat_group6 + breed_concat_group7 + breed_concat_group8 + breed_concat_group9 +
#                   breed_concat_group10 + breed_concat_group11 + lga_group5 + lga_group7 +
#                   s(freq, k = 30) + te(freq, annual_temp) + te(dwelling_price, weighted_income),
#                 family = gaussian(link = "identity"), data = data.frame(TRAIN_OH) %>% filter(claim_paid > 0) %>% mutate(freq = claim_count / earned_units), select = TRUE)
# 
sev_gam_ln = gam(formula = log(claim_paid) ~
                   lga_group7 + family_idealFALSE.TRUE.FALSE.FALSE + s(avg_weight_kg) + breed_concat_group10 + excess200 +
                   s(area) + s(protected_area) + s(dwelling_price) + s(pm25) + s(seifa_score, by = excess200) + s(married, breed_group5) +
                   breed_group5 + breed_concat_group9 + pet_gendermale + breed_concat_group2 + s(age_difference) + pet_is_switcherTRUE +
                   breed_traitretriever + lga_group4 + number_of_breeds2 + s(unemployment) + s(avg_lifespan_yrs) + s(income_by_lga) +
                   is_multi_pet_planTRUE + contribution100 + s(rent_property) + s(internet_access_proportion) +
                   breed_traitpointer + pet_de_sexedTRUE + pet_de_sexed_age2..years + excess100 + s(owner_age_years) + s(avg_household_size) +
                   s(quote_month) + s(avg_children_per_family) + breed_group2 +
                   breed_traithound + te(freq, annual_temp) + claim_count,
                 family = gaussian(link = "identity"), data = data.frame(TRAIN_OH) %>% filter(claim_paid > 0) %>% mutate(freq = claim_count / earned_units), select = TRUE)

pred_sev_test = predict(sev_gam_ln, newdata = data.frame(TEST_OH) %>% filter(claim_paid > 0) %>% mutate(freq = claim_count / earned_units), type = "response")
RMSE(pred_sev_test, TEST %>% filter(claim_paid > 0) %>% pull(claim_paid))

# sev_gam_ln = gam(formula = claim_paid ~
#                    lga_group7 + family_idealFALSE.TRUE.FALSE.FALSE + s(avg_weight_kg) + breed_concat_group10 + excess200 + 
#                    s(area) + s(protected_area) + s(dwelling_price) + s(pm25) + s(seifa_score, by = excess200) + s(married, breed_group5) + 
#                    breed_group5 + breed_concat_group9 + pet_gendermale + breed_concat_group2 + s(age_difference) + pet_is_switcherTRUE + 
#                    breed_traitretriever + lga_group4 + number_of_breeds2 + s(unemployment) + s(avg_lifespan_yrs) + s(income_by_lga) +
#                    is_multi_pet_planTRUE + contribution100 + s(rent_property) + s(internet_access_proportion) +
#                    breed_traitpointer + pet_de_sexedTRUE + pet_de_sexed_age2..years + excess100 + s(owner_age_years) + s(avg_household_size) +
#                    s(quote_month) + s(avg_children_per_family) + breed_group2 + 
#                    breed_traithound +
#                    te(freq, annual_temp), 
#                  family = Gamma(link = "log"), data = data.frame(TRAIN_OH) %>% filter(claim_paid > 0) %>% mutate(freq = claim_count / earned_units), select = TRUE)
# 
# sev_gam_gamma = gam(formula = claim_paid ~
#                       pet_de_sexed_age2..years + contribution100 * excess100 +
#                       stateVIC + stateWA * number_of_breeds2 +
#                       breed_typeunnamed.cross + breed_traithound +
#                       quote_time_groupEvening..18.00...23.59. +
#                       genetic_disorder_count3 + genetic_disorder_count4 +
#                       energy_level2 + energy_level3 +
#                       lga_quintile3 + s(avg_weight_kg, by = pet_gendermale) +
#                       s(dwelling_price, k = 10) +
#                       s(avg_household_size, k = 10) +
#                       s(protected_area, k = 10) + s(pm25, k = 10) +
#                       s(weighted_income, k = 20) +
#                       family_idealTRUE.TRUE.FALSE.FALSE + breed_concat_group3 + breed_concat_group4 + breed_concat_group5 +
#                       breed_concat_group6 + breed_concat_group7 + breed_concat_group8 + breed_concat_group9 +
#                       breed_concat_group10 + breed_concat_group11 + lga_group5 + lga_group7 +
#                       s(freq, k = 30) + te(freq, annual_temp) + te(dwelling_price, weighted_income) + claim_count,
#                     family = Gamma(link = "log"), data = data.frame(TRAIN_OH) %>% filter(claim_paid > 0) %>% mutate(freq = claim_count / earned_units), select = TRUE)

sev_gam_gamma = gam(formula = claim_paid ~
                      lga_group8 + contribution100 * excess200 + family_idealFALSE.TRUE.FALSE.FALSE + 
                      stateVIC + stateWA * number_of_breeds2 +
                      breed_typeunnamed.cross + breed_traithound + breed_traitpointer +
                      quote_time_groupEvening..18.00...23.59. +
                      genetic_disorder_count3 + genetic_disorder_count4 +
                      energy_level2 + energy_level3 +
                      lga_quintile3 + s(avg_weight_kg, by = pet_gendermale) +
                      s(avg_household_size, k = 10) +
                      s(protected_area, k = 10) + s(pm25, k = 10) +
                      s(weighted_income, k = 20) +
                      s(area) + 
                      s(married) +
                      s(seifa_score) +
                      s(unemployment) +
                      s(income_by_lga) +
                      s(quote_month) +
                      s(avg_children_per_family) + 
                      s(internet_access_proportion) +
                      family_idealTRUE.TRUE.FALSE.FALSE + breed_concat_group2  + breed_group2 +
                      breed_concat_group10 + breed_concat_group9 + lga_group5 + lga_group7 + lga_group4 +
                      s(freq, k = 30) + te(freq, annual_temp) + te(dwelling_price, weighted_income) + claim_count,
                    family = Gamma(link = "log"), data = data.frame(TRAIN_OH) %>% filter(claim_paid > 0) %>% mutate(freq = claim_count / earned_units), select = TRUE)



# sev_gam_gamma = gam(formula = claim_paid ~
#                   breed_traitbull + breed_group3 + lga_group4 + breed_traitpinscher + breed_traitpointer +
#                   number_of_breeds3 + lga_group5 + breed_traitmastiff + household_idealFALSE.TRUE.TRUE.TRUE +
#                   pet_de_sexed_age1.2.years + breed_traithound + family_idealFALSE.FALSE.TRUE.FALSE + lga_quintile2 +
#                   pet_gendermale + energy_level4 + breed_traitshepherd.type +
#                   family_idealFALSE.TRUE.FALSE.FALSE +
#                   household_idealTRUE.FALSE.FALSE.TRUE +  stateWA * number_of_breeds2 + avg_children_per_family +
#                   genetic_disorder_count3 + breed_group4 + s(worked_from_home) + s(annual_temp) + s(owner_age_years) +
#                   s(quote_month) + genetic_disorder_count6 +
#                   te(freq, annual_temp) + s(dwelling_price, k = 10) +
#                   contribution100 * excess200,
#                 family = Gamma(link = "log"), data = data.frame(TRAIN_OH) %>% filter(claim_paid > 0) %>% mutate(freq = claim_count / earned_units), select = TRUE)


# Severity Prediction
pred_sev_test = predict(sev_gam_gamma, newdata = data.frame(TEST_OH) %>% filter(claim_paid > 0) %>% mutate(freq = claim_count / earned_units), type = "response")
RMSE(pred_sev_test, TEST %>% filter(claim_paid > 0) %>% pull(claim_paid))

pred_sev_train = predict(sev_gam_gamma, newdata = data.frame(TRAIN_OH) %>% filter(claim_paid > 0) %>% mutate(freq = claim_count / earned_units), type = "response")
RMSE(pred_sev_train, TRAIN %>% filter(claim_paid > 0) %>% pull(claim_paid))

gam.check(sev_gam_gamma)
summary(sev_gam_gamma)

mean(pred_sev_test)
mean(data.frame(TEST_OH) %>% filter(claim_paid > 0) %>% pull(claim_paid))

## Model Diagnostics - Gamma GAM
residuals <- residuals(sev_gam_gamma, type = "deviance")
fitted_values <- fitted(sev_gam_gamma)
linear_predictors <- predict(sev_gam_gamma, type = "link")

# 1. Response vs Fitted Values Plot
response_vs_fitted <- data.frame(
  fitted_values = fitted_values,
  response = sev_gam_gamma$y  # Use the response variable from the model
)
plot1 <- ggplot(response_vs_fitted, aes(x = fitted_values, y = response)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "#49A5F1", linetype = "dashed") +
  labs(title = "Response vs Fitted Values", x = "Fitted values", y = "Response") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.line = element_line(linewidth = 0.5, colour = "grey80"), 
        text = element_text(family = "DM Sans", size = 8))

# 2. Histogram of Residuals Plot
plot2 <- ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "black", alpha = 0.4) +
  labs(title = "Histogram of Deviance Residuals", x = "Residuals", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.line = element_line(linewidth = 0.5, colour = "grey80"), 
        text = element_text(family = "DM Sans", size = 8))

# 3. Residuals vs Linear Predictors Plot
residuals_vs_linear_pred <- data.frame(
  linear_predictors = linear_predictors,
  residuals = residuals
)
plot3 <- ggplot(residuals_vs_linear_pred, aes(x = linear_predictors, y = residuals)) +
  geom_point(alpha = 0.25, size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#49A5F1") +
  labs(title = "Residuals vs Linear Predictors", x = "Linear Predictors", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.line = element_line(linewidth = 0.5, colour = "grey80"), 
        text = element_text(family = "DM Sans", size = 8))

# 4. Q-Q Plot of Deviance Residuals as grob
sev_gam_gamma_Viz <- getViz(sev_gam_gamma)
qq_plot <- qq.gamViz(sev_gam_gamma_Viz, rep = 50, level = 0.9, 
                     a.qqpoi = list(shape = 1, size = 0.5, alpha = 0.25),
                     a.ablin = list(colour = "#49A5F1")) +
  labs(title = "Q-Q Plot of Deviance Residuals", x = "Theoretical Quantiles", y = "Deviance Residuals") +
  theme_grey() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.line = element_line(linewidth = 0.5, colour = "grey80"), 
        text = element_text(family = "DM Sans", size = 8))
qq_grob <- grid::grid.grabExpr(print(qq_plot))

# Arrange all four plots in a 2x2 grid using grid.arrange
combined_plot_gamma_gam = grid.arrange(plot1, plot2, plot3, qq_grob, ncol = 2, nrow = 2)
ggsave("Model Diagnostics Gamma GAM.png", path = "Graphs and Figures/", combined_plot_gamma_gam, 
       width = 15, height = 15, units = "cm")



### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.2. Ensemble Methods ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Residuals
freq_residuals_train <- TRAIN$claim_count - pred_freq_train
sev_residuals_train <- TRAIN %>% filter(claim_paid > 0) %>% pull(claim_paid) - pred_sev_train

freq_residuals_test <- TEST$claim_count / TEST$earned_units - pred_freq_test
sev_residuals_test <- TEST %>% filter(claim_paid > 0) %>% pull(claim_paid) - pred_sev_test

# Preparing Dataset
TRAIN_OH = data.frame(TRAIN_OH)
TEST_OH = data.frame(TEST_OH)

FREQ_TRAIN_OH <- data.frame(TRAIN_OH %>% dplyr::select(-claim_count, -claim_paid, -avg_weight_kg, -quote_month, 
                                                       -owner_age_years, -protected_area, -contribution90, -seifa_score, 
                                                       -area, -avg_children_per_family, -annual_temp, -stateNSW, 
                                                       -rent_property, -pm25, -internet_access_proportion, -lga_group7, 
                                                       -breed_group5, -family_idealTRUE.FALSE.FALSE.FALSE, 
                                                       -breed_traithound, -contribution100, -dwelling_price, 
                                                       -breed_concat_group9, -breed_typedesignerbreed, 
                                                       -worked_from_home, -breed_concat_group3, -breed_typepurebred, 
                                                       -breed_traitpointer, -stateTAS, -breed_traitterrier, 
                                                       -family_idealTRUE.TRUE.FALSE.FALSE, -genetic_disorder_count2, 
                                                       -pet_gendermale, -energy_level4, -breed_concat_group5, 
                                                       -number_of_breeds2, -breed_concat_group10, -breed_concat_group4, 
                                                       -breed_traitteckel, -lga_group4, -avg_lifespan_yrs, 
                                                       -address_type_adjHouse, -pet_age, -weighted_income, 
                                                       -unemployment, -age_difference), 
                            freq_resid = freq_residuals_train)
SEV_TRAIN_OH <- data.frame(TRAIN_OH %>% 
                             filter(claim_paid > 0) %>% 
                             dplyr::select(-claim_count, -claim_paid, -pet_de_sexed_age2..years, -contribution100, 
                                           -excess100, -stateVIC, -stateWA, -number_of_breeds2, 
                                           -breed_typeunnamed.cross, -breed_traithound, 
                                           -quote_time_groupEvening..18.00...23.59., -genetic_disorder_count3, 
                                           -genetic_disorder_count4, -energy_level2, -energy_level3, -lga_quintile3, 
                                           -avg_weight_kg, -dwelling_price, -avg_household_size, -protected_area, -pm25,
                                           -weighted_income, -family_idealTRUE.TRUE.FALSE.FALSE, -breed_concat_group3, 
                                           -breed_concat_group4, -breed_concat_group5, -breed_concat_group6, 
                                           -breed_concat_group7, -breed_concat_group8, -breed_concat_group9, 
                                           -breed_concat_group10, -breed_concat_group11, -lga_group5, -lga_group7, 
                                           -annual_temp, -dwelling_price, -weighted_income), 
                           sev_resid = sev_residuals_train)

freq_dmatrix <- xgb.DMatrix(data = as.matrix(FREQ_TRAIN_OH[ , -ncol(FREQ_TRAIN_OH)]), label = FREQ_TRAIN_OH$freq_resid)
sev_dmatrix <- xgb.DMatrix(data = as.matrix(SEV_TRAIN_OH[ , -ncol(SEV_TRAIN_OH)]), label = SEV_TRAIN_OH$sev_resid)


FREQ_TEST_OH <- data.frame(TEST_OH %>% dplyr::select(-claim_count, -claim_paid, -avg_weight_kg, -quote_month, 
                                                     -owner_age_years, -protected_area, -contribution90, -seifa_score, 
                                                     -area, -avg_children_per_family, -annual_temp, -stateNSW, 
                                                     -rent_property, -pm25, -internet_access_proportion, -lga_group7, 
                                                     -breed_group5, -family_idealTRUE.FALSE.FALSE.FALSE, 
                                                     -breed_traithound, -contribution100, -dwelling_price, 
                                                     -breed_concat_group9, -breed_typedesignerbreed, 
                                                     -worked_from_home, -breed_concat_group3, -breed_typepurebred, 
                                                     -breed_traitpointer, -stateTAS, -breed_traitterrier, 
                                                     -family_idealTRUE.TRUE.FALSE.FALSE, -genetic_disorder_count2, 
                                                     -pet_gendermale, -energy_level4, -breed_concat_group5, 
                                                     -number_of_breeds2, -breed_concat_group10, -breed_concat_group4, 
                                                     -breed_traitteckel, -lga_group4, -avg_lifespan_yrs, 
                                                     -address_type_adjHouse, -pet_age, -weighted_income, 
                                                     -unemployment, -age_difference))
SEV_TEST_OH <- data.frame(TEST_OH %>% 
                            filter(claim_paid > 0) %>% 
                            dplyr::select(-claim_count, -claim_paid, -pet_de_sexed_age2..years, -contribution100, 
                                          -excess100, -stateVIC, -stateWA, -number_of_breeds2, 
                                          -breed_typeunnamed.cross, -breed_traithound, 
                                          -quote_time_groupEvening..18.00...23.59., -genetic_disorder_count3, 
                                          -genetic_disorder_count4, -energy_level2, -energy_level3, -lga_quintile3, 
                                          -avg_weight_kg, -dwelling_price, -avg_household_size, -protected_area, -pm25,
                                          -weighted_income, -family_idealTRUE.TRUE.FALSE.FALSE, -breed_concat_group3, 
                                          -breed_concat_group4, -breed_concat_group5, -breed_concat_group6, 
                                          -breed_concat_group7, -breed_concat_group8, -breed_concat_group9, 
                                          -breed_concat_group10, -breed_concat_group11, -lga_group5, -lga_group7, 
                                          -annual_temp, -dwelling_price, -weighted_income))



# Frequency Model
freq_xgb <- xgboost(
  data = freq_dmatrix,
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.0973,
  gamma = 3.06,
  nrounds = 57,
  min_chid_weight = 8.67,
  subsample = 0.896,
  colsample_bytree = 0.588,
  lambda = 4.07,
  alpha = 3.63
)

# Severity Model
sev_xgb <- xgboost(
  data = sev_dmatrix,
  objective = "reg:squarederror",
  max_depth = 12,
  eta = 0.0103,
  gamma = 3.02,
  nrounds = 104, # 294
  min_chid_weight = 12.4,
  subsample = 0.524,
  colsample_bytree = 0.875,
  lambda = 3.92,
  alpha = 2.95
)

# Predictions
freq_xgb_pred <- predict(freq_xgb, newdata = as.matrix(FREQ_TEST_OH))
freq_xgb_pred = ifelse(pred_freq_test + freq_xgb_pred < 0, 0, freq_xgb_pred)

sev_xgb_pred <- predict(sev_xgb, newdata = as.matrix(SEV_TEST_OH)) 
sev_xgb_pred = ifelse(pred_sev_test + sev_xgb_pred < 0, 0, sev_xgb_pred)


# Combining Results
final_freq_pred <- pred_freq_test + freq_xgb_pred
final_sev_pred <- pred_sev_test + sev_xgb_pred

# Evaluate performance
rmse_freq_final <- RMSE(final_freq_pred, TEST$claim_count)
rmse_sev_final <- RMSE(final_sev_pred, TEST %>% filter(claim_paid > 0) %>% pull(claim_paid))

cat("Final RMSE for Frequency Prediction:", rmse_freq_final, "\n")
cat("Final RMSE for Severity Prediction:", rmse_sev_final, "\n")


### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.3. Hyperparameter Tuning ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# Ensure reproducibility
set.seed(4305)

# Create Regression Tasks
freq_traintask <- makeRegrTask(data = FREQ_TRAIN_OH, target = "freq_resid")
sev_traintask <- makeRegrTask(data = SEV_TRAIN_OH, target = "sev_resid")

# Create Learner
lrn <- makeLearner("regr.xgboost", predict.type = "response")

# Set parameter space for tuning (including regularization parameters)
params <- makeParamSet(
  makeDiscreteParam("booster", values = c("gbtree")),  # Focus on tree-based booster
  makeIntegerParam("max_depth", lower = 3L, upper = 15L),
  makeNumericParam("eta", lower = 0.01, upper = 0.3),
  makeNumericParam("gamma", lower = 0, upper = 5),
  makeNumericParam("lambda", lower = 0, upper = 5),
  makeNumericParam("alpha", lower = 0, upper = 5),
  makeIntegerParam("nrounds", lower = 50L, upper = 300L),
  makeNumericParam("min_child_weight", lower = 5L, upper = 15L),
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1)
)

# Set Resampling Strategy (5-fold Cross-Validation)
rdesc <- makeResampleDesc("CV", iters = 10)

# Increase iterations in random search for better hyperparameter tuning
ctrl <- makeTuneControlRandom(maxit = 50)

# Start Parallel Processing
parallelStartSocket(cpus = detectCores())

### Frequency Model ###

# Parameter Tuning for Frequency Model
freq_mytune <- tuneParams(
  learner = lrn,
  task = freq_traintask,
  resampling = rdesc,
  measures = rmse,
  par.set = params,
  control = ctrl,
  show.info = TRUE
)

# Train Frequency model with Tuned Hyperparameters
freq_lrn_tune <- setHyperPars(lrn, par.vals = freq_mytune$x)
freq_xgmodel_final <- train(learner = freq_lrn_tune, task = freq_traintask)


### Severity Model ###

# Parameter Tuning for Severity Model
sev_mytune <- tuneParams(
  learner = lrn,
  task = sev_traintask,
  resampling = rdesc,
  measures = rmse,
  par.set = params,
  control = ctrl,
  show.info = TRUE
)

# Train Severity model with Tuned Hyperparameters
sev_lrn_tune <- setHyperPars(lrn, par.vals = sev_mytune$x)
sev_xgmodel_final <- train(learner = sev_lrn_tune, task = sev_traintask)


## Predictions for test set
freq_xgb_pred <- predict(freq_xgmodel_final, newdata = FREQ_TEST_OH)$data$response / 3.5
freq_xgb_pred = ifelse(pred_freq_test + freq_xgb_pred < 0, 0, freq_xgb_pred)

sev_xgb_pred <- predict(sev_xgmodel_final, newdata = SEV_TEST_OH)$data$response / 5
sev_xgb_pred = ifelse(pred_sev_test + sev_xgb_pred < 0, 0, sev_xgb_pred)

# Combine predictions
final_freq_pred <- pred_freq_test + freq_xgb_pred
final_sev_pred <- pred_sev_test + sev_xgb_pred

# RMSE for final predictions
rmse_freq_final <- RMSE(final_freq_pred, TEST$claim_count)
rmse_sev_final <- RMSE(final_sev_pred, TEST %>% filter(claim_paid > 0) %>% pull(claim_paid))

cat("Final RMSE for Frequency Prediction:", rmse_freq_final, "\n")
cat("Final RMSE for Severity Prediction:", rmse_sev_final, "\n")


# Stop Parallel Processing
parallelStop()



### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.4. GBM with Best Parameters ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# Frequency Model
freq_xgb <- xgboost(
  data = freq_dmatrix,
  objective = "reg:squarederror",
  max_depth = 4,
  eta = 0.0973,
  gamma = 3.06,
  nrounds = 57,
  min_chid_weight = 8.67,
  subsample = 0.896,
  colsample_bytree = 0.588,
  lambda = 4.07,
  alpha = 3.63
)

# Severity Model
sev_xgb <- xgboost(
  data = sev_dmatrix,
  objective = "reg:squarederror",
  max_depth = 12,
  eta = 0.0103,
  gamma = 3.02,
  nrounds = 104, # 294
  min_chid_weight = 12.4,
  subsample = 0.524,
  colsample_bytree = 0.875,
  lambda = 3.92,
  alpha = 2.95
)

# Predictions
freq_xgb_pred <- predict(freq_xgb, newdata = as.matrix(FREQ_TEST_OH)) / 3.5
freq_xgb_pred = ifelse(pred_freq_test + freq_xgb_pred < 0, 0, freq_xgb_pred)

sev_xgb_pred <- predict(sev_xgb, newdata = as.matrix(SEV_TEST_OH)) / 5
sev_xgb_pred = ifelse(pred_sev_test + sev_xgb_pred < 0, 0, sev_xgb_pred)


# Combining Results
final_freq_pred <- pred_freq_test + freq_xgb_pred
final_sev_pred <- pred_sev_test + sev_xgb_pred

# Evaluate performance
rmse_freq_final <- RMSE(final_freq_pred, TEST$claim_count)
rmse_sev_final <- RMSE(final_sev_pred, TEST %>% filter(claim_paid > 0) %>% pull(claim_paid))

cat("Final RMSE for Frequency Prediction:", rmse_freq_final, "\n")
cat("Final RMSE for Severity Prediction:", rmse_sev_final, "\n")

mean(final_sev_pred)
mean(TEST %>% filter(claim_paid > 0) %>% pull(claim_paid))

# # Cross-Validation for Frequency Model
# cv_freq <- xgb.cv(
#   params = freq_mytune$x,
#   data = freq_dmatrix,
#   nrounds = 100,
#   nfold = 5,  # Number of folds for cross-validation
#   metrics = "rmse",
#   early_stopping_rounds = 10,
#   print_every_n = 10,
#   as_iterator = FALSE,
#   maximize = FALSE
# )
# 
# # Cross-Validation for Severity Model
# cv_sev <- xgb.cv(
#   params = sev_mytune$x,
#   data = sev_dmatrix,
#   nrounds = 100,
#   nfold = 5,  # Number of folds for cross-validation
#   metrics = "rmse",
#   early_stopping_rounds = 10,
#   print_every_n = 10,
#   as_iterator = FALSE,
#   maximize = FALSE
# )
# 
# # Retrieve and print the best RMSE from CV results
# best_rmse_freq <- min(cv_freq$evaluation_log$test_rmse_mean)
# best_rmse_sev <- min(cv_sev$evaluation_log$test_rmse_mean)
# 
# cat("Best RMSE for Frequency Model (Cross-Validation):", best_rmse_freq, "\n")
# cat("Best RMSE for Severity Model (Cross-Validation):", best_rmse_sev, "\n")



## Model Diagnostics
freq_residuals <- final_freq_pred - TEST$claim_count
sev_residuals <- final_sev_pred - TEST %>% filter(claim_paid > 0) %>% pull(claim_paid)

# Creating DataFrame
model_data_freq <- data.frame(
  .fitted = final_freq_pred,
  .resid = freq_residuals
)

model_data_sev <- data.frame(
  .fitted = final_sev_pred,
  .resid = sev_residuals
)

### Plot 1: Residuals vs Fitted for Frequency Model
plot1_freq <- ggplot(model_data_freq, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 20))

### Plot 2: Normal Q-Q plot for Frequency Model
plot2_freq <- ggplot(model_data_freq, aes(sample = .resid)) +
  stat_qq(size = 0.5) +
  stat_qq_line(size = 0.5, color = "#49A5F1") +
  #geom_abline(slope = 1, intercept = 0, size = 0.5, color = "#49A5F1") +
  ylim(-1, 3) +
  labs(title = "Normal Q-Q Plot", 
       x = "Theoretical Quantiles", 
       y = "Sample Residuals") + 
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 20))

### Plot 3: Scale-Location plot for Frequency Model
plot3_freq <- ggplot(model_data_freq, aes(x = .fitted, y = sqrt(abs(.resid)))) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Scale-Location Plot", 
       x = "Fitted values", 
       y = "√|Residuals|") + 
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 20))

# Plot 4: Residuals vs Leverage for Frequency Model
model_data_freq$.hat <- predict(freq_xgb, newdata = as.matrix(FREQ_TEST_OH), type = "response")
plot4_freq <- ggplot(model_data_freq, aes(x = .hat, y = .resid)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Leverage", 
       x = "Leverage", 
       y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 20))

# Combine all frequency plots into one
combined_plot_freq <- (plot1_freq | plot2_freq) / (plot3_freq | plot4_freq) +
  plot_annotation(title = "Model Diagnostics for Ensemble Frequency Model",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 40, family = "DM Sans")))

combined_plot_freq
ggsave("Model Diagnostics for Ensemble Frequency Model.png", path = "Graphs and Figures/", combined_plot_freq, 
       width = 15, height = 15, units = "cm")

# Plot 1: Residuals vs Fitted for Severity Model
plot1_sev <- ggplot(model_data_sev, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 20))

# Plot 2: Normal Q-Q plot for Severity Model
plot2_sev <- ggplot(model_data_sev, aes(sample = .resid)) +
  stat_qq(size = 0.5) +
  stat_qq_line(size = 0.5, color = "#49A5F1") +
  labs(title = "Normal Q-Q Plot", 
       x = "Theoretical Quantiles", 
       y = "Sample Residuals") + 
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 20))

# Plot 3: Scale-Location plot for Severity Model
plot3_sev <- ggplot(model_data_sev, aes(x = .fitted, y = sqrt(abs(.resid)))) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Scale-Location Plot", 
       x = "Fitted values", 
       y = "√|Residuals|") + 
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 20))

# Plot 4: Residuals vs Leverage for Severity Model
model_data_sev$.hat <- predict(sev_xgb, 
                               newdata = as.matrix(SEV_TEST_OH), 
                               type = "response")
plot4_sev <- ggplot(model_data_sev, aes(x = .hat, y = .resid)) +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(method = "loess", color = "#49A5F1", se = FALSE, size = 0.5) +
  labs(title = "Residuals vs Leverage", 
       x = "Leverage", 
       y = "Residuals") +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme(plot.title = element_text(hjust = 0.5, size = 24),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 20))

# Combine all severity plots into one
combined_plot_sev <- (plot1_sev | plot2_sev) / (plot3_sev | plot4_sev) +
  plot_annotation(title = "Model Diagnostics for Ensemble Severity Model",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 40, family = "DM Sans")))

combined_plot_sev
ggsave("Model Diagnostics for Ensemble Severity Model.png", path = "Graphs and Figures/", combined_plot_sev, 
       width = 15, height = 15, units = "cm")



### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# 4. Random Forest ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 4.1. Training Model ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
set.seed(4305)
freq_rf <- randomForest(claim_count ~ . - earned_units - claim_paid, 
                        data = TRAIN, 
                        ntree = 500, 
                        importance = TRUE,
                        case.weights = TRAIN$earned_units)
set.seed(4305)
sev_rf <- randomForest(claim_paid ~ . - earned_units - claim_count + I(claim_count / earned_units), 
                       data = TRAIN %>% filter(claim_paid > 0), 
                       ntree = 500, 
                       importance = TRUE)

# Frequency Prediction
pred_freq <- predict(freq_rf, newdata = TEST)
rmse_freq <- RMSE(pred_freq, TEST$claim_count)

# Severity Prediciton
pred_sev <- predict(sev_rf, newdata = TEST %>% filter(claim_paid > 0), type = "response")
rmse_sev <- RMSE(pred_sev, TEST %>% filter(claim_paid > 0) %>% pull(claim_paid))

# Print the RMSE for both models to compare
print(rmse_freq)
print(rmse_sev)

# R-squared
rsq_freq <- R2(pred_freq, TEST$claim_count)
rsq_sev <- R2(pred_sev, TEST %>% filter(claim_paid > 0) %>% pull(claim_paid)) 
print(rsq_freq)
print(rsq_sev)

# OOB error
print(freq_rf)
print(sev_rf)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 4.2. Feature Selection III ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == 
## Frequency Model
# Get variable importance scores
freq_importance_scores <- as.data.frame(importance(freq_rf))
sev_importance_scores <- as.data.frame(importance(sev_rf))

# Add variable names
freq_importance_scores$Variable <- rownames(freq_importance_scores)
sev_importance_scores$Variable <- rownames(sev_importance_scores)

# Sort by %IncMSE
freq_importance_scores <- freq_importance_scores %>%
  arrange(desc(`%IncMSE`))  
sev_importance_scores <- sev_importance_scores %>%
  arrange(desc(`%IncMSE`))

# Select important variables with positive %IncMSE
freq_selected_variables <- freq_importance_scores %>%
  filter(`%IncMSE` > 0) %>%  
  pull(Variable)
sev_selected_variables <- sev_importance_scores %>%
  filter(`%IncMSE` > 0) %>%  filter(Variable != "I(claim_count/earned_units)") %>%
  pull(Variable)

cat("Selected Variables: ", freq_selected_variables, "\n")
cat("Selected Variables: ", sev_selected_variables, "\n")

rm(freq_importance_scores,sev_importance_scores)

set.seed(4305)
freq_rf <- randomForest(claim_count ~ . - earned_units - claim_paid, 
                        data = TRAIN[, c("claim_count", "earned_units", "claim_paid", freq_selected_variables)], 
                        ntree = 500,
                        importance = TRUE,
                        case.weights = TRAIN$earned_units)
set.seed(4305)
sev_rf <- randomForest(claim_paid ~ . - earned_units - claim_count + I(claim_count / earned_units), 
                       data = TRAIN[, c("claim_count", "earned_units", "claim_paid", sev_selected_variables)] 
                       %>% filter(claim_paid > 0), 
                       ntree = 500,
                       importance = TRUE)

# Frequency Prediction
pred_freq <- predict(freq_rf, newdata = TEST[, c("claim_count", "earned_units", "claim_paid", freq_selected_variables)])
rmse_freq <- RMSE(pred_freq, TEST[, c("claim_count", "earned_units", "claim_paid", freq_selected_variables)]$claim_count)

# Severity Prediciton
pred_sev <- predict(sev_rf, newdata = TEST[, c("claim_count", "earned_units", "claim_paid", sev_selected_variables)] 
                    %>% filter(claim_paid > 0), type = "response")
rmse_sev <- RMSE(pred_sev, TEST[, c("claim_count", "earned_units", "claim_paid", sev_selected_variables)] 
                 %>% filter(claim_paid > 0) %>% pull(claim_paid))

# Print the RMSE for both models to compare
print(rmse_freq)
print(rmse_sev)

# R-squared
rsq_freq <- R2(pred_freq, TEST[, c("claim_count", "earned_units", "claim_paid", freq_selected_variables)]$claim_count)
rsq_sev <- R2(pred_sev, TEST[, c("claim_count", "earned_units", "claim_paid", sev_selected_variables)] 
              %>% filter(claim_paid > 0) %>% pull(claim_paid)) 
print(rsq_freq)
print(rsq_sev)

# OOB error
print(freq_rf)
print(sev_rf)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# 5. XGBoost Model ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 5.1. Training Model ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# Train the XGBoost model
set.seed(4305)
params <- list(objective = "reg:squarederror",  # For regression
               booster = "gbtree",  # Use trees
               eta = 0.1,  # Learning rate
               max_depth = 6,  # Tree depth
               subsample = 0.8,  # Row subsampling
               colsample_bytree = 0.8,  # Feature subsampling
               alpha = 0.1,  # L1 regularization (Lasso)
               lambda = 1)  # L2 regularization (Ridge)

# find best nround
set.seed(4305)
freq_xgbcv <- xgb.cv( params = params, 
                      data = FREQ_TRAIN_OH_D, 
                      nrounds = 200, 
                      nfold = 5, 
                      showsd = T, 
                      stratified = T, 
                      print_every_n = 10,  
                      maximize = F)
set.seed(4305)
sev_xgbcv <- xgb.cv( params = params, 
                     data = SEV_TRAIN_OH_D, 
                     nrounds = 200, 
                     nfold = 5, 
                     showsd = T, 
                     stratified = T, 
                     print_every_n = 10,  
                     maximize = F)

# Plot the test and training error against the number of boosting rounds
freq_cv_results <- as.data.frame(freq_xgbcv$evaluation_log)
sev_cv_results <- as.data.frame(sev_xgbcv$evaluation_log)

ggplot(freq_cv_results, aes(x = iter)) +
  geom_line(aes(y = train_rmse_mean, color = "Train RMSE")) + 
  geom_line(aes(y = test_rmse_mean, color = "Test RMSE")) +
  labs(x = "Number of Boosting Rounds", y = "RMSE", 
       title = "RMSE vs Boosting Rounds - Frequency") +
  theme_minimal() +
  scale_color_manual(name = "Metrics", values = c("Train RMSE" = "blue", "Test RMSE" = "red"))

ggplot(sev_cv_results, aes(x = iter)) +
  geom_line(aes(y = train_rmse_mean, color = "Train RMSE")) + 
  geom_line(aes(y = test_rmse_mean, color = "Test RMSE")) +
  labs(x = "Number of Boosting Rounds", y = "RMSE", 
       title = "RMSE vs Boosting Rounds - Severity") +
  theme_minimal() +
  scale_color_manual(name = "Metrics", values = c("Train RMSE" = "blue", "Test RMSE" = "red"))

# get best nrounds
freq_best_nrounds <- which.min(freq_xgbcv$evaluation_log$test_rmse_mean)
sev_best_nrounds <- which.min(sev_xgbcv$evaluation_log$test_rmse_mean)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 5.2. Hyperparameter Tuning II ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
set.seed(4305)
freq_xgb1 <- xgb.train (params = params, 
                        data = FREQ_TRAIN_OH_D, 
                        nrounds = freq_best_nrounds, 
                        watchlist = list(val=FREQ_TEST_OH_D,train=FREQ_TRAIN_OH_D),
                        print_every_n = 10, 
                        maximize = F , 
                        eval_metric = "rmse")
set.seed(4305)
sev_xgb1 <- xgb.train (params = params, 
                       data = SEV_TRAIN_OH_D, 
                       nrounds = sev_best_nrounds, 
                       watchlist = list(val=SEV_TEST_OH_D,train=SEV_TRAIN_OH_D),
                       print_every_n = 10, 
                       maximize = F , 
                       eval_metric = "rmse")
# model prediction
freq_xgbpred <- predict (freq_xgb1, FREQ_TEST_OH_D)
sev_xgbpred <- predict (sev_xgb1, SEV_TEST_OH_D)

# Calculate RMSE for model evaluation
freq_rmse <- sqrt(mean((freq_xgbpred - FREQ_TEST_Y$claim_count) ^ 2))
sev_rmse <- sqrt(mean((sev_xgbpred - SEV_TEST_Y$claim_paid) ^ 2))
print(freq_rmse)
print(sev_rmse)

#view variable importance plot
freq_mat <- xgb.importance (feature_names = colnames(FREQ_TRAIN_OH_D),model = freq_xgb1)
xgb.plot.importance (importance_matrix = freq_mat)
sev_mat <- xgb.importance (feature_names = colnames(SEV_TRAIN_OH_D),model = sev_xgb1)
xgb.plot.importance (importance_matrix = sev_mat) 

### FREQUENCY ###
# Combine one-hot encoded predictors and target variable into data frames
FREQ_TRAIN_OH <- data.frame(FREQ_TRAIN_OH_X, claim_count = FREQ_TRAIN_Y$claim_count)
FREQ_TEST_OH <- data.frame(FREQ_TEST_OH_X, claim_count = FREQ_TEST_Y$claim_count)

# Create regression tasks with the one-hot encoded data
traintask <- makeRegrTask(data = FREQ_TRAIN_OH, target = "claim_count")  # Change to makeRegrTask
testtask <- makeRegrTask(data = FREQ_TEST_OH, target = "claim_count")

# Create learner for regression
lrn <- makeLearner("regr.xgboost", predict.type = "response")  # Change to regression learner
lrn$par.vals <- list(
  objective = "reg:squarederror",  # Change to regression objective
  eval_metric = "rmse",  # Change to RMSE for regression
  nrounds = freq_best_nrounds,
  eta = 0.1
)

# Set parameter space for tuning
params <- makeParamSet(
  makeDiscreteParam("booster", values = c("gbtree", "gblinear")),
  makeIntegerParam("max_depth", lower = 3L, upper = 10L),
  makeNumericParam("min_child_weight", lower = 1L, upper = 10L),
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1)
)

# Set resampling strategy
rdesc <- makeResampleDesc("CV", iters = 5)
# Search strategy
ctrl <- makeTuneControlRandom(maxit = 10)
# Set parallel backend
parallelStartSocket(cpus = detectCores())

# Parameter tuning
set.seed(4305)
freq_mytune <- tuneParams(learner = lrn,
                          task = traintask,
                          resampling = rdesc,
                          measures = rmse,  # Change to RMSE measure
                          par.set = params,
                          control = ctrl,
                          show.info = T)
freq_mytune$y  # Best RMSE obtained

# Set hyperparameters in learner
freq_lrn_tune <- setHyperPars(lrn, par.vals = freq_mytune$x)
# Train model
set.seed(4305)
freq_xgmodel <- train(learner = freq_lrn_tune, task = traintask)
# Extract the xgboost model from the trained model
freq_xgbmodel <- getLearnerModel(freq_xgmodel)
# Predict on test data
freq_xgpred <- predict(freq_xgmodel, testtask)
# Evaluate model performance (RMSE)
freq_rmse <- performance(freq_xgpred, measures = rmse)
print(freq_rmse)

# view variable importance plot
freq_mat <- xgb.importance(feature_names = freq_xgbmodel$featureNames, model = freq_xgbmodel)
xgb.plot.importance(importance_matrix = freq_mat)

# Stop parallelization
parallelStop()

## SEVERITY ###
# Combine one-hot encoded predictors and target variable into data frames
SEV_TRAIN_OH <- data.frame(SEV_TRAIN_OH_X, claim_paid = SEV_TRAIN_Y$claim_paid)
SEV_TEST_OH <- data.frame(SEV_TEST_OH_X, claim_paid = SEV_TEST_Y$claim_paid)

# Create regression tasks
traintask <- makeRegrTask(data = SEV_TRAIN_OH, target = "claim_paid")  # Change to makeRegrTask
testtask <- makeRegrTask(data = SEV_TEST_OH, target = "claim_paid")

# Create learner for regression
lrn <- makeLearner("regr.xgboost", predict.type = "response")  # Change to regression learner
lrn$par.vals <- list(
  objective = "reg:squarederror",  # Change to regression objective
  eval_metric = "rmse",  # Change to RMSE for regression
  nrounds = sev_best_nrounds,
  eta = 0.1
)

# Set parameter space for tuning
params <- makeParamSet(
  makeDiscreteParam("booster", values = c("gbtree", "gblinear")),
  makeIntegerParam("max_depth", lower = 3L, upper = 10L),
  makeNumericParam("min_child_weight", lower = 1L, upper = 10L),
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1)
)

# Set resampling strategy
rdesc <- makeResampleDesc("CV", iters = 5)
# Search strategy
ctrl <- makeTuneControlRandom(maxit = 10)
# Set parallel backend
parallelStartSocket(cpus = detectCores())

# Parameter tuning
set.seed(4305)
sev_mytune <- tuneParams(learner = lrn,
                         task = traintask,
                         resampling = rdesc,
                         measures = rmse,  # Change to RMSE measure
                         par.set = params,
                         control = ctrl,
                         show.info = T)
sev_mytune$y  # Best RMSE obtained

# Set hyperparameters in learner
sev_lrn_tune <- setHyperPars(lrn, par.vals = sev_mytune$x)
# Train model
set.seed(4305)
sev_xgmodel <- train(learner = sev_lrn_tune, task = traintask)
# Extract the xgboost model from the trained model
sev_xgbmodel <- getLearnerModel(sev_xgmodel)
# Predict on test data
sev_xgpred <- predict(sev_xgmodel, testtask)
# Evaluate model performance (RMSE)
sev_rmse <- performance(sev_xgpred, measures = rmse)
print(sev_rmse)

# view variable importance plot
sev_mat <- xgb.importance(feature_names = sev_xgbmodel$featureNames, model = sev_xgbmodel)
xgb.plot.importance(importance_matrix = sev_mat)

# Stop parallelization
parallelStop()

rm(params, freq_xgbcv, sev_xgbcv, freq_cv_results, sev_cv_results, freq_best_nrounds, sev_best_nrounds, freq_xgb1,
   sev_xgb1, freq_xgbpred, sev_xgbpred, freq_rmse, sev_rmse, freq_mat, sev_mat, FREQ_TRAIN_OH, FREQ_TEST_OH, traintask,
   testtask, lrn, rdesc, ctrl, freq_mytune, freq_lrn_tune, freq_xgmodel, freq_xgbmodel, freq_xgpred, SEV_TRAIN_OH,
   SEV_TEST_OH, sev_mytune, sev_lrn_tune, sev_xgmodel, sev_xgbmodel, sev_xgpred)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# 6. Model Implementation ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# Earned Units
earned_units <- rep(1, nrow(PRICING))
earned_units_df <- data.frame(earned_units)
PRICING_Pred <- cbind(PRICING, earned_units_df)

PRICING_XGB_FREQ <- data.frame(PRICING_Pred %>% dplyr::select(-avg_weight_kg, -quote_month, 
                                                                        -owner_age_years, -protected_area, -contribution90, -seifa_score, 
                                                                        -area, -avg_children_per_family, -annual_temp, -stateNSW, 
                                                                        -rent_property, -pm25, -internet_access_proportion, -lga_group7, 
                                                                        -breed_group5, -family_idealTRUE.FALSE.FALSE.FALSE, 
                                                                        -breed_traithound, -contribution100, -dwelling_price, 
                                                                        -breed_concat_group9, -breed_typedesignerbreed, 
                                                                        -worked_from_home, -breed_concat_group3, -breed_typepurebred, 
                                                                        -breed_traitpointer, -stateTAS, -breed_traitterrier, 
                                                                        -family_idealTRUE.TRUE.FALSE.FALSE, -genetic_disorder_count2, 
                                                                        -pet_gendermale, -energy_level4, -breed_concat_group5, 
                                                                        -number_of_breeds2, -breed_concat_group10, -breed_concat_group4, 
                                                                        -breed_traitteckel, -lga_group4, -avg_lifespan_yrs, 
                                                                        -address_type_adjHouse, -pet_age, -weighted_income, 
                                                                        -unemployment, -age_difference, -lga_group6))
PRICING_XGB_SEV <- data.frame(PRICING_Pred %>% 
                                          dplyr::select(-pet_de_sexed_age2..years, -contribution100, 
                                                        -excess100, -stateVIC, -stateWA, -number_of_breeds2, 
                                                        -breed_typeunnamed.cross, -breed_traithound, 
                                                        -quote_time_groupEvening..18.00...23.59., -genetic_disorder_count3, 
                                                        -genetic_disorder_count4, -energy_level2, -energy_level3, -lga_quintile3, 
                                                        -avg_weight_kg, -dwelling_price, -avg_household_size, -protected_area, -pm25,
                                                        -weighted_income, -family_idealTRUE.TRUE.FALSE.FALSE, -breed_concat_group3, 
                                                        -breed_concat_group4, -breed_concat_group5, -breed_concat_group6, 
                                                        -breed_concat_group7, -breed_concat_group8, -breed_concat_group9, 
                                                        -breed_concat_group10, -breed_concat_group11, -lga_group5, -lga_group7, 
                                                        -annual_temp, -dwelling_price, -weighted_income, -lga_group6))


# Frequency Prediction
pred_freq = predict(freq_gam_nb, newdata = PRICING_Pred, type = "response") * 1.2
resid_freq <- predict(freq_xgmodel_final, newdata = PRICING_XGB_FREQ, type = "response")$data$response / 2
resid_freq = ifelse(pred_freq + resid_freq < 0, 0, resid_freq)
freq_final = pred_freq + resid_freq
summary(freq_final)

# Claim Count
claim_count <- freq_final
PRICING_Pred <- cbind(PRICING_Pred, claim_count)
colnames(PRICING_Pred)[ncol(PRICING_Pred)] <- "claim_count"

# Severity Prediction
pred_sev = predict(sev_gam_gamma, newdata = data.frame(PRICING_Pred) %>% mutate(freq = claim_count / earned_units), type = "response") * 1.2
resid_sev <- predict(sev_xgmodel_final, newdata = PRICING_XGB_SEV, type = "response")$data$response / 3
resid_sev = ifelse(pred_sev + resid_sev < 0, 0, resid_sev)
sev_final = pred_sev + resid_sev

# Adjusting for Dependence
r = 0.708
mu = pred_freq
p = r / (r + mu) # NB Probability Parameter

sevglm = glm(formula = claim_paid ~ 
               pet_de_sexed_age2..years + contribution100 * excess100 + stateVIC + stateWA * number_of_breeds2 +
               breed_typeunnamed.cross + breed_traithound + quote_time_groupEvening..18.00...23.59. + 
               genetic_disorder_count3 + genetic_disorder_count4 + energy_level2 + energy_level3 + lga_quintile3 + 
               avg_weight_kg + dwelling_price + avg_household_size + protected_area + pm25 + weighted_income +
               family_idealTRUE.TRUE.FALSE.FALSE + breed_concat_group3 + breed_concat_group4 + breed_concat_group5 +
               breed_concat_group6 + breed_concat_group7 + breed_concat_group8 + breed_concat_group9 +
               breed_concat_group10 + breed_concat_group11 + lga_group5 + lga_group7 + claim_count,
             family = Gamma(link = "log"), data = data.frame(TRAIN_OH) %>% filter(claim_paid > 0))

alpha = coef(sevglm)["claim_count"][[1]]

correction = (p / (1 - (1 - p) * exp(alpha))) ^ r
summary(correction)

# Final Premium Calculation
price = round(freq_final * sev_final * correction, 2)

PRICING_DF <- PRICING_DF %>%
  mutate(Full_month_premium = price)

ggplot(PRICING_DF) + 
  geom_histogram(aes(x = Full_month_premium), bins = 50)
summary(PRICING_DF$Full_month_premium)

write.csv(PRICING_DF, "UG23_pricing_output.csv")


########################################################################################################################
######################################## Fluffsure Pet Insurance Pricing Model ######################################### 
################################################## Model Development ################################################### 
########################################################################################################################