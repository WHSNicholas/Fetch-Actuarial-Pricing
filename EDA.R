########################################################################################################################
######################################## Fluffsure Pet Insurance Pricing Model ######################################### 
############################################## Exploratory Data Analysis ############################################### 
########################################################################################################################


### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# Title: Fluffsure Pet Insurance Pricing Model
# Script: Exploratory Data Analysis
#
# Authors: Nicholas Wong, Vivian Chan, Nala Hong, Catherine Xie, Rachel Vy
# Date: September 2024
#
# Purpose: This script develops a pricing model for pet insurance, leveraging historical claims data provided by Fetch 
# Pet Health Pty Ltd. The model integrates both internal and external data to identify the factors influencing premiums,
# and aims to accurately and fairly price pet insurance for prospective customers.
#
# Dependencies: tidyverse, maps, showtext, readxl, sf, VIM, xgboost, raster, exactextractr
#
# Instructions: Set your working directory to the folder "UG23_reproducible_codes" in the provided folder and all the 
# code should run without any additions. Please also install the required font DMSans-Regular.ttf and ensure the path in
# font_add() matches the location on device. There are two .r files, an EDA.r and a MD.r. Please run the EDA.r file 
# first to ensure that the outputted .csv files are ready to be imported by the MD.r file.
# 
# Data Sources (Internal): 
# - Fetch Pet Health Pty Ltd: "UNSW_claims_data.csv", "UNSW_earned_data.csv", "New_Customers_Pricing_Output_File.csv"
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
# 1. Data Integration
#   1.1. Preamble
#   1.2. Importing Data
#   1.3. Internal Data
#   1.4. External Data (Geographic)
#   1.5. External Data (Socioeconomic)
#   1.6. External Data (Breed)
#   1.7. External Data (Nature)
#   1.8. External Data (Temperature)
#   1.9. External Data (Air Pollution)
# 2. Data Cleaning
#   2.1. Structuring Data
#   2.2. NA Cleaning
# 3. Data Exploration
#   3.1. Breed
#   3.2. Gender
#   3.3. Multi Pet Plan
#   3.4. Age
#   3.5. Quintiles
#   3.6. Income
#   3.7. Average Weight
#   3.8. Frequency-Severity Covariation
# 4. Data Transformation
#   4.1. Transformation
#   4.2. Dimension Reduction
#   4.3. NA Cleaning
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==


### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# 1 Data Integration ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.1. Preamble ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Required Packages
library(tidyverse)
library(maps)
library(showtext)
library(readxl)
library(sf)
library(VIM)
library(xgboost)
library(raster)
library(exactextractr)

## Settings
options(scipen = 999)
font_add(family = "DM Sans", regular = "/Library/Fonts/DMSans-Regular.ttf")
showtext_auto()

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.2. Importing Data ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Internal Data
CLAIMS_DF <- read_csv("Raw Internal Data/UNSW_claims_data.csv")
EARNED_DF <- read_csv("Raw Internal Data/UNSW_earned_data_adjusted_Sep27.csv")
PRICING_DF <- read_csv("Raw Internal Data/New_Customers_Pricing_Output_File.csv")

## External Data - Geographic Data
POA_SHAPES <- read_sf("Raw External Data/Shapefiles/POA_2021_AUST_GDA2020.shp") 

LGA_SHAPES <- read_sf("Raw External Data/Shapefiles/LGA_2024_AUST_GDA2020.shp") 

SA3_MAPPING <- read_excel("Raw External Data/ABS - Geographic/POSTCODE_SA3_MAPPING.xls", 
                          sheet = "Table 3",
                          skip = 5, 
                          range = "B6:E3636",
                          col_names = TRUE)

## External Data - Socioeconomic Data
LGA_QUINTILE <- read_xlsx("Raw External Data/ABS - Socioeconomic/Local Government Area, Indexes, SEIFA 2021.xlsx", 
                          sheet = "Table 3", 
                          range = "A7:D553",
                          col_names = c("LGA_CODE24", "LGA_NAME24", "POPULATION", "SCORE"))

ABS_ECONOMY_INDUSTRY_LGA <- read_xlsx("Raw External Data/ABS - Socioeconomic/ABS_SA_DATA.xlsx", 
                                      sheet = "ECONOMY_AND_INDUSTRY_LGA", 
                                      range = "A1:E4924")

ABS_FAMILY_COMMUNITY <- read_xlsx("Raw External Data/ABS - Socioeconomic/ABS_SA_DATA.xlsx", 
                                  sheet = "FAMILY_AND_COMMUNITY", 
                                  range = "A1:U4377", 
                                  col_names = TRUE)

ABS_EDUCATION_EMPLOYMENT <- read_xlsx("Raw External Data/ABS - Socioeconomic/ABS_SA_DATA.xlsx", 
                                      sheet = "EDUCATION_AND_EMPLOYMENT", 
                                      range = "A1:D4377", 
                                      col_names = TRUE)

INCOME_LGA <- read_excel("Raw External Data/ABS - Socioeconomic/INCOME_SA2-4.xlsx", 
                         sheet = 3, 
                         range = "A7:BP4383")

INCOME_AGE <- read_excel("Raw External Data/ABS - Socioeconomic/63060DO003_202305.xlsx", 
                         sheet = 2, 
                         skip = 4)

## External Data - Breed Data
GENETIC_DISORDERS <- read_xlsx("Raw External Data/HSVMA - Breed/Genetic_Disorders.xlsx", 
                               range = "A1:B189", 
                               col_names = TRUE)

AKC_BREED_TRAIT <- read_xlsx("Raw External Data/AKC - Breed/AKC_Breed_Data.xlsx", 
                             range = "A1:D312", 
                             col_names = TRUE)

## External Data - Nature Data
NATURE <- read_excel("Raw External Data/ABS - Nature/LANDANDENVIRONMENT_SA2-4.xlsx", 
                     sheet = "Table 1", 
                     skip = 6, 
                     col_names = TRUE) 

PM2.5 <- read_xlsx("Raw External Data/NPI - Air Pollution/Pollution.xlsx", 
                   sheet = "PM2.5", 
                   range = "A1:M2118", 
                   col_names = TRUE)

CARBON_MONOXIDE <- read_xlsx("Raw External Data/NPI - Air Pollution/Pollution.xlsx", 
                             sheet = "Carbon_Monoxide", 
                             range = "A1:M2153", 
                             col_names = TRUE)

TEMPERATURE <- raster("Raw External Data/BOM - Temperature/mxtan.txt")

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.3. Internal Data ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Creating gender_de_sex
EARNED_DF <- EARNED_DF %>%
  mutate(gender_de_sex = case_when(
    pet_gender == "male" & pet_de_sexed == TRUE ~ "MD",    # Male Desexed
    pet_gender == "male" & pet_de_sexed == FALSE ~ "MND",  # Male Not Desexed
    pet_gender == "female" & pet_de_sexed == TRUE ~ "FD",  # Female Desexed
    pet_gender == "female" & pet_de_sexed == FALSE ~ "FND" # Female Not Desexed
  )) 

## Cleaning pet_de_sexed_age
EARNED_DF <- EARNED_DF %>%
  mutate(pet_de_sexed_age = case_when(
    pet_de_sexed_age %in% c("7-12 mo", "7-12 months") ~ "7-12 months",
    pet_de_sexed_age %in% c("0-3 mo", "0-3 months") ~ "0-3 months",
    pet_de_sexed_age %in% c("4-6 mo") ~ "4-6 months",
    pet_de_sexed_age %in% c("1-2 yr") ~ "1-2 years",
    pet_de_sexed_age %in% c("2+ yr", "2+ years") ~ "2+ years",
    TRUE ~ pet_de_sexed_age
  ))

## Removing rows with earned_units == 0
EARNED_DF <- EARNED_DF %>%
  filter(earned_units > 0)

## Data Types for Earned
EARNED_DF <- EARNED_DF %>%
  mutate(across(where(is.character), ~ na_if(., "")), # Changing strings to NA
         UW_Date = as.Date(UW_Date), 
         nb_policy_first_inception_date = as.Date(nb_policy_first_inception_date), 
         person_dob = as.Date(person_dob), 
         quote_date = as.Date(quote_date), 
         nb_postcode = ifelse(nchar(nb_postcode) == 3, paste0(0, nb_postcode), nb_postcode),
         across(c(pet_gender, 
                  pet_de_sexed, 
                  pet_de_sexed_age, 
                  pet_is_switcher, 
                  nb_contribution, 
                  nb_excess,
                  nb_address_type_adj, 
                  nb_suburb,
                  nb_postcode,
                  nb_state, 
                  pet_age_years, 
                  nb_breed_type, 
                  nb_breed_trait, 
                  nb_breed_name_unique, 
                  nb_breed_name_unique_concat, 
                  is_multi_pet_plan, 
                  quote_time_group,
                  gender_de_sex), 
                as.factor)
  ) %>% # Changing data types
  dplyr::select(-UW_Date, -tenure, -nb_policy_first_inception_date, -nb_contribution_excess, 
                -exposure_id_1, -row_num, -`...1`) # Dropping columns 

## Removing rows where claim_paid or total_claim_amount is 0
CLAIMS_DF <- CLAIMS_DF %>%
  filter((claim_paid != 0) &
           (total_claim_amount != 0))

## Grouping duplicate claim_id
CLAIMS_DF <- CLAIMS_DF %>%
  group_by(claim_id) %>%
  summarize(across(-c(claim_paid, total_claim_amount), 
                   ~ ifelse(length(unique(.)) == 1, unique(.), NA)),  # For all other columns
            claim_paid = sum(claim_paid, na.rm = TRUE),  # Replace with summed value
            total_claim_amount = sum(total_claim_amount, na.rm = TRUE),  # Replace with summed value
            .groups = 'drop')

## Data Types for Claims
CLAIMS_DF <- CLAIMS_DF %>% 
  mutate(across(where(is.character), ~ na_if(., "")), # Changing strings to NA
         claim_start_date = as.Date(claim_start_date), 
         claim_status = as.factor(claim_status), 
         condition_category = as.factor(condition_category)
  ) # Changing data types

## Data Types for New Customers
## Creating gender_de_sex
PRICING_DF <- PRICING_DF %>%
  mutate(gender_de_sex = case_when(
    pet_gender == "male" & pet_de_sexed == TRUE ~ "MD",    # Male Desexed
    pet_gender == "male" & pet_de_sexed == FALSE ~ "MND",  # Male Not Desexed
    pet_gender == "female" & pet_de_sexed == TRUE ~ "FD",  # Female Desexed
    pet_gender == "female" & pet_de_sexed == FALSE ~ "FND" # Female Not Desexed
  )) 

## Cleaning pet_de_sexed_age
PRICING_DF <- PRICING_DF %>%
  mutate(pet_de_sexed_age = case_when(
    pet_de_sexed_age %in% c("7-12 mo", "7-12 months") ~ "7-12 months",
    pet_de_sexed_age %in% c("0-3 mo", "0-3 months") ~ "0-3 months",
    pet_de_sexed_age %in% c("4-6 mo") ~ "4-6 months",
    pet_de_sexed_age %in% c("1-2 yr") ~ "1-2 years",
    pet_de_sexed_age %in% c("2+ yr", "2+ years") ~ "2+ years",
    TRUE ~ pet_de_sexed_age
  ))

## Data Types for Pricing
PRICING_DF <- PRICING_DF %>%
  mutate(across(where(is.character), ~ na_if(., "")), # Changing strings to NA
         person_dob = as.Date(person_dob), 
         quote_date = as.Date(quote_date), 
         nb_postcode = ifelse(nchar(nb_postcode) == 3, paste0(0, nb_postcode), nb_postcode),
         across(c(pet_gender, 
                  pet_de_sexed, 
                  pet_de_sexed_age, 
                  pet_is_switcher, 
                  nb_contribution, 
                  nb_excess,
                  nb_address_type_adj, 
                  nb_suburb,
                  nb_postcode,
                  nb_state, 
                  pet_age_years, 
                  nb_breed_type, 
                  nb_breed_trait, 
                  nb_breed_name_unique, 
                  nb_breed_name_unique_concat, 
                  is_multi_pet_plan, 
                  quote_time_group,
                  gender_de_sex), 
                as.factor)
  ) %>% # Changing data types
  dplyr::select(-nb_contribution_excess) %>%
  filter(!nb_breed_trait %in% c('siamese', 'forest cat', 'spotted cats', 'persian', 'burmese'))

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.4. External Data (Geographic) ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Postal area shape file
POA_SHAPES <- POA_SHAPES %>%
  dplyr::select(POA_CODE21, geometry, AREASQKM21) %>%
  rename(nb_postcode = POA_CODE21) %>%
  filter(!st_is_empty(POA_SHAPES)) # Filter out No address, Migratory and Overseas postcodes 

## LGA area shape file
LGA_SHAPES <- LGA_SHAPES %>%
  dplyr::select(LGA_CODE24, LGA_NAME24, geometry, STE_CODE21, AREASQKM) %>% 
  filter(!st_is_empty(.)) # Filter out No address, Migratory and Overseas postcodes)

## Create centroids 
POA_SHAPES_CEN <- POA_SHAPES 
POA_SHAPES_CEN$centroids <- suppressWarnings(
  st_transform(POA_SHAPES, 3395) %>%
    st_centroid() %>%
    st_transform(., '+proj=longlat +ellps=GRS80 +no_defs') %>%
    st_geometry()
)  

## Assign centroids as new geometry
POA_SHAPES_CEN <- POA_SHAPES_CEN %>% 
  st_set_geometry("centroids") %>% # Reset to centroids
  dplyr::select(-geometry) %>% 
  st_transform(st_crs(LGA_SHAPES))

## Create merged shapefile 
SHAPES <- st_join(POA_SHAPES_CEN, LGA_SHAPES, join = st_within) # Point in multipolygon merge

## Use minimum distance to remap those which do not map directly
unmapped <- SHAPES %>%
  filter(is.na(LGA_CODE24)) %>% # Find postcodes that didnt match to LGA 
  dplyr::select(-nb_postcode)

dist_matrix <- st_distance(unmapped$centroids, LGA_SHAPES) # Create distance matrix

unmapped <- unmapped %>%
  mutate(distance = apply(dist_matrix, 1, min), # Select minimum
         index = apply(dist_matrix, 1, which.min), # Map index
         LGA_NAME24 = LGA_SHAPES$LGA_NAME24[index], # Extract nearest LGA
         LGA_CODE24 = LGA_SHAPES$LGA_CODE24[index] # Extract LGA code
  ) %>%
  dplyr::select(LGA_NAME24, LGA_CODE24, STE_CODE21)

# Restack on unmapped rows
SHAPES <- SHAPES %>%
  st_join(unmapped) %>% 
  mutate(LGA_NAME24 = coalesce(LGA_NAME24.x, LGA_NAME24.y),  # Replace NAs for LGA_NAME24
         LGA_CODE24 = coalesce(LGA_CODE24.x, LGA_CODE24.y),   # Replace NAs for LGA_CODE24
         STE_CODE21 = coalesce(STE_CODE21.x, STE_CODE21.y),   # Replace NAs for STE_CODE21
         nb_postcode = as.factor(nb_postcode)
  ) %>%
  dplyr::select(-LGA_NAME24.x, -LGA_NAME24.y, -LGA_CODE24.x, -LGA_CODE24.y, - STE_CODE21.x, -STE_CODE21.y, -AREASQKM, -AREASQKM21)

# Manual fix for postcodes which are not mapping
SHAPES <- SHAPES %>% 
  mutate(LGA_NAME24 = ifelse(nb_postcode == "2620", "Queanbeyan-Palerang", LGA_NAME24), 
         LGA_CODE24 = ifelse(nb_postcode == "2620", "16490", LGA_CODE24)
  ) %>% # Fix mapping issue for Queanbeyan-Palerang LGA
  mutate(LGA_NAME24 = ifelse(nb_postcode == "2611", "Unincorporated ACT", LGA_NAME24), 
         LGA_CODE24 = ifelse(nb_postcode == "2611", "89399", LGA_CODE24)
  ) %>% # Fix mapping issue for Unincorporated ACT LGA
  mutate(LGA_NAME24 = ifelse(nb_postcode == "5690", "Unincorporated SA", LGA_NAME24), 
         LGA_CODE24 = ifelse(nb_postcode == "5690", "49399", LGA_CODE24)
  ) %>% # Fix mapping issue for Yalata Suburb
  dplyr::select(-STE_CODE21)

## Decluttering Environment
rm(unmapped, POA_SHAPES_CEN, dist_matrix)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.5. External Data (Socioeconomic) ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Quintiles
LGA_QUINTILE <- LGA_QUINTILE %>%
  arrange(SCORE) %>%
  mutate(LGA_QUINTILE = ntile(SCORE, 5)) %>%
  mutate(LGA_CODE24 = as.character(LGA_CODE24))

## Population
LGA_POPULATION_DENSITY <- LGA_QUINTILE %>%
  mutate(LGA_CODE24 = as.character(LGA_CODE24)) %>%
  dplyr::select(LGA_CODE24, POPULATION) %>%
  left_join(LGA_SHAPES %>% dplyr::select(LGA_CODE24, AREASQKM)  %>% st_drop_geometry(), by = "LGA_CODE24") %>%
  mutate(population_density = POPULATION/AREASQKM)

## Median House/Apartment Price
LGA_MEDIAN_DWELLING_PRICE <- ABS_ECONOMY_INDUSTRY_LGA %>%
  filter(Year == 2023) %>%
  pivot_longer(
    cols = c(median_house_price, median_apt_price),
    names_to = "nb_address_type_adj",
    values_to = "median_dwelling_price_lga"
  ) %>%
  mutate(nb_address_type_adj = recode_factor(nb_address_type_adj, 
                                             "median_apt_price" = "Apartment", 
                                             "median_house_price" = "House"),
         LGA_CODE24 = as.character(LGA_CODE24),
         nb_address_type_adj = as.factor(nb_address_type_adj)) %>%
  dplyr::select(-Year, -LGA)

## Unemployment Rate
ABS_EDUCATION_EMPLOYMENT <- ABS_EDUCATION_EMPLOYMENT %>%
  mutate(Year = as.numeric(Year),
         unemployment_rate = suppressWarnings(as.numeric(unemployment_rate))) %>%
  filter(!is.na(unemployment_rate))

UNEMPLOYMENT_2024 <- data.frame(LGA_CODE24 = character(), unemployment_2024 = numeric(), stringsAsFactors = FALSE)

for (lga in unique(ABS_EDUCATION_EMPLOYMENT$LGA_CODE24)) {
  lga_data <- ABS_EDUCATION_EMPLOYMENT %>%
    filter(LGA_CODE24 == lga)
  
  if (nrow(lga_data) >= 2) {
    model <- lm(unemployment_rate ~ Year, data = lga_data)
    pred_2024 <- predict(model, newdata = data.frame(Year = 2024))
    
    UNEMPLOYMENT_2024 <- rbind(UNEMPLOYMENT_2024, data.frame(LGA_CODE24 = lga, unemployment_2024 = pred_2024))
  }
}

## Internet Access
INTERNET_ACCESS <- ABS_FAMILY_COMMUNITY %>%
  dplyr::select(LGA_CODE24, internet_access_proportion_2016) %>%
  mutate(internet_access_proportion_2016 = suppressWarnings(as.numeric(internet_access_proportion_2016))) %>%
  filter(!is.na(internet_access_proportion_2016))

## Family Community Variables
ABS_FAMILY_COMMUNITY <- ABS_FAMILY_COMMUNITY %>%
  dplyr::select(-internet_access_proportion_2016) %>%
  mutate(suppressWarnings(across(c(travelled_to_work_by_car,
                                   worked_from_home,
                                   total_persons_employed,
                                   family_households,
                                   total_households,
                                   avg_household_size,
                                   avg_family_size,
                                   married,
                                   household_no_car,
                                   household_one_car,
                                   household_two_cars,
                                   household_three_cars,
                                   household_four_cars,
                                   avg_children_per_family,
                                   property_owned_outright,
                                   property_owned_mortgage,
                                   property_rented), 
                                 ~ as.numeric(.)))) %>%
  filter(!is.na(travelled_to_work_by_car) &
           !is.na(worked_from_home) &
           !is.na(total_persons_employed) &
           !is.na(family_households) &
           !is.na(total_households) &
           !is.na(avg_household_size) &
           !is.na(avg_family_size) &
           !is.na(married) &
           !is.na(household_no_car) &
           !is.na(household_one_car) &
           !is.na(household_two_cars) &
           !is.na(household_three_cars) &
           !is.na(household_four_cars) &
           !is.na(avg_children_per_family) &
           !is.na(property_owned_outright) &
           !is.na(property_owned_mortgage) &
           !is.na(property_rented)) %>%
  mutate(property_owned_outright = property_owned_outright/100,
         property_owned_mortgage = property_owned_mortgage/100,
         property_rented = property_rented/100)

ABS_FAMILY_COMMUNITY_2024 <- data.frame(LGA_CODE24 = character(), 
                                        travelled_to_work_by_car_2024 = numeric(), 
                                        worked_from_home_2024 = numeric(), 
                                        total_persons_employed_2024 = numeric(), 
                                        family_households_2024 = numeric(),
                                        total_households_2024 = numeric(),
                                        avg_household_size_2024 = numeric(),
                                        avg_family_size_2024 = numeric(),
                                        married_2024 = numeric(),
                                        household_no_car = numeric(),
                                        household_one_car_2024 = numeric(),
                                        household_two_cars_2024 = numeric(),
                                        household_three_cars_2024 = numeric(),
                                        household_four_cars_2024 = numeric(),
                                        avg_children_per_family_2024 = numeric(),
                                        property_owned_outright_2024 = numeric(),
                                        property_owned_mortgage_2024 = numeric(),
                                        property_rented_2024 = numeric(),
                                        stringsAsFactors = FALSE)

for (lga in unique(ABS_FAMILY_COMMUNITY$LGA_CODE24)) {
  lga_data <- ABS_FAMILY_COMMUNITY %>%
    filter(LGA_CODE24 == lga)
  
  #Predicting socioeconomic factors for 2024
  if (nrow(lga_data) >= 2) {
    model_travelled_to_work_by_car <- lm(travelled_to_work_by_car ~ Year, data = lga_data)
    pred_travelled_to_work_by_car_2024 <- predict(model_travelled_to_work_by_car, newdata = data.frame(Year=2024))
    
    model_worked_from_home <- lm(worked_from_home ~ Year, data = lga_data)
    pred_worked_from_home_2024 <- predict(model_worked_from_home, newdata = data.frame(Year=2024))
    
    model_total_persons_employed <- lm(total_persons_employed ~ Year, data = lga_data)
    pred_total_persons_employed_2024 <- predict(model_total_persons_employed, newdata = data.frame(Year=2024))
    
    model_family_households <- lm(family_households ~ Year, data = lga_data)
    pred_family_households_2024 <- predict(model_family_households, newdata = data.frame(Year=2024))
    
    model_total_households <- lm(total_households ~ Year, data = lga_data)
    pred_total_households_2024 <- predict(model_total_households, newdata = data.frame(Year=2024))
    
    model_avg_household_size <- lm(avg_household_size ~ Year, data = lga_data)
    pred_avg_household_size_2024 <- predict(model_avg_household_size, newdata = data.frame(Year=2024))
    
    model_avg_family_size <- lm(avg_family_size ~ Year, data = lga_data)
    pred_avg_family_size_2024 <- predict(model_avg_family_size, newdata = data.frame(Year=2024))
    
    model_married <- lm(married ~ Year, data = lga_data)
    pred_married_2024 <- predict(model_married, newdata = data.frame(Year=2024))
    
    model_household_no_car <- lm(household_no_car ~ Year, data = lga_data)
    pred_household_no_car_2024 <- predict(model_household_no_car, newdata = data.frame(Year=2024))
    
    model_household_one_car <- lm(household_one_car ~ Year, data = lga_data)
    pred_household_one_car_2024 <- predict(model_household_one_car, newdata = data.frame(Year=2024))
    
    model_household_two_cars <- lm(household_two_cars ~ Year, data = lga_data)
    pred_household_two_cars_2024 <- predict(model_household_two_cars, newdata = data.frame(Year=2024))
    
    model_household_three_cars <- lm(household_three_cars ~ Year, data = lga_data)
    pred_household_three_cars_2024 <- predict(model_household_three_cars, newdata = data.frame(Year=2024))
    
    model_household_four_cars <- lm(household_four_cars ~ Year, data = lga_data)
    pred_household_four_cars_2024 <- predict(model_household_four_cars, newdata = data.frame(Year=2024))
    
    model_avg_children_per_family <- lm(avg_children_per_family ~ Year, data = lga_data)
    pred_avg_children_per_family_2024 <- predict(model_avg_children_per_family, newdata = data.frame(Year=2024))
    
    model_property_owned_outright <- lm(property_owned_outright ~ Year, data = lga_data)
    pred_property_owned_outright_2024 <- predict(model_property_owned_outright, newdata = data.frame(Year=2024))
    
    model_property_owned_mortgage <- lm(property_owned_mortgage ~ Year, data = lga_data)
    pred_property_owned_mortgage_2024 <- predict(model_property_owned_mortgage, newdata = data.frame(Year=2024))
    
    model_property_rented <- lm(property_rented ~ Year, data = lga_data)
    pred_property_rented_2024 <- predict(model_property_rented, newdata = data.frame(Year=2024))
    
    ABS_FAMILY_COMMUNITY_2024 <- rbind(ABS_FAMILY_COMMUNITY_2024, 
                                       data.frame(LGA_CODE24 = lga, 
                                                  travelled_to_work_by_car_2024 = pred_travelled_to_work_by_car_2024, 
                                                  worked_from_home_2024 = pred_worked_from_home_2024,
                                                  total_persons_employed_2024 = pred_total_persons_employed_2024,
                                                  family_households_2024 = pred_family_households_2024,
                                                  total_households_2024 = pred_total_households_2024,
                                                  avg_household_size_2024 = pred_avg_household_size_2024,
                                                  avg_family_size_2024 = pred_avg_family_size_2024,
                                                  married_2024 = pred_married_2024,
                                                  household_no_car_2024 = pred_household_no_car_2024,
                                                  household_one_car_2024 = pred_household_one_car_2024,
                                                  household_two_cars_2024 = pred_household_two_cars_2024,
                                                  household_three_cars_2024 = pred_household_three_cars_2024,
                                                  household_four_cars_2024 = pred_household_four_cars_2024,
                                                  avg_children_per_family_2024 = pred_avg_children_per_family_2024,
                                                  property_owned_outright_2024 = pred_property_owned_outright_2024,
                                                  property_owned_mortgage_2024 = pred_property_owned_mortgage_2024,
                                                  property_rented_2024 = pred_property_rented_2024
                                       ))
  }
}


## Mapping variables
UNEMPLOYMENT_2024 <- UNEMPLOYMENT_2024 %>%
  mutate(LGA_CODE24 = as.character(LGA_CODE24),
         unemployment_2024 = unemployment_2024/100)

INTERNET_ACCESS <- INTERNET_ACCESS %>%
  mutate(LGA_CODE24 = as.character(LGA_CODE24),
         internet_access_proportion_2016 = internet_access_proportion_2016/100)

ABS_FAMILY_COMMUNITY_2024 <- ABS_FAMILY_COMMUNITY_2024 %>%
  mutate(LGA_CODE24 = as.character(LGA_CODE24),
         car_total_households = household_no_car_2024 + 
           household_one_car_2024 + 
           household_two_cars_2024 + 
           household_three_cars_2024 + 
           household_four_cars_2024,
         avg_cars_per_household_2024 = (0 * household_no_car_2024 +
                                          1 * household_one_car_2024 +
                                          2 * household_two_cars_2024 +
                                          3 * household_three_cars_2024 +
                                          4 * household_four_cars_2024) / car_total_households,
         own_property_2024 = property_owned_outright_2024 + property_owned_mortgage_2024,
         rent_property_2024 = property_rented_2024,
         married_2024 = married_2024/100,
         travelled_to_work_by_car_2024 = travelled_to_work_by_car_2024/total_persons_employed_2024,
         worked_from_home_2024 = worked_from_home_2024/total_persons_employed_2024,
         family_household_proportion_2024 = family_households_2024/total_households_2024
  )

## Income
INCOME_LGA <- INCOME_LGA %>%
  dplyr::select('Code', 'Year', 'Mean employee income ($)') %>%
  rename(LGA_CODE24 = Code,
         income_by_lga = `Mean employee income ($)`) %>%
  mutate(income_by_lga = na_if(income_by_lga, "-")) %>%
  filter(!is.na(income_by_lga)) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(LGA_CODE24) %>%
  filter(Year == max(Year)) %>%
  dplyr::select(LGA_CODE24, income_by_lga) %>%
  ungroup()

INCOME_LGA <- INCOME_LGA %>%
  mutate(LGA_CODE24 = as.character(LGA_CODE24))

INCOME_AGE <- INCOME_AGE %>%
  dplyr::slice(-c(1, 2)) %>%
  dplyr::slice(1:35) %>%
  rename(Income_AGE = '...1') %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  mutate(Average_Income = c(100, 250, 350, 450, 550, 650, 750, 850, 950,
                            1050, 1150, 1250, 1350, 1450, 1550, 1650, 
                            1750, 1850, 1950, 2050, 2150, 2250, 2350, 
                            2450, 2550, 2650, 2750, 2850, 2950, 3050, 
                            3150, 3250, 3350, 3450, 3600)) 

INCOME_AGE <- INCOME_AGE %>%
  mutate(Income_AGE = case_when(
    Income_AGE == "Under $200" ~ "0-199",
    Income_AGE == "$200 to under $300" ~ "200-299",
    Income_AGE == "$300 to under $400" ~ "300-399",
    Income_AGE == "$400 to under $500" ~ "400-499",
    Income_AGE == "$500 to under $600" ~ "500-599",
    Income_AGE == "$600 to under $700" ~ "600-699",
    Income_AGE == "$700 to under $800" ~ "700-799",
    Income_AGE == "$800 to under $900" ~ "800-899",
    Income_AGE == "$900 to under $1,000" ~ "900-999",
    Income_AGE == "$1,000 to under $1,100" ~ "1000-1099",
    Income_AGE == "$1,100 to under $1,200" ~ "1100-1199",
    Income_AGE == "$1,200 to under $1,300" ~ "1200-1299",
    Income_AGE == "$1,300 to under $1,400" ~ "1300-1399",
    Income_AGE == "$1,400 to under $1,500" ~ "1400-1499",
    Income_AGE == "$1,500 to under $1,600" ~ "1500-1599",
    Income_AGE == "$1,600 to under $1,700" ~ "1600-1699",
    Income_AGE == "$1,700 to under $1,800" ~ "1700-1799",
    Income_AGE == "$1,800 to under $1,900" ~ "1800-1899",
    Income_AGE == "$1,900 to under $2,000" ~ "1900-1999",
    Income_AGE == "$2,000 to under $2,100" ~ "2000-2099",
    Income_AGE == "$2,100 to under $2,200" ~ "2100-2199",
    Income_AGE == "$2,200 to under $2,300" ~ "2200-2299",
    Income_AGE == "$2,300 to under $2,400" ~ "2300-2399",
    Income_AGE == "$2,400 to under $2,500" ~ "2400-2499",
    Income_AGE == "$2,500 to under $2,600" ~ "2500-2599",
    Income_AGE == "$2,600 to under $2,700" ~ "2600-2699",
    Income_AGE == "$2,700 to under $2,800" ~ "2700-2799",
    Income_AGE == "$2,800 to under $2,900" ~ "2800-2899",
    Income_AGE == "$2,900 to under $3,000" ~ "2900-2999",
    Income_AGE == "$3,000 to under $3,100" ~ "3000-3099",
    Income_AGE == "$3,100 to under $3,200" ~ "3100-3199",
    Income_AGE == "$3,200 to under $3,300" ~ "3200-3299",
    Income_AGE == "$3,300 to under $3,400" ~ "3300-3399",
    Income_AGE == "$3,400 to under $3,500" ~ "3400-3499",
    Income_AGE == "$3,500 and over" ~ "3500 +"
  ))

INCOME_AGE$Income_AGE <- factor(INCOME_AGE$Income_AGE, 
                                levels = c("0-199", "200-299", "300-399", "400-499","500-599", "600-699", "700-799", 
                                           "800-899", "900-999", "1000-1099", "1100-1199", "1200-1299", "1300-1399", 
                                           "1400-1499", "1500-1599", "1600-1699", "1700-1799", "1800-1899", "1900-1999", 
                                           "2000-2099", "2100-2199", "2200-2299", "2300-2399", "2400-2499", "2500-2599", 
                                           "2600-2699", "2700-2799", "2800-2899", "2900-2999", "3000-3099", "3100-3199", 
                                           "3200-3299", "3300-3399", "3400-3499", "3500 +"))

## Decluttering Environment
rm(ABS_EDUCATION_EMPLOYMENT, ABS_FAMILY_COMMUNITY,
   lga, lga_data, model, model_avg_children_per_family, model_avg_family_size, model_avg_household_size,
   model_family_households, model_household_four_cars, model_household_no_car, model_household_one_car,
   model_household_three_cars, model_household_two_cars, model_married, model_property_owned_mortgage,
   model_property_owned_outright, model_property_rented, model_total_households, model_total_persons_employed,
   model_travelled_to_work_by_car, model_worked_from_home,
   pred_2024, pred_avg_children_per_family_2024, pred_avg_family_size_2024, pred_avg_household_size_2024,
   pred_family_households_2024, pred_household_four_cars_2024, pred_household_no_car_2024, 
   pred_household_one_car_2024, pred_household_three_cars_2024, pred_household_two_cars_2024,
   pred_married_2024, pred_property_owned_mortgage_2024, pred_property_owned_outright_2024,
   pred_property_rented_2024, pred_total_households_2024, pred_total_persons_employed_2024,
   pred_travelled_to_work_by_car_2024, pred_worked_from_home_2024)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.6. External Data (Breed) ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Breed Genetic Disorders
match_breeds_and_accumulate <- function(breed_string, breed_disorders_df) {
  matches <- breed_disorders_df %>%
    filter(str_detect(tolower(breed_string), tolower(Breed))) %>%
    pull(Numbers)
  
  disorder_numbers_accumulated <- unique(unlist(str_split(matches, ", ")))
  
  if ("0" %in% disorder_numbers_accumulated && length(disorder_numbers_accumulated) > 1) {
    disorder_numbers_accumulated <- disorder_numbers_accumulated[disorder_numbers_accumulated != "0"]}
  
  if (length(disorder_numbers_accumulated) == 0) {
    return(NA)} 
  else {
    return(paste(disorder_numbers_accumulated, collapse = ", "))
  }
}

EARNED_DF$genetic_disorder_codes <- sapply(EARNED_DF$nb_breed_name_unique_concat, 
                                           match_breeds_and_accumulate, 
                                           breed_disorders_df = GENETIC_DISORDERS)

PRICING_DF$genetic_disorder_codes <- sapply(PRICING_DF$nb_breed_name_unique_concat, 
                                            match_breeds_and_accumulate, 
                                            breed_disorders_df = GENETIC_DISORDERS)

count_genetic_codes <- function(genetic_disorder_codes) {
  
  if (is.na(genetic_disorder_codes) || genetic_disorder_codes == "") {
    return(NA)
  }
  
  category_1 <- c("10", "21", "81") 
  category_2 <- c("38", "85b")
  
  disorder_codes <- unique(str_split(genetic_disorder_codes, ",\\s*")[[1]])
  
  unique_count <- 0
  
  if (any(disorder_codes %in% category_1)) {
    unique_count <- unique_count + 1}
  
  if (any(disorder_codes %in% category_2)) {
    unique_count <- unique_count + 1}
  
  remaining_codes <- disorder_codes[!disorder_codes %in% c(category_1, category_2)]
  unique_count <- unique_count + length(remaining_codes)
  
  return(unique_count)
}

EARNED_DF$genetic_disorder_count <- sapply(EARNED_DF$genetic_disorder_codes, function(x) {
  if (is.na(x)) {return(NA)} 
  else {return(count_genetic_codes(x))}
})

PRICING_DF$genetic_disorder_count <- sapply(PRICING_DF$genetic_disorder_codes, function(x) {
  if (is.na(x)) {return(NA)} 
  else {return(count_genetic_codes(x))}
})

## Other Breed Traits
map_traits_to_earned <- function(earned_df, akc_df) {
  akc_df <- akc_df %>%
    mutate(Breed_lower = str_trim(tolower(Breed)))
  
  results <- earned_df %>%
    mutate(avg_weight_kg = NA,
           avg_lifespan_yrs = NA,
           energy_level = NA)
  
  for (i in seq_len(nrow(akc_df))) {
    akc_breed <- akc_df$Breed_lower[i]
    results <- results %>%
      mutate(
        avg_weight_kg = ifelse(str_detect(tolower(nb_breed_name_unique), paste0("\\b", akc_breed, "(\\s|\\b)")), 
                               akc_df$avg_weight_kg[i], avg_weight_kg),
        avg_lifespan_yrs = ifelse(str_detect(tolower(nb_breed_name_unique), paste0("\\b", akc_breed, "(\\s|\\b)")), 
                                  akc_df$avg_lifespan_yrs[i], avg_lifespan_yrs),
        energy_level = ifelse(str_detect(tolower(nb_breed_name_unique), paste0("\\b", akc_breed, "(\\s|\\b)")), 
                              akc_df$energy_level[i], energy_level)
      )
  }
  
  return(results)
}

EARNED_DF <- map_traits_to_earned(EARNED_DF, AKC_BREED_TRAIT)

PRICING_DF <- map_traits_to_earned(PRICING_DF, AKC_BREED_TRAIT)

## Decluttering Environment
rm(match_breeds_and_accumulate, count_genetic_codes, GENETIC_DISORDERS, AKC_BREED_TRAIT, map_traits_to_earned)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.7. External Data (Nature) ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==

## Nature variables

NATURE <- NATURE %>%
  mutate(Code = as.integer(Code)) %>%
  filter(Code > 10000, Code < 100000) %>% 
  filter(`Total protected land area (%)` != "-") %>% 
  group_by(Label) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  mutate(SA3_code = as.character(Code)) %>%
  dplyr::select(SA3_code, `Total protected land area (%)`) %>%
  rename(protected_area_SA3 = `Total protected land area (%)`) %>%
  mutate(protected_area_SA3 = as.numeric(protected_area_SA3))

# Manually assign postcodes not in mapping
unmapped <- data.frame(
  POSTCODE = c("3336","3213","3358","0834"), 
  SA3_CODE_2011 = c("21304","20302", "20101","70103"),
  SA3_NAME_2011 = c("Melton - Bacchus Marsh", "Geelong", "Ballarat","Litchfield"), 
  RATIO = c(1,1,1,1))

SA3_MAPPING <- rbind(SA3_MAPPING, unmapped)

# Merge protected areas with SA3 mapping
PROTECTED_AREAS <- SA3_MAPPING %>%
  dplyr::slice(-1) %>%
  rename(nb_postcode = POSTCODE,
         SA3_code = SA3_CODE_2011) %>%
  left_join(NATURE, by = "SA3_code") %>%
  mutate(protected_area_SA3 = ifelse(is.na(protected_area_SA3), 0, protected_area_SA3),
         weighted_protected_area = as.numeric(RATIO) * protected_area_SA3) %>%
  group_by(nb_postcode) %>%
  summarise(protected_area = sum(weighted_protected_area))

# Declutter
rm(SA3_MAPPING, NATURE, unmapped)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.8. External Data (Temperature) ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==

# Match crs
TEMP_POA_MAPPING <- st_transform(POA_SHAPES, crs = st_crs(TEMPERATURE)) %>%
  dplyr::select(-AREASQKM21)

# Find average temperature by postcode
TEMP_POA_MAPPING$mean_annual_temp <- exact_extract(TEMPERATURE, TEMP_POA_MAPPING, 'mean')

# Plot the average annual temperature by postcode
#ggplot() +
#  geom_sf(data = TEMP_POA_MAPPING, aes(fill = mean_annual_temp), color = "black", linewidth = 0.3) +
#  scale_fill_viridis_c(option = "magma", direction = -1) +
#  labs(title = "Mean annual temperature by postcode",
#       fill = "Mean annual temperature") +
#  xlim(c(110, 155)) +
#  theme(plot.title = element_text(hjust = 0.5),
#        text = element_text(family = "DM Sans", size = 12))

# Declutter
rm(TEMPERATURE)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 1.9. External Data (Air Pollution) ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Particulate matter
# PM2.5 general facilities raw data plot
#ggplot() +
#  borders("world", regions = "Australia", colour = "gray50", fill = "gray80") +
#  geom_point(data = PM2.5, aes(x = longitude, y = latitude, size = air_total_emission_kg, color = air_total_emission_kg), alpha = 0.7) +
#  scale_color_gradient(low = "blue", high = "red") +
#  labs(title = "PM2.5 Emissions from Facilities",
#       x = "Longitude",
#       y = "Latitude",
#       size = "Total PM2.5 (kg)",
#       color = "Total PM2.5 (kg)") +
#  theme(plot.title=element_text(hjust=0.5, size=16),
#        axis.line=element_line(linewidth=1, colour="grey80"),
#        text=element_text(family="DM Sans", size=12))

PM2.5 <- PM2.5 %>% 
  dplyr::select(air_total_emission_kg, postcode) %>%
  rename(nb_postcode = postcode) %>%
  group_by(nb_postcode) %>%
  summarise(total_pm25 = sum(air_total_emission_kg)) %>%
  ungroup()

# Carbon monoxide general facilities raw data plot
#ggplot() +
#  borders("world", regions = "Australia", colour = "gray50", fill = "gray80") +
#  geom_point(data = CARBON_MONOXIDE, aes(x = longitude, y = latitude, size = air_total_emission_kg, color = air_total_emission_kg), alpha = 0.7) +
#  scale_color_gradient(low = "blue", high = "red") +
#  labs(title = "Carbon Monoxide Emissions from Facilities",
#       x = "Longitude",
#       y = "Latitude",
#       size = "Total Carbon Monoxide (kg)",
#       color = "Total Carbon Monoxide (kg)") +
#  theme(plot.title=element_text(hjust=0.5, size=16),
#        axis.line=element_line(linewidth=1, colour="grey80"),
#        text=element_text(family="DM Sans", size=12))

## Carbon monoxide
CARBON_MONOXIDE <- CARBON_MONOXIDE %>% 
  dplyr::select(air_total_emission_kg, postcode) %>%
  rename(nb_postcode = postcode) %>%
  group_by(nb_postcode) %>%
  summarise(total_carbon_monoxide = sum(air_total_emission_kg)) %>%
  ungroup()

# Map to postcodes and log transform
AIRPOLLUTION_POA_MAPPING <- POA_SHAPES %>%
  left_join(PM2.5, by = "nb_postcode") %>%  
  left_join(CARBON_MONOXIDE, by = "nb_postcode") %>%
  mutate(total_pm25 = ifelse(is.na(total_pm25), 0, total_pm25), 
         log_pm25_per_sqkm = log1p(total_pm25/AREASQKM21),
         total_carbon_monoxide = ifelse(is.na(total_carbon_monoxide), 0, total_carbon_monoxide), 
         log_carbon_monoxide_per_sqkm = log1p(total_carbon_monoxide/AREASQKM21)) %>%
  dplyr::select(-total_pm25, -total_carbon_monoxide, -AREASQKM21)

# Plot the PM2.5 emissions per square km by postcode
#ggplot() +
#  geom_sf(data = AIRPOLLUTION_POA_MAPPING, aes(fill = log_pm25_per_sqkm), color = "black", linewidth = 0.3) +
#  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
#  labs(title = "Log-transformed PM2.5 emissions per square km by postcode",
#       fill = "Log(PM2.5 emissions per sq km)") +
#  xlim(c(110,155)) +
#  theme(plot.title = element_text(hjust = 0.5),
#        text = element_text(family = "DM Sans", size = 12))

# Plot the Carbon Monoxide emissions per square km by postcode
#ggplot() +
#  geom_sf(data = AIRPOLLUTION_POA_MAPPING, aes(fill = log_carbon_monoxide_per_sqkm), color = "black", linewidth = 0.3) +
#  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
#  labs(title = "Log-transformed Carbon Monoxide emissions per square km by postcode",
#       fill = "Log(Carbon Monoxide emissions per sq km)") +
#  xlim(c(110,155)) +
#  theme(plot.title = element_text(hjust = 0.5),
#        text = element_text(family = "DM Sans", size = 12))

# Declutter
rm(CARBON_MONOXIDE, PM2.5)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# 2. Data Cleaning ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 2.1. Structuring Data ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Earned Data Collapsing by earned_units
EARNED_DF <- EARNED_DF %>%
  dplyr::select(-genetic_disorder_codes, -person_dob) %>%
  group_by(exposure_id) %>%
  mutate(earned_units = sum(earned_units)) %>%
  ungroup() %>%
  distinct()

# Function to compute the most common condition_category
get_mode <- function(x) {
  x <- na.omit(x)
  
  if (length(x) == 0) {
    return(NA) 
  }
  
  uniq_x <- unique(x)
  freqs <- tabulate(match(x, uniq_x))
  max_freq <- max(freqs)
  
  modes <- uniq_x[freqs == max_freq]
  
  if (length(modes) > 1) {
    return(sample(modes, 1)) 
  } else {
    return(modes)
  }
}


## Merged Earned and Claims Data
EARNED_FULL_DF <- EARNED_DF %>%
  left_join(CLAIMS_DF %>% 
              group_by(exposure_id) %>% 
              summarise(claim_count = n(), 
                        claim_paid = mean(claim_paid, na.rm = TRUE), 
                        total_claim_amount = mean(total_claim_amount, na.rm = TRUE), 
                        condition_category = get_mode(condition_category)) %>% 
              ungroup(), 
            by = "exposure_id") %>% 
  left_join(SHAPES, by = 'nb_postcode') %>% # Postcode Data
  left_join(INCOME_LGA, by = "LGA_CODE24") %>% # Income Data
  mutate(income_by_lga = as.integer(income_by_lga)) %>%
  left_join(LGA_QUINTILE %>% dplyr::select(LGA_CODE24, LGA_QUINTILE), by = "LGA_CODE24") %>% # Quintiles 
  left_join(LGA_QUINTILE %>% dplyr::select(LGA_CODE24, SCORE), by = "LGA_CODE24") %>% # Seifa Score
  left_join(LGA_POPULATION_DENSITY %>% dplyr::select(LGA_CODE24, AREASQKM, population_density), by = "LGA_CODE24") %>% # LGA area sqkm and population density
  left_join(LGA_MEDIAN_DWELLING_PRICE, by = c("LGA_CODE24" = "LGA_CODE24", "nb_address_type_adj" = "nb_address_type_adj")) %>% # Median Dwelling Price by LGA
  left_join(UNEMPLOYMENT_2024, by = "LGA_CODE24") %>% # Unemployment
  left_join(INTERNET_ACCESS, by = "LGA_CODE24") %>% # Internet Access
  left_join(ABS_FAMILY_COMMUNITY_2024 %>% # Other socioeconomic variables
              dplyr::select(LGA_CODE24,
                            travelled_to_work_by_car_2024,
                            worked_from_home_2024,
                            avg_household_size_2024,
                            married_2024,
                            avg_children_per_family_2024,
                            avg_cars_per_household_2024,
                            own_property_2024,
                            rent_property_2024,
                            family_household_proportion_2024), 
            by = "LGA_CODE24") %>%
  left_join(PROTECTED_AREAS, by = "nb_postcode") %>% # Protected areas
  left_join(st_drop_geometry(TEMP_POA_MAPPING), by = "nb_postcode") %>% # Temperature
  left_join(st_drop_geometry(AIRPOLLUTION_POA_MAPPING), by = "nb_postcode") %>% # Air pollution
  mutate(across(c(gender_de_sex,
                  LGA_NAME24, 
                  LGA_CODE24, 
                  LGA_QUINTILE
  ), as.factor))

## Creating merged claims data
CLAIMS_FULL_DF <- CLAIMS_DF %>% 
  left_join(EARNED_DF, by = 'exposure_id')

# Creating merged pricing data
PRICING_DF <- PRICING_DF %>%
  dplyr::select(-genetic_disorder_codes, -person_dob)

PRICING_FULL_DF <- PRICING_DF %>%
  left_join(SHAPES, by = 'nb_postcode') %>% # Postcode Data
  left_join(INCOME_LGA, by = "LGA_CODE24") %>% # Income Data
  mutate(income_by_lga = as.integer(income_by_lga)) %>%
  left_join(LGA_QUINTILE %>% dplyr::select(LGA_CODE24, LGA_QUINTILE), by = "LGA_CODE24") %>% # Quintiles 
  left_join(LGA_QUINTILE %>% dplyr::select(LGA_CODE24, SCORE), by = "LGA_CODE24") %>% # Seifa Score
  left_join(LGA_POPULATION_DENSITY %>% dplyr::select(LGA_CODE24, AREASQKM, population_density), by = "LGA_CODE24") %>% # LGA area sqkm and population density
  left_join(LGA_MEDIAN_DWELLING_PRICE, by = c("LGA_CODE24" = "LGA_CODE24", "nb_address_type_adj" = "nb_address_type_adj")) %>% # Median Dwelling Price by LGA
  left_join(UNEMPLOYMENT_2024, by = "LGA_CODE24") %>% # Unemployment
  left_join(INTERNET_ACCESS, by = "LGA_CODE24") %>% # Internet Access
  left_join(ABS_FAMILY_COMMUNITY_2024 %>% # Other socioeconomic variables
              dplyr::select(LGA_CODE24,
                            travelled_to_work_by_car_2024,
                            worked_from_home_2024,
                            avg_household_size_2024,
                            married_2024,
                            avg_children_per_family_2024,
                            avg_cars_per_household_2024,
                            own_property_2024,
                            rent_property_2024,
                            family_household_proportion_2024), 
            by = "LGA_CODE24") %>%
  left_join(PROTECTED_AREAS, by = "nb_postcode") %>% # Protected areas
  left_join(st_drop_geometry(TEMP_POA_MAPPING), by = "nb_postcode") %>% # Temperature
  left_join(st_drop_geometry(AIRPOLLUTION_POA_MAPPING), by = "nb_postcode") %>% # Air pollution
  mutate(across(c(gender_de_sex,
                  LGA_NAME24, 
                  LGA_CODE24, 
                  LGA_QUINTILE
  ), as.factor))

## Decluttering Environment
rm(ABS_FAMILY_COMMUNITY_2024, INTERNET_ACCESS, SHAPES, UNEMPLOYMENT_2024, LGA_POPULATION_DENSITY, 
   TEMP_POA_MAPPING, AIRPOLLUTION_POA_MAPPING, LGA_MEDIAN_DWELLING_PRICE)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 2.2. NA Cleaning ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## pet_is_switcher
# Treat NA's as FALSE
EARNED_DF <- EARNED_DF %>%
  mutate(pet_is_switcher = as.character(pet_is_switcher),
         pet_is_switcher = ifelse(is.na(pet_is_switcher), FALSE, pet_is_switcher),
         pet_is_switcher = factor(pet_is_switcher))

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  mutate(pet_is_switcher = as.character(pet_is_switcher),
         pet_is_switcher = ifelse(is.na(pet_is_switcher), FALSE, pet_is_switcher),
         pet_is_switcher = factor(pet_is_switcher))

CLAIMS_FULL_DF <- CLAIMS_FULL_DF %>%
  mutate(pet_is_switcher = as.character(pet_is_switcher),
         pet_is_switcher = ifelse(is.na(pet_is_switcher), FALSE, pet_is_switcher),
         pet_is_switcher = factor(pet_is_switcher))

PRICING_FULL_DF <- PRICING_FULL_DF %>%
  mutate(pet_is_switcher = as.character(pet_is_switcher),
         pet_is_switcher = ifelse(is.na(pet_is_switcher), FALSE, pet_is_switcher),
         pet_is_switcher = factor(pet_is_switcher))

## pet_de_sexed_age
EARNED_DF <- EARNED_DF %>%
  mutate(pet_de_sexed_age = as.character(pet_de_sexed_age),
         pet_de_sexed_age = ifelse(is.na(pet_de_sexed_age), "Not desexed", pet_de_sexed_age),
         pet_de_sexed_age = factor(pet_de_sexed_age))

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  mutate(pet_de_sexed_age = as.character(pet_de_sexed_age),
         pet_de_sexed_age = ifelse(is.na(pet_de_sexed_age), "Not desexed", pet_de_sexed_age),
         pet_de_sexed_age = factor(pet_de_sexed_age))

CLAIMS_FULL_DF <- CLAIMS_FULL_DF %>%
  mutate(pet_de_sexed_age = as.character(pet_de_sexed_age),
         pet_de_sexed_age = ifelse(is.na(pet_de_sexed_age), "Not desexed", pet_de_sexed_age),
         pet_de_sexed_age = factor(pet_de_sexed_age))

PRICING_FULL_DF <- PRICING_FULL_DF %>%
  mutate(pet_de_sexed_age = as.character(pet_de_sexed_age),
         pet_de_sexed_age = ifelse(is.na(pet_de_sexed_age), "Not desexed", pet_de_sexed_age),
         pet_de_sexed_age = factor(pet_de_sexed_age))

## Owner Age
EARNED_DF <- kNN(EARNED_DF, variable = "owner_age_years", k = 5)
EARNED_FULL_DF <- kNN(EARNED_FULL_DF[, !names(EARNED_FULL_DF) %in% "centroids"], variable = "owner_age_years", k = 5)
CLAIMS_FULL_DF <- kNN(CLAIMS_FULL_DF, variable = "owner_age_years", k = 5)
PRICING_FULL_DF <- kNN(PRICING_FULL_DF[, !names(PRICING_FULL_DF) %in% "centroids"], variable = "owner_age_years", k = 5)

EARNED_DF <- EARNED_DF %>% dplyr::select(-owner_age_years_imp)
EARNED_FULL_DF <- EARNED_FULL_DF %>% dplyr::select(-owner_age_years_imp)
CLAIMS_FULL_DF <- CLAIMS_FULL_DF %>% dplyr::select(-owner_age_years_imp)
PRICING_FULL_DF <- PRICING_FULL_DF %>% dplyr::select(-owner_age_years_imp)

## nb_breed_trait
# List all rows where breed_trait is NA
rows_with_na_breed_trait <- EARNED_DF %>%
  filter(is.na(nb_breed_trait)) %>%
  dplyr::select(nb_breed_trait, nb_breed_name_unique, nb_breed_type)

# Group rows where breed_trait is NA by breed_name_unique
rows_with_na_breed_trait %>%
  group_by(nb_breed_name_unique) %>%
  summarize(count = n())

# Group rows where breed_trait is NA by breed_type (welpp all of them are cross)
rows_with_na_breed_trait %>%
  group_by(nb_breed_type) %>%
  summarize(count = n()) 

# Get list of all available breed_trait in data set for mapping
EARNED_DF %>%
  group_by(nb_breed_trait) %>%
  summarize(count = n()) 

# Declutter Environment
rm(rows_with_na_breed_trait)

# Treat NA's as cross since respective breed_type for all NA breed_trait is cross 
# NALA - I will double check whether this is best
EARNED_DF <- EARNED_DF %>%
  mutate(nb_breed_trait = as.character(nb_breed_trait),
         nb_breed_trait = ifelse(is.na(nb_breed_trait), "cross", nb_breed_trait),
         nb_breed_trait = factor(nb_breed_trait))

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  mutate(nb_breed_trait = as.character(nb_breed_trait),
         nb_breed_trait = ifelse(is.na(nb_breed_trait), "cross", nb_breed_trait),
         nb_breed_trait = factor(nb_breed_trait))

CLAIMS_FULL_DF <- CLAIMS_FULL_DF %>%
  mutate(nb_breed_trait = as.character(nb_breed_trait),
         nb_breed_trait = ifelse(is.na(nb_breed_trait), "cross", nb_breed_trait),
         nb_breed_trait = factor(nb_breed_trait))

PRICING_FULL_DF <- PRICING_FULL_DF %>%
  mutate(nb_breed_trait = as.character(nb_breed_trait),
         nb_breed_trait = ifelse(is.na(nb_breed_trait), "cross", nb_breed_trait),
         nb_breed_trait = factor(nb_breed_trait))

## Change NA's in claims to 0
EARNED_FULL_DF <- EARNED_FULL_DF %>%
  replace_na(list(claim_count = 0, 
                  claim_paid = 0, 
                  total_claim_amount = 0))

## Using Regression to predict income
train <- EARNED_FULL_DF %>% filter(!is.na(income_by_lga))
test <- EARNED_FULL_DF %>% filter(is.na(income_by_lga))

train <- train %>%
  mutate(across(c(gender_de_sex, LGA_NAME24, LGA_CODE24, LGA_QUINTILE), as.factor))

y <- log(train$income_by_lga)
x <- model.matrix(log(income_by_lga) ~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                    nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                    nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                    gender_de_sex + LGA_QUINTILE + SCORE + 
                    population_density + unemployment_2024 + internet_access_proportion_2016 + 
                    travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                    avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                    family_household_proportion_2024, 
                  data = train)[, -1] 
x_test <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                         nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                         nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                         gender_de_sex + LGA_QUINTILE + SCORE + 
                         population_density + unemployment_2024 + internet_access_proportion_2016 + 
                         travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                         avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                         family_household_proportion_2024, 
                       data = test)[, -1]

## xgboost
dtrain <- xgb.DMatrix(data = x, label = y)
dtest <- xgb.DMatrix(data = x_test)

params <- list(objective = "reg:squarederror",  # For regression
               booster = "gbtree",  # Use trees
               eta = 0.1,  # Learning rate
               max_depth = 6,  # Tree depth
               subsample = 0.8,  # Row subsampling
               colsample_bytree = 0.8,  # Feature subsampling
               alpha = 0.1,  # L1 regularization (Lasso)
               lambda = 1)  # L2 regularization (Ridge)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, 
                       watchlist = list(train = dtrain), verbose = 0)

xgb_predictions <- exp(predict(xgb_model, newdata = dtest))
EARNED_FULL_DF$income_by_lga[is.na(EARNED_FULL_DF$income_by_lga)] <- xgb_predictions

# also apply to PRICING_FULL_DF
pricing <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                          nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                          nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                          gender_de_sex + LGA_QUINTILE + SCORE + 
                          population_density + unemployment_2024 + internet_access_proportion_2016 + 
                          travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                          avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                          family_household_proportion_2024, 
                        data = PRICING_FULL_DF %>% filter(is.na(income_by_lga)))[, -1]
dtest_pricing <- xgb.DMatrix(data = pricing)

xgb_predictions_pricing <- exp(predict(xgb_model, newdata = dtest_pricing))
PRICING_FULL_DF$income_by_lga[is.na(PRICING_FULL_DF$income_by_lga)] <- xgb_predictions_pricing


## Imputing genetic_disorder_count
ggplot(EARNED_FULL_DF, aes(x = genetic_disorder_count)) +
  geom_bar(fill = "#C859EB", color = "black") +
  labs(title = "Bar Chart of Genetic Disorder Counts before imputation", x = "Genetic Disorder Code", y = "Frequency") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

train <- EARNED_FULL_DF %>% filter(!is.na(genetic_disorder_count))
test <- EARNED_FULL_DF %>% filter(is.na(genetic_disorder_count))

train <- train %>%
  mutate(across(c(gender_de_sex, LGA_NAME24, LGA_CODE24, LGA_QUINTILE), as.factor))

y <- train$genetic_disorder_count
x <- model.matrix(genetic_disorder_count ~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                    nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                    nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                    gender_de_sex + LGA_QUINTILE + SCORE + 
                    population_density + unemployment_2024 + internet_access_proportion_2016 + 
                    travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                    avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                    family_household_proportion_2024, 
                  data = train)[, -1] 
x_test <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                         nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                         nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                         gender_de_sex + LGA_QUINTILE + SCORE + 
                         population_density + unemployment_2024 + internet_access_proportion_2016 + 
                         travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                         avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                         family_household_proportion_2024, 
                       data = test)[, -1]

## xgboost
dtrain <- xgb.DMatrix(data = x, label = y)
dtest <- xgb.DMatrix(data = x_test)

params <- list(objective = "count:poisson",  # Use count:poisson for Poisson regression
               booster = "gbtree",  # Use trees
               eta = 0.1,  # Learning rate
               max_depth = 6,  # Tree depth
               subsample = 0.8,  # Row subsampling
               colsample_bytree = 0.8,  # Feature subsampling
               alpha = 0.1,  # L1 regularization (Lasso)
               lambda = 1)  # L2 regularization (Ridge)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, 
                       watchlist = list(train = dtrain), verbose = 0)

xgb_predictions <- predict(xgb_model, newdata = dtest)
EARNED_FULL_DF$genetic_disorder_count[is.na(EARNED_FULL_DF$genetic_disorder_count)] <- round(xgb_predictions)


ggplot(EARNED_FULL_DF, aes(x = genetic_disorder_count)) +
  geom_bar(fill = "#49A5F1", color = "black") +
  labs(title = "Bar Chart of Genetic Disorder Counts after imputation", x = "Genetic Disorder Code", y = "Frequency") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

# also apply to PRICING_FULL_DF
pricing <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                          nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                          nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                          gender_de_sex + LGA_QUINTILE + SCORE + 
                          population_density + unemployment_2024 + internet_access_proportion_2016 + 
                          travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                          avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                          family_household_proportion_2024, 
                        data = PRICING_FULL_DF %>% filter(is.na(genetic_disorder_count)))[, -1]
dtest_pricing <- xgb.DMatrix(data = pricing)

xgb_predictions_pricing <- predict(xgb_model, newdata = dtest_pricing)
PRICING_FULL_DF$genetic_disorder_count[is.na(PRICING_FULL_DF$genetic_disorder_count)] <- round(xgb_predictions_pricing)


## Imputing avg_weight_kg
ggplot(EARNED_FULL_DF, aes(x = avg_weight_kg)) +
  geom_histogram(binwidth = 1, fill = "#C859EB", color = "black") +
  labs(title = "Histogram of Average Weight (kg) before imputation", x = "Average Weight", y = "Frequency") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

train <- EARNED_FULL_DF %>% filter(!is.na(avg_weight_kg))
test <- EARNED_FULL_DF %>% filter(is.na(avg_weight_kg))

train <- train %>%
  mutate(across(c(gender_de_sex, LGA_NAME24, LGA_CODE24, LGA_QUINTILE), as.factor))

y <- train$avg_weight_kg
x <- model.matrix(avg_weight_kg ~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                    nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                    nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                    gender_de_sex + LGA_QUINTILE + SCORE + 
                    population_density + unemployment_2024 + internet_access_proportion_2016 + 
                    travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                    avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                    family_household_proportion_2024, 
                  data = train)[, -1] 
x_test <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                         nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                         nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                         gender_de_sex + LGA_QUINTILE + SCORE + 
                         population_density + unemployment_2024 + internet_access_proportion_2016 + 
                         travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                         avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                         family_household_proportion_2024, 
                       data = test)[, -1]

## xgboost
dtrain <- xgb.DMatrix(data = x, label = y)
dtest <- xgb.DMatrix(data = x_test)

params <- list(objective = "reg:squarederror",
               booster = "gbtree",  # Use trees
               eta = 0.1,  # Learning rate
               max_depth = 6,  # Tree depth
               subsample = 0.8,  # Row subsampling
               colsample_bytree = 0.8,  # Feature subsampling
               alpha = 0.1,  # L1 regularization (Lasso)
               lambda = 1)  # L2 regularization (Ridge)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, 
                       watchlist = list(train = dtrain), verbose = 0)

xgb_predictions <- predict(xgb_model, newdata = dtest)
EARNED_FULL_DF$avg_weight_kg[is.na(EARNED_FULL_DF$avg_weight_kg)] <- xgb_predictions

ggplot(EARNED_FULL_DF, aes(x = avg_weight_kg)) +
  geom_histogram(binwidth = 1, fill = "#49A5F1", color = "black") +
  labs(title = "Histogram of Average Weight (kg) after imputation", x = "Average Weight", y = "Frequency") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

# also apply to PRICING_FULL_DF
pricing <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                          nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                          nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                          gender_de_sex + LGA_QUINTILE + SCORE + 
                          population_density + unemployment_2024 + internet_access_proportion_2016 + 
                          travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                          avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                          family_household_proportion_2024, 
                        data = PRICING_FULL_DF %>% filter(is.na(avg_weight_kg)))[, -1]
dtest_pricing <- xgb.DMatrix(data = pricing)

xgb_predictions_pricing <- predict(xgb_model, newdata = dtest_pricing)
PRICING_FULL_DF$avg_weight_kg[is.na(PRICING_FULL_DF$avg_weight_kg)] <- xgb_predictions_pricing


## Imputing avg_lifespan_yrs
ggplot(EARNED_FULL_DF, aes(x = avg_lifespan_yrs)) +
  geom_histogram(binwidth = 1, fill =  "#C859EB", color = "black") +
  labs(title = "Histogram of Average Lifespan (years) before imputation", x = "Average Lifespan (years)", y = "Frequency") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

train <- EARNED_FULL_DF %>% filter(!is.na(avg_lifespan_yrs))
test <- EARNED_FULL_DF %>% filter(is.na(avg_lifespan_yrs))

train <- train %>%
  mutate(across(c(gender_de_sex, LGA_NAME24, LGA_CODE24, LGA_QUINTILE), as.factor))

y <- train$avg_lifespan_yrs
x <- model.matrix(avg_lifespan_yrs ~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                    nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                    nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                    gender_de_sex + LGA_QUINTILE + SCORE + 
                    population_density + unemployment_2024 + internet_access_proportion_2016 + 
                    travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                    avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                    family_household_proportion_2024, 
                  data = train)[, -1] 
x_test <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                         nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                         nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                         gender_de_sex + LGA_QUINTILE + SCORE + 
                         population_density + unemployment_2024 + internet_access_proportion_2016 + 
                         travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                         avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                         family_household_proportion_2024, 
                       data = test)[, -1]

## xgboost
dtrain <- xgb.DMatrix(data = x, label = y)
dtest <- xgb.DMatrix(data = x_test)

params <- list(objective = "reg:squarederror",
               booster = "gbtree",  # Use trees
               eta = 0.1,  # Learning rate
               max_depth = 6,  # Tree depth
               subsample = 0.8,  # Row subsampling
               colsample_bytree = 0.8,  # Feature subsampling
               alpha = 0.1,  # L1 regularization (Lasso)
               lambda = 1)  # L2 regularization (Ridge)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, 
                       watchlist = list(train = dtrain), verbose = 0)

xgb_predictions <- predict(xgb_model, newdata = dtest)
EARNED_FULL_DF$avg_lifespan_yrs[is.na(EARNED_FULL_DF$avg_lifespan_yrs)] <- xgb_predictions


ggplot(EARNED_FULL_DF, aes(x = avg_lifespan_yrs)) +
  geom_histogram(binwidth = 1, fill = "#49A5F1", color = "black") +
  labs(title = "Histogram of Average Lifespan (years) after imputation", x = "Average Lifespan (years)", y = "Frequency") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

# also apply to PRICING_FULL_DF
pricing <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                          nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                          nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                          gender_de_sex + LGA_QUINTILE + SCORE + 
                          population_density + unemployment_2024 + internet_access_proportion_2016 + 
                          travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                          avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                          family_household_proportion_2024, 
                        data = PRICING_FULL_DF %>% filter(is.na(avg_lifespan_yrs)))[, -1]
dtest_pricing <- xgb.DMatrix(data = pricing)

xgb_predictions_pricing <- predict(xgb_model, newdata = dtest_pricing)
PRICING_FULL_DF$avg_lifespan_yrs[is.na(PRICING_FULL_DF$avg_lifespan_yrs)] <- xgb_predictions_pricing

## Imputing energy_level
ggplot(EARNED_FULL_DF, aes(x = energy_level)) +
  geom_bar(fill = "#C859EB", color = "black") +
  labs(title = "Bar Chart of AKC Energy Levels before imputation", x = "AKC Energy Level", y = "Frequency") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

train <- EARNED_FULL_DF %>% filter(!is.na(energy_level))
test <- EARNED_FULL_DF %>% filter(is.na(energy_level))

train <- train %>%
  mutate(across(c(gender_de_sex, LGA_NAME24, LGA_CODE24, LGA_QUINTILE), as.factor))

y <- train$energy_level
x <- model.matrix(energy_level ~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                    nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                    nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                    gender_de_sex + LGA_QUINTILE + SCORE + 
                    population_density + unemployment_2024 + internet_access_proportion_2016 + 
                    travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                    avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                    family_household_proportion_2024, 
                  data = train)[, -1] 
x_test <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                         nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                         nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                         gender_de_sex + LGA_QUINTILE + SCORE + 
                         population_density + unemployment_2024 + internet_access_proportion_2016 + 
                         travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                         avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                         family_household_proportion_2024, 
                       data = test)[, -1]

## xgboost
dtrain <- xgb.DMatrix(data = x, label = y)
dtest <- xgb.DMatrix(data = x_test)

params <- list(objective = "count:poisson",
               booster = "gbtree",  # Use trees
               eta = 0.1,  # Learning rate
               max_depth = 6,  # Tree depth
               subsample = 0.8,  # Row subsampling
               colsample_bytree = 0.8,  # Feature subsampling
               alpha = 0.1,  # L1 regularization (Lasso)
               lambda = 1)  # L2 regularization (Ridge)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, 
                       watchlist = list(train = dtrain), verbose = 0)

xgb_predictions <- predict(xgb_model, newdata = dtest)
EARNED_FULL_DF$energy_level[is.na(EARNED_FULL_DF$energy_level)] <- round(xgb_predictions)

ggplot(EARNED_FULL_DF, aes(x = energy_level)) +
  geom_bar(fill = "#49A5F1", color = "black") +
  labs(title = "Bar Chart of AKC Energy Levels after imputation", x = "AKC Energy Level", y = "Frequency") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

# also apply to PRICING_FULL_DF
pricing <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                          nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                          nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                          gender_de_sex + LGA_QUINTILE + SCORE + 
                          population_density + unemployment_2024 + internet_access_proportion_2016 + 
                          travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                          avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                          family_household_proportion_2024, 
                        data = PRICING_FULL_DF %>% filter(is.na(energy_level)))[, -1]
dtest_pricing <- xgb.DMatrix(data = pricing)

xgb_predictions_pricing <- predict(xgb_model, newdata = dtest_pricing)
PRICING_FULL_DF$energy_level[is.na(PRICING_FULL_DF$energy_level)] <- round(xgb_predictions_pricing)

## Imputing median_dwelling_price_lga
EARNED_FULL_DF <- EARNED_FULL_DF %>% 
  mutate(median_dwelling_price_lga = as.numeric(median_dwelling_price_lga))

train <- EARNED_FULL_DF %>% filter(!is.na(median_dwelling_price_lga))
test <- EARNED_FULL_DF %>% filter(is.na(median_dwelling_price_lga))

train <- train %>%
  mutate(across(c(gender_de_sex, LGA_NAME24, LGA_CODE24, LGA_QUINTILE), as.factor))

y <- train$median_dwelling_price_lga
x <- model.matrix(median_dwelling_price_lga ~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                    nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                    nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                    gender_de_sex + LGA_QUINTILE + SCORE + 
                    population_density + unemployment_2024 + internet_access_proportion_2016 + 
                    travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                    avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                    family_household_proportion_2024, 
                  data = train)[, -1] 
x_test <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                         nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                         nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                         gender_de_sex + LGA_QUINTILE + SCORE + 
                         population_density + unemployment_2024 + internet_access_proportion_2016 + 
                         travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                         avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                         family_household_proportion_2024, 
                       data = test)[, -1]

## xgboost
dtrain <- xgb.DMatrix(data = x, label = y)
dtest <- xgb.DMatrix(data = x_test)

params <- list(objective = "reg:squarederror",
               booster = "gbtree",  # Use trees
               eta = 0.1,  # Learning rate
               max_depth = 6,  # Tree depth
               subsample = 0.8,  # Row subsampling
               colsample_bytree = 0.8,  # Feature subsampling
               alpha = 0.1,  # L1 regularization (Lasso)
               lambda = 1)  # L2 regularization (Ridge)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, 
                       watchlist = list(train = dtrain), verbose = 0)

xgb_predictions <- predict(xgb_model, newdata = dtest)
EARNED_FULL_DF$median_dwelling_price_lga[is.na(EARNED_FULL_DF$median_dwelling_price_lga)] <- xgb_predictions

## Imputing condition_category
train <- EARNED_FULL_DF %>% filter(claim_paid > 0) %>% filter(!is.na(condition_category))
test <- EARNED_FULL_DF %>% filter(claim_paid > 0) %>% filter(is.na(condition_category))

train <- train %>%
  mutate(across(c(gender_de_sex, LGA_NAME24, LGA_CODE24, LGA_QUINTILE), as.factor))

y <- as.numeric(factor(train$condition_category)) - 1

x <- model.matrix(condition_category ~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                    nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                    nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                    gender_de_sex + LGA_QUINTILE + SCORE + 
                    population_density + unemployment_2024 + internet_access_proportion_2016 + 
                    travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                    avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                    family_household_proportion_2024, 
                  data = train)[, -1]

x_test <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + nb_contribution + 
                         nb_excess + nb_address_type_adj + nb_state + pet_age_years + owner_age_years + nb_number_of_breeds + 
                         nb_average_breed_size + nb_breed_type + nb_breed_trait + is_multi_pet_plan + quote_time_group + 
                         gender_de_sex + LGA_QUINTILE + SCORE + 
                         population_density + unemployment_2024 + internet_access_proportion_2016 + 
                         travelled_to_work_by_car_2024 + worked_from_home_2024 + avg_household_size_2024 + married_2024 + 
                         avg_children_per_family_2024 + avg_cars_per_household_2024 + own_property_2024 + rent_property_2024 + 
                         family_household_proportion_2024, 
                       data = test)[, -1]

# Prepare data for xgboost
dtrain <- xgb.DMatrix(data = x, label = y)
dtest <- xgb.DMatrix(data = x_test)

num_classes <- length(unique(y))

params <- list(objective = "multi:softmax",
               num_class = num_classes,
               booster = "gbtree",  
               eta = 0.1,  
               max_depth = 6,  
               subsample = 0.8,  
               colsample_bytree = 0.8,  
               alpha = 0.1,  
               lambda = 1)


xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, 
                       watchlist = list(train = dtrain), verbose = 0)
original_condition_categories <- levels(factor(train$condition_category))
xgb_predictions <- predict(xgb_model, newdata = dtest)
predicted_categories <- original_condition_categories[xgb_predictions + 1]

EARNED_FULL_DF %>%
  mutate(condition_category = ifelse(is.na(condition_category) & claim_paid > 0, 
                                     predicted_categories, 
                                     as.character(condition_category))) %>% 
  mutate(condition_category = factor(condition_category, levels = original_condition_categories))



## Decluttering Environment
rm(train, test, params, x, x_test, xgb_model, dtest, dtrain, xgb_predictions, y, pricing, dtest_pricing, xgb_predictions_pricing)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# 3. Data Exploration ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.1. Breed ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
claims_only <- EARNED_FULL_DF %>%
  filter(claim_count > 0)

## Breed trait
btrait <- EARNED_FULL_DF %>% 
  group_by(nb_breed_trait) %>%
  summarise(claim_count = sum(claim_count), 
            exposure = sum(earned_units), 
            claim_freq = claim_count/exposure
  ) %>%
  mutate(proportion = exposure/sum(exposure)) %>%
  filter(exposure > 2500)

claims_only_btrait <- claims_only %>% filter(nb_breed_trait %in% btrait$nb_breed_trait)

## Plot proportions
ggplot(btrait) +
  geom_bar(aes(x = nb_breed_trait, y = exposure), stat = "identity", 
           fill = c("#49A5F1", 
                    "#E6830C", 
                    "#C859EB", 
                    "#EF5493", 
                    "#49A5F1", 
                    "#E6830C", 
                    "#C859EB")) +
  geom_text(aes(x = as.numeric(factor(nb_breed_trait)), y = exposure, label = paste0(round(proportion * 100, 2), "%")), 
            vjust = -0.5, size = 4, color = "black", family = "DM Sans") +
  scale_y_continuous(sec.axis = sec_axis(~./sum(btrait$exposure), name = "Proportion")) +
  labs(title = "Count and Proportion of Breed Trait",
       y = "Count",
       x = "Breed Trait") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Plot claim frequency
ggplot(btrait) +
  geom_bar(aes(x = nb_breed_trait, y = claim_freq), stat = "identity", 
           fill = c("#49A5F1", 
                    "#E6830C", 
                    "#C859EB", 
                    "#EF5493", 
                    "#49A5F1", 
                    "#E6830C", 
                    "#C859EB")) +
  geom_text(aes(x = as.numeric(factor(nb_breed_trait)), y = claim_freq, label = paste0(round(claim_freq, 5))), 
            vjust = -0.5, size = 4, color = "black", family = "DM Sans") +
  labs(title = "Claim frequency by Breed Trait",
       y = "Claim frequency",
       x = "Breed Trait") + 
  ylim(0, 0.13) +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Plot claim severity
ggplot(claims_only_btrait, aes(x = nb_breed_trait, y = total_claim_amount, fill = nb_breed_trait)) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 1000)) +
  labs(title = "Boxplot of Claim Amount by Breed Trait",
       x = "Breed Trait",
       y = "Claim amount",
       fill = "Breed Trait") +
  scale_fill_manual(values = c("#49A5F1", 
                               "#E6830C", 
                               "#C859EB", 
                               "#EF5493", 
                               "#49A5F1", 
                               "#E6830C", 
                               "#C859EB")) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12),
        legend.position = "none")

## Study individual breeds
chosen_breed_data <- CLAIMS_FULL_DF %>%
  filter(nb_breed_trait == "collie related") %>% # Select breed
  group_by(condition_category) %>%
  summarise(count = n(), 
            average_claim = mean(total_claim_amount)
  ) %>%
  arrange(desc(count))

chosen_breed_data

## Decluttering Environment
rm(btrait, claims_only, claims_only_btrait, chosen_breed_data)


### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.2. Gender ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Pet Gender - Frequency
EARNED_FULL_DF %>%
  group_by(pet_gender) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = pet_gender, y = Count, fill = pet_gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Pet Gender", x = "Pet Gender", y = "Count") +
  scale_fill_manual(values = c("male" = "#49A5F1", "female" = "#EF5493")) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

EARNED_FULL_DF %>%
  group_by(pet_gender) %>%
  summarise(
    ClaimCount = sum(claim_count, na.rm = TRUE), 
    Exposure = sum(earned_units, na.rm = TRUE)
  ) %>%
  mutate(Frequency = ClaimCount/Exposure) %>%
  ggplot(aes(x = pet_gender, y = Frequency, fill = pet_gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim Frequency by Pet Gender", x = "Pet Gender", y = "Frequency") +
  scale_fill_manual(values = c("male" = "#49A5F1", "female" = "#EF5493")) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Pet Gender - Severity
EARNED_FULL_DF %>%
  group_by(pet_gender) %>%
  summarise(Amount = sum(total_claim_amount*claim_count),
            Exposure = sum(earned_units, na.rm = TRUE)) %>%
  mutate(per_claim = Amount/Exposure) %>%
  ggplot(aes(x = pet_gender, y = per_claim, fill = pet_gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim Severity by Pet Gender", x = "Pet Gender", y = "Average Claim Amount") +
  scale_fill_manual(values = c("male" = "#49A5F1", "female" = "#EF5493")) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Pet Gender - Large Claims
threshold <- quantile(EARNED_FULL_DF$total_claim_amount, 0.95, na.rm = TRUE)

gender_above_threshold <- EARNED_FULL_DF %>%
  filter(total_claim_amount > threshold) %>%
  group_by(pet_gender) %>%
  summarise(Count = n())

EARNED_FULL_DF %>% 
  filter(total_claim_amount > 0) %>%
  ggplot(aes(x = pet_gender, y = claim_paid)) +
  geom_boxplot(outlier.colour = "#E6830C", outlier.shape = 16, outlier.size = 2) +  
  ylim(0, 1000) +
  labs(title = "Box and Whisker Plot of Average Total Claim Amounts by Gender",
       x = "Pet Gender",
       y = "Average Claim Amount") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))
scale_y_continuous(labels = scales::dollar_format())  

under_threshold <- EARNED_FULL_DF %>% filter(total_claim_amount > 0) %>% filter(total_claim_amount < threshold)

ggplot(under_threshold, aes(x = pet_gender, y = total_claim_amount, fill = pet_gender)) +
  geom_boxplot() +  
  labs(title = "Box and Whisker Plot of Average Total Claim Amounts by Gender (95%)",
       x = "Pet Gender",
       y = "Average Claim Amount") + 
  scale_fill_manual(values = c("#EF5493",
                               "#49A5F1")) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))
scale_y_continuous(labels = scales::dollar_format())  

# Decluttering Environment
rm(threshold, gender_above_threshold)


### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.3. Multi Pet Plan ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Multi Pet - Frequency
EARNED_FULL_DF %>%
  group_by(is_multi_pet_plan) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = is_multi_pet_plan, y = Count, fill = is_multi_pet_plan)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of multi pet plan", x = "Multi pet plan", y = "Count") +
  scale_fill_manual(values = c("TRUE" = "#E6830C", "FALSE" = "#49A5F1")) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

EARNED_FULL_DF %>%
  group_by(is_multi_pet_plan) %>%  
  summarise(
    ClaimCount = sum(claim_count, na.rm = TRUE), 
    Exposure = sum(earned_units, na.rm = TRUE)
  ) %>%
  mutate(Frequency = ClaimCount/Exposure) %>%
  ggplot(aes(x = is_multi_pet_plan, y = Frequency, fill = is_multi_pet_plan)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim Frequency by multi pet plan", x = "Multi pet plan", y = "Frequency") +
  scale_fill_manual(values = c("TRUE" = "#E6830C", "FALSE" = "#49A5F1")) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Multi Pet - Severity
EARNED_FULL_DF %>%
  group_by(is_multi_pet_plan) %>%
  summarise(Amount = sum(total_claim_amount*claim_count),
            Exposure = sum(earned_units, na.rm = TRUE)) %>%
  mutate(per_claim = Amount/Exposure) %>%
  ggplot(aes(x = is_multi_pet_plan, y = per_claim, fill = is_multi_pet_plan)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Total Claim Amount by multi pet plan", x = "Multi pet plan", y = "Average Claim Amount") +
  scale_fill_manual(values = c("TRUE" = "#E6830C", "FALSE" = "#49A5F1")) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Multi Pet - Large Claims
ggplot(EARNED_FULL_DF, aes(x = is_multi_pet_plan, y = total_claim_amount)) +
  geom_boxplot(outlier.colour = "#C859EB", outlier.shape = 16, outlier.size = 2) +  
  labs(title = "Box and Whisker Plot of Average Total Claim Amounts by multi pet plan",
       x = "Multi pet plan",
       y = "Average Claim Amount") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12)) + 
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_manual(values = c("TRUE" = "#E6830C", "FALSE" = "#49A5F1"))

ggplot(under_threshold, aes(x = is_multi_pet_plan, y = total_claim_amount, fill = is_multi_pet_plan)) +
  geom_boxplot(outlier.colour = "#C859EB", outlier.shape = 16, outlier.size = 2) +  
  labs(title = "Box and Whisker Plot of Average Total Claim Amounts by Multi Pet Plan (95%)",
       x = "Multi Pet Plan",
       y = "Average Claim Amount") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_fill_manual(values = c("TRUE" = "#E6830C", "FALSE" = "#49A5F1"))

## Statistical tests - Severity
claims_summary_exposure_data <- EARNED_FULL_DF %>%
  mutate(claim_amount_per_exposure = total_claim_amount/earned_units) %>%
  mutate(claim_count_per_exposure = claim_count/earned_units)

kruskal_test <- function(independent_var, dependent_var) {
  formula <- as.formula(paste(dependent_var, "~", independent_var))
  kruskal_result <- kruskal.test(formula, data = claims_summary_exposure_data)
  return(kruskal_result)
}

independent_variables_table <- claims_summary_exposure_data %>%
  select_if(is.factor)

independent_variables <- names(independent_variables_table)

valid_independent_variables <- independent_variables[ 
  sapply(independent_variables_table, function(col) length(unique(col)) > 1)
]

dependent_var <- "claim_amount_per_exposure" 

kruskal_results <- lapply(valid_independent_variables, function(indep_var) {
  kruskal_test(indep_var, dependent_var)
})

kruskal_pvalues <- sapply(kruskal_results, function(result) {
  result$p.value
})

kruskal_summary_amt <- data.frame(
  Variable = valid_independent_variables,
  P_Value = kruskal_pvalues
) %>%
  arrange(P_Value)

kruskal_summary_amt

## Frequency
dependent_var <- "claim_count_per_exposure" 

kruskal_results <- lapply(valid_independent_variables, function(indep_var) {
  kruskal_test(indep_var, dependent_var)
})

kruskal_pvalues <- sapply(kruskal_results, function(result) {
  result$p.value
})

kruskal_summary_freq <- data.frame(
  Variable = valid_independent_variables,
  P_Value = kruskal_pvalues
) %>%
  arrange(P_Value)

kruskal_summary_freq

## Decluttering Environment
rm(claims_summary_exposure_data, independent_variables_table, kruskal_results, kruskal_summary_amt, 
   kruskal_summary_freq, under_threshold, dependent_var, independent_variables, kruskal_pvalues, 
   valid_independent_variables)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.4. Age ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Pet Age Variable Preparation
EARNED_FULL_DF <- EARNED_FULL_DF %>%
  mutate(pet_age = pet_age_months/12)

PRICING_FULL_DF <- PRICING_FULL_DF %>%
  mutate(pet_age = pet_age_months/12)

## Continuous age
pet_age_count <- EARNED_FULL_DF %>%
  group_by(pet_age) %>%
  summarise(Count = n(),
            Exposure = sum(earned_units)) %>%
  mutate(avg_exposure = Exposure/Count)

pet_age <- EARNED_FULL_DF %>%
  group_by(pet_age) %>%
  summarise(Amount = sum(total_claim_amount, na.rm = TRUE)) %>%
  mutate(per_claim = Amount/pet_age_count$Count) %>%
  arrange(desc(per_claim)) 

## Categorical age
pet_age_count1 <- EARNED_FULL_DF %>%
  mutate(pet_age = floor(pet_age)) %>%
  group_by(pet_age) %>%
  summarise(Count = n(),
            Exposure = sum(earned_units)) %>%
  mutate(avg_exposure = Exposure/Count)

pet_age1 <- EARNED_FULL_DF %>%
  mutate(pet_age = floor(pet_age)) %>%
  group_by(pet_age) %>%
  summarise(Amount = sum(total_claim_amount, na.rm = TRUE), 
            Count = sum(claim_count, na.rm = TRUE)) %>%
  mutate(avg_claim = Amount/pet_age_count1$Count,
         avg_freq = Count/pet_age_count1$Exposure)

## Summary Statistics of Age
EARNED_FULL_DF %>% summarize(
  mean_age = mean(pet_age),
  median_age = median(pet_age),
  min_age = min(pet_age),
  max_age = max(pet_age),
  sd_age = sd(pet_age)
)

## Distribution of pet ages
ggplot(EARNED_FULL_DF, aes(x = pet_age)) +
  geom_histogram(binwidth = 0.5, fill = "#EF5493", color = "white") +
  labs(title = "Distribution of Pet Ages", x = "Age (Years)", y = "Frequency") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Cat's EDA but in years
ggplot(pet_age, aes(x = pet_age, y = per_claim)) +
  geom_bar(stat = "identity", fill = "#49A5F1", color = "#49A5F1") +
  labs(title = "Total Claim Amount by Pet Age", 
       x = "Pet Age (Years)", 
       y = "Total Claim Amount per Claim") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Boxplot of claim amounts by each age group
CLAIMS_FULL_DF <- CLAIMS_FULL_DF %>%
  mutate(pet_age = pet_age_months/12)

boxplot1 <- CLAIMS_FULL_DF %>% # Not grouped by age because need all data for boxplot
  dplyr::select(pet_age, total_claim_amount) %>%
  mutate(pet_age = floor(pet_age))

ggplot(boxplot1, aes(x = as.factor(pet_age), y = total_claim_amount)) + 
  geom_boxplot(fill = "#C859EB", color = "black") +
  labs(title = "Distribution of Total Claim Amount for Each Age", 
       x = "Pet Age",
       y = "Total Claim Amount") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

# Zooming into boxes
ggplot(boxplot1, aes(x = as.factor(pet_age), y = total_claim_amount)) + 
  geom_boxplot(fill = c("#49A5F1",
                        "#E6830C",
                        "#C859EB",
                        "#EF5493",
                        "#49A5F1",
                        "#E6830C",
                        "#C859EB",
                        "#EF5493",
                        "#49A5F1",
                        "#E6830C"),
               color = "black") +
  ylim(0,2000) +
  labs(title = "Distribution of Total Claim Amount for Each Age", 
       x = "Pet Age (years)",
       y = "Total Claim Amount") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Claim severity and Frequency
scale <- 200/0.15

ggplot() +
  geom_bar(data = pet_age1, aes(x = as.factor(pet_age), y = avg_claim), 
           stat = "identity", fill = "#E6830C") +
  geom_line(data = pet_age1, aes(x = as.factor(pet_age), y = avg_freq * scale, group = 1)) +
  labs(title = "Average Claim Amount and Frequency by Pet Age", 
       x = "Pet Age (Years)", 
       y = "Average Claim Amount") +
  scale_y_continuous(
    name = "Average Claim Amount",
    sec.axis = sec_axis(~ . / scale, name = "Average Frequency")
  ) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Total claim amount and frequency for puppies
puppies <- EARNED_FULL_DF %>%
  filter(pet_age_months <= 12)

pup_age_count <- puppies %>%
  group_by(pet_age_months) %>%
  summarise(Count = n(),
            Exposure = sum(earned_units))

pup_age <- puppies %>%
  group_by(pet_age_months) %>%
  summarise(Amount = sum(total_claim_amount, na.rm = TRUE), 
            Count = sum(claim_count, na.rm = TRUE)) %>%
  mutate(avg_claim = Amount/pup_age_count$Count,
         avg_freq = Count/pup_age_count$Exposure)

scale <- 175/0.4

ggplot() +
  geom_bar(data = pup_age, aes(x = as.factor(pet_age_months), y = avg_claim), 
           stat = "identity", fill = "#49A5F1") +
  geom_line(data = pup_age, aes(x = as.factor(pet_age_months), y = avg_freq * scale, group = 1)) +
  labs(title = "Average Claim Amount and Frequency for Puppies Aged 0-1", 
       x = "Pet Age (Months)", 
       y = "Average Claim Amount") +
  scale_y_continuous(
    name = "Average Claim Amount",
    sec.axis = sec_axis(~ . / scale, name = "Average Frequency")  
  ) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## ANOVA for Age
variables <- c("total_claim_amount", "earned_units", "claim_paid", 
               "claim_count")

anova_results_df <- data.frame(Variable = character(), P_Value = numeric(), stringsAsFactors = FALSE)

for (i in variables) {
  # Perform ANOVA
  anova <- as.formula(paste(i, "~ pet_age_years"))
  aov_result <- aov(anova, data = EARNED_FULL_DF)
  p_value <- summary(aov_result)[[1]][["Pr(>F)"]][1]
  anova_results_df <- rbind(anova_results_df, data.frame(Variable = i, p_value = p_value))
}

anova_results_df <- anova_results_df %>%
  arrange(p_value)

anova_results_df


## Age vs Exposure
# Total Exposure
ggplot() +
  geom_bar(data = pet_age_count1, aes(x = as.factor(pet_age), y = Exposure), 
           stat = "identity", fill = "#E6830C") +
  labs(title = "Total Exposure per Pet Age", 
       x = "Pet Age (Years)",
       y = "Total Exposure") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

# Average Exposure
## Categorical age
ggplot() +
  geom_line(data = pet_age_count1, aes(x = as.factor(pet_age), y = avg_exposure, group = 1)) +
  labs(title = "Average Exposure by Pet Age", 
       x = "Pet Age (Years)",
       y = "Average Exposure") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Continuous age
ggplot() +
  geom_line(data = pet_age_count, aes(x = pet_age, y = avg_exposure, group = 1)) +
  labs(title = "Average Exposure by Pet Age", 
       x = "Pet Age (Years)",
       y = "Average Exposure") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))


## Age vs condition
#ggplot(CLAIMS_FULL_DF, aes(x = condition_category, y = pet_age)) + 
#  geom_boxplot(fill = c("#49A5F1",
#                        "#E6830C",
#                        "#C859EB",
#                        "#EF5493",
#                        "#49A5F1",
#                        "#E6830C",
#                        "#C859EB",
#                        "#EF5493",
#                        "#49A5F1",
#                        "#E6830C",
#                        "#C859EB"),
#               color = "black") +
#  labs(title = "Distribution of Age for each Condition", 
#       x = "Condition",
#       y = "Age") +
#  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
#        plot.title=element_text(hjust=0.5, size=16),
#        axis.line=element_line(linewidth=1, colour="grey80"),
#        text=element_text(family="DM Sans", size=12))

#ggplot(CLAIMS_FULL_DF, aes(x = condition_category, y = pet_age)) + 
#  geom_boxplot(fill = c("#49A5F1",
#                        "#E6830C",
#                        "#C859EB",
#                        "#EF5493",
#                        "#49A5F1",
#                        "#E6830C",
#                        "#C859EB",
#                        "#EF5493",
#                        "#49A5F1",
#                        "#E6830C",
#                        "#C859EB"),
#               color = "black") +
#  labs(title = "Distribution of Age (0-3 years) for each Condition", 
#       x = "Condition",
#       y = "Age") +
#  ylim(0, 3) +
#  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
#        plot.title=element_text(hjust=0.5, size=16),
#        axis.line=element_line(linewidth=1, colour="grey80"),
#        text=element_text(family="DM Sans", size=12))

## Age vs Breed Trait
ggplot(CLAIMS_FULL_DF, aes(x = nb_breed_trait, y = pet_age)) + 
  geom_boxplot(color = "black") +
  labs(title = "Distribution of Age for each Breed Trait", 
       x = "Breed Trait",
       y = "Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Age vs Breed Type
ggplot(CLAIMS_FULL_DF, aes(x = nb_breed_type, y = pet_age)) + 
  geom_boxplot(fill = c("#49A5F1",
                        "#E6830C",
                        "#C859EB",
                        "#EF5493"),
               color = "black") +
  labs(title = "Distribution of Age for each Breed Trait", 
       x = "Breed Type",
       y = "Age") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Age vs Gender
ggplot(CLAIMS_FULL_DF, aes(x = pet_age, fill = pet_gender)) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.5) +  # Alpha controls transparency
  labs(x = "Age", y = "Frequency", title = "Age Distribution by Gender") +
  scale_fill_manual(values = c("#49A5F1",  "#EF5493")) +  # Custom colors for Male and Female
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Decluttering Environment
rm(anova_results_df, aov_result, boxplot1, pet_age, pet_age_count, pet_age_count1, pet_age1, pup_age, pup_age_count, 
   puppies, anova, i, p_value, scale, variables)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.5. Quintiles ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Quintiles - Frequency
EARNED_FULL_DF %>%
  group_by(LGA_QUINTILE) %>%
  mutate(LGA_QUINTILE = as.factor(LGA_QUINTILE)) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = LGA_QUINTILE, y = Count, fill = LGA_QUINTILE)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Policyholders per ISRAD Quintile", x = "ISRAD Quintiles", y = "Count") +
  scale_fill_manual(values = c("#49A5F1",
                               "#E6830C",
                               "#C859EB",
                               "#EF5493",
                               "#49A5F1")) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

EARNED_FULL_DF %>%
  group_by(LGA_QUINTILE) %>%
  mutate(LGA_QUINTILE = as.factor(LGA_QUINTILE)) %>%
  summarise(
    ClaimCount = sum(claim_count, na.rm = TRUE), 
    Exposure = sum(earned_units, na.rm = TRUE)
  ) %>%
  mutate(Frequency = ClaimCount/Exposure) %>%
  ggplot(aes(x = LGA_QUINTILE, y = Frequency, fill = LGA_QUINTILE)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim Frequency by ISRAD Quintile", x = "ISRAD Quintile", y = "Claim Frequency") +
  scale_fill_manual(values = c("#49A5F1",
                               "#E6830C",
                               "#C859EB",
                               "#EF5493",
                               "#49A5F1")) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Quintiles - Severity
EARNED_FULL_DF %>%
  group_by(LGA_QUINTILE) %>%
  mutate(LGA_QUINTILE = as.factor(LGA_QUINTILE)) %>%
  summarise(ClaimPaid = sum(claim_paid*claim_count),
            Exposure = sum(earned_units, na.rm = TRUE)) %>%
  mutate(per_claim = ClaimPaid/Exposure) %>%
  ggplot(aes(x = LGA_QUINTILE, y = per_claim, fill = LGA_QUINTILE)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim Severity by ISRAD Quintile", x = "ISRAD Quintile", y = "Average Claim Paid") +
  scale_fill_manual(values = c("#49A5F1",
                               "#E6830C",
                               "#C859EB",
                               "#EF5493",
                               "#49A5F1")) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

# Combining the plots
EARNED_FULL_DF %>%
  group_by(LGA_QUINTILE) %>%
  mutate(LGA_QUINTILE = as.factor(LGA_QUINTILE)) %>%
  summarise(
    Count = n(),
    ClaimCount = sum(claim_count, na.rm = TRUE), 
    Exposure = sum(earned_units, na.rm = TRUE),
    ClaimPaid = sum(claim_paid*claim_count)
  ) %>%
  mutate(Frequency = ClaimCount / Exposure,
         per_claim = ClaimPaid/Exposure) %>%
  ggplot(aes(x = LGA_QUINTILE)) +
  geom_bar(aes(y = per_claim, fill = LGA_QUINTILE), stat = "identity") +
  geom_line(aes(y = Frequency * 40 / 0.12, group = 1, color = "Claim Frequency"), linewidth = 1.2) +
  geom_point(aes(y = Frequency * 40 / 0.12, group = 1, color = "Claim Frequency"), size = 3) +
  
  labs(title = "Claim Severity and Claim Frequency by ISRAD Quintile",
       x = "ISRAD Quintile", y = "Claim Severity") +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * 0.12 / 40, name = "Claim Frequency")) +
  scale_fill_manual(values = c("#49A5F1", "#E6830C", "#C859EB", "#EF5493", "#49A5F1")) +
  scale_color_manual(values = "black") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 12),
        legend.position = "right")

## SCORE EDA

## SCORE - Frequency
EARNED_FULL_DF %>%
  group_by(SCORE) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = SCORE, y = Count)) +
  geom_line() +  # Use geom_line for continuous data
  labs(title = "Count of Policyholders per SEIFA Score", x = "SEIFA Score", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 12))

EARNED_FULL_DF %>%
  group_by(SCORE) %>%
  summarise(
    ClaimCount = sum(claim_count, na.rm = TRUE), 
    Exposure = sum(earned_units, na.rm = TRUE)) %>%
  mutate(Frequency = ClaimCount / Exposure) %>%
  filter(Exposure > 0) %>%
  ggplot(aes(x = SCORE, y = Frequency)) +
  geom_point(alpha = 0.6, color = "#49A5F1") +
  ylim(0,0.25) +
  labs(title = "Claim Frequency by SEIFA Score", x = "SEIFA Score", y = "Claim Frequency") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 12))

## SCORE - Severity
EARNED_FULL_DF %>%
  group_by(SCORE) %>%
  summarise(ClaimPaid = sum(claim_paid*claim_count),
            Exposure = sum(earned_units, na.rm = TRUE)) %>%
  mutate(per_claim = ClaimPaid/Exposure) %>%
  filter(Exposure > 0) %>%
  ggplot(aes(x = SCORE, y = per_claim)) +
  geom_point(alpha = 0.6, color = "#49A5F1") +
  labs(title = "Claim Severity by SEIFA Score", x = "SEIFA Score", y = "Average Claim Severity") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))


# Decluttering Environment
rm(LGA_QUINTILE, LGA_SHAPES)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.6. Income ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
ggplot(INCOME_AGE, aes(x = Average_Income, y = `20 years and under`)) + 
  geom_col(fill = "#49A5F1", color = "black") +
  labs(title = "Income Distribution for 20 and under", x = "Income ($)", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(INCOME_AGE, aes(x = Average_Income, y = `21 to 34 years`)) +  
  geom_col(fill = "#49A5F1", color = "black") +
  labs(title = "Income Distribution for 21-34 years", x = "Income ($)", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(INCOME_AGE, aes(x = Average_Income, y = `35 to 44 years`)) +  
  geom_col(fill = "#49A5F1", color = "black") +
  labs(title = "Income Distribution for 35-44 years", x = "Income ($)", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(INCOME_AGE, aes(x = Average_Income, y = `45 to 54 years`)) +  
  geom_col(fill = "#49A5F1", color = "black") +
  labs(title = "Income Distribution for 45-54 years", x = "Income ($)", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(INCOME_AGE, aes(x = Average_Income, y = `55 years and over`)) +  
  geom_col(fill = "#49A5F1", color = "black") +
  labs(title = "Income Distribution for 55+ years", x = "Income ($)", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

Avg_Income_AGE <- INCOME_AGE %>%
  mutate(Total_20 = `20 years and under`*Average_Income,
         Total_21_34 = `21 to 34 years`*Average_Income,
         Total_35_44 = `35 to 44 years`*Average_Income,
         Total_45_54 = `45 to 54 years`*Average_Income,
         Total_55 = `55 years and over`*Average_Income) %>%
  dplyr::select(Average_Income, Total_20, Total_21_34, Total_35_44, Total_45_54, Total_55 )

income_sums <- colSums(Avg_Income_AGE[,-1]) 

count_sums <- colSums(INCOME_AGE[2:6])

average_income <- income_sums/count_sums

## SD 
variance_income <- INCOME_AGE %>%
  mutate(
    Var_20 = (`20 years and under` * (Average_Income - average_income[1])^2),
    Var_21_34 = (`21 to 34 years` * (Average_Income - average_income[2])^2),
    Var_35_44 = (`35 to 44 years` * (Average_Income - average_income[3])^2),
    Var_45_54 = (`45 to 54 years` * (Average_Income - average_income[4])^2),
    Var_55 = (`55 years and over` * (Average_Income - average_income[5])^2)
  ) 

# Calculate total variance for each age group
total_variance <- colSums(variance_income[9:13]) / colSums(INCOME_AGE[2:6])

# Calculate the standard deviation
SD_income <- sqrt(total_variance)

## Graphing
generate_lognormal <- function(mean_value, sd_value, n = 1000) {
  mu <- log(mean_value^2 / sqrt(sd_value^2 + mean_value^2))
  sigma <- sqrt(log(sd_value^2 / mean_value^2 + 1))
  
  rlnorm(n, meanlog = mu, sdlog = sigma)
}

lognormal_samples_list <- lapply(1:length(average_income), function(i) {
  generate_lognormal(average_income[i], SD_income[i])
})

lognormal_samples_df <- as.data.frame(do.call(cbind, lognormal_samples_list))

colnames(lognormal_samples_df) <- paste0("lognormal_samples_", names(SD_income))

for (i in seq_along(lognormal_samples_list)) {
  
  plot_data <- data.frame(Weekly_Income = lognormal_samples_df[[i]], 
                          Distribution = colnames(lognormal_samples_df)[i])
  
  p <- ggplot(plot_data, aes(x = Weekly_Income)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = paste("Log-Normal Distribution for", colnames(lognormal_samples_df)[i]),
         x = "Weekly Income",
         y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_vline(aes(xintercept = average_income[i]), color = "red", linetype = "dashed") +
    xlim(0, 3600)  
  
  print(p)
}

## Merging income by age with data
generate_lognormal_by_age <- function(age) {
  if (is.na(age)) {
    return(NA)  
  }
  
  age_ranges <- c(20, 34, 44, 54, Inf) 
  index <- findInterval(age, age_ranges) + 1
  
  mu <- log(average_income[index]^2 / sqrt(SD_income[index]^2 + average_income[index]^2))
  sigma <- sqrt(log(SD_income[index]^2 / average_income[index]^2 + 1))
  
  return(rlnorm(1, meanlog = mu, sdlog = sigma))
}

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  mutate(income_by_age = 48*sapply(owner_age_years, generate_lognormal_by_age))

PRICING_FULL_DF <- PRICING_FULL_DF %>%
  mutate(income_by_age = 48*sapply(owner_age_years, generate_lognormal_by_age))

## Living at home/moved out weighting 
# RANDOM
weights <- c(0.8, 0.5, 0.2, 0.1, 0.05)

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  mutate(
    income_by_lga = as.numeric(income_by_lga),
    income_by_age = as.numeric(income_by_age),
    weighted_income = case_when(
      is.na(owner_age_years) ~ income_by_lga,  # If owner_age_years is NA, return income_by_lga
      owner_age_years <= 20 ~ coalesce(weights[1] * income_by_lga + (1 - weights[1]) * income_by_age, 
                                       income_by_age, income_by_lga),
      owner_age_years >= 21 & owner_age_years <= 34 ~ coalesce(weights[2] * income_by_lga + (1 - weights[2]) * income_by_age, 
                                                               income_by_age, income_by_lga),
      owner_age_years >= 35 & owner_age_years <= 44 ~ coalesce(weights[3] * income_by_lga + (1 - weights[3]) * income_by_age,
                                                               income_by_age, income_by_lga),
      owner_age_years >= 45 & owner_age_years <= 54 ~ coalesce(weights[4] * income_by_lga + (1 - weights[4]) * income_by_age,
                                                               income_by_age, income_by_lga),
      owner_age_years >= 55 ~ coalesce(weights[5] * income_by_lga + (1 - weights[5]) * income_by_age,
                                       income_by_age, income_by_lga)
    )
  )

PRICING_FULL_DF <- PRICING_FULL_DF %>%
  mutate(
    income_by_lga = as.numeric(income_by_lga),
    income_by_age = as.numeric(income_by_age),
    weighted_income = case_when(
      is.na(owner_age_years) ~ income_by_lga,  # If owner_age_years is NA, return income_by_lga
      owner_age_years <= 20 ~ coalesce(weights[1] * income_by_lga + (1 - weights[1]) * income_by_age, 
                                       income_by_age, income_by_lga),
      owner_age_years >= 21 & owner_age_years <= 34 ~ coalesce(weights[2] * income_by_lga + (1 - weights[2]) * income_by_age, 
                                                               income_by_age, income_by_lga),
      owner_age_years >= 35 & owner_age_years <= 44 ~ coalesce(weights[3] * income_by_lga + (1 - weights[3]) * income_by_age,
                                                               income_by_age, income_by_lga),
      owner_age_years >= 45 & owner_age_years <= 54 ~ coalesce(weights[4] * income_by_lga + (1 - weights[4]) * income_by_age,
                                                               income_by_age, income_by_lga),
      owner_age_years >= 55 ~ coalesce(weights[5] * income_by_lga + (1 - weights[5]) * income_by_age,
                                       income_by_age, income_by_lga)
    )
  )

## Income Claim Severity 
EARNED_FULL_DF %>%
  mutate(Amount = total_claim_amount * claim_count,
         Severity = Amount / earned_units) %>%
  ggplot(aes(x = weighted_income, y = Severity)) +
  geom_point(colour = "#49A5F1", alpha = 0.6) +
  labs(title = "Claim Severity by Owner Income", 
       x = "Weighted Income", 
       y = "Claim Severity") +
  xlim(0, 250000) +
  ylim(0, 1000) +
  theme_minimal()

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  mutate(Amount = total_claim_amount * claim_count,
         Severity = Amount / earned_units)

## Income Claim frequency 
EARNED_FULL_DF %>%
  mutate(Frequency = claim_count / earned_units) %>%
  ggplot(aes(x = weighted_income, y = Frequency)) +
  geom_point(colour = "#49A5F1", alpha = 0.6) +
  labs(title = "Claim Frequency by Owner Income", 
       x = "Weighted Income", 
       y = "Claim Frequency") +
  xlim(0, 250000) +
  ylim(0, 4) +
  theme_minimal()

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  mutate(Frequency = claim_count / earned_units)

model_poisson <- glm(Frequency ~ weighted_income, family = poisson, data = EARNED_FULL_DF)
summary(model_poisson)

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  dplyr::select(-Frequency, -Severity, -Amount)

## Decluttering Environment
rm(Avg_Income_AGE, INCOME_AGE, INCOME_LGA, lognormal_samples_df, lognormal_samples_list, model_poisson, p, 
   plot_data, variance_income, average_income, count_sums, i, income_sums, SD_income, total_variance, weights, 
   generate_lognormal, generate_lognormal_by_age, kruskal_test)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.7. Average Weight ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## WEIGHT - Frequency
EARNED_FULL_DF %>%
  group_by(avg_weight_kg) %>%
  summarise(
    ClaimCount = sum(claim_count, na.rm = TRUE), 
    Exposure = sum(earned_units, na.rm = TRUE)) %>%
  mutate(Frequency = log(ClaimCount / Exposure)) %>%
  filter(Exposure > 0) %>%
  ggplot(aes(x = avg_weight_kg, y = Frequency)) +
  geom_point(alpha = 0.6, color = "#C859EB") +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Claim Frequency by Average Weight of Breeds", x = "Average Weight of Breed", y = "Claim Frequency (log scale)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 12))

## WEIGHT - Severity
EARNED_FULL_DF %>%
  group_by(avg_weight_kg) %>%
  summarise(ClaimPaid = sum(claim_paid*claim_count),
            Exposure = sum(earned_units, na.rm = TRUE)) %>%
  mutate(per_claim = log(ClaimPaid/Exposure)) %>%
  filter(Exposure > 0) %>%
  ggplot(aes(x = avg_weight_kg, y = per_claim)) +
  geom_point(alpha = 0.6, color = "#C859EB") +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Claim Severity by Average Weight of Breeds", x = "Average Weight of Breed", y = "Average Claim Severity") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

# Define size categories and calculate claims
size_proportions <- EARNED_FULL_DF %>%
  mutate(size_category = case_when(
    avg_weight_kg < 14 ~ "Small",
    avg_weight_kg > 26 ~ "Big",
    TRUE ~ "Medium"
  )) %>%
  group_by(size_category) %>%
  summarise(
    TotalDogs = n(),
    TotalClaims = sum(claim_count, na.rm = TRUE),
    ProportionClaiming = TotalClaims / TotalDogs,
    AvgClaimPaid = mean(claim_paid, na.rm = TRUE)
  ) %>%
  filter(size_category %in% c("Small", "Big", "Medium"))

# Create proportional bar plot
ggplot(size_proportions, aes(x = size_category, y = ProportionClaiming, fill = size_category)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1) +
  labs(title = "Proportion of Dogs Claiming by Weight Category",
       x = "Size Category", 
       y = "Proportion Claiming") +
  scale_fill_manual(values = c("#49A5F1",
                               "#E6830C",
                               "#C859EB")) +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

# Create average claim severity bar plot
ggplot(size_proportions, aes(x = size_category, y = AvgClaimPaid, fill = size_category)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1) +
  scale_fill_manual(values = c("#49A5F1",
                               "#E6830C",
                               "#C859EB")) +
  labs(title = "Average Claim Severity by Dog Weight Category",
       x = "Size Category", 
       y = "Average Claim Paid") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

rm(size_proportions)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 3.8. Frequency-Severity Covariation ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Extracting Frequency and Severity
sev = EARNED_FULL_DF %>% filter(claim_paid > 0) %>% pull(claim_paid)
freq = EARNED_FULL_DF %>% 
  filter(claim_paid > 0) %>% 
  mutate(freq_per_exposure = claim_count / earned_units) %>% 
  pull(freq_per_exposure)

## Running Correlation Test
cor.test(sev, freq, method = "spearman")

## Visualisation
ggplot(EARNED_FULL_DF %>% filter(claim_paid > 0), aes(x = claim_count / earned_units, y = claim_paid)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 1), aes(colour = "Linear Interpolation"), se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), aes(colour = "Cubic Interpolation"), se = FALSE) +
  ylim(0, 2000) + 
  labs(title = "Relationship between Claim Count and Claim Severity",
       x = "Claim Frequency", y = "Claim Severity") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Decluttering Environment
rm(sev, freq)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# 4. Data Transformation ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 4.1. Transformation ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## Renaming and Reordering Columns
EARNED_FULL_DF <- EARNED_FULL_DF %>%
  rename(lga_name = LGA_NAME24, lga_code = LGA_CODE24, lga_quintile = LGA_QUINTILE, seifa_score = SCORE) %>%
  rename_with(~ sub("^nb_", "", .), starts_with("nb_")) %>%
  rename_with(~ gsub("_2024$", "", .), ends_with("_2024")) %>%
  rename_with(~ gsub("_2016$", "", .), ends_with("_2016")) %>%
  dplyr::select(-earned_units, -claim_count, -claim_paid, -total_claim_amount, everything(), 
                earned_units, claim_count, claim_paid, total_claim_amount)


EARNED_FULL_DF <- EARNED_FULL_DF %>% 
  mutate(household_ideal = interaction(avg_household_size > 3,
                                       own_property > 0.75,
                                       avg_cars_per_household > 2, 
                                       family_household_proportion > 0.5), # Household ideal profile
         family_ideal = interaction(owner_age_years < 60 & owner_age_years > 30,
                                    number_of_breeds == 1,
                                    married > 0.5,
                                    avg_children_per_family > 5), # Family ideal profile
  )

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  mutate(gender_de_sex = case_when(
    pet_gender == "male" & pet_de_sexed == TRUE ~ "MD",    # Male Desexed
    pet_gender == "male" & pet_de_sexed == FALSE ~ "MND",  # Male Not Desexed
    pet_gender == "female" & pet_de_sexed == TRUE ~ "FD",  # Female Desexed
    pet_gender == "female" & pet_de_sexed == FALSE ~ "FND" # Female Not Desexed
  ))

# also apply to PRICING_FULL_DF
PRICING_FULL_DF <- PRICING_FULL_DF %>%
  rename(lga_name = LGA_NAME24, lga_code = LGA_CODE24, lga_quintile = LGA_QUINTILE, seifa_score = SCORE) %>%
  rename_with(~ sub("^nb_", "", .), starts_with("nb_")) %>%
  rename_with(~ gsub("_2024$", "", .), ends_with("_2024")) %>%
  rename_with(~ gsub("_2016$", "", .), ends_with("_2016"))


PRICING_FULL_DF <- PRICING_FULL_DF %>% 
  mutate(household_ideal = interaction(avg_household_size > 3,
                                       own_property > 0.75,
                                       avg_cars_per_household > 2, 
                                       family_household_proportion > 0.5), # Household ideal profile
         family_ideal = interaction(owner_age_years < 60 & owner_age_years > 30,
                                    number_of_breeds == 1,
                                    married > 0.5,
                                    avg_children_per_family > 5), # Family ideal profile
  )

PRICING_FULL_DF <- PRICING_FULL_DF %>%
  mutate(gender_de_sex = case_when(
    pet_gender == "male" & pet_de_sexed == TRUE ~ "MD",    # Male Desexed
    pet_gender == "male" & pet_de_sexed == FALSE ~ "MND",  # Male Not Desexed
    pet_gender == "female" & pet_de_sexed == TRUE ~ "FD",  # Female Desexed
    pet_gender == "female" & pet_de_sexed == FALSE ~ "FND" # Female Not Desexed
  )) 

## Owner-Pet Age Difference
dog_to_human_years <- function(age, size) {
  # Map the continuous scale to breed size categories
  if (size <= 2) {
    size_category <- 'small'
  } else if (size <= 3) {
    size_category <- 'medium'
  } else {
    size_category <- 'large'
  }
  
  # Calculate human years based on the mapped size category
  age <- floor(age/12)
  if (age <= 0) {
    return(0)
  } else if (age == 1) {
    return(15)
  } else if (age == 2) {
    return(24)  # 15 + 9
  } else {
    if (size_category == 'small') {
      return(24 + (age - 2) * 4)
    } else if (size_category == 'large') {
      return(24 + (age - 2) * 6)
    } else {
      return(24 + (age - 2) * 5)  # medium breed
    }
  }
}

EARNED_FULL_DF$pet_age_human_years <- mapply(dog_to_human_years, EARNED_FULL_DF$pet_age_months, EARNED_FULL_DF$average_breed_size)

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  mutate(age_difference = (owner_age_years - pet_age_human_years) / owner_age_years)

PRICING_FULL_DF$pet_age_human_years <- mapply(dog_to_human_years, PRICING_FULL_DF$pet_age_months, PRICING_FULL_DF$average_breed_size)

PRICING_FULL_DF <- PRICING_FULL_DF %>%
  mutate(age_difference = (owner_age_years - pet_age_human_years) / owner_age_years)

## Age_difference - Frequency
EARNED_FULL_DF %>%
  group_by(age_difference) %>%
  summarise(
    ClaimCount = sum(claim_count, na.rm = TRUE), 
    Exposure = sum(earned_units, na.rm = TRUE)) %>%
  mutate(Frequency = log(ClaimCount / Exposure)) %>%
  filter(Exposure > 0) %>%
  ggplot(aes(x = age_difference, y = Frequency)) +
  geom_point(alpha = 0.6, color = "#C859EB") +
  geom_smooth(method = "loess", color = "blue") +
  xlim(-1, 1) +
  labs(title = "Claim Frequency by Owner-Pet Age Difference", x = "Age Difference (human years)", y = "Claim Frequency (log scale)") +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "DM Sans", size = 12))

## Age_difference- Severity
EARNED_FULL_DF %>%
  group_by(age_difference) %>%
  summarise(ClaimPaid = sum(claim_paid*claim_count),
            Exposure = sum(earned_units, na.rm = TRUE)) %>%
  mutate(per_claim = log(ClaimPaid/Exposure)) %>%
  filter(Exposure > 0) %>%
  ggplot(aes(x = age_difference, y = per_claim)) +
  geom_point(alpha = 0.6, color = "#C859EB") +
  geom_smooth(method = "loess", color = "blue") +
  xlim(-1,1) +
  labs(title = "Claim Severity by Owner-Pet Age Difference", x = "Age Difference (human years)", y = "Average Claim Severity (log scale)") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

## Month (Seasonality)
EARNED_FULL_DF$quote_month <- format(EARNED_FULL_DF$quote_date, "%m")
PRICING_FULL_DF$quote_month <- format(PRICING_FULL_DF$quote_date, "%m")

## Decluttering Environment 
rm(dog_to_human_years)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 4.2. Dimension Reduction ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## High-risk Breeds (Clustering on Breeds)
breed_means <- aggregate(cbind(claim_count / earned_units, claim_paid) ~ breed_name_unique, 
                         data = EARNED_FULL_DF, mean)

wcss_values <- sapply(1:15, function(k) kmeans(breed_means[, -1], centers = k)$tot.withinss)

data.frame(clusters = 1:15, wcss_values = wcss_values) %>%
  ggplot(aes(x = clusters, y = wcss_values)) +
  geom_line() +          # Line plot
  geom_point() +         # Points
  labs(x = 'Number of Clusters', y = 'Within-Cluster Sum of Squares', 
       title = "Hyperparameter (k) Tuning Using Elbow Method") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

kmeans_result <- kmeans(breed_means[, -1], centers = 5)
breed_means$cluster <- as.factor(kmeans_result$cluster)

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  left_join(breed_means[, c("breed_name_unique", "cluster")], by = "breed_name_unique") 

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  rename(breed_group = cluster)

# also apply to PRICING_FULL_DF
PRICING_FULL_DF <- PRICING_FULL_DF %>%
  left_join(breed_means[, c("breed_name_unique", "cluster")], by = "breed_name_unique") 

PRICING_FULL_DF <- PRICING_FULL_DF %>%
  rename(breed_group = cluster)

# Scatter plot to visualize clusters
EARNED_FULL_DF %>% 
  ggplot(aes(x = claim_count / earned_units, y = claim_paid)) +
  geom_point(aes(color = breed_group, shape = breed_group), size = 1.5, alpha = 0.7) +
  labs(
    title = "K-Means Clustering of Claim Frequency and Severity",
    x = "Claim Frequency",
    y = "Claim Severity",
    color = "Breed Group",
    shape = "Breed Group"
  ) +
  facet_wrap(~ breed_group, ncol = 2) +
  scale_color_manual(values = c("#49A5F1", "#E6830C", "#C859EB", "#FFD300", "#EF5493")) +
  scale_shape_manual(values = c(16, 17, 15, 18, 19)) + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

kmeans_result$centers

## High-risk Breeds Unique Concat (Clustering on Breeds)
breed_means <- aggregate(cbind(claim_count / earned_units, claim_paid) ~ breed_name_unique_concat, 
                         data = EARNED_FULL_DF, mean)

wcss_values <- sapply(1:15, function(k) kmeans(breed_means[, -1], centers = k)$tot.withinss)

data.frame(clusters = 1:15, wcss_values = wcss_values) %>%
  ggplot(aes(x = clusters, y = wcss_values)) +
  geom_line() +          # Line plot
  geom_point() +         # Points
  labs(x = 'Number of Clusters', y = 'Within-Cluster Sum of Squares', 
       title = "Hyperparameter (k) Tuning Using Elbow Method") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

kmeans_result <- kmeans(breed_means[, -1], centers = 11)
breed_means$cluster <- as.factor(kmeans_result$cluster)

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  left_join(breed_means[, c("breed_name_unique_concat", "cluster")], by = "breed_name_unique_concat") 

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  rename(breed_concat_group = cluster)

# also apply to PRICING_FULL_DF
PRICING_FULL_DF <- PRICING_FULL_DF %>%
  left_join(breed_means[, c("breed_name_unique_concat", "cluster")], by = "breed_name_unique_concat") 

PRICING_FULL_DF <- PRICING_FULL_DF %>%
  rename(breed_concat_group = cluster)

kmeans_result$centers

## High-risk LGA's (Clustering on LGA)
breed_means <- aggregate(cbind(claim_count / earned_units, claim_paid) ~ lga_code, 
                         data = EARNED_FULL_DF, mean)

wcss_values <- sapply(1:15, function(k) kmeans(breed_means[, -1], centers = k)$tot.withinss)

data.frame(clusters = 1:15, wcss_values = wcss_values) %>%
  ggplot(aes(x = clusters, y = wcss_values)) +
  geom_line() +          # Line plot
  geom_point() +         # Points
  labs(x = 'Number of Clusters', y = 'Within-Cluster Sum of Squares', 
       title = "Hyperparameter (k) Tuning Using Elbow Method") + 
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

set.seed(4305)
kmeans_result <- kmeans(breed_means[, -1], centers = 8)
breed_means$cluster <- as.factor(kmeans_result$cluster)


cluster_sizes <- table(kmeans_result$cluster)
small_clusters <- which(cluster_sizes < 5)

euclidean_distance <- function(x, y) {
  sqrt(sum((x - y) ^ 2))
}


centroids <- kmeans_result$centers

for (small_cluster in small_clusters) {
  small_centroid <- centroids[small_cluster, ]
  
  distances <- sapply(1:nrow(centroids), function(i) {
    if (i != small_cluster) {
      euclidean_distance(small_centroid, centroids[i, ])
    } else {
      Inf
    }
  })
  
  closest_cluster <- which.min(distances)
  
  breed_means$cluster[breed_means$cluster == small_cluster] <- closest_cluster
}

# Merge cluster 
EARNED_FULL_DF <- EARNED_FULL_DF %>%
  left_join(breed_means[, c("lga_code", "cluster")], by = "lga_code")

EARNED_FULL_DF <- EARNED_FULL_DF %>%
  rename(lga_group = cluster)

# also apply to PRICING_FULL_DF
PRICING_FULL_DF <- PRICING_FULL_DF %>%
  left_join(breed_means[, c("lga_code", "cluster")], by = "lga_code") 

PRICING_FULL_DF <- PRICING_FULL_DF %>%
  rename(lga_group = cluster)

# Scatter plot to visualize clusters
EARNED_FULL_DF %>% 
  ggplot(aes(x = claim_count / earned_units, y = claim_paid)) +
  geom_point(aes(color = lga_group, shape = lga_group), size = 1.5, alpha = 0.7) +
  labs(
    title = "K-Means Clustering of Claim Frequency and Severity",
    x = "Claim Frequency",
    y = "Claim Severity",
    color = "LGA Group",
    shape = "LGA Group"
  ) +
  scale_color_manual(values = c("#49A5F1", "#E6830C", "#C859EB", "#FFD300", "#EF5493", "#50C")) +
  scale_shape_manual(values = c(16, 17, 15, 18, 19, 20)) + 
  facet_wrap(~ lga_group, ncol = 2) +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

kmeans_result$centers

#Slide charts - Owner Risk
#LGA GROUP
#Plotting LGA groups on map
lga_clusters_summary <- EARNED_FULL_DF %>%
  group_by(lga_group) %>%
  summarize(lgas = list(lga_code))

lga_clusters_expanded <- lga_clusters_summary %>%
  unnest(lgas) %>%
  distinct(lgas, lga_group)

LGA_SHAPES <- read_sf("Raw External Data/Shapefiles/LGA_2024_AUST_GDA2020.shp") 

aus_lga_clustered <- LGA_SHAPES %>%
  left_join(lga_clusters_expanded, by = c("LGA_CODE24" = "lgas"))

# Plot the map with different colors for each LGA group
#ggplot(aus_lga_clustered) +
#  geom_sf(aes(fill = lga_group), color = "grey30", size = 0.2) +
#  scale_color_manual(values = c("red", "blue", "green", "purple", "orange","#EF5493"), na.value = "grey80")+ # Choose palette
#  labs(title = "Australian LGAs by Cluster Group",
#       fill = "LGA Group") +
#  theme(plot.title=element_text(hjust=0.5, size=16),
#        text=element_text(family="DM Sans", size=12))

#DWELLING PRICE
dwelling_plot <- EARNED_FULL_DF %>% 
  dplyr::select(exposure_id, median_dwelling_price_lga, address_type_adj, claim_paid, claim_count, earned_units) %>% 
  mutate(median_dwelling_price_lga = as.numeric(as.character(median_dwelling_price_lga)))

ggplot(dwelling_plot, aes(x = address_type_adj, y = median_dwelling_price_lga)) +
  geom_boxplot(fill = "#49A5F1", colour = "black", alpha = 0.7) +  
  labs(title = "Box Plot of Median Dwelling Price by Address Type", 
       x = "Address Type", 
       y = "Median Dwelling Price") +
  theme(plot.title=element_text(hjust=0.5, size=16),
        axis.line=element_line(linewidth=1, colour="grey80"),
        text=element_text(family="DM Sans", size=12))

dwelling_plot_freq <- dwelling_plot %>%
  group_by(median_dwelling_price_lga) %>%
  summarise(freq = sum(claim_count, na.rm = TRUE) / sum(earned_units, na.rm = TRUE)) %>%
  na.omit() 

custom_breaks <- seq(0,4500000, 100000)

ggplot(dwelling_plot_freq, aes(x = factor(median_dwelling_price_lga), y = freq)) +  
  geom_bar(stat = "identity", fill = "#C859EB", width = 1) +
  labs(
    title = "Claim Frequency by Median Dwelling Price",
    x = "Median Dwelling Price",
    y = "Claim Frequency"
  ) +
  scale_x_discrete(breaks = custom_breaks, labels = custom_breaks) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text=element_text(family="DM Sans", size=12))  

dwelling_plot_sev <- dwelling_plot %>%
  group_by(median_dwelling_price_lga) %>%
  summarise(sev = sum(claim_count*claim_paid, na.rm = TRUE) / sum(earned_units, na.rm = TRUE)) %>%
  na.omit() 

ggplot(dwelling_plot_sev, aes(x = factor(median_dwelling_price_lga), y = sev)) +  
  geom_bar(stat = "identity", fill = "#C859EB", width = 1) +
  labs(
    title = "Claim Severity by Median Dwelling Price",
    x = "Median Dwelling Price",
    y = "Claim Severity"
  ) +
  scale_x_discrete(breaks = custom_breaks, labels = custom_breaks) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text=element_text(family="DM Sans", size=12))  

#Income
income_plot <- EARNED_FULL_DF %>% 
  dplyr::select(exposure_id, income_by_lga, income_by_age, weighted_income, claim_paid, claim_count, earned_units) 

income_freq <- income_plot %>%
  group_by(income_by_lga) %>%
  summarise(freq = sum(claim_count, na.rm = TRUE) / sum(earned_units, na.rm = TRUE)) %>%
  na.omit() 

custom_breaks <-c(min(income_freq$income_by_lga), median(income_freq$income_by_lga), max(income_freq$income_by_lga))

ggplot(income_freq, aes(x = factor(income_by_lga), y = freq)) +  
  geom_bar(stat = "identity", fill = "#49A5F1", width = 1) +
  labs(
    title = "Claim Frequency by Income",
    x = "Income",
    y = "Claim Frequency"
  ) +
  scale_x_discrete(breaks = custom_breaks, labels = custom_breaks) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text=element_text(family="DM Sans", size=12))   

income_sev <- income_plot %>%
  group_by(income_by_lga) %>%
  summarise(sev = sum(claim_count*claim_paid, na.rm = TRUE) / sum(earned_units, na.rm = TRUE)) %>%
  na.omit() 

ggplot(income_sev, aes(x = factor(income_by_lga), y = sev)) +  
  geom_bar(stat = "identity", fill = "#49A5F1", width = 1) +
  labs(
    title = "Claim Severity by Income",
    x = "Income",
    y = "Claim Severity"
  ) +
  scale_x_discrete(breaks = custom_breaks, labels = custom_breaks) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text=element_text(family="DM Sans", size=12))   

INCOME_LGA <- read_excel("Raw External Data/ABS - Socioeconomic/INCOME_SA2-4.xlsx", 
                         sheet = 3, 
                         range = "A7:BP4383")

INCOME_LGA <- INCOME_LGA %>%
  dplyr::select('Code', 'Year', 'Mean employee income ($)') %>%
  rename(LGA_CODE24 = Code,
         income_by_lga = `Mean employee income ($)`) %>%
  mutate(income_by_lga = na_if(income_by_lga, "-")) %>%
  filter(!is.na(income_by_lga)) %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(LGA_CODE24) %>%
  filter(Year == max(Year)) %>%
  dplyr::select(LGA_CODE24, income_by_lga) %>%
  ungroup()

INCOME_LGA <- INCOME_LGA %>%
  mutate(LGA_CODE24 = as.character(LGA_CODE24))

income_lga_mapped <- LGA_SHAPES %>%
  left_join(INCOME_LGA, by = "LGA_CODE24") %>% 
  mutate(income_by_lga = as.numeric(income_by_lga))

#ggplot(income_lga_mapped) +
#  geom_sf(aes(fill = income_by_lga)) + 
#  scale_fill_viridis_c(option = "magma", name = "Income by LGA") +  
#  labs(
#    title = "Income by Local Government Area (LGA) in Australia",
#    subtitle = "Each LGA shaded by income level"
#  ) +
#  theme_minimal() 

#Ideal household/family
family <- EARNED_FULL_DF %>%
  group_by(family_ideal) %>%
  summarise(frequency = sum(claim_count)/sum(earned_units),
            severity = sum(claim_paid*claim_count)/sum(earned_units))

ggplot(family, aes(x = family_ideal)) +
  geom_bar(aes(y = frequency), stat = "identity", fill = "#EF5493") +
  geom_line(aes(y = severity / 300, group = 1), color = "purple", size = 1) +  
  geom_point(aes(y = severity / 300), color = "purple", size = 1) +            
  scale_y_continuous(
    name = "Frequency",
    limits = c(0, 0.15),                     
    sec.axis = sec_axis(~.*300, name = "Severity", breaks = seq(0, 300, 10))  
  ) +
  labs(x = "Family Ideal", title = "Claim Frequency and Severity by Family Ideal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text=element_text(family="DM Sans", size=10))  

household <- EARNED_FULL_DF %>%
  group_by(household_ideal) %>%
  summarise(frequency = sum(claim_count)/sum(earned_units),
            severity = sum(claim_paid*claim_count)/sum(earned_units))

ggplot(household, aes(x = household_ideal)) +
  geom_bar(aes(y = frequency), stat = "identity", fill = "#EF5493") +
  geom_line(aes(y = severity / 300, group = 1), color = "purple", size = 1) +  
  geom_point(aes(y = severity / 300), color = "purple", size = 1) +           
  scale_y_continuous(
    name = "Frequency",
    limits = c(0, 0.15),                      
    sec.axis = sec_axis(~.*300, name = "Severity", breaks = seq(0, 300, 10)) 
  ) +
  labs(x = "Household Ideal", title = "Claim Frequency and Severity by Household Ideal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text=element_text(family="DM Sans", size=10))  

## Decluttering Environment 
rm(breed_means, kmeans_result, wcss_values, dwelling_plot, dwelling_plot_freq, dwelling_plot_sev, income_freq,
   income_lga_mapped, income_plot, income_sev, lga_clusters_expanded, lga_clusters_summary, LGA_SHAPES, POA_SHAPES, 
   PROTECTED_AREAS)

### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
## 4.3. NA Cleaning ----
### == == == == == == == == == == == == == == == == == == == == == == == == == == == == ==
### Imputing breed_group
train <- EARNED_FULL_DF %>% filter(!is.na(breed_group))
test <- PRICING_FULL_DF %>% filter(is.na(breed_group))

train <- train %>%
  mutate(across(c(gender_de_sex, lga_name, lga_code, lga_quintile), as.factor))

y <- train$breed_group
x <- model.matrix(breed_group ~ pet_de_sexed_age + pet_is_switcher + pet_age_months + contribution + 
                    excess + address_type_adj + state + pet_age_years + owner_age_years + number_of_breeds + 
                    average_breed_size + breed_type + breed_trait + is_multi_pet_plan + quote_time_group + 
                    gender_de_sex + lga_quintile + seifa_score + 
                    population_density + unemployment + internet_access_proportion + 
                    travelled_to_work_by_car + worked_from_home + avg_household_size + married + 
                    avg_children_per_family + avg_cars_per_household + own_property + rent_property + 
                    family_household_proportion, 
                  data = train)[, -1] 
x_test <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + contribution + 
                         excess + address_type_adj + state + pet_age_years + owner_age_years + number_of_breeds + 
                         average_breed_size + breed_type + breed_trait + is_multi_pet_plan + quote_time_group + 
                         gender_de_sex + lga_quintile + seifa_score + 
                         population_density + unemployment + internet_access_proportion + 
                         travelled_to_work_by_car + worked_from_home + avg_household_size + married + 
                         avg_children_per_family + avg_cars_per_household + own_property + rent_property + 
                         family_household_proportion, 
                       data = test)[, -1]

## xgboost
dtrain <- xgb.DMatrix(data = x, label = y)
dtest <- xgb.DMatrix(data = x_test)

params <- list(objective = "reg:squarederror",
               booster = "gbtree",  # Use trees
               eta = 0.1,  # Learning rate
               max_depth = 6,  # Tree depth
               subsample = 0.8,  # Row subsampling
               colsample_bytree = 0.8,  # Feature subsampling
               alpha = 0.1,  # L1 regularization (Lasso)
               lambda = 1)  # L2 regularization (Ridge)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, 
                       watchlist = list(train = dtrain), verbose = 0)
xgb_predictions <- predict(xgb_model, newdata = dtest)
predicted_levels <- factor(round(xgb_predictions), levels = levels(train$breed_group))
PRICING_FULL_DF$breed_group[is.na(PRICING_FULL_DF$breed_group)] <- predicted_levels

### Imputing breed_concat_group
train <- EARNED_FULL_DF %>% filter(!is.na(breed_concat_group))
test <- PRICING_FULL_DF %>% filter(is.na(breed_concat_group))

train <- train %>%
  mutate(across(c(gender_de_sex, lga_name, lga_code, lga_quintile), as.factor))

y <- train$breed_concat_group
x <- model.matrix(breed_concat_group ~ pet_de_sexed_age + pet_is_switcher + pet_age_months + contribution + 
                    excess + address_type_adj + state + pet_age_years + owner_age_years + number_of_breeds + 
                    average_breed_size + breed_type + breed_trait + is_multi_pet_plan + quote_time_group + 
                    gender_de_sex + lga_quintile + seifa_score + 
                    population_density + unemployment + internet_access_proportion + 
                    travelled_to_work_by_car + worked_from_home + avg_household_size + married + 
                    avg_children_per_family + avg_cars_per_household + own_property + rent_property + 
                    family_household_proportion, 
                  data = train)[, -1] 
x_test <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + contribution + 
                         excess + address_type_adj + state + pet_age_years + owner_age_years + number_of_breeds + 
                         average_breed_size + breed_type + breed_trait + is_multi_pet_plan + quote_time_group + 
                         gender_de_sex + lga_quintile + seifa_score + 
                         population_density + unemployment + internet_access_proportion + 
                         travelled_to_work_by_car + worked_from_home + avg_household_size + married + 
                         avg_children_per_family + avg_cars_per_household + own_property + rent_property + 
                         family_household_proportion, 
                       data = test)[, -1]

## xgboost
dtrain <- xgb.DMatrix(data = x, label = y)
dtest <- xgb.DMatrix(data = x_test)

params <- list(objective = "reg:squarederror",
               booster = "gbtree",  # Use trees
               eta = 0.1,  # Learning rate
               max_depth = 6,  # Tree depth
               subsample = 0.8,  # Row subsampling
               colsample_bytree = 0.8,  # Feature subsampling
               alpha = 0.1,  # L1 regularization (Lasso)
               lambda = 1)  # L2 regularization (Ridge)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, 
                       watchlist = list(train = dtrain), verbose = 0)
xgb_predictions <- predict(xgb_model, newdata = dtest)
predicted_levels <- factor(round(xgb_predictions), levels = levels(train$breed_concat_group))
PRICING_FULL_DF$breed_concat_group[is.na(PRICING_FULL_DF$breed_concat_group)] <- predicted_levels

### Imputing lga_group
train <- EARNED_FULL_DF %>% filter(!is.na(lga_group))
test <- PRICING_FULL_DF %>% filter(is.na(lga_group))

train <- train %>%
  mutate(across(c(gender_de_sex, lga_name, lga_code, lga_quintile), as.factor))

y <- train$lga_group
x <- model.matrix(lga_group ~ pet_de_sexed_age + pet_is_switcher + pet_age_months + contribution + 
                    excess + address_type_adj + state + pet_age_years + owner_age_years + number_of_breeds + 
                    average_breed_size + breed_type + breed_trait + is_multi_pet_plan + quote_time_group + 
                    gender_de_sex + lga_quintile + seifa_score + 
                    population_density + unemployment + internet_access_proportion + 
                    travelled_to_work_by_car + worked_from_home + avg_household_size + married + 
                    avg_children_per_family + avg_cars_per_household + own_property + rent_property + 
                    family_household_proportion, 
                  data = train)[, -1] 
x_test <- model.matrix(~ pet_de_sexed_age + pet_is_switcher + pet_age_months + contribution + 
                         excess + address_type_adj + state + pet_age_years + owner_age_years + number_of_breeds + 
                         average_breed_size + breed_type + breed_trait + is_multi_pet_plan + quote_time_group + 
                         gender_de_sex + lga_quintile + seifa_score + 
                         population_density + unemployment + internet_access_proportion + 
                         travelled_to_work_by_car + worked_from_home + avg_household_size + married + 
                         avg_children_per_family + avg_cars_per_household + own_property + rent_property + 
                         family_household_proportion, 
                       data = test)[, -1]

## xgboost
dtrain <- xgb.DMatrix(data = x, label = y)
dtest <- xgb.DMatrix(data = x_test)

params <- list(objective = "reg:squarederror",
               booster = "gbtree",  # Use trees
               eta = 0.1,  # Learning rate
               max_depth = 6,  # Tree depth
               subsample = 0.8,  # Row subsampling
               colsample_bytree = 0.8,  # Feature subsampling
               alpha = 0.1,  # L1 regularization (Lasso)
               lambda = 1)  # L2 regularization (Ridge)

xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, 
                       watchlist = list(train = dtrain), verbose = 0)
xgb_predictions <- predict(xgb_model, newdata = dtest)
predicted_levels <- factor(round(xgb_predictions), levels = levels(train$lga_group))
PRICING_FULL_DF$lga_group[is.na(PRICING_FULL_DF$lga_group)] <- predicted_levels

rm(train, test, y, x, x_test, dtrain, dtest, params, xgb_model, xgb_predictions, predicted_levels)


## Exporting Data for Model Development.r 
write.csv(EARNED_FULL_DF, "EARNED_FULL_DF.csv")
write.csv(PRICING_FULL_DF, "PRICING_DF.csv")


########################################################################################################################
######################################## Fluffsure Pet Insurance Pricing Model #########################################
############################################## Exploratory Data Analysis ############################################### 
########################################################################################################################