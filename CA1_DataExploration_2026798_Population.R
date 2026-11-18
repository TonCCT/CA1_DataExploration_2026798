
# Github link: https://github.com/TonCCT/CA1_DataExploration_2026798.git

# Provides population data for calculation of rates, and to describe the demographic distribution of the population, 
# for CDPH, other CalHHS departments, Local Health Jurisdictions.

install.packages(c("ggplot2", "rsample", "recipes", "themis")) # Run once if packages are missing

library(ggplot2)                                               # Create visualisations
library(rsample)                                               # Split data into training and testing
library(recipes)                                               # Perform data preparation
library(themis)                                                # Apply SMOTE and NearMiss sampling
library(gridExtra)


getwd()
setwd("C:/Users/Yamit/Documents/Assignment/Computer 4Year/Data Exploration and Preparation/Data")

pop_data <- read.csv("2000-2030-lhj-age-race-sex-population-file-vintage-2026-2-released-on-04-08-2026.csv")
pop_data

# SHOW TOTAL OF ROWS AND COLUMNS
dim(pop_data)

# FIND MISSING VALUES AND DUPLICATES

colSums(is.na(pop_data))                                             # Count missing values in each column
sum(duplicated(pop_data))                                            # Count exact duplicate passenger records

head(pop_data)

# SHOWING DATASET STRUCTURE 

table(select(pop_data,county_lhj))
pop_data %>% select(county_lhj) %>% table() %>% prop.table()

table(select(pop_data,sex))

table(select(pop_data,race_eth))

table(select(pop_data,population))

table(select(pop_data,county_indicator))


# DISPLAYING THE INTERNAL STRUCTURE
str(pop_data)




