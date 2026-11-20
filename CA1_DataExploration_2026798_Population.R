
# Github link: https://github.com/TonCCT/CA1_DataExploration_2026798.git

# To investigate how California's population age structure and racial/ethnic composition have 
# changed between 2000 and 2030, identifying differences in racial/ethnic groups and people's age 
# patterns across demographic groups and local health jurisdictions.

install.packages(c("ggplot2", "DataExplorer", "skimr","naniar")) 

library(ggplot2)                                               # Create visualisations
library(recipes)                                               # Perform data preparation
#library(gridExtra)
library(DataExplorer)
library(tidyverse)
library(janitor)
library(skimr)
library(naniar)

# =====================================================
# INITIAL DATA EXPLORATION
# =====================================================

getwd()
setwd("C:/Users/Yamit/Documents/Assignment/Computer 4Year/Data Exploration and Preparation/Data")

pop_data <- read.csv("2000-2030-lhj-age-race-sex-population-file-vintage-2026-2-released-on-04-08-2026.csv")
pop_data

# Show number of rows and columns
dim(pop_data)

# Missing value

colSums(is.na(pop_data))                                             # Count missing values in each column
plot_missing(pop_data)
sum(duplicated(pop_data))                                            # Count exact duplicate passenger records

head(pop_data)

# Columns name

names(pop_data)

# Explore number of records

table(pop_data$county_lhj)
table(pop_data$sex)
table(pop_data$race_eth)
table(pop_data$county_indicator)

# Understandin data type

str(pop_data)

# Visualizing the description of the data

plot_intro(pop_data)

# summary statistics

summary(pop_data)

# Detailed exploration

skim(pop_data)

# checking the number fo unique value

sapply(pop_data, function (x)
  {length(unique(x))})

# Selecting numerical columns

numeric_data <- pop_data %>%
  select(where(is.numeric))

# Showing numerical variables

names(numeric_data)

# Correlation plot (initial inspection)

plot_correlation(pop_data)

# Calculate to identify the population value below which approximately 25%.
Q1 <- quantile(pop_data$population, 0.25, na.rm = TRUE)
Q1

# Calculate to identify the population value below which approximately 75%.
Q3 <- quantile(pop_data$population, 0.75, na.rm = TRUE)
Q3

# Calculate the interquartile range (IQR) to measure the spread of the middle 50% of population
IQR_value <- IQR(pop_data$population, na.rm = TRUE)
IQR_value

# Calculate the lower and upper limits to identify population values that are unusually.
lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value

lower_limit
upper_limit

# Count the population records that exceed the upper limit to identify potential high outliers.
sum(pop_data$population > upper_limit, na.rm = TRUE)



sum(pop_data$population < lower_limit, na.rm = TRUE)


# Identify population records outside the IQR limits to investigate potential low or high outliers.
potential_outliers <- pop_data %>%
  filter(population < lower_limit | population > upper_limit)
potential_outliers

# Display the first 10 potential outliers to inspect their geographical and demographic information.
head(potential_outliers, 10)

#A boxplot gives you a visual way to inspect the distribution
ggplot(pop_data, aes(y = population)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Population Values",
    y = "Population",
    x = ""
  ) +
  theme_minimal()

#This shows which areas, years, sexes, ages and race groups have been flagged.
potential_outliers %>%
  select(county_lhj, year, sex, age, race_eth, population) %>%
  arrange(desc(population)) %>%
  head(20)

























