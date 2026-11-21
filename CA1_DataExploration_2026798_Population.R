
# Github link: https://github.com/TonCCT/CA1_DataExploration_2026798.git

# =====================================================
# POPULATION AND RACE CHANGES IN CALIFORNIA (2000-2030)
# DATA EXPLORATION AND PREPARATION
# =====================================================

# To investigate how California's population age structure and racial/ethnic composition have 
# changed between 2000 and 2030, identifying differences in racial/ethnic groups and people's age 
# patterns across demographic groups and local health jurisdictions.



install.packages(c("ggplot2", "DataExplorer", "skimr","naniar")) 

library(ggplot2)                                              
library(DataExplorer)
library(tidyverse)
library(janitor)
library(skimr)
library(naniar)
library(scales)

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


# Count the population records that exceed the lower limit to identify potential lower outliers.
sum(pop_data$population < lower_limit, na.rm = TRUE)


# Identify population records outside the IQR limits to investigate potential low or high outliers.
potential_outliers <- pop_data %>%
  filter(population < lower_limit | population > upper_limit)
potential_outliers

# Display the first 10 potential outliers to inspect their geographical and demographic information.
head(potential_outliers, 10)

#This shows which areas, years, sexes, ages and race groups have been flagged.
potential_outliers %>%
  select(county_lhj, year, sex, age, race_eth, population) %>%
  arrange(desc(population)) %>%
  head(20)

# Compare population distributions across racial/ethnic groups using a logarithmic scale to make differences and potential outliers easier to see.

ggplot(pop_data, aes(x = race_eth, y = log10(population + 1))) +
  geom_boxplot(fill= "purple",outlier.alpha = 0.3) +
  labs(
    title = "Population Distribution by Racial/Ethnic Group",
    x = "Racial/Ethnic Group",
    y = "Population"
  ) +
  theme_minimal()

# Display the frequency distribution of population values using a logarithmic transformation.
ggplot(pop_data, aes(x = log10(population + 1))) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Population Values",
    x = "Population",
    y = "Number of Records"
  ) +
  theme_minimal()

# =====================================================
# PREPARING THE DATA
# =====================================================

# How has the population of California changed between 2000 and 2030 in terms of age and race

# Select county records
county_data <- pop_data %>%
  filter(county_indicator == 1)

# Group ages into children, working-age adults and older adults.
county_data <- county_data %>%
  mutate(
    age_group = case_when(
      age <= 14 ~ "Children (0-14)",
      age <= 64 ~ "Working-age adults (15-64)",
      age >= 65 ~ "Older adults (65+)"
    )
  )
county_data %>% ggplot() +
  geom_bar(mapping = aes(x = age_group, fill = age_group), color = "orange") +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Group ages Children , adults and olders", x = "Population",
       y = "Group by ages") +
  coord_flip() + theme_minimal() + theme(text = element_text(size = 12))

# =====================================================
# POPULATION CHANGES BY AGE GROUP
# How the three age groups change over time, helps plan services for different age groups.
# =====================================================

# Calculate the total population for each age group and year.
age_population <- county_data %>%
  group_by(year, age_group) %>%
  summarise(total_population = sum(population), .groups = "drop")
age_population

# Create a line graph to compare population changes between age groups.
ggplot(age_population, aes(x = year, y = total_population, colour = age_group)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Population Changes by Age Group (2000-2030)",
    x = "Year",
    y = "Population",
    colour = "Age Group"
  ) +
  theme_minimal()

# =====================================================
# POPULATION CHANGES BY RACE AND ETHNICITY
# How the different groups change over time. Helps understand the communities served by health services.
# =====================================================

# Calculate the total population for each racial and ethnic group and year.
race_population <- county_data %>%
  group_by(year, race_eth) %>%
  summarise(total_population = sum(population), .groups = "drop")
race_population

# Create a line graph to compare population changes between racial and ethnic groups.
ggplot(race_population, aes(x = year, y = total_population, colour = race_eth)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = label_comma()) +
    labs(
    title = "Population Changes by Racial and Ethnic Group (2000-2030)",
    x = "Year",
    y = "Population",
    colour = "Racial/Ethnic Group"
  ) +
  theme_minimal()

# =====================================================
# GE DISTRIBUTION BY RACE AND ETHNICITY
# Differences in age structure between groups. Helps identify different population needs.
# =====================================================

# Select 2030 to compare the projected age distribution between racial and ethnic groups.
age_race <- county_data %>%
  filter(year == 2030) %>%
  group_by(race_eth, age_group) %>%
  summarise(total_population = sum(population), .groups = "drop")
age_race

# Calculate the percentage of each age group within each racial and ethnic group.
age_race <- age_race %>%
  group_by(race_eth) %>%
  mutate(percentage = total_population / sum(total_population) * 100) %>%
  ungroup()
age_race

# Create a stacked bar graph to compare age distributions between groups.
ggplot(age_race, aes(x = race_eth, y = percentage, fill = age_group)) +
  geom_col() +
  labs(
    title = "Age Distribution by Racial and Ethnic Group (2030)",
    x = "Racial/Ethnic Group",
    y = "Percentage (%)",
    fill = "Age Group"
  ) +
  theme_minimal()

# =====================================================
# PUBLIC HEALTH PLANNING
# Which areas have more older adults. Helps plan older adult care and support services.
# =====================================================

# Calculate the projected number of older adults in each area in 2030.
older_adults <- county_data %>%
  filter(year == 2030, age >= 65) %>%
  group_by(county_lhj) %>%
  summarise(population_65_plus = sum(population), .groups = "drop") %>%
  arrange(desc(population_65_plus))
older_adults

# Create a bar graph to compare older adult populations between areas.
ggplot(older_adults, aes(x = reorder(county_lhj, population_65_plus),
                         y = population_65_plus)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Projected Older Adult Population by Area (2030)",
    x = "Area",
    y = "Population Aged 65+"
  ) +
  theme_minimal()

# =====================================================
# DATA ENCODING AND PCA
# =====================================================

# Selecting three years that will be analized.
pop_base_year <- pop_data %>%
  filter(county_indicator == 1,
         county_lhj != "Madera",
         year %in% c(2000, 2015, 2030)) %>%
  mutate(age_group = case_when(
    age <= 14 ~ "Children",
    age <= 64 ~ "Working_age_adults",
    age >= 65 ~ "Older_adults"
  ))
pop_base_year

# Convert age, race and sex categories into separate binary columns.
bi_age_sex <- bind_cols(
  pop_base_year %>% select(county_lhj, year, population),
  as.data.frame(model.matrix(~ age_group - 1, pop_base_year)),
  as.data.frame(model.matrix(~ race_eth - 1, pop_base_year)),
  as.data.frame(model.matrix(~ sex - 1, pop_base_year))
)
bi_age_sex

# Calculate population percentages for each area and year.
pop_base_year <- encoded %>%
  group_by(county_lhj, year) %>%
  summarise(
    total = sum(population),
    across(-population, ~ sum(.x * population)),
    .groups = "drop"
  ) %>%
  mutate(across(-c(county_lhj, year, total),
                ~ .x / total * 100))
pop_base_year

# Select and standardise the demographic variables.
pop_features <- pop_base_year %>%
  select(-county_lhj, -year, -total)

pop_features <- pop_features[
  , sapply(pop_features, sd) > 0
]

pop_scaled <- scale(pop_features)
pop_features

# Examine relationships between demographic variables.
cor_matrix <- cor(pop_features)
cor_matrix


# Reduce the demographic variables into principal components.
pop_result <- prcomp(pop_scaled,
                     center = FALSE, scale. = FALSE)

# Examine the variation explained by each component.
summary(pop_result)

# Display the areas and years using the first two components.
pop_scores <- as.data.frame(pop_result$x) %>%
  bind_cols(pop_base_year %>% select(county_lhj, year))

ggplot(pop_scores, aes(PC1, PC2, colour = factor(year))) +
  geom_point(size = 3) +
  labs(title = "PCA of Population Profiles (2000-2030)",
       x = "PC1", y = "PC2", colour = "Year") +
  theme_minimal()

