getwd()

# loading packages -----
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(googlesheets4)
library(readxl)

#reading in data -----
dep_symptoms <- read_csv("depression_symptoms.csv")
dep_prevalence <- read_csv("depression_prevalence.csv")
mi_prevalence <- read_csv("mental_illness_prevalence.csv")
cpri_data <- read_csv("countries_primary_data.csv")

# exploring data -----
View(dep_symptoms)
dim(dep_symptoms)
str(dep_symptoms)
glimpse(dep_symptoms)
head(dep_symptoms)
tail(dep_symptoms)
summary(dep_symptoms)
skim(dep_symptoms)

# editing columns 
# renaming columns
library(dplyr)
dep_symptoms <- dep_symptoms %>%
  rename(Symptom = Entity)
View(dep_symptoms)

cpri_data <- cpri_data %>%
  rename(Disorders = Entity)
View(cpri_data)

dep_prevalence <- dep_prevalence %>%
  rename(Region = Entity)
View(dep_prevalence)

mi_prevalence <- mi_prevalence %>%
  rename(Country = Entity)
View(mi_prevalence) 

# replace lengthy column name to shorter one, before summarizing data ----
colnames(mi_prevalence)[colnames(mi_prevalence) == "Depressive disorders (share of population) - Sex: Both - Age: Age-standardized"] <- "Depressive_disorders"

# filter all mi data for the year 2019 ----
data_2019 <- mi_prevalence %>% filter(Year == 2019)


# group data by country -----
# create summaries - calculate max, median, and min prevalence for depressive disorders ----
summary_Depressive_disorders <- data_2019 %>%
  group_by(Country) %>%
  summarise(Max_Depressive_disorders = max(Depressive_disorders, na.rm = TRUE),
            Median_Depressive_disorders = median(Depressive_disorders, na.rm = TRUE),
            Min_Depressive_disorders = min(Depressive_disorders, na.rm = TRUE))


# comparing trends depressive disorder trends for Germany from 1990 to 2019 ----

# load necessary libraries
library(tidyverse)
library(ggplot2)

# load data
mi_prevalence <- read.csv("mental_illness_prevalence.csv")

## filter data for Germany and Depressive disorders only
germany_data <- data %>%
  filter(Country == "Germany", Disorder == "Depressive_disorders")
## run dplyr package to counter error due to use of filter function conflict
library(dplyr)
## run code again 
## ensure data contains elements Germany and Depressive disorders
germany_data <- mi_prevalence %>%
  dplyr::filter("Germany", "Depressive disorders (share of population) - Sex: Both - Age: Age-standardized")

# new approach ----
# filter for Germany -----
germany_data <- subset(mi_prevalence, Country == "Germany")
# select Depressive disorders column -------
depression_column <- "Depressive_disorders"
# plot the trends
# run ggplot package
library(ggplot2)

## convert the column to numeric if necessary ???
germany_data$Depressive.disorders <- as.numeric(germany_data$Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized)

# create a line plot
ggplot(germany_data, aes(x = Year, y = Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized)) +
  geom_line() +
  labs(title = "Trends in Prevalence of Depressive Disorders in Germany (1990-2019)",
       x = "Year",
       y = "Prevalence of Depressive_disorders") +
  theme_minimal()

# plotting a data frame as a table
library(knitr)
kable(dep_symptoms)
kable(dep_prevalence)

table(dep_symptoms)

tabyl(dep_symptoms)
library(janitor)
tabyl(dep_symptoms)
tabyl(dep_prevalence)
tabyl(cpri_data)

library(grid)
table(dep_prevalence)
table(dep_symptoms)

install.packages("gridExtra")
library(gridExtra)
plot(tableGrob(dep_symptoms))
plot(tableGrob(dep_prevalence))

