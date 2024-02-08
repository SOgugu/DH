# PART 1A - Initial Interaction with the csv Data Files from Kaggle.com
getwd()

# loading packages -----
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(shinyalert)

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

# PART 1B - Plotting a Line Graph for Depression Trends in Germany
# Load necessary libraries
library(tidyverse)
library(plotly)

# Load data
mi_prevalence <- read.csv("mental_illness_prevalence.csv")

#Rename dataset column
mi_prevalence <- mi_prevalence %>%
  rename(Country = Entity)

# Filter for Germany
germany_data <- subset(mi_prevalence, Country == "Germany")

# Convert the column to numeric if necessary
germany_data$Depressive.disorders <- as.numeric(germany_data$Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized)

# Create a line plot with Plotly
plot_ly(germany_data, x = ~Year, y = ~Depressive.disorders..share.of.population....Sex..Both...Age..Age.standardized, type = 'scatter', mode = 'lines') %>%
  layout(title = "Trends in Prevalence of Depressive Disorders in Germany (1990-2019)",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Prevalence of Depressive Disorders"),
         showlegend = FALSE)%>%
  
  # PART 1C - Plotting a Bar Graph for Depression Symptoms in 2014
  
  # Load libraries
  library(tidyverse)
library(plotly)

# Read data
depression_data <- read_csv("depression_symptoms.csv")

# Exclude "Code" and "Year" columns
depression_data <- depression_data[, !(colnames(depression_data) %in% c("Code", "Year"))]

# Convert "Entity" column to character type
depression_data$Entity <- as.character(depression_data$Entity)

# Convert numeric columns to character
depression_data[, c("Nearly every day", "More than half the days", "Several days", "Not at all")] <-
  lapply(depression_data[, c("Nearly every day", "More than half the days", "Several days", "Not at all")], as.character)

# Check the unique values in the "Entity" column
unique_entities <- unique(depression_data$Entity)

# Create a factor variable for "Entity" column
depression_data$Entity <- factor(depression_data$Entity, levels = unique_entities)

#Install Reshape2 package
#Load package
library(reshape2)

# Reshape the data for plotting
melted_data <- melt(depression_data, id.vars = "Entity", variable.name = "Frequency", value.name = "Score")

# Plot the bar graph using plot_ly
plot_ly(melted_data, x = ~Entity, y = ~Score, color = ~Frequency, type = "bar") %>%
  layout(title = "Depression Symptoms",
         xaxis = list(title = "Entity"),
         yaxis = list(title = "Score"),
         barmode = "group")


