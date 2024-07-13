
# Library Importation

## Loading Necessary Libraries

library(tidyverse)
library(plotly)     
library(dplyr)
library(caret)      
library(knitr)      
library(DT)
library(data.table)


# Data Preparation

## Loading the Dataset
data <- read.csv("C:\\Users\\user\\Desktop\\MouseWithoutBorders\\BrainTumor.csv")

## First 5 rows


head(data,n=5)


## Missing Values


## Number of missing values in the dataset 
sum(is.na(data))



## checking the columns with missing values 
colSums(is.na(data))


## Handling Missing Values


numeric_columns = data %>% 
  select(where(is.numeric))
character_columns = data %>% 
  select(where(is.character))

## Handling missing values in numeric variables
data <- data %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), -1, .)))


## Handling missing values in character variables 
data <- data %>% 
  mutate(across(where(is.character), ~ ifelse(is.na(.), "unknown", .)))



## Checking if Missing Values still Exist


colSums(is.na(data))


# Exploratory Data Analysis

## Statistical Summary


datatable(summary(data))


## Variable Distributions

### Age Distribution
age_distro <- plot_ly(data, x=~Age, type="histogram", width=500, height=400)

age_distro <- age_distro %>% 
  layout(
    title = "Age Distribution",
    xaxis = list(
      title = "Age"
    ),
    yaxis = list(
      title = "Frequency"
    )
  )
age_distro


### Gender Distribution
gender_count <- table(data$Gender)
gender_count <- as.data.frame(gender_count)
colnames(gender_count) <- c("Gender", "Count")


gender_distro <- plot_ly(gender_count, x = ~Gender, y = ~Count, type = 'bar',width=500, height = 200) %>%
  layout(
    title = list(
      text = "Gender Distribution",
      font = list(size=15)),
    xaxis = list(title = list(
      text = "Gender",
      font =list(size=10))),
    yaxis = list(title =list(
      text = "Count",
      font = list(size=10))))
gender_count
gender_distro


### Tumor Type Distribution
type_count <- table(data$Tumor.Type)
type_df <- as.data.frame(type_count)
colnames(type_df) <- c("Type","Count")

type_fig <- plot_ly(type_df, x=~Type,y=~Count, type = "bar",width = 500, height = 300) %>% 
  layout(
    title = list(
      text = "Tumor Type Distribution",
      font = list(size=15)
    ),
    xaxis = list(
      title = "Tumor Type",
      font = list(size=9),
      tickfont = list(size = 8)
    ),
    yaxis = list(
      title = "Count",
      font = list(size = 9)
    )
  )
type_df
type_fig


### Gender-Based Distribution of Tumor Types
type_gender_count <- table(data$Tumor.Type, data$Gender)
type_gender_df <- as.data.frame(type_gender_count)
colnames(type_gender_df) <- c("Type", "Gender", "Count")

# Create Plotly grouped bar chart
type_gender_fig <- plot_ly(type_gender_df, x = ~Type, y = ~Count, color = ~Gender, type = "bar", width = 500, height = 300) %>% 
  layout(
    title = list(
      text = "Tumor Type Distribution by Gender",
      font = list(size = 15)
    ),
    xaxis = list(
      title = "Tumor Type",
      font = list(size = 9),
      tickfont = list(size = 8)
    ),
    yaxis = list(
      title = "Count",
      font = list(size = 9)
    )
  )
type_gender_df
type_gender_fig

### Tumor Location Distribution
location_count <- table(data$Tumor.Location)
location_df <- as.data.frame(location_count)
colnames(location_df) <- c("Location","Count")

location_fig <- plot_ly(location_df, x=~Location,y=~Count, type = "bar",width = 500, height = 300) %>% 
  layout(
    title = list(
      text = "Tumor Location Distribution",
      font = list(size=15)
    ),
    xaxis = list(
      title = "Tumor Location",
      font = list(size=9),
      tickfont = list(size = 8)
    ),
    yaxis = list(
      title = "Count",
      font = list(size = 9)
    )
  )
location_df
location_fig


### Gender-Based Distribution of Tumor Location
location_gender_count <- table(data$Tumor.Location, data$Gender)
location_gender_df <- as.data.frame(location_gender_count)
colnames(location_gender_df) <- c("Location", "Gender", "Count")

# Create Plotly grouped bar chart
location_gender_fig <- plot_ly(location_gender_df, x = ~Location, y = ~Count, color = ~Gender, type = "bar", width = 500, height = 300) %>% 
  layout(
    title = list(
      text = "Tumor Location Distribution by Gender",
      font = list(size = 15)
    ),
    xaxis = list(
      title = "Tumor Location",
      font = list(size = 9),
      tickfont = list(size = 8)
    ),
    yaxis = list(
      title = "Count",
      font = list(size = 9)
    )
  )
location_gender_df
location_gender_fig


### Tumor Treatment Distribution by Type
treatment_type_count <- table(data$Tumor.Type, data$Treatment)
treatment_type_df <- as.data.frame(treatment_type_count)
colnames(treatment_type_df) <- c("Type", "Treatment", "Count")

# Create Plotly grouped bar chart
treatment_type_fig <- plot_ly(treatment_type_df, x = ~Treatment, y = ~Count, color = ~Type, type = "bar", width = 700, height = 300) %>% 
  layout(
    title = list(
      text = "Tumor Treatment Distribution by Type",
      font = list(size = 15)
    ),
    xaxis = list(
      title = "Tumor Treatment",
      font = list(size = 9),
      tickfont = list(size = 10)
    ),
    yaxis = list(
      title = "Count",
      font = list(size = 9)
    )
  )
treatment_type_df
treatment_type_fig




### Tumor Treatment Distribution by Outcome
treatment_outcome_count <- table(data$Treatment.Outcome, data$Treatment)
treatment_outcome_df <- as.data.frame(treatment_outcome_count)
colnames(treatment_outcome_df) <- c("Outcome", "Treatment", "Count")

# Create Plotly grouped bar chart
treatment_outcome_fig <- plot_ly(treatment_outcome_df, x = ~Treatment, y = ~Count, color = ~Outcome, type = "bar", width = 700, height = 300) %>% 
  layout(
    title = list(
      text = "Tumor Treatment Distribution by Outcome",
      font = list(size = 15)
    ),
    xaxis = list(
      title = "Tumor Treatment",
      font = list(size = 9),
      tickfont = list(size = 10)
    ),
    yaxis = list(
      title = "Count",
      font = list(size = 9)
    )
  )
treatment_outcome_df
treatment_outcome_fig


### Average Survival Month by Treatment
average_survival_treatment <- data %>% 
  group_by(Treatment) %>% 
  summarize(average_survival = round(mean(data$Survival.Time..months.,na.rm=TRUE)))
average_survival_treatment
survival_average_df <- as.data.frame(average_survival_treatment)
colnames(survival_average_df) <- c("Treatment", "Average Survival Month")

survival_average_fig <- plot_ly(survival_average_df, x = ~Treatment, y = ~`Average Survival Month`,type = "bar", width = 700, height = 300) %>% 
  layout(
    title = list(
      text = "Average Survival Months by Treatment",
      font = list(size = 15)
    ),
    xaxis = list(
      title = "Tumor Treatment",
      font = list(size = 9),
      tickfont = list(size = 10)
    ),
    yaxis = list(
      title = "Average Survival Months",
      font = list(size = 9)
    )
  )
survival_average_df
survival_average_fig


### Correlation Analysis
correlation_matrix <- cor(numeric_columns)
correlation_matrix