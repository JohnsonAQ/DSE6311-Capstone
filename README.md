#  Predictive Modeling to Support At Risk Intervention and Prevention Healthcare Through Behavior and Physiological Data

## Background

Modern healthcare and data-related technological advancement are rapidly evolving areas in today’s society. As both the healthcare and the technological fields evolve, there is an ever-increasing convergence, as these two areas are becoming more intertwined. Data-related technology and methodologies are being applied to healthcare, as healthcare data is being leveraged to rapidly identify at-risk patients, and improve decision making by healthcare professions. For example, machine learning techniques have been applied to improve accuracy of heart disease prediction by applying a logistic regression, to apply classification to accurately examine breast cancer data, and to predict the risk factors linked to heart disease using the K-means clustering algorithm (Badawy, et. al., 2023). Utilizing the many available data science tools and methodologies in conjunction with health care expertise has proven to be powerful in improving patient health, by anticipating and reducing complications, planning interventions and implementing preventative measures, and optimizing resource allocation. 


## Research Question

Can health-related attributes be used to accurately classify health risk in people, and can specific attributes that contribute more to this risk be identified?

## Hypothesis 

H0: There is no predictable or statistically significant relationship between demographic, lifestyle, and health condition variables and overall health risk

H1: Individuals’ health risk can be predicted and classified based on demographic, lifestyle, and health condition variables. Certain variables significantly influence this risk.

## Prediction 

Using routinely collected healthcare data, we aim to create a predictive model that can classify individuals in high and low risk health categories. The model will use variables centered on demographics, lifestyle factors, and already existing health conditions, or lack thereof, to identify individuals who are at risk for detrimental health outcomes, as well as the main factors playing a role in influencing this outcome.

## Exploratory Data Analysis 

-----Start Code------

The dataset is in an CSV

```{r}
file.choose()

health_data <- read.csv("C:\\Users\\APjoh\\OneDrive\\Desktop\\Merrimack College MS Data Science\\CAPSTONE\\Week 4\\CVD_cleaned.csv")
```

General view of the data
```{r}
head(health_data)
str(health_data)
summary(health_data)
```

Checking for Missing Values and Duplicates

```{r}
colSums(is.na(health_data))

any(duplicated(health_data))
sum(duplicated(health_data))
health_data[duplicated(health_data), ]

#Removing duplicates 
health_data <- health_data[!duplicated(health_data), ]
any(duplicated(health_data))
```

Cleaning up Column name formatting
```{r}
colnames(health_data) <- tolower(colnames(health_data))
colnames(health_data) <- gsub("\\.", "", colnames(health_data))
colnames(health_data)[colnames(health_data) == "friedpotato_consumption"] <- "fried_potato_consumption"

colnames(health_data)
```

Separating categorial and continous varaibles 
```{r}

cat_vars <- c("general_health","checkup","exercise","heart_disease",
              "skin_cancer","other_cancer","depression","diabetes",
              "arthritis","sex","age_category","smoking_history")

cont_vars <- c("height_cm","weight_kg","bmi","fruit_consumption",
               "green_vegetables_consumption","fried_potato_consumption","alcohol_consumption")


```

Summary statistics for each of the varaibles

```{r}
for (var in cat_vars) {
  cat("Categorical Variable:", var, "\n")
  freq <- table(health_data[[var]])
  perc <- round(prop.table(freq) * 100, 2)
  print(data.frame(Frequency = freq, Percentage = perc))
  cat("\n----------------------\n")
}


for (var in cont_vars) {
  cat("Continuous Variable:", var, "\n")
  x <- health_data[[var]]
  stats <- data.frame(
    Mean = mean(x),
    Median = median(x),
    SD = sd(x),
    Min = min(x),
    Max = max(x),
    Range = max(x) - min(x)
  )
  print(stats)
  cat("\n----------------------\n")
}
```

----VISUALIZATION----
Code 

---
title: "R Notebook"
output: html_notebook
---

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Cmd+Shift+Enter*. 

```{r}
# Load libraries
library(tidyverse)
library(skimr)
library(corrplot)
library(factoextra)

# Read your dataset
df <- read_csv("/Users/lovepreetk/Downloads/health_data.csv")

# Check structure
glimpse(df)

# Remove duplicates (you said 80 duplicates)
df <- distinct(df)

```



Variable summary table

```{r}
skim(df %>%
  select(height_cm, weight_kg, bmi, alcohol_consumption,
         fruit_consumption, green_vegetables_consumption,
         fried_potato_consumption))
```
Histogram of Continuous Variable 

```{r}
df%>%
  select(height_cm, weight_kg, bmi, alcohol_consumption,
         fruit_consumption, green_vegetables_consumption,
         fried_potato_consumption) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(Value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  facet_wrap(~Variable, scales = "free", ncol = 3) +
  theme_minimal() +
  labs(title = "Distribution of Continuous Variables",
       x = "Value", y = "Count")
```

Box plot 
```{r}
df %>%
  select(height_cm, weight_kg, bmi, alcohol_consumption,
         fruit_consumption, green_vegetables_consumption,
         fried_potato_consumption) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Boxplots of Continuous Health Variables",
       x = NULL, y = "Value")
```
Bar Graph- General Health 

```{r}
df %>%
  count(general_health) %>%
  ggplot(aes(x = reorder(general_health, n), y = n, fill = general_health)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Distribution of General Health Ratings",
       x = "General Health", y = "Count")


```

Bar graph for exercise status

```{r}
df %>%
  count(exercise) %>%
  ggplot(aes(x = exercise, y = n, fill = exercise)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Exercise Participation Among Respondents",
       x = "Exercise (Yes/No)", y = "Count")

```

Illness prevalene (depression)


```{r}
df %>%
  count(depression) %>%
  ggplot(aes(x = depression, y = n, fill = depression)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Prevalence of Depression", x = "Depression", y = "Count")

```

correlation Heatmap 

```{r correlation-heatmap, fig.width=10, fig.height=8, fig.align='center'}
library(corrplot)

num_vars <- df %>%
  select(height_cm, weight_kg, bmi, alcohol_consumption,
         fruit_consumption, green_vegetables_consumption,
         fried_potato_consumption)

cor_matrix <- cor(num_vars, use = "pairwise.complete.obs")

corrplot(cor_matrix,
         method = "color",
         type = "lower",
         order = "hclust",
         addCoef.col = "black",      # show correlation values
         number.cex = 1.2,           # text size for correlation numbers
         tl.cex = 1.4,               # label size
         tl.srt = 30,                # rotate variable names
         col = colorRampPalette(c("red", "white", "blue"))(200),
         mar = c(1, 1, 2, 1),
         title = "Correlation Heatmap of Continuous Variables")
```

