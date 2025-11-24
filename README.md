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

-----Start EDA Code------
---AJ---

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
---LK---

```{r}
# Load libraries
library(tidyverse)
library(skimr)
library(corrplot)
library(factoextra)

# Read your dataset
health_data <- read_csv("/Users/lovepreetk/Downloads/health_data.csv")

# Check structure
glimpse(health_data)

```


Variable summary table

```{r}
skim(health_data %>%
  select(height_cm, weight_kg, bmi, alcohol_consumption,
         fruit_consumption, green_vegetables_consumption,
         fried_potato_consumption))
```
Histogram of Continuous Variable 

```{r}
health_data %>%
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
health_data %>%
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
health_data %>%
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
health_data %>%
  count(exercise) %>%
  ggplot(aes(x = exercise, y = n, fill = exercise)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Exercise Participation Among Respondents",
       x = "Exercise (Yes/No)", y = "Count")

```

Illness prevalene (depression)


```{r}
health_data %>%
  count(depression) %>%
  ggplot(aes(x = depression, y = n, fill = depression)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Prevalence of Depression", x = "Depression", y = "Count")

```

Correlation Heatmap 

```{r correlation-heatmap, fig.width=10, fig.height=8, fig.align='center'}
library(corrplot)

num_vars <- health_data %>%
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

Other Disease Variable Visualization

---AJ---

```{r}
library(ggplot2)
library(scales)
```

Heart Disease

```{r}
ggplot(health_data, aes(x = heart_disease, fill = heart_disease)) +
  geom_bar() +
  scale_fill_manual(values = c("Yes" = "lightblue", "No" = "coral")) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "Prevalence of Heart Disease", x = "Heart Disease", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
```

Skin Cancer

```{r}
ggplot(health_data, aes(x = skin_cancer, fill = skin_cancer)) +
  geom_bar() +
  scale_fill_manual(values = c("Yes" = "lightblue", "No" = "coral")) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "Prevalence of Skin Cancer", x = "Skin Cancer", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
```


Other Cancer

```{r}
ggplot(health_data, aes(x = other_cancer, fill = other_cancer)) +
  geom_bar() +
  scale_fill_manual(values = c("Yes" = "lightblue", "No" = "coral")) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "Prevalence of Other Cancer", x = "Other Cancer", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

``` 
Diabetes

```{r}
ggplot(health_data, aes(x = diabetes, fill = diabetes)) +
  geom_bar() +
  scale_fill_manual(values = c(
    "Yes" = "lightblue",
    "Yes, but female told only during pregnancy" = "lightblue",
    "No, pre-diabetes or borderline diabetes" = "coral",
    "No" = "coral"
  )) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "Prevalence of Diabetes", x = "Diabetes", y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )
```

Arthiritis 

```{r}
ggplot(health_data, aes(x = arthritis, fill = arthritis)) +
  geom_bar() +
  scale_fill_manual(values = c("Yes" = "lightblue", "No" = "coral")) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(title = "Prevalence of Arthritis", x = "Arthritis", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
```
---End EDA Code---


## Preprocessing & Feature Engineering 
---Start PFE Code---

```{r}
file.choose()
health_data <-read.csv("C:\\Users\\APjoh\\OneDrive\\Desktop\\Merrimack College MS Data Science\\CAPSTONE\\Week 3\\health_data.csv")
```
 Variable transformation
Checkup variable recoding 

```{r}
library(dplyr)

unique(health_data$checkup)

health_data$checkup_recoded <- recode(health_data$checkup,
                              "Within the past year" = 1,
                              "Within the past 2 years" = 2,
                              "Within the past 5 years" = 3,
                              "5 or more years ago" = 4,
                              "Never" = 5
)
```

```{r}
unique(health_data$checkup_recoded)

table(health_data$checkup)
table(health_data$checkup_recoded)
```


Diabetes variable engineering

```{r}
unique(health_data$diabetes)

health_data <- health_data %>%
  mutate(
    gestational_diabetes = ifelse(
      diabetes == "Yes, but female told only during pregnancy", "Yes", "No"
    ),
    
    prediabetic = ifelse(
      diabetes == "No, pre-diabetes or borderline diabetes", "Yes", "No"
    )
  )

table(health_data$diabetes)
table(health_data$gestational_diabetes)
table(health_data$prediabetic)
```

```{r}

#New variables have been created; changing the labels in the
#originals diabetes variable to no longer include these labels

health_data <- health_data %>%
  mutate(
    diabetes = case_when(
      diabetes == "Yes, but female told only during pregnancy" ~ "No",
      diabetes == "No, pre-diabetes or borderline diabetes" ~ "No",
      TRUE ~ diabetes  
    )
  )


table(health_data$diabetes)
```

General Health Variable recoding

```{r}
unique(health_data$general_health)

health_data$general_health_recoded <- recode(health_data$general_health,
                                      "Excellent" = 1,
                                      "Very Good" = 2,
                                      "Good" = 3,
                                      "Fair" = 4,
                                      "Poor" = 5
)

table(health_data$general_health)
table(health_data$general_health_recoded)
```

Target Variable Creation/Engineering

```{r}
health_data <- health_data %>%
  mutate(
    health_risk = case_when(
      diabetes == "Yes" | prediabetic == "Yes" |
        heart_disease == "Yes" | skin_cancer == "Yes" | other_cancer == "Yes" ~ "Yes",
      TRUE ~ "No"
    )
  )

table(health_data$health_risk)
colnames(health_data)
```

Removing original and target-related variables

```{r}
health_data_altered <- health_data %>%
  select(-checkup,            
         -general_health,     
         -diabetes,           
         -prediabetic,        
         -heart_disease,      
         -skin_cancer,        
         -other_cancer)       

colnames(health_data_altered)
```

Numerical Variables Summary

```{r}
summary(dplyr::select_if(health_data_altered, is.numeric))
```

Multicolinear Variable Removal (Weight)

```{r}
health_data_altered <- health_data_altered %>%
  select(-weight_kg)

colnames(health_data_altered)
```

Z_SCORE Scaling for Logistic Regression Model

```{r}
num_vars_sc <- c("height_cm", "bmi", 
              "fruit_consumption", "green_vegetables_consumption", 
              "fried_potato_consumption", "alcohol_consumption")


health_data_altered[num_vars_sc] <- scale(health_data_altered[num_vars_sc])

summary(health_data_altered[num_vars_sc])
str(health_data_altered)
```

Age Category Variable Conversion to Factor

```{r}
unique(health_data_altered$age_category)

age_levels <- c("18-24","25-29","30-34","35-39","40-44",
                "45-49","50-54","55-59","60-64","65-69",
                "70-74","75-79","80+")


health_data_altered$age_category <- factor(
  health_data_altered$age_category,
  levels = age_levels,
  ordered = TRUE
)


str(health_data_altered$age_category)
table(health_data_altered$age_category)
```

Variable Encoding

```r

char_vars_en <- sapply(health_data_altered, is.character)

char_data <- health_data_altered[, char_vars_en]

lapply(char_data, unique)
```

---LK---

```r
library(readr)

health_data_altered <- read_csv("/Users/lovepreetk/Downloads/health_data_altered.csv")
health_data_altered <- health_data_altered %>%
  mutate(
    sex = ifelse(sex == "Male", 1, 0),
    smoking_history = ifelse(smoking_history == "Yes", 1, 0),
    arthritis = ifelse(arthritis == "Yes", 1, 0),
    depression = ifelse(depression == "Yes", 1, 0),
    health_risk = ifelse(health_risk == "Yes", 1, 0),
    gestational_diabetes = ifelse(gestational_diabetes == "Yes", 1, 0),
    exercise = ifelse(exercise == "Yes", 1, 0)
  )
summary(health_data_altered[, c("sex","smoking_history","arthritis","depression","health_risk",
               "gestational_diabetes","exercise")])

```
---End PFE Code---


