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


## Preprocessing & Feature Engineering  (PFE)
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


## Preprocessing & Model  (PM)

---Start PM Code---

---LK---

```{r}
library(caret)
library(pROC)
library(dplyr)

set.seed(123)

df <- read.csv("/Users/lovepreetk/Downloads/health_data_updated.csv")
df$health_risk <- factor(df$health_risk, levels = c(0,1))

trainIndex <- createDataPartition(df$health_risk, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test  <- df[-trainIndex, ]

wts <- ifelse(train$health_risk == 1, 5, 1)

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = "final"
)

train$health_risk <- factor(train$health_risk, labels = c("Low", "High"))
test$health_risk  <- factor(test$health_risk, labels = c("Low", "High"))

logit_model <- train(
  health_risk ~ .,
  data = train,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  weights = wts,
  metric = "ROC"
)

test_probs <- predict(logit_model, newdata = test, type = "prob")[,"High"]
test_pred  <- ifelse(test_probs > 0.5, "High", "Low")

cm <- confusionMatrix(factor(test_pred, levels=c("Low","High")), test$health_risk, positive="High")
roc_obj <- roc(test$health_risk, test_probs)
auc(roc_obj)

```
```{r}
# Predictions
test_probs <- predict(logit_model, newdata = test, type = "prob")[,"High"]
test_pred  <- ifelse(test_probs > 0.5, "High", "Low")
test_pred  <- factor(test_pred, levels = c("Low", "High"))

# Confusion Matrix
cm <- confusionMatrix(
  data = test_pred,
  reference = test$health_risk,
  positive = "High"
)

# Extract metrics
accuracy     <- cm$overall["Accuracy"]
precision    <- cm$byClass["Precision"]
recall       <- cm$byClass["Recall"]
specificity  <- cm$byClass["Specificity"]
f1           <- cm$byClass["F1"]

# AUC
roc_obj <- roc(test$health_risk, test_probs)
auc_val <- auc(roc_obj)

# Print results
accuracy
precision
recall
specificity
f1
auc_val
cm$table    # confusion matrix
```
```{r}

# Training set predictions
train_probs <- predict(logit_model, newdata = train, type = "prob")[,"High"]
train_pred  <- ifelse(train_probs > 0.5, "High", "Low")
train_pred  <- factor(train_pred, levels = c("Low", "High"))

# Training Confusion Matrix
cm_train <- confusionMatrix(
  data = train_pred,
  reference = train$health_risk,
  positive = "High"
)

# Extract metrics
train_accuracy     <- cm_train$overall["Accuracy"]
train_precision    <- cm_train$byClass["Precision"]
train_recall       <- cm_train$byClass["Recall"]
train_specificity  <- cm_train$byClass["Specificity"]
train_f1           <- cm_train$byClass["F1"]

# Training AUC
roc_train <- roc(train$health_risk, train_probs)
train_auc <- auc(roc_train)

# Print results
train_accuracy
train_precision
train_recall
train_specificity
train_f1
train_auc
cm_train$table

```
```{r}
model_coeffs <- coef(logit_model$finalModel)
Model_coeffs
```

---End PM Code---

## Model Evaluation and Tuning (Logistic), Addtional Models (XGboost) 
----AJ-----
Logistic Model
Finding the best Probability threshold based on F1 score

```{r}
# Predicted probabilities for the High class
thrF1_test_probs <- predict(logit_model, newdata = test, type = "prob")[,"High"]
```

```{r}
# Evaluatinf thresholds from 0.05 to 0.95 for F1 score
thrF1_thresholds <- seq(0.05, 0.95, by = 0.01)
thrF1_results <- data.frame()

for (thr in thrF1_thresholds) {
  
  thrF1_pred <- ifelse(thrF1_test_probs > thr, "High", "Low")
  thrF1_pred <- factor(thrF1_pred, levels = c("Low","High"))
  
  thrF1_cm <- confusionMatrix(
    data = thrF1_pred,
    reference = test$health_risk,
    positive = "High"
  )
  
  thrF1_results <- rbind(thrF1_results, data.frame(
    threshold = thr,
    precision = thrF1_cm$byClass["Precision"],
    recall = thrF1_cm$byClass["Recall"],
    F1 = thrF1_cm$byClass["F1"]
  ))
}
```

```{r}
#Find the threshold with the maximum F1
thrF1_best_threshold <- thrF1_results$threshold[which.max(thrF1_results$F1)]
thrF1_best_threshold
```

```{r}
#Applying this optimal threshold to classify
thrF1_final_pred <- ifelse(thrF1_test_probs > thrF1_best_threshold, "High", "Low")
thrF1_final_pred <- factor(thrF1_final_pred, levels = c("Low","High"))
```

```{r}
#Confusion matrix at optimal threshold
thrF1_final_cm <- confusionMatrix(
  thrF1_final_pred,
  test$health_risk,
  positive = "High"
)

thrF1_final_cm
thrF1_best_threshold
```

XGBOOST MODEL

```{r}
#XGBOOST
library(caret)
library(pROC)
library(dplyr)
library(xgboost)
library(Matrix)
```

```{r}
set.seed(123)


df <- read.csv("C:\\Users\\APjoh\\OneDrive\\Desktop\\Merrimack College MS Data Science\\CAPSTONE\\Week 5\\health_data_updated - health_data_updated.csv")

#Structure of variables
str(df)

unique(df$age_category)

#Prepare to recode Age_category variable
age_map <- c(
  "18-24" = 1,
  "25-29" = 2,
  "30-34" = 3,
  "35-39" = 4,
  "40-44" = 5,
  "45-49" = 6,
  "50-54" = 7,
  "55-59" = 8,
  "60-64" = 9,
  "65-69" = 10,
  "70-74" = 11,
  "75-79" = 12,
  "80+"   = 13
)
```

```{r}
# TRAIN/TEST SPLIT
trainIndex <- createDataPartition(df$health_risk, p = 0.8, list = FALSE)
train <- df[trainIndex, ]
test  <- df[-trainIndex, ]

train$age_category <- as.numeric(age_map[train$age_category])
test$age_category  <- as.numeric(age_map[test$age_category])
```

```{r}
#Preparing  matrices for XGBoost
features <- setdiff(names(train), "health_risk")

train_matrix <- xgb.DMatrix(
  data = as.matrix(train[, features]),
  label = train$health_risk
)

test_matrix <- xgb.DMatrix(
  data = as.matrix(test[, features]),
  label = test$health_risk
)
```

```{r}
 #Setting XGBoost parameters 
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  scale_pos_weight = sum(train$health_risk == 0) / sum(train$health_risk == 1),
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)
```

```{r}
#Training XGBoost model
set.seed(123)
xgb_model <- xgb.train(
  params = xgb_params,
  data = train_matrix,
  nrounds = 100,
  verbose = 1
)

```

```{r}
#Predictions on test set
test_probs <- predict(xgb_model, test_matrix)
test_pred <- ifelse(test_probs > 0.5, 1, 0)

#Confusion matrix
cm <- confusionMatrix(
  factor(test_pred, levels = c(0,1)),
  factor(test$health_risk, levels = c(0,1)),
  positive = "1"
)

#AUC
roc_obj <- roc(test$health_risk, test_probs)
auc_val <- auc(roc_obj)

```

```{r}
#Metrics
accuracy     <- cm$overall["Accuracy"]
precision    <- cm$byClass["Precision"]
recall       <- cm$byClass["Recall"]
specificity  <- cm$byClass["Specificity"]
f1           <- cm$byClass["F1"]

accuracy
precision
recall
specificity
f1
auc_val
cm$table

```

```{r}
#Training Metrics
train_probs <- predict(xgb_model, train_matrix)

train_pred <- ifelse(train_probs > 0.5, 1, 0)

#Confusion matrix 
cm_train <- confusionMatrix(
  factor(train_pred, levels = c(0,1)),
  factor(train$health_risk, levels = c(0,1)),
  positive = "1"
)

#Metrics 
train_accuracy     <- cm_train$overall["Accuracy"]
train_precision    <- cm_train$byClass["Precision"]
train_recall       <- cm_train$byClass["Recall"]
train_specificity  <- cm_train$byClass["Specificity"]
train_f1           <- cm_train$byClass["F1"]

#AUC 
roc_train <- roc(train$health_risk, train_probs)
train_auc <- auc(roc_train)

train_accuracy
train_precision
train_recall
train_specificity
train_f1
train_auc
cm_train$table
```

```{r}
#F1 Threshold Tuning

thrF1_thresholds <- seq(0.05, 0.95, by = 0.01)
thrF1_results <- data.frame()

for (thr in thrF1_thresholds) {
  
  # Classifying using the threshold
  thrF1_pred <- ifelse(test_probs > thr, 1, 0)
  thrF1_pred <- factor(thrF1_pred, levels = c(0,1))
  
  #Confusion matrix
  thrF1_cm <- confusionMatrix(
    data = thrF1_pred,
    reference = factor(test$health_risk, levels = c(0,1)),
    positive = "1"
  )
  
  #Storing precision, recall, F1
  thrF1_results <- rbind(thrF1_results, data.frame(
    threshold = thr,
    precision = thrF1_cm$byClass["Precision"],
    recall = thrF1_cm$byClass["Recall"],
    F1 = thrF1_cm$byClass["F1"]
  ))
}

#Threshold with maximum F1 
thrF1_best_threshold <- thrF1_results$threshold[which.max(thrF1_results$F1)]
thrF1_best_threshold

#Applying the optimal threshold to classify 
thrF1_final_pred <- ifelse(test_probs > thrF1_best_threshold, 1, 0)
thrF1_final_pred <- factor(thrF1_final_pred, levels = c(0,1))

#Confusion matrix at optimal threshold 
thrF1_final_cm <- confusionMatrix(
  thrF1_final_pred,
  factor(test$health_risk, levels = c(0,1)),
  positive = "1"
)

#Metrics
accuracy     <- thrF1_final_cm$overall["Accuracy"]
precision    <- thrF1_final_cm$byClass["Precision"]
recall       <- thrF1_final_cm$byClass["Recall"]
specificity  <- thrF1_final_cm$byClass["Specificity"]
f1           <- thrF1_final_cm$byClass["F1"]

#AUC 
roc_obj <- roc(test$health_risk, test_probs)
auc_val <- auc(roc_obj)


accuracy
precision
recall
specificity
f1
auc_val
thrF1_final_cm$table   
```
-----End Model Tuning and Additonal Model code----
