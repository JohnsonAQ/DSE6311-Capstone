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

#The dataset is in an CSV

file.choose()

health_data <- read.csv("C:\\Users\\APjoh\\OneDrive\\Desktop\\Merrimack College MS Data Science\\CAPSTONE\\Week 4\\CVD_cleaned.csv")

#General view of the data
head(health_data)
str(health_data)
summary(health_data)

#Checking for missing values
colSums(is.na(health_data))

#Checking for duplicates
any(duplicated(health_data))
sum(duplicated(health_data))
health_data[duplicated(health_data), ]


#Removing duplicates 
health_data <- health_data[!duplicated(health_data), ]
any(duplicated(health_data))

#Cleaning up Column name formatting
colnames(health_data) <- tolower(colnames(health_data))
colnames(health_data) <- gsub("\\.", "", colnames(health_data))
colnames(health_data)[colnames(health_data) == "friedpotato_consumption"] <- "fried_potato_consumption"

colnames(health_data)


#Separating categorial and continous varaibles 

cat_vars <- c("general_health","checkup","exercise","heart_disease",
              "skin_cancer","other_cancer","depression","diabetes",
              "arthritis","sex","age_category","smoking_history")

cont_vars <- c("height_cm","weight_kg","bmi","fruit_consumption",
               "green_vegetables_consumption","fried_potato_consumption","alcohol_consumption")




#Summary statistics for each of the varaibles


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
