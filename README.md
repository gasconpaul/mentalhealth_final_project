# mentalhealth_final_project

#dataset can be found on: https://www.kaggle.com/datasets/jajidhasan/mental-health/data

data <- read.csv("Mental Health Dataset.csv", header= TRUE)
head(data)
dim(data)
View(data)


#Chapter 2 =============================================================================================================Chapter 2
# ================================
#Summary Statistics
# ================================

# Load package
library(psych)

# Basic summary statistics
summary(data)

# Detailed summary statistics table
numeric_summary <- data.frame(
  Variable = names(data[, sapply(data, is.numeric)]),
  Mean = round(sapply(data[, sapply(data, is.numeric)], mean, na.rm = TRUE), 0),
  SD = round(sapply(data[, sapply(data, is.numeric)], sd, na.rm = TRUE), 2),
  Median = round(sapply(data[, sapply(data, is.numeric)], median, na.rm = TRUE), 2),
  Min = round(sapply(data[, sapply(data, is.numeric)], min, na.rm = TRUE), 2),
  Max = round(sapply(data[, sapply(data, is.numeric)], max, na.rm = TRUE), 2),
  Missing_Percent = round(
    colMeans(is.na(data[, sapply(data, is.numeric)])) * 100,
    2
  )
)

# Sort alphabetically by variable name
numeric_summary <- numeric_summary[
  order(numeric_summary$Variable),
]

# Display summary table
numeric_summary

# Additional descriptive statistics
describe(data[, sapply(data, is.numeric)])


# =========================================
#Distribution Analysis
# =========================================

# -------------------------
# Histograms for Numerical Variables
# -------------------------

par(mfrow = c(3, 3))

hist(data$Age,
     main = "Age Distribution",
     xlab = "Age",
     col = "lightblue",
     border = "white")

hist(data$Daily_Screen_Time,
     main = "Daily Screen Time Distribution",
     xlab = "Daily Screen Time",
     col = "lightblue",
     border = "white")

hist(data$Social_Media_Usage,
     main = "Social Media Usage Distribution",
     xlab = "Social Media Usage",
     col = "lightblue",
     border = "white")

hist(data$Sleep_Hours,
     main = "Sleep Hours Distribution",
     xlab = "Sleep Hours",
     col = "lightblue",
     border = "white")

hist(data$Stress_Level,
     main = "Stress Level Distribution",
     xlab = "Stress Level",
     col = "lightblue",
     border = "white")

hist(data$Work_Study_Hours,
     main = "Work/Study Hours Distribution",
     xlab = "Work/Study Hours",
     col = "lightblue",
     border = "white")

hist(data$Social_Interaction_Score,
     main = "Social Interaction Score Distribution",
     xlab = "Social Interaction Score",
     col = "lightblue",
     border = "white")

hist(data$Caffeine_Intake,
     main = "Caffeine Intake Distribution",
     xlab = "Caffeine Intake",
     col = "lightblue",
     border = "white")

# =========================================
# Categorical Variable Distributions
# =========================================

par(mfrow = c(1, 3))

# -------------------------
# Gender Distribution
# -------------------------

gender_counts <- table(data$Gender)

barplot(gender_counts,
        main = paste("Gender Distribution\nThere are",
                     gender_counts["Male"], "males and",
                     gender_counts["Female"], "females"),
        col = "skyblue")

# -------------------------
# Occupation Distribution
# -------------------------

occupation_counts <- table(data$Occupation)

barplot(occupation_counts,
        main = paste("Occupation Distribution\nTotal Counts:",
                     paste(names(occupation_counts),
                           occupation_counts,
                           collapse = ", ")),
        col = "orange")

# -------------------------
# Physical Activity Distribution
# -------------------------

activity_counts <- table(data$Physical_Activity)

barplot(activity_counts,
        main = paste("Physical Activity Distribution\nTotal Counts:",
                     paste(names(activity_counts),
                           activity_counts,
                           collapse = ", ")),
        col = "lightgreen")


# -------------------------
# Binary Variable Graphs
# -------------------------

par(mfrow = c(2, 3))

night_counts <- table(data$Night_Usage)

barplot(night_counts,
        main = paste("Night Usage\n0 =",
                     night_counts["0"],
                     ", 1 =",
                     night_counts["1"]),
        col = c("gray70", "red"))

smoking_counts <- table(data$Smoking)

barplot(smoking_counts,
        main = paste("Smoking\n0 =",
                     smoking_counts["0"],
                     ", 1 =",
                     smoking_counts["1"]),
        col = c("gray70", "red"))

alcohol_counts <- table(data$Alcohol)

barplot(alcohol_counts,
        main = paste("Alcohol\n0 =",
                     alcohol_counts["0"],
                     ", 1 =",
                     alcohol_counts["1"]),
        col = c("gray70", "red"))

depression_counts <- table(data$Depression)

barplot(depression_counts,
        main = paste("Depression\n0 =",
                     depression_counts["0"],
                     ", 1 =",
                     depression_counts["1"]),
        col = c("gray70", "red"))

anxiety_counts <- table(data$Anxiety)

barplot(anxiety_counts,
        main = paste("Anxiety\n0 =",
                     anxiety_counts["0"],
                     ", 1 =",
                     anxiety_counts["1"]),
        col = c("gray70", "red"))

burnout_counts <- table(data$Burnout)

barplot(burnout_counts,
        main = paste("Burnout\n0 =",
                     burnout_counts["0"],
                     ", 1 =",
                     burnout_counts["1"]),
        col = c("gray70", "red"))

# Reset plotting layout
par(mfrow = c(1,1))

# =========================================
# Chapter 2 — Correlation Matrix
# =========================================

# Load package
library(corrplot)

# Select numerical variables
numeric_data <- data[, sapply(data, is.numeric)]

# Compute correlation matrix
cor_matrix <- cor(numeric_data)

par(mar = c(1,1,1,1))
# Create heatmap
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.6,
         tl.col = "black",
         tl.cex = 0.7,
         diag = FALSE)


# =========================================
# Missing Values and Outliers
# =========================================

# -------------------------
# Missing Values
# -------------------------

# Count missing values per variable
missing_values <- colSums(is.na(data))

# Display missing values
missing_values

# Total missing values in dataset
total_missing <- sum(is.na(data))

cat("Total missing values in dataset:", total_missing, "\n\n")

# Remove rows with missing values
data <- na.omit(data)

cat("Dataset dimensions after removing missing values:\n")
dim(data)

# -------------------------
# Outlier Detection (IQR Method)
# -------------------------

# Select numeric variables
numeric_data <- data[, sapply(data, is.numeric)]

# Function to count outliers
count_outliers <- function(x) {
  
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  sum(x < lower_bound | x > upper_bound)
}

# Count outliers for each numeric variable
outlier_counts <- sapply(numeric_data, count_outliers)

# Display outlier counts
outlier_counts

# -------------------------
# Remove Outliers
# -------------------------

# Function to identify non-outlier rows
remove_outliers <- function(df) {
  
  numeric_cols <- sapply(df, is.numeric)
  
  for(col in names(df)[numeric_cols]) {
    
    Q1 <- quantile(df[[col]], 0.25)
    Q3 <- quantile(df[[col]], 0.75)
    
    IQR_value <- Q3 - Q1
    
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    
    df <- df[df[[col]] >= lower_bound &
               df[[col]] <= upper_bound, ]
  }
  
  return(df)
}

# Remove outliers
data_clean <- remove_outliers(data)

# Compare dimensions
cat("Original dimensions:", dim(data), "\n")
cat("Dimensions after removing outliers:", dim(data_clean), "\n")


# =========================================
#Train/Validation/Test Split
# =========================================

# Load package
library(caret)

# Set seed for reproducibility
set.seed(4230)

# -------------------------
# 70% Training Set
# -------------------------

train_index <- createDataPartition(data_clean$Depression,
                                   p = 0.70,
                                   list = FALSE)

train_data <- data_clean[train_index, ]
temp_data  <- data_clean[-train_index, ]

# -------------------------
# Remaining 30% Split Into:
# 15% Validation
# 15% Test
# -------------------------

validation_index <- createDataPartition(temp_data$Depression,
                                        p = 0.50,
                                        list = FALSE)

validation_data <- temp_data[validation_index, ]
test_data       <- temp_data[-validation_index, ]

# -------------------------
# Dataset Dimensions
# -------------------------

cat("Training Set Dimensions:\n")
dim(train_data)

cat("\nValidation Set Dimensions:\n")
dim(validation_data)

cat("\nTest Set Dimensions:\n")
dim(test_data)





























#Chapter 3 =============================================================================================================Chapter 3
# =========================================
# Chapter 3 — Simple Linear Regression
# =========================================

# -----------------------------------------
# 1. Sleep Hours and Stress Level
# -----------------------------------------

# Linear regression model
model1 <- lm(Stress_Level ~ Sleep_Hours, data = data_clean)

# Model summary
summary(model1)

# Regression equation
cat("Stress_Level =",
    round(coef(model1)[1], 3),
    "+",
    round(coef(model1)[2], 3),
    "* Sleep_Hours\n\n")

# Scatterplot with regression line
plot(data_clean$Sleep_Hours,
     data_clean$Stress_Level,
     main = "Stress Level vs Sleep Hours",
     xlab = "Sleep Hours",
     ylab = "Stress Level",
     pch = 19,
     col = "blue")

abline(model1,
       col = "red",
       lwd = 2)

# -----------------------------------------
# 2. Caffeine Intake and Sleep Hours
# -----------------------------------------

# Linear regression model
model2 <- lm(Sleep_Hours ~ Caffeine_Intake, data = data_clean)

# Model summary
summary(model2)

# Regression equation
cat("Sleep_Hours =",
    round(coef(model2)[1], 3),
    "+",
    round(coef(model2)[2], 3),
    "* Caffeine_Intake\n\n")

# Scatterplot with regression line
plot(data_clean$Caffeine_Intake,
     data_clean$Sleep_Hours,
     main = "Sleep Hours vs Caffeine Intake",
     xlab = "Caffeine Intake",
     ylab = "Sleep Hours",
     pch = 19,
     col = "darkgreen")

abline(model2,
       col = "red",
       lwd = 2)

# -----------------------------------------
# 3. Sleep Hours and Burnout
# -----------------------------------------

# Linear regression model
model3 <- lm(Burnout ~ Sleep_Hours, data = data_clean)

# Model summary
summary(model3)

# Regression equation
cat("Burnout =",
    round(coef(model3)[1], 3),
    "+",
    round(coef(model3)[2], 3),
    "* Sleep_Hours\n\n")

# Scatterplot with regression line
plot(data_clean$Sleep_Hours,
     as.numeric(as.character(data_clean$Burnout)),
     main = "Burnout vs Sleep Hours",
     xlab = "Sleep Hours",
     ylab = "Burnout",
     pch = 19,
     col = "purple")

abline(model3,
       col = "red",
       lwd = 2)





























#Chapter 4 =============================================================================================================Chapter 4
# =========================================
# Chapter 4 — Multiple Linear Regression
# Questions 1, 2
# =========================================

# Required packages
library(car)
library(leaps)

# -----------------------------------------
# Data Type Setup
# -----------------------------------------

data_clean$Gender <- as.factor(data_clean$Gender)
data_clean$Occupation <- as.factor(data_clean$Occupation)
data_clean$Physical_Activity <- as.factor(data_clean$Physical_Activity)

data_clean$Night_Usage <- as.numeric(as.character(data_clean$Night_Usage))
data_clean$Depression  <- as.numeric(as.character(data_clean$Depression))
data_clean$Anxiety     <- as.numeric(as.character(data_clean$Anxiety))
data_clean$Burnout     <- as.numeric(as.character(data_clean$Burnout))


# =========================================
# Question 1:
# Which combination of lifestyle factors best predicts Stress_Level?
# =========================================

model_stress <- lm(
  Stress_Level ~ Sleep_Hours + Daily_Screen_Time + Social_Media_Usage +
    Work_Study_Hours + Caffeine_Intake + Social_Interaction_Score +
    Physical_Activity + Night_Usage,
  data = data_clean
)

# Coefficient table
stress_coefficients <- round(coef(summary(model_stress)), 3)
stress_coefficients

# Model summary table
stress_model_summary <- data.frame(
  R_squared = round(summary(model_stress)$r.squared, 3),
  Adjusted_R_squared = round(summary(model_stress)$adj.r.squared, 3),
  Residual_SE = round(summary(model_stress)$sigma, 3),
  F_statistic = round(summary(model_stress)$fstatistic[1], 3),
  P_value = round(
    pf(summary(model_stress)$fstatistic[1],
       summary(model_stress)$fstatistic[2],
       summary(model_stress)$fstatistic[3],
       lower.tail = FALSE),
    3
  )
)

stress_model_summary

# Dummy variables
contrasts(data_clean$Physical_Activity)

# VIF
stress_vif <- round(vif(model_stress), 3)
stress_vif

# Subset selection
subset_stress <- regsubsets(
  Stress_Level ~ Sleep_Hours + Daily_Screen_Time + Social_Media_Usage +
    Work_Study_Hours + Caffeine_Intake + Social_Interaction_Score +
    Physical_Activity + Night_Usage,
  data = data_clean,
  nvmax = 10
)

summary_stress <- summary(subset_stress)

stress_subset_table <- data.frame(
  Model_Size = 1:length(summary_stress$adjr2),
  Adjusted_R_squared = round(summary_stress$adjr2, 3),
  BIC = round(summary_stress$bic, 3),
  Cp = round(summary_stress$cp, 3)
)

stress_subset_table

stress_best_models <- data.frame(
  Best_By_Adjusted_R_squared = which.max(summary_stress$adjr2),
  Best_By_BIC = which.min(summary_stress$bic),
  Best_By_Cp = which.min(summary_stress$cp)
)

stress_best_models

# -----------------------------------------
# Predicted vs Actual Stress Level
# -----------------------------------------

predicted_stress <- predict(model_stress)

plot(jitter(predicted_stress),
     jitter(data_clean$Stress_Level),
     main = "Predicted vs Actual Stress Level",
     xlab = "Predicted Stress Level",
     ylab = "Actual Stress Level",
     pch = 19,
     col = "blue")

abline(0, 1,
       col = "red",
       lwd = 2)


# =========================================
# Question 2:
# Which variables are most associated with reduced Sleep_Hours?
# =========================================

model_sleep <- lm(
  Sleep_Hours ~ Caffeine_Intake + Stress_Level + Daily_Screen_Time +
    Social_Media_Usage + Night_Usage + Work_Study_Hours +
    Depression + Anxiety,
  data = data_clean
)

# Coefficient table
sleep_coefficients <- round(coef(summary(model_sleep)), 3)
sleep_coefficients

# Model summary table
sleep_model_summary <- data.frame(
  R_squared = round(summary(model_sleep)$r.squared, 3),
  Adjusted_R_squared = round(summary(model_sleep)$adj.r.squared, 3),
  Residual_SE = round(summary(model_sleep)$sigma, 3),
  F_statistic = round(summary(model_sleep)$fstatistic[1], 3),
  P_value = round(
    pf(summary(model_sleep)$fstatistic[1],
       summary(model_sleep)$fstatistic[2],
       summary(model_sleep)$fstatistic[3],
       lower.tail = FALSE),
    3
  )
)

sleep_model_summary

# VIF
sleep_vif <- round(vif(model_sleep), 3)
sleep_vif

# Subset selection
subset_sleep <- regsubsets(
  Sleep_Hours ~ Caffeine_Intake + Stress_Level + Daily_Screen_Time +
    Social_Media_Usage + Night_Usage + Work_Study_Hours +
    Depression + Anxiety,
  data = data_clean,
  nvmax = 8
)

summary_sleep <- summary(subset_sleep)

sleep_subset_table <- data.frame(
  Model_Size = 1:length(summary_sleep$adjr2),
  Adjusted_R_squared = round(summary_sleep$adjr2, 3),
  BIC = round(summary_sleep$bic, 3),
  Cp = round(summary_sleep$cp, 3)
)

sleep_subset_table

sleep_best_models <- data.frame(
  Best_By_Adjusted_R_squared = which.max(summary_sleep$adjr2),
  Best_By_BIC = which.min(summary_sleep$bic),
  Best_By_Cp = which.min(summary_sleep$cp)
)

sleep_best_models

# -----------------------------------------
# Predicted vs Actual Sleep Hours
# -----------------------------------------

predicted_sleep <- predict(model_sleep)

plot(jitter(predicted_sleep),
     jitter(data_clean$Sleep_Hours),
     main = "Predicted vs Actual Sleep Hours",
     xlab = "Predicted Sleep Hours",
     ylab = "Actual Sleep Hours",
     pch = 19,
     col = "darkgreen")

abline(0, 1,
       col = "red",
       lwd = 2)





























#Chapter 5 =============================================================================================================Chapter 5
# =========================================
# Chapter 5 — Logistic Regression
# Questions 1, 2, and 3
# With Probability Curve Plots
# =========================================

library(caret)

# -----------------------------------------
# Variable Setup
# -----------------------------------------

data_clean$Gender <- as.factor(data_clean$Gender)
data_clean$Occupation <- as.factor(data_clean$Occupation)
data_clean$Physical_Activity <- as.factor(data_clean$Physical_Activity)

data_clean$Depression  <- as.numeric(as.character(data_clean$Depression))
data_clean$Burnout     <- as.numeric(as.character(data_clean$Burnout))
data_clean$Anxiety     <- as.numeric(as.character(data_clean$Anxiety))
data_clean$Night_Usage <- as.numeric(as.character(data_clean$Night_Usage))

# =========================================
# Train/Test Split
# =========================================

set.seed(4230)

train_index <- createDataPartition(data_clean$Burnout,
                                   p = 0.70,
                                   list = FALSE)

train_data <- data_clean[train_index, ]
test_data  <- data_clean[-train_index, ]


# =========================================
# Question 1:
# Which factors best predict Depression?
# =========================================

model_depression <- glm(
  Depression ~ Sleep_Hours + Stress_Level + Daily_Screen_Time +
    Social_Media_Usage + Night_Usage + Caffeine_Intake +
    Work_Study_Hours + Physical_Activity,
  data = train_data,
  family = binomial
)

summary(model_depression)

depression_odds <- round(exp(coef(model_depression)), 3)
depression_odds

depression_prob <- predict(model_depression,
                           newdata = test_data,
                           type = "response")

depression_pred <- ifelse(depression_prob >= 0.5, 1, 0)

table(Predicted = depression_pred,
      Actual = test_data$Depression)

mean(depression_pred == test_data$Depression)



# Probability curve: Depression vs Sleep Hours
simple_depression <- glm(Depression ~ Sleep_Hours,
                         data = data_clean,
                         family = binomial)

sleep_range <- seq(min(data_clean$Sleep_Hours),
                   max(data_clean$Sleep_Hours),
                   length = 100)

depression_curve_prob <- predict(simple_depression,
                                 newdata = data.frame(
                                   Sleep_Hours = sleep_range
                                 ),
                                 type = "response")

plot(sleep_range,
     depression_curve_prob,
     type = "l",
     lwd = 3,
     col = "blue",
     main = "Probability of Depression vs Sleep Hours",
     xlab = "Sleep Hours",
     ylab = "Predicted Probability of Depression")


# =========================================
# Question 2:
# Which variables increase the probability of Burnout?
# =========================================

model_burnout <- glm(
  Burnout ~ Sleep_Hours + Stress_Level + Work_Study_Hours +
    Daily_Screen_Time + Social_Media_Usage + Night_Usage +
    Caffeine_Intake + Physical_Activity,
  data = train_data,
  family = binomial
)

summary(model_burnout)

burnout_odds <- round(exp(coef(model_burnout)), 3)
burnout_odds

burnout_prob <- predict(model_burnout,
                        newdata = test_data,
                        type = "response")

burnout_pred <- ifelse(burnout_prob >= 0.5, 1, 0)

table(Predicted = burnout_pred,
      Actual = test_data$Burnout)

mean(burnout_pred == test_data$Burnout)



# Probability curve: Burnout vs Stress Level
simple_burnout <- glm(Burnout ~ Stress_Level,
                      data = data_clean,
                      family = binomial)

stress_range <- seq(min(data_clean$Stress_Level),
                    max(data_clean$Stress_Level),
                    length = 100)

burnout_curve_prob <- predict(simple_burnout,
                              newdata = data.frame(
                                Stress_Level = stress_range
                              ),
                              type = "response")

plot(stress_range,
     burnout_curve_prob,
     type = "l",
     lwd = 3,
     col = "red",
     main = "Probability of Burnout vs Stress Level",
     xlab = "Stress Level",
     ylab = "Predicted Probability of Burnout")


# =========================================
# Question 3:
# Can technology usage patterns predict Anxiety?
# =========================================

model_anxiety <- glm(
  Anxiety ~ Daily_Screen_Time + Social_Media_Usage + Night_Usage +
    Sleep_Hours + Caffeine_Intake + Work_Study_Hours,
  data = train_data,
  family = binomial
)

summary(model_anxiety)

anxiety_odds <- round(exp(coef(model_anxiety)), 3)
anxiety_odds

anxiety_prob <- predict(model_anxiety,
                        newdata = test_data,
                        type = "response")

anxiety_pred <- ifelse(anxiety_prob >= 0.5, 1, 0)

table(Predicted = anxiety_pred,
      Actual = test_data$Anxiety)

mean(anxiety_pred == test_data$Anxiety)



# Probability curve: Anxiety vs Daily Screen Time
simple_anxiety <- glm(Anxiety ~ Daily_Screen_Time,
                      data = data_clean,
                      family = binomial)

screen_range <- seq(min(data_clean$Daily_Screen_Time),
                    max(data_clean$Daily_Screen_Time),
                    length = 100)

anxiety_curve_prob <- predict(simple_anxiety,
                              newdata = data.frame(
                                Daily_Screen_Time = screen_range
                              ),
                              type = "response")

plot(screen_range,
     anxiety_curve_prob,
     type = "l",
     lwd = 3,
     col = "darkgreen",
     main = "Probability of Anxiety vs Daily Screen Time",
     xlab = "Daily Screen Time",
     ylab = "Predicted Probability of Anxiety")





























#Chapter 6 =============================================================================================================Chapter 6
# =========================================
# Chapter 6 — Resampling Methods
# Questions 1, 2, and 3
# =========================================

library(boot)

# =========================================
# Question 1:
# Which regression model best predicts Stress_Level
# using cross-validation?
# =========================================

deg_grid <- 1:10
val_mse <- rep(NA, length(deg_grid))

set.seed(4230)

for(i in deg_grid) {
  
  model_cv <- glm(
    Stress_Level ~ poly(Sleep_Hours, i),
    data = data_clean
  )
  
  cv_error <- cv.glm(
    data_clean,
    model_cv,
    K = 10
  )
  
  val_mse[i] <- cv_error$delta[1]
}

plot(deg_grid,
     val_mse,
     type = "b",
     pch = 19,
     col = "blue",
     xlab = "Polynomial Degree",
     ylab = "Validation-Set MSE",
     main = "10-Fold Cross-Validation")

best_deg <- deg_grid[which.min(val_mse)]

abline(v = best_deg,
       lty = 2,
       col = "red",
       lwd = 2)

best_deg

cat("Best Polynomial Degree:",
    best_deg, "\n")

cat("Lowest Validation MSE:",
    round(min(val_mse), 3), "\n\n")


# =========================================
# Question 2:
# Which predictors remain consistently important
# across bootstrap resamples?
# =========================================

bootstrap_function <- function(data, indices) {
  
  sample_data <- data[indices, ]
  
  model <- lm(
    Stress_Level ~ Sleep_Hours +
      Daily_Screen_Time +
      Social_Media_Usage +
      Work_Study_Hours +
      Caffeine_Intake +
      Social_Interaction_Score,
    data = sample_data
  )
  
  return(coef(model))
}

set.seed(4230)

bootstrap_results <- boot(
  data = data_clean,
  statistic = bootstrap_function,
  R = 1000
)

bootstrap_results

bootstrap_se <- apply(bootstrap_results$t, 2, sd)

bootstrap_summary <- data.frame(
  Predictor = c("Intercept",
                "Sleep_Hours",
                "Daily_Screen_Time",
                "Social_Media_Usage",
                "Work_Study_Hours",
                "Caffeine_Intake",
                "Social_Interaction_Score"),
  Original_Estimate = round(bootstrap_results$t0, 3),
  Bootstrap_SE = round(bootstrap_se, 3)
)

bootstrap_summary

# Bootstrap confidence intervals
boot.ci(bootstrap_results, type = "perc", index = 2)
boot.ci(bootstrap_results, type = "perc", index = 3)
boot.ci(bootstrap_results, type = "perc", index = 4)
boot.ci(bootstrap_results, type = "perc", index = 5)
boot.ci(bootstrap_results, type = "perc", index = 6)
boot.ci(bootstrap_results, type = "perc", index = 7)

# Bootstrap coefficient distributions
boxplot(bootstrap_results$t,
        main = "Bootstrap Coefficient Distributions",
        ylab = "Coefficient Estimate",
        col = "lightgray",
        names = c("Intercept",
                  "Sleep",
                  "Screen",
                  "Social",
                  "Work",
                  "Caffeine",
                  "Interaction"),
        las = 2)

hist(bootstrap_results$t[, 2],
     main = "Bootstrap Distribution of Sleep_Hours Coefficient",
     xlab = "Sleep_Hours Coefficient",
     col = "lightblue",
     border = "white")

hist(bootstrap_results$t[, 3],
     main = "Bootstrap Distribution of Daily Screen Time Coefficient",
     xlab = "Daily_Screen_Time Coefficient",
     col = "lightgreen",
     border = "white")


# =========================================
# Question 3:
# Which resampling method provides more stable
# model evaluation for this dataset?
# =========================================

# Train/Test Split Evaluation

set.seed(4230)

train_index <- sample(1:nrow(data_clean),
                      size = 0.70 * nrow(data_clean))

train_data <- data_clean[train_index, ]
test_data  <- data_clean[-train_index, ]

train_test_model <- lm(
  Stress_Level ~ Sleep_Hours +
    Daily_Screen_Time +
    Social_Media_Usage +
    Work_Study_Hours +
    Caffeine_Intake +
    Social_Interaction_Score,
  data = train_data
)

test_predictions <- predict(train_test_model,
                            newdata = test_data)

test_mse <- mean((test_data$Stress_Level - test_predictions)^2)
test_rmse <- sqrt(test_mse)

# Cross-validation RMSE from Question 1
cv_rmse <- sqrt(min(val_mse))

# Bootstrap RMSE approximation
bootstrap_mse <- mean((data_clean$Stress_Level -
                         predict(lm(
                           Stress_Level ~ Sleep_Hours +
                             Daily_Screen_Time +
                             Social_Media_Usage +
                             Work_Study_Hours +
                             Caffeine_Intake +
                             Social_Interaction_Score,
                           data = data_clean
                         )))^2)

bootstrap_rmse <- sqrt(bootstrap_mse)

resampling_comparison <- data.frame(
  Method = c("Train/Test Split",
             "10-Fold Cross-Validation",
             "Bootstrap"),
  RMSE = round(c(test_rmse,
                 cv_rmse,
                 bootstrap_rmse), 3)
)

resampling_comparison

barplot(resampling_comparison$RMSE,
        names.arg = resampling_comparison$Method,
        main = "Comparison of Resampling Methods",
        ylab = "RMSE",
        col = c("lightblue", "lightgreen", "lightgray"),
        las = 2)

cat("\nResampling Method Comparison:\n")
print(resampling_comparison)




























#Chapter 7 =============================================================================================================Chapter 7
# =========================================
# Chapter 7 — Regularization
# Questions 1, 2, and 3
# Ridge + Lasso
# =========================================

library(glmnet)

# -----------------------------------------
# Data Setup
# -----------------------------------------

regularization_data <- subset(data_clean, select = -Person_ID)

regularization_data$Gender <- as.factor(regularization_data$Gender)
regularization_data$Occupation <- as.factor(regularization_data$Occupation)
regularization_data$Physical_Activity <- as.factor(regularization_data$Physical_Activity)

regularization_data$Depression <- as.numeric(as.character(regularization_data$Depression))
regularization_data$Anxiety <- as.numeric(as.character(regularization_data$Anxiety))
regularization_data$Burnout <- as.numeric(as.character(regularization_data$Burnout))
regularization_data$Night_Usage <- as.numeric(as.character(regularization_data$Night_Usage))


# =========================================
# Question 1:
# Which predictors remain important for predicting Stress_Level
# after ridge and lasso regularization?
# =========================================

y_stress <- regularization_data$Stress_Level

x_stress <- model.matrix(
  Stress_Level ~ .,
  data = regularization_data
)[, -1]

set.seed(4230)

train_index <- sample(1:nrow(x_stress),
                      size = 0.70 * nrow(x_stress))

x_train <- x_stress[train_index, ]
x_test  <- x_stress[-train_index, ]

y_train <- y_stress[train_index]
y_test  <- y_stress[-train_index]

# Ridge Regression
set.seed(4230)

ridge_stress_cv <- cv.glmnet(
  x_train,
  y_train,
  alpha = 0
)

ridge_stress_lambda <- ridge_stress_cv$lambda.min

ridge_stress_pred <- predict(
  ridge_stress_cv,
  s = ridge_stress_lambda,
  newx = x_test
)

ridge_stress_rmse <- sqrt(mean((y_test - ridge_stress_pred)^2))

ridge_stress_coef <- coef(
  ridge_stress_cv,
  s = ridge_stress_lambda
)

ridge_stress_table <- data.frame(
  Predictor = rownames(as.matrix(ridge_stress_coef)),
  Coefficient = round(as.numeric(ridge_stress_coef), 3)
)

# Lasso Regression
set.seed(4230)

lasso_stress_cv <- cv.glmnet(
  x_train,
  y_train,
  alpha = 1
)

lasso_stress_lambda <- lasso_stress_cv$lambda.min

lasso_stress_pred <- predict(
  lasso_stress_cv,
  s = lasso_stress_lambda,
  newx = x_test
)

lasso_stress_rmse <- sqrt(mean((y_test - lasso_stress_pred)^2))

lasso_stress_coef <- coef(
  lasso_stress_cv,
  s = lasso_stress_lambda
)

lasso_stress_table <- data.frame(
  Predictor = rownames(as.matrix(lasso_stress_coef)),
  Coefficient = round(as.numeric(lasso_stress_coef), 3)
)

lasso_stress_selected <- lasso_stress_table[
  lasso_stress_table$Coefficient != 0,
]

stress_results <- data.frame(
  Method = c("Ridge", "Lasso"),
  Best_Lambda = round(c(ridge_stress_lambda, lasso_stress_lambda), 3),
  Test_RMSE = round(c(ridge_stress_rmse, lasso_stress_rmse), 3)
)

stress_results
ridge_stress_table
lasso_stress_table
lasso_stress_selected

plot(ridge_stress_cv,
     main = "Ridge CV: Stress Level")

plot(lasso_stress_cv,
     main = "Lasso CV: Stress Level")

barplot(stress_results$Test_RMSE,
        names.arg = stress_results$Method,
        main = "Ridge vs Lasso RMSE: Stress Level",
        ylab = "Test RMSE",
        col = c("lightblue", "lightgreen"))


# =========================================
# Question 2:
# Which predictors retain the largest coefficients
# after ridge shrinkage when predicting Depression?
# =========================================

y_depression <- regularization_data$Depression

x_depression <- model.matrix(
  Depression ~ .,
  data = regularization_data
)[, -1]

set.seed(4230)

train_index_dep <- sample(1:nrow(x_depression),
                          size = 0.70 * nrow(x_depression))

x_train_dep <- x_depression[train_index_dep, ]
x_test_dep  <- x_depression[-train_index_dep, ]

y_train_dep <- y_depression[train_index_dep]
y_test_dep  <- y_depression[-train_index_dep]

# Ridge Logistic Regression
set.seed(4230)

ridge_dep_cv <- cv.glmnet(
  x_train_dep,
  y_train_dep,
  alpha = 0,
  family = "binomial"
)

ridge_dep_lambda <- ridge_dep_cv$lambda.min

ridge_dep_prob <- predict(
  ridge_dep_cv,
  s = ridge_dep_lambda,
  newx = x_test_dep,
  type = "response"
)

ridge_dep_pred <- ifelse(ridge_dep_prob >= 0.5, 1, 0)

ridge_dep_accuracy <- mean(ridge_dep_pred == y_test_dep)

ridge_dep_coef <- coef(
  ridge_dep_cv,
  s = ridge_dep_lambda
)

ridge_dep_table <- data.frame(
  Predictor = rownames(as.matrix(ridge_dep_coef)),
  Coefficient = round(as.numeric(ridge_dep_coef), 3)
)

ridge_dep_table_sorted <- ridge_dep_table[
  order(abs(ridge_dep_table$Coefficient), decreasing = TRUE),
]

ridge_dep_results <- data.frame(
  Method = "Ridge Logistic Regression",
  Best_Lambda = round(ridge_dep_lambda, 3),
  Test_Accuracy = round(ridge_dep_accuracy, 3)
)

ridge_dep_results
ridge_dep_table
ridge_dep_table_sorted

table(Predicted = ridge_dep_pred,
      Actual = y_test_dep)

plot(ridge_dep_cv,
     main = "Ridge CV: Depression")

barplot(head(abs(ridge_dep_table_sorted$Coefficient[-1]), 10),
        names.arg = head(ridge_dep_table_sorted$Predictor[-1], 10),
        main = "Largest Ridge Coefficients: Depression",
        ylab = "Absolute Coefficient",
        col = "lightblue",
        las = 2)


# =========================================
# Question 3:
# Which variables are consistently selected by lasso regression
# for predicting Anxiety?
# =========================================

y_anxiety <- regularization_data$Anxiety

x_anxiety <- model.matrix(
  Anxiety ~ .,
  data = regularization_data
)[, -1]

set.seed(4230)

train_index_anx <- sample(1:nrow(x_anxiety),
                          size = 0.70 * nrow(x_anxiety))

x_train_anx <- x_anxiety[train_index_anx, ]
x_test_anx  <- x_anxiety[-train_index_anx, ]

y_train_anx <- y_anxiety[train_index_anx]
y_test_anx  <- y_anxiety[-train_index_anx]

# Lasso Logistic Regression
set.seed(4230)

lasso_anx_cv <- cv.glmnet(
  x_train_anx,
  y_train_anx,
  alpha = 1,
  family = "binomial"
)

lasso_anx_lambda <- lasso_anx_cv$lambda.min

lasso_anx_prob <- predict(
  lasso_anx_cv,
  s = lasso_anx_lambda,
  newx = x_test_anx,
  type = "response"
)

lasso_anx_pred <- ifelse(lasso_anx_prob >= 0.5, 1, 0)

lasso_anx_accuracy <- mean(lasso_anx_pred == y_test_anx)

lasso_anx_coef <- coef(
  lasso_anx_cv,
  s = lasso_anx_lambda
)

lasso_anx_table <- data.frame(
  Predictor = rownames(as.matrix(lasso_anx_coef)),
  Coefficient = round(as.numeric(lasso_anx_coef), 3)
)

lasso_anx_selected <- lasso_anx_table[
  lasso_anx_table$Coefficient != 0,
]

lasso_anx_results <- data.frame(
  Method = "Lasso Logistic Regression",
  Best_Lambda = round(lasso_anx_lambda, 3),
  Test_Accuracy = round(lasso_anx_accuracy, 3)
)

lasso_anx_results
lasso_anx_table
lasso_anx_selected

table(Predicted = lasso_anx_pred,
      Actual = y_test_anx)

plot(lasso_anx_cv,
     main = "Lasso CV: Anxiety")

barplot(abs(lasso_anx_selected$Coefficient[-1]),
        names.arg = lasso_anx_selected$Predictor[-1],
        main = "Selected Lasso Coefficients: Anxiety",
        ylab = "Absolute Coefficient",
        col = "lightgreen",
        las = 2)


# =========================================
# Final Chapter 7 Summary Tables
# =========================================

chapter7_summary <- data.frame(
  Question = c("Q1: Stress_Level",
               "Q2: Depression",
               "Q3: Anxiety"),
  Method = c("Ridge and Lasso",
             "Ridge Logistic Regression",
             "Lasso Logistic Regression"),
  Best_Lambda = round(c(lasso_stress_lambda,
                        ridge_dep_lambda,
                        lasso_anx_lambda), 3),
  Performance = c(
    paste("Best RMSE =", round(min(ridge_stress_rmse,
                                   lasso_stress_rmse), 3)),
    paste("Accuracy =", round(ridge_dep_accuracy, 3)),
    paste("Accuracy =", round(lasso_anx_accuracy, 3))
  )
)

chapter7_summary































#Chapter 8 =============================================================================================================Chapter 8

# =========================================
# Chapter 8 — Question 1: Which variables most strongly split individuals into different levels of Stress_Level using a regression tree?
# Regression Tree for Stress_Level
# Presentable Tree Graphs
# =========================================

library(rpart)
library(rpart.plot)

# -----------------------------------------
# Data Setup
# -----------------------------------------

tree_data <- subset(data_clean, select = -Person_ID)

tree_data$Gender <- as.factor(tree_data$Gender)
tree_data$Occupation <- as.factor(tree_data$Occupation)
tree_data$Physical_Activity <- as.factor(tree_data$Physical_Activity)

# -----------------------------------------
# Train/Test Split
# -----------------------------------------

set.seed(4230)

train_index <- sample(1:nrow(tree_data),
                      size = 0.70 * nrow(tree_data))

train_tree <- tree_data[train_index, ]
test_tree  <- tree_data[-train_index, ]

# =========================================
# Regression Tree Model
# =========================================

stress_tree <- rpart(
  Stress_Level ~ Sleep_Hours +
    Daily_Screen_Time +
    Social_Media_Usage +
    Work_Study_Hours +
    Caffeine_Intake +
    Social_Interaction_Score +
    Gender +
    Occupation +
    Physical_Activity +
    Depression +
    Anxiety +
    Burnout,
  data = train_tree,
  method = "anova"
)

summary(stress_tree)

# Presentable tree plot
par(mar = c(2, 2, 5, 2))

rpart.plot(stress_tree,
           main = "Regression Tree for Stress_Level",
           type = 0,
           extra = 101,
           fallen.leaves = FALSE,
           box.palette = "Blues",
           shadow.col = "gray",
           cex = 0.6,
           branch = 1,
           uniform = TRUE)

# =========================================
# Predictions
# =========================================

stress_pred <- predict(stress_tree,
                       newdata = test_tree)

stress_mse <- mean((test_tree$Stress_Level - stress_pred)^2)
stress_rmse <- sqrt(stress_mse)

cat("Regression Tree Test RMSE:",
    round(stress_rmse, 3), "\n")

# =========================================
# Variable Importance
# =========================================

stress_importance <- stress_tree$variable.importance

stress_importance <- data.frame(
  Variable = names(stress_importance),
  Importance = round(stress_importance, 3)
)

stress_importance

par(mar = c(12,4,4,2))

barplot(stress_importance$Importance,
        names.arg = stress_importance$Variable,
        main = "Variable Importance for Stress_Level",
        ylab = "Importance",
        col = "lightblue",
        las = 2,
        cex.names = 0.8)

# =========================================
# Pruning
# =========================================

printcp(stress_tree)

plotcp(stress_tree,
       main = "Cross-Validation Error for Tree Pruning")

best_cp <- stress_tree$cptable[
  which.min(stress_tree$cptable[, "xerror"]),
  "CP"
]

best_cp

pruned_stress_tree <- prune(
  stress_tree,
  cp = best_cp
)

# Presentable pruned tree plot
rpart.plot(pruned_stress_tree,
           main = "Pruned Regression Tree for Stress_Level",
           type = 0,
           extra = 101,
           fallen.leaves = FALSE,
           box.palette = "Greens",
           shadow.col = "gray",
           cex = 0.6,
           branch = 1,
           uniform = TRUE)

# =========================================
# Pruned Tree Predictions
# =========================================

pruned_pred <- predict(pruned_stress_tree,
                       newdata = test_tree)

pruned_mse <- mean((test_tree$Stress_Level - pruned_pred)^2)
pruned_rmse <- sqrt(pruned_mse)

cat("Pruned Tree Test RMSE:",
    round(pruned_rmse, 3), "\n")

# =========================================
# Final Results Table
# =========================================

tree_results <- data.frame(
  Model = c("Original Regression Tree",
            "Pruned Regression Tree"),
  RMSE = round(c(stress_rmse,
                 pruned_rmse), 3)
)

tree_results

# =========================================
# Chapter 8 — Question 2:
# Can a decision tree classify Burnout status using lifestyle and behavioral variables?
# =========================================

# -----------------------------------------
# Depression Tree
# -----------------------------------------

depression_tree <- rpart(
  Depression ~ Sleep_Hours + Stress_Level +
    Work_Study_Hours + Daily_Screen_Time +
    Social_Media_Usage + Night_Usage +
    Caffeine_Intake + Social_Interaction_Score +
    Gender + Occupation + Physical_Activity,
  data = train_tree,
  method = "class"
)

rpart.plot(depression_tree,
           main = "Classification Tree for Depression",
           type = 0,
           extra = 104,
           fallen.leaves = FALSE,
           box.palette = "Blues",
           shadow.col = "gray",
           cex = 0.6,
           branch = 1,
           uniform = TRUE)

depression_pred <- predict(depression_tree,
                           newdata = test_tree,
                           type = "class")

depression_table <- table(Predicted = depression_pred,
                          Actual = test_tree$Depression)

depression_accuracy <- mean(depression_pred == test_tree$Depression)

depression_table
cat("Depression Tree Accuracy:",
    round(depression_accuracy, 3), "\n")


# -----------------------------------------
# Anxiety Tree
# -----------------------------------------

anxiety_tree <- rpart(
  Anxiety ~ Sleep_Hours + Stress_Level +
    Work_Study_Hours + Daily_Screen_Time +
    Social_Media_Usage + Night_Usage +
    Caffeine_Intake + Social_Interaction_Score +
    Gender + Occupation + Physical_Activity,
  data = train_tree,
  method = "class"
)

rpart.plot(anxiety_tree,
           main = "Classification Tree for Anxiety",
           type = 0,
           extra = 104,
           fallen.leaves = FALSE,
           box.palette = "Blues",
           shadow.col = "gray",
           cex = 0.6,
           branch = 1,
           uniform = TRUE)

anxiety_pred <- predict(anxiety_tree,
                        newdata = test_tree,
                        type = "class")

anxiety_table <- table(Predicted = anxiety_pred,
                       Actual = test_tree$Anxiety)

anxiety_accuracy <- mean(anxiety_pred == test_tree$Anxiety)

anxiety_table
cat("Anxiety Tree Accuracy:",
    round(anxiety_accuracy, 3), "\n")


# -----------------------------------------
# Burnout Tree
# -----------------------------------------

burnout_tree <- rpart(
  Burnout ~ Sleep_Hours + Stress_Level +
    Work_Study_Hours + Daily_Screen_Time +
    Social_Media_Usage + Night_Usage +
    Caffeine_Intake + Social_Interaction_Score +
    Gender + Occupation + Physical_Activity,
  data = train_tree,
  method = "class"
)

rpart.plot(burnout_tree,
           main = "Classification Tree for Burnout",
           type = 0,
           extra = 104,
           fallen.leaves = FALSE,
           box.palette = "Blues",
           shadow.col = "gray",
           cex = 0.6,
           branch = 1,
           uniform = TRUE)

burnout_pred <- predict(burnout_tree,
                        newdata = test_tree,
                        type = "class")

burnout_table <- table(Predicted = burnout_pred,
                       Actual = test_tree$Burnout)

burnout_accuracy <- mean(burnout_pred == test_tree$Burnout)

burnout_table
cat("Burnout Tree Accuracy:",
    round(burnout_accuracy, 3), "\n")


# -----------------------------------------
# Classification Results Comparison
# -----------------------------------------

mental_health_tree_results <- data.frame(
  Outcome = c("Depression", "Anxiety", "Burnout"),
  Accuracy = round(c(depression_accuracy,
                     anxiety_accuracy,
                     burnout_accuracy), 3)
)

mental_health_tree_results

barplot(mental_health_tree_results$Accuracy,
        names.arg = mental_health_tree_results$Outcome,
        main = "Decision Tree Accuracy by Mental-Health Outcome",
        ylab = "Accuracy",
        col = c("lightblue", "lightgreen", "lightgray"),
        ylim = c(0, 1))

best_outcome <- mental_health_tree_results$Outcome[
  which.max(mental_health_tree_results$Accuracy)
]

cat("The easiest mental-health outcome to classify is:",
    best_outcome, "\n")





























#Chapter 9 =============================================================================================================Chapter 9

# =========================================
# Chapter 9 — Tree Ensembles
# Question 1:
# Which ensemble method provides the best
# prediction performance for Burnout?
# =========================================

# Required packages
library(randomForest)
library(gbm)

# -----------------------------------------
# Data Setup
# -----------------------------------------

ensemble_data <- subset(data_clean, select = -Person_ID)

ensemble_data$Gender <- as.factor(ensemble_data$Gender)
ensemble_data$Occupation <- as.factor(ensemble_data$Occupation)
ensemble_data$Physical_Activity <- as.factor(ensemble_data$Physical_Activity)

ensemble_data$Burnout <- as.factor(ensemble_data$Burnout)

# -----------------------------------------
# Train/Test Split
# -----------------------------------------

set.seed(4230)

train_index <- sample(1:nrow(ensemble_data),
                      size = 0.70 * nrow(ensemble_data))

train_ensemble <- ensemble_data[train_index, ]
test_ensemble  <- ensemble_data[-train_index, ]

# =========================================
# Bagging
# =========================================

set.seed(4230)

bagging_model <- randomForest(
  Burnout ~ Sleep_Hours +
    Stress_Level +
    Work_Study_Hours +
    Daily_Screen_Time +
    Social_Media_Usage +
    Night_Usage +
    Caffeine_Intake +
    Social_Interaction_Score +
    Gender +
    Occupation +
    Physical_Activity,
  data = train_ensemble,
  mtry = 11,
  importance = TRUE
)

bagging_model

# Predictions
bagging_pred <- predict(
  bagging_model,
  newdata = test_ensemble
)

# Confusion matrix
bagging_table <- table(
  Predicted = bagging_pred,
  Actual = test_ensemble$Burnout
)

bagging_table

# Accuracy
bagging_accuracy <- mean(
  bagging_pred == test_ensemble$Burnout
)

cat("Bagging Accuracy:",
    round(bagging_accuracy, 3), "\n")

# OOB Error
bagging_oob <- tail(bagging_model$err.rate[,1], 1)

cat("Bagging OOB Error:",
    round(bagging_oob, 3), "\n")

# Variable importance
varImpPlot(bagging_model,
           main = "Bagging Variable Importance")


# =========================================
# Random Forest
# =========================================

set.seed(4230)

rf_model <- randomForest(
  Burnout ~ Sleep_Hours +
    Stress_Level +
    Work_Study_Hours +
    Daily_Screen_Time +
    Social_Media_Usage +
    Night_Usage +
    Caffeine_Intake +
    Social_Interaction_Score +
    Gender +
    Occupation +
    Physical_Activity,
  data = train_ensemble,
  importance = TRUE
)

rf_model

# Predictions
rf_pred <- predict(
  rf_model,
  newdata = test_ensemble
)

# Confusion matrix
rf_table <- table(
  Predicted = rf_pred,
  Actual = test_ensemble$Burnout
)

rf_table

# Accuracy
rf_accuracy <- mean(
  rf_pred == test_ensemble$Burnout
)

cat("Random Forest Accuracy:",
    round(rf_accuracy, 3), "\n")

# OOB Error
rf_oob <- tail(rf_model$err.rate[,1], 1)

cat("Random Forest OOB Error:",
    round(rf_oob, 3), "\n")

# Variable importance
varImpPlot(rf_model,
           main = "Random Forest Variable Importance")


# =========================================
# Boosting
# =========================================

# Convert response to numeric for gbm
train_ensemble$Burnout_num <- ifelse(
  train_ensemble$Burnout == 1,
  1,
  0
)

test_ensemble$Burnout_num <- ifelse(
  test_ensemble$Burnout == 1,
  1,
  0
)

set.seed(4230)

boost_model <- gbm(
  Burnout_num ~ Sleep_Hours +
    Stress_Level +
    Work_Study_Hours +
    Daily_Screen_Time +
    Social_Media_Usage +
    Night_Usage +
    Caffeine_Intake +
    Social_Interaction_Score +
    Gender +
    Occupation +
    Physical_Activity,
  data = train_ensemble,
  distribution = "bernoulli",
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.01,
  cv.folds = 5
)

summary(boost_model)

# Set best number of trees manually
best_trees <- 1000

best_trees

# Predictions
boost_prob <- predict(
  boost_model,
  newdata = test_ensemble,
  n.trees = best_trees,
  type = "response"
)

boost_pred <- ifelse(boost_prob > 0.5, 1, 0)

boost_pred <- as.factor(boost_pred)

# Confusion matrix
boost_table <- table(
  Predicted = boost_pred,
  Actual = test_ensemble$Burnout
)

boost_table

# Accuracy
boost_accuracy <- mean(
  boost_pred == test_ensemble$Burnout
)

cat("Boosting Accuracy:",
    round(boost_accuracy, 3), "\n")

# -----------------------------------------
# Boosting Variable Importance
# -----------------------------------------

boost_importance <- summary(boost_model,
                            n.trees = best_trees,
                            plotit = FALSE)

boost_importance

par(mar = c(12,4,4,2))

barplot(boost_importance$rel.inf,
        names.arg = boost_importance$var,
        main = "Boosting Variable Importance",
        ylab = "Relative Influence",
        col = "lightpink",
        las = 2)


# =========================================
# Ensemble Comparison
# =========================================

ensemble_results <- data.frame(
  Method = c("Bagging",
             "Random Forest",
             "Boosting"),
  Accuracy = round(c(
    bagging_accuracy,
    rf_accuracy,
    boost_accuracy
  ), 3),
  OOB_Error = c(
    round(bagging_oob, 3),
    round(rf_oob, 3),
    NA
  )
)

ensemble_results

# -----------------------------------------
# Accuracy Comparison Plot
# -----------------------------------------

barplot(ensemble_results$Accuracy,
        names.arg = ensemble_results$Method,
        main = "Ensemble Method Accuracy Comparison",
        ylab = "Accuracy",
        col = c("lightblue",
                "lightgreen",
                "lightgray"),
        ylim = c(0,1))

# -----------------------------------------
# OOB Error Plot
# -----------------------------------------

oob_values <- c(bagging_oob, rf_oob)

barplot(oob_values,
        names.arg = c("Bagging",
                      "Random Forest"),
        main = "OOB Error Comparison",
        ylab = "OOB Error",
        col = c("lightblue",
                "lightgreen"))

# =========================================
# Final Results
# =========================================

best_method <- ensemble_results$Method[
  which.max(ensemble_results$Accuracy)
]

cat("Best Ensemble Method:",
    best_method, "\n")





























#Chapter 10 ============================================================================================================Chapter 10

# =========================================
# Chapter 10 — Support Vector Machines
# Question 1:
# Which mental-health outcome (Depression, Anxiety, or Burnout)
# is classified most accurately using SVMs?
# =========================================

library(e1071)
library(ggplot2)

# -----------------------------------------
# Data Setup
# -----------------------------------------

svm_data <- subset(data_clean, select = -Person_ID)

svm_data$Gender <- as.factor(svm_data$Gender)
svm_data$Occupation <- as.factor(svm_data$Occupation)
svm_data$Physical_Activity <- as.factor(svm_data$Physical_Activity)

svm_data$Depression <- as.factor(svm_data$Depression)
svm_data$Anxiety <- as.factor(svm_data$Anxiety)
svm_data$Burnout <- as.factor(svm_data$Burnout)

# -----------------------------------------
# Train/Test Split
# -----------------------------------------

set.seed(4230)

train_index <- sample(1:nrow(svm_data),
                      size = 0.70 * nrow(svm_data))

train_svm <- svm_data[train_index, ]
test_svm  <- svm_data[-train_index, ]


# =========================================
# Depression SVM
# =========================================

depression_svm <- svm(
  Depression ~ Sleep_Hours + Stress_Level +
    Work_Study_Hours + Daily_Screen_Time +
    Social_Media_Usage + Night_Usage +
    Caffeine_Intake + Social_Interaction_Score +
    Gender + Occupation + Physical_Activity,
  data = train_svm,
  kernel = "radial",
  cost = 1,
  gamma = 0.1,
  scale = TRUE
)

depression_pred <- predict(depression_svm,
                           newdata = test_svm)

depression_table <- table(Predicted = depression_pred,
                          Actual = test_svm$Depression)

depression_accuracy <- mean(depression_pred == test_svm$Depression)

depression_table

cat("Depression SVM Accuracy:",
    round(depression_accuracy, 3), "\n")


# =========================================
# Anxiety SVM
# =========================================

anxiety_svm <- svm(
  Anxiety ~ Sleep_Hours + Stress_Level +
    Work_Study_Hours + Daily_Screen_Time +
    Social_Media_Usage + Night_Usage +
    Caffeine_Intake + Social_Interaction_Score +
    Gender + Occupation + Physical_Activity,
  data = train_svm,
  kernel = "radial",
  cost = 1,
  gamma = 0.1,
  scale = TRUE
)

anxiety_pred <- predict(anxiety_svm,
                        newdata = test_svm)

anxiety_table <- table(Predicted = anxiety_pred,
                       Actual = test_svm$Anxiety)

anxiety_accuracy <- mean(anxiety_pred == test_svm$Anxiety)

anxiety_table

cat("Anxiety SVM Accuracy:",
    round(anxiety_accuracy, 3), "\n")


# =========================================
# Burnout SVM
# =========================================

burnout_svm <- svm(
  Burnout ~ Sleep_Hours + Stress_Level +
    Work_Study_Hours + Daily_Screen_Time +
    Social_Media_Usage + Night_Usage +
    Caffeine_Intake + Social_Interaction_Score +
    Gender + Occupation + Physical_Activity,
  data = train_svm,
  kernel = "radial",
  cost = 1,
  gamma = 0.1,
  scale = TRUE
)

burnout_pred <- predict(burnout_svm,
                        newdata = test_svm)

burnout_table <- table(Predicted = burnout_pred,
                       Actual = test_svm$Burnout)

burnout_accuracy <- mean(burnout_pred == test_svm$Burnout)

burnout_table

cat("Burnout SVM Accuracy:",
    round(burnout_accuracy, 3), "\n")


# =========================================
# SVM Accuracy Comparison
# =========================================

svm_results <- data.frame(
  Outcome = c("Depression", "Anxiety", "Burnout"),
  Accuracy = round(c(depression_accuracy,
                     anxiety_accuracy,
                     burnout_accuracy), 3)
)

svm_results

barplot(svm_results$Accuracy,
        names.arg = svm_results$Outcome,
        main = "SVM Accuracy by Mental-Health Outcome",
        ylab = "Accuracy",
        col = c("lightblue",
                "lightgreen",
                "lightgray"),
        ylim = c(0, 1))

best_svm_outcome <- svm_results$Outcome[
  which.max(svm_results$Accuracy)
]

cat("The mental-health outcome classified most accurately by SVM is:",
    best_svm_outcome, "\n")


# =========================================
# Function for Maximal Margin / Linear SVM Plot
# Similar to Python plot_svm_decision_boundary()
# =========================================

plot_svm_decision_boundary <- function(response_var, title_text) {
  
  # Use only two predictors for 2D visualization
  plot_data <- data.frame(
    Stress_Level = data_clean$Stress_Level,
    Sleep_Hours = data_clean$Sleep_Hours,
    Outcome = as.factor(data_clean[[response_var]])
  )
  
  # Linear SVM for visualization
  clf <- svm(
    Outcome ~ Stress_Level + Sleep_Hours,
    data = plot_data,
    kernel = "linear",
    cost = 10,
    scale = TRUE,
    decision.values = TRUE
  )
  
  # Prediction grid
  x_range <- seq(min(plot_data$Stress_Level),
                 max(plot_data$Stress_Level),
                 length.out = 200)
  
  y_range <- seq(min(plot_data$Sleep_Hours),
                 max(plot_data$Sleep_Hours),
                 length.out = 200)
  
  grid <- expand.grid(
    Stress_Level = x_range,
    Sleep_Hours = y_range
  )
  
  # Predicted classes on grid
  grid$pred <- predict(clf,
                       newdata = grid)
  
  # Decision values for boundary and margins
  grid$decision <- attributes(
    predict(clf,
            newdata = grid,
            decision.values = TRUE)
  )$decision.values[, 1]
  
  # Support vectors
  support_vectors <- plot_data[clf$index, ]
  
  # Plot
  ggplot() +
    geom_tile(data = grid,
              aes(x = Stress_Level,
                  y = Sleep_Hours,
                  fill = pred),
              alpha = 0.25) +
    
    geom_contour(data = grid,
                 aes(x = Stress_Level,
                     y = Sleep_Hours,
                     z = decision),
                 breaks = 0,
                 color = "black",
                 linewidth = 1.2) +
    
    geom_contour(data = grid,
                 aes(x = Stress_Level,
                     y = Sleep_Hours,
                     z = decision),
                 breaks = c(-1, 1),
                 color = "black",
                 linetype = "dashed",
                 linewidth = 0.8) +
    
    geom_point(data = plot_data,
               aes(x = Stress_Level,
                   y = Sleep_Hours,
                   color = Outcome),
               size = 2,
               alpha = 0.75) +
    
    geom_point(data = support_vectors,
               aes(x = Stress_Level,
                   y = Sleep_Hours),
               shape = 21,
               size = 4,
               stroke = 1.2,
               color = "black",
               fill = NA) +
    
    labs(title = title_text,
         x = "Stress Level",
         y = "Sleep Hours",
         fill = "Predicted Class",
         color = "Observed Class") +
    
    theme_minimal()
}


# =========================================
# Maximal Margin / Linear SVM Visualizations
# =========================================

plot_svm_decision_boundary(
  response_var = "Depression",
  title_text = "Linear SVM, C = 10: Depression"
)

plot_svm_decision_boundary(
  response_var = "Anxiety",
  title_text = "Linear SVM, C = 10: Anxiety"
)

plot_svm_decision_boundary(
  response_var = "Burnout",
  title_text = "Linear SVM, C = 10: Burnout"
)


























#Chapter 11 ============================================================================================================Chapter 11

# =========================================
# Chapter 11 — K-Nearest Neighbors
# Question:
# Which mental-health outcome (Depression, Anxiety, or Burnout)
# is predicted most accurately using KNN?
# Corrected to avoid leakage
# =========================================

library(class)
library(caret)

# -----------------------------------------
# Data Setup
# -----------------------------------------

knn_data <- subset(data_clean, select = -Person_ID)

knn_data$Gender <- as.factor(knn_data$Gender)
knn_data$Occupation <- as.factor(knn_data$Occupation)
knn_data$Physical_Activity <- as.factor(knn_data$Physical_Activity)

knn_data$Depression <- as.factor(knn_data$Depression)
knn_data$Anxiety <- as.factor(knn_data$Anxiety)
knn_data$Burnout <- as.factor(knn_data$Burnout)

# -----------------------------------------
# Remove outcome variables from predictors
# -----------------------------------------

predictor_data <- subset(
  knn_data,
  select = -c(Depression, Anxiety, Burnout)
)

# -----------------------------------------
# Dummy Encoding
# -----------------------------------------

dummy_model <- dummyVars(~ ., data = predictor_data)

knn_x <- predict(dummy_model, newdata = predictor_data)

# -----------------------------------------
# Standardization
# -----------------------------------------

preprocess_model <- preProcess(
  knn_x,
  method = c("center", "scale")
)

knn_x_scaled <- predict(preprocess_model, knn_x)

# -----------------------------------------
# Train/Test Split
# -----------------------------------------

set.seed(4230)

train_index <- sample(
  1:nrow(knn_x_scaled),
  size = 0.70 * nrow(knn_x_scaled)
)

x_train <- knn_x_scaled[train_index, ]
x_test  <- knn_x_scaled[-train_index, ]

# =========================================
# Function for KNN Evaluation
# =========================================

evaluate_knn <- function(response_var) {
  
  y <- knn_data[[response_var]]
  
  y_train <- y[train_index]
  y_test  <- y[-train_index]
  
  k_values <- seq(1, 25, by = 2)
  
  accuracy_values <- rep(NA, length(k_values))
  
  for(i in 1:length(k_values)) {
    
    knn_pred <- knn(
      train = x_train,
      test = x_test,
      cl = y_train,
      k = k_values[i]
    )
    
    accuracy_values[i] <- mean(knn_pred == y_test)
  }
  
  results <- data.frame(
    K = k_values,
    Accuracy = round(accuracy_values, 3)
  )
  
  best_k <- results$K[which.max(results$Accuracy)]
  
  final_pred <- knn(
    train = x_train,
    test = x_test,
    cl = y_train,
    k = best_k
  )
  
  final_accuracy <- mean(final_pred == y_test)
  
  confusion <- table(
    Predicted = final_pred,
    Actual = y_test
  )
  
  list(
    results = results,
    best_k = best_k,
    accuracy = final_accuracy,
    confusion = confusion
  )
}

# =========================================
# Depression KNN
# =========================================

depression_knn <- evaluate_knn("Depression")

depression_knn$results
depression_knn$confusion

cat("Best K for Depression:",
    depression_knn$best_k, "\n")

cat("Depression Accuracy:",
    round(depression_knn$accuracy, 3), "\n")

plot(depression_knn$results$K,
     depression_knn$results$Accuracy,
     type = "b",
     pch = 19,
     col = "blue",
     main = "KNN Accuracy for Depression",
     xlab = "K",
     ylab = "Accuracy")

abline(v = depression_knn$best_k,
       col = "red",
       lty = 2,
       lwd = 2)

# =========================================
# Anxiety KNN
# =========================================

anxiety_knn <- evaluate_knn("Anxiety")

anxiety_knn$results
anxiety_knn$confusion

cat("Best K for Anxiety:",
    anxiety_knn$best_k, "\n")

cat("Anxiety Accuracy:",
    round(anxiety_knn$accuracy, 3), "\n")

plot(anxiety_knn$results$K,
     anxiety_knn$results$Accuracy,
     type = "b",
     pch = 19,
     col = "darkgreen",
     main = "KNN Accuracy for Anxiety",
     xlab = "K",
     ylab = "Accuracy")

abline(v = anxiety_knn$best_k,
       col = "red",
       lty = 2,
       lwd = 2)

# =========================================
# Burnout KNN
# =========================================

burnout_knn <- evaluate_knn("Burnout")

burnout_knn$results
burnout_knn$confusion

cat("Best K for Burnout:",
    burnout_knn$best_k, "\n")

cat("Burnout Accuracy:",
    round(burnout_knn$accuracy, 3), "\n")

plot(burnout_knn$results$K,
     burnout_knn$results$Accuracy,
     type = "b",
     pch = 19,
     col = "purple",
     main = "KNN Accuracy for Burnout",
     xlab = "K",
     ylab = "Accuracy")

abline(v = burnout_knn$best_k,
       col = "red",
       lty = 2,
       lwd = 2)

# =========================================
# KNN Comparison
# =========================================

knn_comparison <- data.frame(
  Outcome = c("Depression",
              "Anxiety",
              "Burnout"),
  Best_K = c(
    depression_knn$best_k,
    anxiety_knn$best_k,
    burnout_knn$best_k
  ),
  Accuracy = round(c(
    depression_knn$accuracy,
    anxiety_knn$accuracy,
    burnout_knn$accuracy
  ), 3)
)

knn_comparison

barplot(knn_comparison$Accuracy,
        names.arg = knn_comparison$Outcome,
        main = "KNN Accuracy by Mental-Health Outcome",
        ylab = "Accuracy",
        col = c("lightblue",
                "lightgreen",
                "lightgray"),
        ylim = c(0, 1))

best_knn_outcome <- knn_comparison$Outcome[
  which.max(knn_comparison$Accuracy)
]

cat("The mental-health outcome predicted most accurately by KNN is:",
    best_knn_outcome, "\n")





























#Chapter 12 ============================================================================================================Chapter 12

# =========================================
# Chapter 12 — Principal Component Analysis
# Question:
# How many principal components are needed
# to explain most of the variation in the dataset?
# =========================================

library(ggplot2)

# -----------------------------------------
# Data Setup
# -----------------------------------------

pca_data <- subset(data_clean, select = -Person_ID)

# Convert categorical variables to factors
pca_data$Gender <- as.factor(pca_data$Gender)
pca_data$Occupation <- as.factor(pca_data$Occupation)
pca_data$Physical_Activity <- as.factor(pca_data$Physical_Activity)

# -----------------------------------------
# Dummy Encoding
# -----------------------------------------

dummy_model <- model.matrix(~ ., data = pca_data)[, -1]

# -----------------------------------------
# Standardization
# -----------------------------------------

pca_scaled <- scale(dummy_model)

# =========================================
# PCA Model
# =========================================

pca_model <- prcomp(
  pca_scaled,
  center = TRUE,
  scale. = TRUE
)

summary(pca_model)

# =========================================
# Eigenvalues
# =========================================

eigenvalues <- pca_model$sdev^2

eigen_table <- data.frame(
  Principal_Component = paste0("PC", 1:length(eigenvalues)),
  Eigenvalue = round(eigenvalues, 3)
)

eigen_table

# =========================================
# Variance Explained
# =========================================

variance_explained <- pca_model$sdev^2 /
  sum(pca_model$sdev^2)

variance_table <- data.frame(
  Principal_Component = paste0("PC", 1:length(variance_explained)),
  Proportion_Variance = round(variance_explained, 3),
  Cumulative_Variance = round(cumsum(variance_explained), 3)
)

variance_table

# =========================================
# Scree Plot
# =========================================

plot(eigenvalues,
     type = "b",
     pch = 19,
     col = "blue",
     main = "Scree Plot",
     xlab = "Principal Component",
     ylab = "Eigenvalue")

abline(h = 1,
       col = "red",
       lty = 2)

# =========================================
# Cumulative Variance Plot
# =========================================

plot(cumsum(variance_explained),
     type = "b",
     pch = 19,
     col = "darkgreen",
     ylim = c(0,1),
     main = "Cumulative Variance Explained",
     xlab = "Number of Principal Components",
     ylab = "Cumulative Proportion of Variance")

abline(h = 0.80,
       col = "red",
       lty = 2)

# =========================================
# Loadings
# =========================================

loading_matrix <- round(
  pca_model$rotation,
  3
)

loading_matrix

# =========================================
# PCA Biplot
# =========================================

biplot(pca_model,
       scale = 0,
       cex = 0.6)

# =========================================
# Number of Components Needed
# =========================================

components_80 <- which(
  cumsum(variance_explained) >= 0.80
)[1]

cat("Number of components needed to explain at least 80% of the variance:",
    components_80, "\n")

# =========================================
# Final PCA Summary Table
# =========================================

pca_summary_table <- data.frame(
  Components_For_80_Percent = components_80,
  Total_Components = length(eigenvalues)
)

pca_summary_table





























#Chapter 13 ============================================================================================================Chapter 13

# =========================================
# Chapter 13 — Neural Networks
# Question:
# Which mental-health outcome (Depression, Anxiety, or Burnout)
# is predicted most accurately using neural networks?
# =========================================

# install.packages(c("tidyverse", "neuralnet", "caret"), dependencies = TRUE)

library(tidyverse)
library(neuralnet)
library(caret)

# -----------------------------------------
# Data Setup
# -----------------------------------------

nn_data <- data_clean %>%
  select(-Person_ID)

nn_data$Depression  <- as.numeric(as.character(nn_data$Depression))
nn_data$Anxiety     <- as.numeric(as.character(nn_data$Anxiety))
nn_data$Burnout     <- as.numeric(as.character(nn_data$Burnout))
nn_data$Night_Usage <- as.numeric(as.character(nn_data$Night_Usage))

# Use recommended predictors
nn_model_data <- nn_data %>%
  select(Sleep_Hours,
         Stress_Level,
         Daily_Screen_Time,
         Caffeine_Intake,
         Work_Study_Hours,
         Night_Usage,
         Depression,
         Anxiety,
         Burnout)

# -----------------------------------------
# Standardize Predictor Variables
# -----------------------------------------

predictor_vars <- c("Sleep_Hours",
                    "Stress_Level",
                    "Daily_Screen_Time",
                    "Caffeine_Intake",
                    "Work_Study_Hours",
                    "Night_Usage")

preprocess_model <- preProcess(nn_model_data[, predictor_vars],
                               method = c("center", "scale"))

scaled_predictors <- predict(preprocess_model,
                             nn_model_data[, predictor_vars])

nn_model_data[, predictor_vars] <- scaled_predictors

# -----------------------------------------
# Train/Test Split — 70/30
# -----------------------------------------

set.seed(4230)

data_rows <- floor(0.70 * nrow(nn_model_data))
train_indices <- sample(1:nrow(nn_model_data), data_rows)

train_data <- nn_model_data[train_indices, ]
test_data  <- nn_model_data[-train_indices, ]

# =========================================
# Depression Neural Network
# =========================================

model_depression <- neuralnet(
  Depression ~ Sleep_Hours + Stress_Level +
    Daily_Screen_Time + Caffeine_Intake +
    Work_Study_Hours + Night_Usage,
  data = train_data,
  hidden = 2,
  linear.output = FALSE,
  stepmax = 1e6
)

plot(model_depression,
     rep = "best",
     main = "Neural Network for Depression")

pred_depression <- predict(model_depression, test_data)

prediction_depression <- ifelse(pred_depression >= 0.5, 1, 0)

table(Actual = test_data$Depression,
      Predicted = prediction_depression)

accuracy_depression <- mean(test_data$Depression == prediction_depression) * 100

cat("Depression Neural Network Accuracy:",
    round(accuracy_depression, 3), "%\n")


# =========================================
# Anxiety Neural Network
# =========================================

model_anxiety <- neuralnet(
  Anxiety ~ Sleep_Hours + Stress_Level +
    Daily_Screen_Time + Caffeine_Intake +
    Work_Study_Hours + Night_Usage,
  data = train_data,
  hidden = 2,
  linear.output = FALSE,
  stepmax = 1e6
)

plot(model_anxiety,
     rep = "best",
     main = "Neural Network for Anxiety")

pred_anxiety <- predict(model_anxiety, test_data)

prediction_anxiety <- ifelse(pred_anxiety >= 0.5, 1, 0)

table(Actual = test_data$Anxiety,
      Predicted = prediction_anxiety)

accuracy_anxiety <- mean(test_data$Anxiety == prediction_anxiety) * 100

cat("Anxiety Neural Network Accuracy:",
    round(accuracy_anxiety, 3), "%\n")


# =========================================
# Burnout Neural Network
# =========================================

model_burnout <- neuralnet(
  Burnout ~ Sleep_Hours + Stress_Level +
    Daily_Screen_Time + Caffeine_Intake +
    Work_Study_Hours + Night_Usage,
  data = train_data,
  hidden = 2,
  linear.output = FALSE,
  stepmax = 1e6
)

plot(model_burnout,
     rep = "best",
     main = "Neural Network for Burnout")

pred_burnout <- predict(model_burnout, test_data)

prediction_burnout <- ifelse(pred_burnout >= 0.5, 1, 0)

table(Actual = test_data$Burnout,
      Predicted = prediction_burnout)

accuracy_burnout <- mean(test_data$Burnout == prediction_burnout) * 100

cat("Burnout Neural Network Accuracy:",
    round(accuracy_burnout, 3), "%\n")


# =========================================
# Final Neural Network Comparison
# =========================================

nn_comparison <- data.frame(
  Outcome = c("Depression", "Anxiety", "Burnout"),
  Accuracy = round(c(accuracy_depression,
                     accuracy_anxiety,
                     accuracy_burnout), 3)
)

nn_comparison

barplot(nn_comparison$Accuracy,
        names.arg = nn_comparison$Outcome,
        main = "Neural Network Accuracy by Mental-Health Outcome",
        ylab = "Accuracy (%)",
        col = c("lightblue", "lightgreen", "lightgray"),
        ylim = c(0, 100))

best_nn_outcome <- nn_comparison$Outcome[
  which.max(nn_comparison$Accuracy)
]

cat("The mental-health outcome predicted most accurately by neural networks is:",
    best_nn_outcome, "\n")
