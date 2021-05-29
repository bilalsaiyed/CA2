# Analyzing and dealing with the data related to the "Stroke"
# for visualizing and predicting the functioning & working 
# of the patients heart and implementing the model for the chances 
# of "Stroke" with the variables present.

# Importing & Installing the required packages & libraries
# wherever needed for analyzing and visualizing
library(dplyr)
# Importing the dataset into a DF
stroke_data <- read.csv("stroke.csv", na ="")

# Converting categorical variables to factors
stroke_data$gender = as.factor(stroke_data$gender)
stroke_data$hypertension = as.factor(stroke_data$hypertension)
stroke_data$heart_disease = as.factor(stroke_data$heart_disease)
stroke_data$ever_married = as.factor(stroke_data$ever_married)
stroke_data$work_type = as.factor(stroke_data$work_type)
stroke_data$Residence_type = as.factor(stroke_data$Residence_type)
stroke_data$smoking_status = as.factor(stroke_data$smoking_status)
stroke_data$stroke = as.factor(stroke_data$stroke)

# Rename the residence_type variable
colnames(stroke_data)[colnames(stroke_data)=="Residence_type"]<-"residence_type"

# ID and Date attributes 
# As these attributes were used to identify the patients
# records only and does not affect the prediction, 
# hence they can be dropped for further processing
stroke_data <- stroke_data[, c(2,3,4,5,6,7,8,9,10,11,12)]
str(stroke_data)

# Gender attribute:
# As there is only 1 entry with "Other" value, hence remove
# patients who were categorized as ‘Other’ in the gender column
stroke_data = stroke_data[stroke_data$gender!= 'Other',]
# Convert from Female/Male to 0/1
gender_col <- ifelse(stroke_data$gender == 'Male', 1, 0)
stroke_data$gender <- as.factor(gender_col)

# BMI attribute:
# Removing the N/A from bmi attribute which account for 3.9% of all values
stroke_data <- stroke_data[stroke_data$bmi != "N/A", ]
str(stroke_data)
# Removed 201 rows with N/A value from the data frame
# Convert the BMI attribute from character to numeric
stroke_data["bmi"] <- as.numeric(stroke_data$bmi)

# Visualize the missing data
# install.packages("VIM")
library(VIM)
missing_values <- aggr(stroke_data, prop = FALSE, numbers = TRUE)

# Display the summary of missing data
summary(missing_values)

# ever_married attribute
# Convert from Yes/No to 1/0
married_col <- ifelse(stroke_data$ever_married=='Yes', 1, 0)
stroke_data$ever_married <- as.factor(married_col)

# Residence_type attribute
# convert from Urban/Rural to 1/0
residence_type_col <- ifelse(stroke_data$residence_type =='Urban', 1, 0)
stroke_data$residence_type <- as.factor(residence_type_col)

# Convert work_type attribute
stroke_data$work_type <- as.factor(unclass(stroke_data$work_type))

# Convert smoking_status attribute
stroke_data$smoking_status <- as.factor(unclass(stroke_data$smoking_status))

# Create a new variable for diabetic status
# 1 Normal range of Glucose level <= 140 mmol/L
# 2 Pre-diabetes: 140 to 200 mmol/L
# 3 Diabetic range of Glucose level > 200 mmol/L
diabetes_col <- cut(stroke_data$avg_glucose_level,
                    breaks = c(0, 140, 199, Inf), 
                    labels = c(1, 2, 3), 
                    right = FALSE,
                    order = TRUE)
stroke_data$diabetes_categ <- as.factor(diabetes_col)
str(stroke_data)

# Create a new variable indicating BMI categories
# 1 Underweight (BMI< 18.5)
# 2 Normal (BMI>=18.5)
# 3 Overweight (BMI>=25 & BMI<30)
# 4 Obese (BMI>= 30)
bmi_col <- cut(stroke_data$bmi,
               breaks = c(0, 18.5, 25, 30, Inf), 
               labels = c(1, 2, 3, 4), 
               right = TRUE,
               order = TRUE)
stroke_data$bmi_categ <- as.factor(bmi_col)

# Create a new variable for Overweight people

overweight_col <- cut(stroke_data$bmi,
                      breaks = c(0, 25, Inf), 
                      labels = c(0, 1), 
                      right = TRUE,
                      order = FALSE)
stroke_data$overweight <- as.factor(overweight_col)

# Display the summary of the data
summary(stroke_data)

str(stroke_data)

# Convert the gender, hypertension, heart_disease, ever_married
# residence_type, stroke, overweight variables to numeric
# converting the stroke data into numeric


stroke_data <- stroke_data %>%
  mutate(gender = case_when(gender == 1 ~1,
                            gender == 0 ~0),
         hypertension = case_when(hypertension == 1 ~1,
                                  hypertension == 0 ~0),
         heart_disease = case_when(heart_disease == 1 ~1,
                                   heart_disease == 0 ~0),
         ever_married = case_when(ever_married == 1 ~1,
                                  ever_married == 0 ~0),
         residence_type = case_when(residence_type == 1 ~1,
                                    residence_type == 0 ~0),
         stroke = case_when(stroke == 1 ~1,
                            stroke == 0 ~0),
         overweight = case_when(overweight == 1 ~1,
                                overweight == 0 ~0)
         
  )
str(stroke_data)

# Create dummy variables for categorical columns:
# smoking_status, work_type, bmi_categ, diabetes_categ
dummy_cols <- c("bmi_categ","work_type","diabetes_categ","smoking_status")
library(fastDummies)
new_stroke_data <- dummy_cols(stroke_data, 
                              select_columns = dummy_cols, 
                              remove_first_dummy = FALSE,
                              remove_selected_columns = FALSE)
str(new_stroke_data)
summary(new_stroke_data)

# Convert the dummy variables to numeric
new_stroke_data[, dummy_cols] <-
  apply(new_stroke_data[, dummy_cols], 2, function(x) as.numeric(x))

str(new_stroke_data)
summary(new_stroke_data)

# Display the first six entries from the DF
head(new_stroke_data)

# Structure of DF
str(new_stroke_data)

# Verifying that it is a DF
class(new_stroke_data)

# Displaying the number of rows and columns
nrow(new_stroke_data)
ncol(new_stroke_data)

attach(new_stroke_data)
str(new_stroke_data)

attach(new_stroke_data)
# colnames(new_stroke_data)
# stepwise regression

# check correlation of variables
stroke_matrix <- cor (new_stroke_data[1:14])
# corrplot(stroke_matrix, method = "number")
library(corrplot)
corrplot(stroke_matrix, type = "lower")

# Initial model
fit <- lm(age ~ 
            hypertension + heart_disease + bmi + avg_glucose_level + stroke + 
            work_type_1 + work_type_2 + work_type_3 + work_type_4 + work_type_5+
            smoking_status_1 + smoking_status_2 + smoking_status_3 + 
            smoking_status_4,
          data = new_stroke_data)

summary(fit)
confint(fit)

# Outliers
library("car")
attach(new_stroke_data)

# qqplot continuous variables
qqPlot(bmi,
       main = "BMI")
qqPlot(avg_glucose_level,
       main = "Avg_Glucose_Level")

plot(fit, pch = 10, cex = 2, main="Influential observations ") 
abline(h = 4 * mean(fit, na.rm=T), col="red")  # add cutoff line
text(x = 1:length(fit) + 1, y = fit, 
     labels=ifelse(fit > 4 * mean(fit, na.rm = T), 
                   names(lm),""), col="red")

# list outliers' value
new_stroke_data[c(4030, 2020), c('age', 'bmi', 'avg_glucose_level')]
new_stroke_data[c(1145, 159), c('age', 'bmi', 'avg_glucose_level')]

# remove the clearly wrong collected data
# 4 rows are removed
new_stroke_data <- new_stroke_data[-c(4030, 2020, 1145, 159), ]

# the model improved a little

fit <- lm(age ~ hypertension + heart_disease + bmi + avg_glucose_level +  
            work_type_1 + work_type_2 + work_type_3 + work_type_4 +
            stroke + ever_married +
            smoking_status_1 + smoking_status_2 + smoking_status_3,
          data = new_stroke_data)
summary(fit)
# Remove bmi, smoking_status_2, smoking_status_3

# replace with dummy variables
fit <- lm(age ~ hypertension + heart_disease + ever_married +
            diabetes_categ_1 + diabetes_categ_2 + stroke +
            work_type_1 + work_type_2 + work_type_3 + work_type_4 +
            smoking_status_1 + overweight,
          data = new_stroke_data)
summary(fit)
confint(fit)

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # divide graph area in 2 columns
hist(new_stroke_data$age, main = "Normality proportion of Age", xlab = "Age")
qqnorm(new_stroke_data$age)
qqline(new_stroke_data$age)
par <- opar

# Add the records id as a variable
new_stroke_data <- cbind(row_id = rownames(new_stroke_data), new_stroke_data)
head(new_stroke_data)

# training & testing dataset
set.seed(1)
no_rows_data <- nrow(new_stroke_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE) 

training_data <- new_stroke_data[sample, ]
testing_data <- new_stroke_data[-sample, ]

fit <- lm(age ~ hypertension + heart_disease + ever_married +
            diabetes_categ_1 + diabetes_categ_2 + stroke +
            work_type_1 + work_type_2 + work_type_3 + work_type_4 +
            smoking_status_1 + overweight,
          data = training_data)

summary(fit)
confint(fit)

# visualization
library(car)
qqPlot(fit, labels=row.names(new_stroke_data), 
       id.method="identify", 
       simulate=TRUE, 
       main="Q-Q Plot")

training_data["4526",]
training_data["433",]

fitted(fit)["4526"]
fitted(fit)["433"]

# histogram of the studentized
# residuals and superimposes a normal curve, kernel-density curve, and rug plot
student_fit <- rstudent(fit)
hist(student_fit,
     breaks = 10,
     freq = FALSE,
     xlab = "Studentized Residual",
     main = "Distribution of errors")

rug(jitter(student_fit), col = "brown")

curve(dnorm(x, mean = mean(student_fit), 
            sd = sd(student_fit)),
      add = TRUE, col = "blue", lwd = 2)

lines(density(student_fit)$x, density(student_fit)$y, 
      col = "red", lwd = 2, lty = 2)

legend("topright", legend = c("normal curve", "kernel density curve"), 
       lty = 1:2, col = c("blue", "red"), cex = 0.7)

library(car)
outlierTest(fit)

# Remove 4526th record
# from new_stroke_data dataset
new_stroke_data <- subset(new_stroke_data, new_stroke_data$row_id != "4526")

# training & testing dataset
set.seed(1)
no_rows_data <- nrow(new_stroke_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE) 

training_data <- new_stroke_data[sample, ]
testing_data <- new_stroke_data[-sample, ]

fit <- lm(age ~ hypertension + heart_disease + ever_married +
            diabetes_categ_1 + diabetes_categ_2 + stroke +
            work_type_1 + work_type_2 + work_type_3 + work_type_4 +
            smoking_status_1 + overweight,
          data = training_data)
summary(fit)
outlierTest(fit)

# Remove 433rd record from new_stroke_data dataset
new_stroke_data <- subset(new_stroke_data, new_stroke_data$row_id != "433")

# training & testing dataset
set.seed(1)
no_rows_data <- nrow(new_stroke_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE) 

training_data <- new_stroke_data[sample, ]
testing_data <- new_stroke_data[-sample, ]

fit <- lm(age ~ hypertension + heart_disease + ever_married +
            diabetes_categ_1 + diabetes_categ_2 + stroke +
            work_type_1 + work_type_2 + work_type_3 + work_type_4 +
            smoking_status_1 + overweight,
          data = training_data)

summary(fit)
outlierTest(fit)

student_fit <- rstudent(fit)
hist(student_fit,
     breaks = 10,
     freq = FALSE,
     xlab = "Studentized Residual",
     main = "Distribution of errors")

rug(jitter(student_fit), col = "brown")

curve(dnorm(x, mean = mean(student_fit), 
            sd = sd(student_fit)),
      add = TRUE, col = "blue", lwd = 2)

lines(density(student_fit)$x, density(student_fit)$y, 
      col = "red", lwd = 2, lty = 2)

legend("topright", legend = c("normal curve", "kernel density curve"), 
       lty = 1:2, col = c("blue", "red"), cex = 0.7)

# CHeck for Linearity
#crPlots(fit)

# Influential observations
cutoff <- 4/(nrow(training_data) - length(fit$coefficients) - 2)
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

library(car)
#avPlots(fit, ask=FALSE)

# Influence plot
library(car)
influencePlot(fit, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

# Homoscedasticity
ncvTest(fit)

spreadLevelPlot(fit)

# subset
# many independent in this dataset are obviously adult related features,
# such as marital status, smoking status
# under age patients only account for a small proportion
str(new_stroke_data[new_stroke_data$age >= 18, ])

# so we adjust the dataset to adult subset
new_stroke_data <- new_stroke_data[new_stroke_data$age >= 18, ]
hist(new_stroke_data$age)

attach(new_stroke_data)
# Rebuild the model by taking square root
new_stroke_data$sqrt_age <- sqrt(new_stroke_data$age)
fit <- lm(sqrt_age ~ 
            hypertension + heart_disease +     
            ever_married + work_type_2 + work_type_3 + work_type_4 + 
            smoking_status_1 + 
            overweight + stroke + diabetes_categ_1 + diabetes_categ_2 ,
          data = new_stroke_data)
summary(fit)

# training & testing dataset
set.seed(1)
no_rows_data <- nrow(new_stroke_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE) 

training_data <- new_stroke_data[sample, ]
testing_data <- new_stroke_data[-sample, ]

# Global validation of linear model assumption
#library(gvlma)
#gvmodel <- gvlma(fit)
#summary(gvmodel)


# Multicollinearity
library(car)
vif(fit)

# We can check whether any of the variables indicate a multicollinearity problem
# if the value > 2
sqrt(vif(fit)) > 2

#Transforming variables
library(car)
summary(powerTransform(new_stroke_data$sqrt_age))

# Comparing models using AIC

sqrt_transform_age <- sqrt(training_data$age)
training_data$age_sqrt <- sqrt_transform_age

fit_model1 <- lm(age ~ hypertension + heart_disease +     
                   ever_married + work_type_2 + work_type_3 + work_type_4 + 
                   smoking_status_1 + 
                   overweight + stroke + diabetes_categ_1 + diabetes_categ_2, data=training_data)
fit_model2 <- lm(age_sqrt ~ hypertension + heart_disease +     
                   ever_married + work_type_2 + work_type_3 + work_type_4 + 
                   smoking_status_1 + 
                   overweight + stroke + diabetes_categ_1 + diabetes_categ_2, data=training_data)
AIC(fit_model1,fit_model2)

spreadLevelPlot(fit_model2)

# Comparing multiple models
# STEPWISE REGRESSION
library(MASS)
fit_test <- lm(age ~ hypertension + heart_disease +     
                 ever_married + work_type_2 + work_type_3 + work_type_4 + 
                 smoking_status_1 + 
                 overweight + stroke + diabetes_categ_1 + diabetes_categ_2, data=training_data)
stepAIC(fit_test, direction="backward")

#install.packages("leaps")
library(leaps)
leaps <-regsubsets(age ~ hypertension + heart_disease +     
                     ever_married + work_type_2 + work_type_3 + work_type_4 + 
                     smoking_status_1 + 
                     overweight + stroke + diabetes_categ_1 + diabetes_categ_2, data=training_data, nbest=4)
plot(leaps, scale="adjr2")

library(MASS)
fit_test <- lm(age_sqrt ~ hypertension + heart_disease +     
                 ever_married + work_type_2 + work_type_3 + work_type_4 + 
                 smoking_status_1 + 
                 overweight + stroke + diabetes_categ_1 + diabetes_categ_2, data=training_data)
stepAIC(fit_test, direction="backward")

#library(leaps)
leaps <-regsubsets(age_sqrt ~ hypertension + heart_disease +     
                     ever_married + work_type_2 + work_type_3 + work_type_4 + 
                     smoking_status_1 + 
                     overweight + stroke + diabetes_categ_1 + diabetes_categ_2, data=training_data, nbest=4)
plot(leaps, scale="adjr2")

#examine predicted accuracy
fit_model <- lm(age ~ hypertension + heart_disease +     
                  ever_married + work_type_2 + work_type_4 + stroke +
                  smoking_status_1 + diabetes_categ_1, data=training_data)
fit_model_sqrt <- lm(age_sqrt ~ hypertension + heart_disease +     
                       ever_married + work_type_2 + work_type_4 + stroke +
                       smoking_status_1 + diabetes_categ_1, data=training_data)

predicted_age <- predict(fit_model, testing_data)
predicted_age_sqrt <- predict(fit_model_sqrt, testing_data)
converted_age_sqrt <- predicted_age_sqrt ^2

# make actuals_predicted dataframe.
actuals_predictions <- data.frame(cbind(actuals = testing_data$age,
                                        predicted = predicted_age))
head(actuals_predictions)

# make actuals_predicted dataframe for sqrt(Murder)
actuals_predictions_sqrt <- data.frame(cbind(actuals = testing_data$age,
                                             predicted = converted_age_sqrt))
head(actuals_predictions_sqrt)

correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy

correlation_accuracy <- cor(actuals_predictions_sqrt)
correlation_accuracy

# Min - max accuracy
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) /
                           apply(actuals_predictions, 1, max))
min_max_accuracy

# Min - max accuracy for sqrt of age
min_max_accuracy <- mean(apply(actuals_predictions_sqrt, 1, min) /
                           apply(actuals_predictions_sqrt, 1, max))
min_max_accuracy

#Residual Standard Error (RSE)
sigma(fit_model)/ mean(testing_data$age)

sigma(fit_model_sqrt)/ mean(testing_data$age)

#Run some output with the final model
summary(new_stroke_data)