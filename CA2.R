# Importing & Installing the required packages & libraries
library(dplyr)

# Importing the dataset into a Data frame
stroke_data <- read.csv("stroke.csv", na ="")

# Display the first six entries from the Data frame
head(stroke_data)

# Structure of Data frame
str(stroke_data)

# Verifying that it is a Data frame
class(stroke_data)

# Displaying the number of rows and columns
nrow(stroke_data)
ncol(stroke_data)

# Displaying the summary
summary(stroke_data)

# Data Preparation:
# Converting categorical variables to factors
stroke_data$gender = as.factor(stroke_data$gender)
stroke_data$hypertension = as.factor(stroke_data$hypertension)
stroke_data$heart_disease = as.factor(stroke_data$heart_disease)
stroke_data$ever_married = as.factor(stroke_data$ever_married)
stroke_data$work_type = as.factor(stroke_data$work_type)
stroke_data$Residence_type = as.factor(stroke_data$Residence_type)
stroke_data$smoking_status = as.factor(stroke_data$smoking_status)
stroke_data$stroke = as.factor(stroke_data$stroke)

# Rename the Residence_type variable
colnames(stroke_data)[colnames(stroke_data)=="Residence_type"]<-"residence_type"

# ID and Date attributes:
# As these attributes were used to identify the patient records
# only and does not affect the prediction, 
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

# Convert the gender, hypertension, heart_disease, ever_married
# residence_type, stroke, overweight variables to numeric
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

# Display the summary of the data
summary(stroke_data)

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

# attach(new_stroke_data)
# library(psych)
# Finding correlation between different variables
#pairs.panels(new_stroke_data, 
#             smooth = FALSE, # If TRUE, draws less smooths
#             scale = FALSE, # If TRUE, scales the correlation text font
#             density = TRUE, # If TRUE, adds density plots and histograms
#             ellipses = FALSE, # If TRUE, draws ellipses
#             method = "spearman", # Correlation method (also "pearson" or "kendall")
#             pch = 21, # pch symbol
#             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
#             cor = TRUE, # If TRUE, reports correlations
#             jiggle = FALSE, # If TRUE, data points are jittered
#             factor = 2, # Jittering factor
#             hist.col = 4, # Histograms color
#             stars = TRUE, # If TRUE, adds significance level with stars
#             ci = TRUE) # If TRUE, adds confidence intervals

################## STROKE DATA ####################
# Stroke Data-set - Predicting the age of a person
# Using "Multiple Linear Regression" Technique
###################################################

### CHECK FOR LINEARITY ###
attach(new_stroke_data)
# check correlation of all variables
stroke_matrix <- cor (new_stroke_data[1:14])
library(corrplot)
corrplot(stroke_matrix, type = "lower")

# scatter plot is used to check detailed correlation of dependent
# and independent variables 
# Independent variable age plotted against all other variables
par(mfrow = c(2, 3)) # divide graph area in 2 rows by 3 columns
# Age ~ Gender
scatter.smooth(x = age, y = gender, 
               main = "Age ~ Gender", 
               xlab = "Age",
               ylab = "Gender")

# Using cor function we can measure correlation
# if value lies between -0.2 < x < 0.2 then its low correlation
cor(age, gender)
# It is giving a correlation value = -0.30
# This is medium correlation 

# age ~ Hypertension
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, hypertension)
# It is giving a correlation value = 0.27
# This is medium correlation

# age ~ heart_diseases
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, heart_disease)
# It is giving a correlation value = 0.25
# This is medium correlation

# age ~ Ever_Married
scatter.smooth(x = age, y = ever_married, 
               main = "Age ~ ever_maried", 
               xlab = "Age",
               ylab = "Ever_Married")

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, ever_married)
# It is giving a correlation value = 0.68
# This is better correlation

# age ~ Work_Type
scatter.smooth(x = age, y = work_type, 
               main = "Age ~ work_type", 
               xlab = "Age",
               ylab = "Work type")

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, work_type)
# It is giving a better correlation value = 0.63

# age ~ residence_type
scatter.smooth(x = age, y = residence_type, 
               main = "Age ~ residence_type", 
               xlab = "Age",
               ylab = "residence_type")

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, residence_type)
# It is giving a correlation value = 0.01
# This is low correlation

# age ~ avg_glucose_level
scatter.smooth(x = age, y = avg_glucose_level, 
               main = "Age ~ avg_glucose_level", 
               xlab = "Age",
               ylab = "avg_glucose_level")

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, avg_glucose_level)
# It is giving a correlation value = 0.23
# This is medium correlation

# age ~ BMI
scatter.smooth(x = age, y = bmi, 
               main = "Age ~ bmi", 
               xlab = "Age",
               ylab = "BMI")

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, bmi)
# It is giving a medium correlation value = 0.33

# age ~ Smoking_Status
scatter.smooth(x = age, y = smoking_status, 
               main = "Age ~ smoking_status", 
               xlab = "Age",
               ylab = "Smoking_status")

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, smoking_status)
# It is giving a negative correlation value = -0.38

# age ~ stroke
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, stroke)
# It is giving a correlation value = 0.23
# This is medium correlation

# age ~ diabetes_categ
scatter.smooth(x = age, y = diabetes_categ, 
               main = "Age ~ diabetes_categ", 
               xlab = "Age",
               ylab = "diabetes_categ")

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, diabetes_categ)
# It is giving a correlation value = 0.26
# This is medium correlation

# age ~ bmi_categ
scatter.smooth(x = age, y = bmi_categ, 
               main = "Age ~ bmi_categ", 
               xlab = "Age",
               ylab = "bmi_categ")

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, bmi_categ)
# It is giving a correlation value = 0.43
# This is better correlation

# age ~ overweight
scatter.smooth(x = age, y = overweight, 
               main = "Age ~ overweight", 
               xlab = "Age",
               ylab = "overweight")

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, overweight)
# It is giving a correlation value = 0.41
# This is better correlation

# To check the correlation for all the variables from the DF
paste("Correlation for the Age & Gender:", cor(age, gender))
paste("Correlation for the Age & hypertension:", cor(age, hypertension))
paste("Correlation for the Age & heart_disease:", cor(age, heart_disease))
paste("Correlation for the Age & ever_married:", cor(age, ever_married))
paste("Correlation for the Age & work_type:", cor(age, work_type))
paste("Correlation for the Age & residence_type:", cor(age, residence_type))
paste("Correlation for the Age & avg_glucose_level:", cor(age, avg_glucose_level))
paste("Correlation for the Age & BMI:", cor(age, bmi))
paste("Correlation for the Age & smoking_status:", cor(age, smoking_status))
paste("Correlation for the Age & stroke:", cor(age, stroke))
paste("Correlation for the Age & diabetes_categ:", cor(age, diabetes_categ))
paste("Correlation for the Age & bmi_categ:", cor(age, bmi_categ))
paste("Correlation for the Age & overweight:", cor(age, overweight))

#"Correlation for the Age & Gender: -0.0302800086314233"
#"Correlation for the Age & hypertension: 0.274394871098281"
#"Correlation for the Age & heart_disease: 0.257104016866548"
#"Correlation for the Age & ever_married: 0.680741893236177"
#"Correlation for the Age & work_type: 0.538224501013831"
#"Correlation for the Age & residence_type: 0.0107952746608523"
#"Correlation for the Age & avg_glucose_level: 0.235999649837701"
#"Correlation for the Age & BMI: 0.333314218067745"
#"Correlation for the Age & smoking_status: -0.387002497954888"
#"Correlation for the Age & stroke: 0.232313013130669"
#"Correlation for the Age & diabetes_categ: 0.26535537616093"
#"Correlation for the Age & bmi_categ: 0.430394760504725"
#"Correlation for the Age & overweight: 0.419079869490302"

# It seems that residence_type has very low correlation with age 
# Therefore removing residence_type
new_stroke_data <- subset(new_stroke_data, select = -c(residence_type))

# Structure of the Data Frame
str(new_stroke_data)
head(new_stroke_data)

### CHECK FOR OUTLIERS ###
# Overview : Outliers have negative impact on results of data analysis
# Removing these outliers will improve fit to normality assumptions of dataframe

# Any data point which lies outside of 1.5 * interquartile-range 
# which is 1.5*IQR is an outlier

# Examine all variables in dataset
# Make the dataframe to use with read only permission
opar <- par(no.readonly = TRUE)
par(mfrow = c(2, 3)) # divide graph area in 2 rows by 3 columns 

attach(new_stroke_data)
boxplot(age, 
        main = "Age", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(age)$out)) # box plot for age
# There is no outlier

boxplot(gender, 
        main = "gender", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(gender)$out)) # box plot for gender
# There is no outlier

boxplot(hypertension, 
        main = "hypertension", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(hypertension)$out)) # box plot for hypertension
# There is 1 outlier

boxplot(heart_disease, 
        main = "heart_disease", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(heart_disease)$out)) # box plot for heart disease
# There is 1 outlier

boxplot(ever_married, 
        main = "ever_married", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(ever_married)$out)) # box plot for ever_married
# There is no outlier

boxplot(work_type, 
        main = "work_type", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(work_type)$out)) # box plot for work_type
# There is 0 outlier

boxplot(avg_glucose_level, 
        main = "avg_glucose_level", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(avg_glucose_level)$out)) # box plot for avg_glucose_level
# There are many outliers

boxplot(bmi, 
        main = "bmi", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(bmi)$out)) # box plot for bmi
# There are many outliers

boxplot(smoking_status, 
        main = "smoking_status", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(smoking_status)$out)) # box plot for smoking_status
# There is no outlier

boxplot(stroke, 
        main = "stroke", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(stroke)$out)) # box plot for stroke
# There is 1 outlier

# In stroke dataset most of the variables are categorical variables so outlier 
# removal is not applicable on those
# There are three continuous variables and have many outliers detected which 
# seems inadequate to the dataset which can not removed 
# Therefore not removing any outliers

par(opar)

### CHECK FOR NORMALITY ###
# There should be linear relationship between dependent and independent variables
# Therefore check whether all the variables are normally distributed
# Use strategy pattern to check normality visually
# One of the way to check skewness of variable

# library e1071 to access skewness fucntion
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow =c(2,3)) # 3rows * 4cols

# skewness of < -1 or > 1 = highly skewed
# -1 to -0.5 and 0.5 to 1 = moderately skewed
# Skewness of -0.5 to 0.5 = approx symetrical

# We can also get to the skewness for all the variables into the Dataframe
# using the following function
# Plot the density graph for the variable specified

# Plot the density graph for "age" variable
plot(density(age), 
     main = "Density plot for Age", 
     ylab = "Frequency", xlab = "Age",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(age), 2)))
# Fill in the area under the plot with red
polygon(density(age), col = "red")

# Plot the density graph for "hypertension" variable
plot(density(hypertension), 
     main = "Density plot for hypertension", 
     ylab = "Frequency", xlab = "hypertension",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(hypertension), 2)))
# Fill in the area under the plot with red
polygon(density(hypertension), col = "red")

# Plot the density graph for "heart_disease" variable
plot(density(heart_disease), 
     main = "Density plot for heart_disease", 
     ylab = "Frequency", xlab = "heart_disease",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(heart_disease), 2)))
# Fill in the area under the plot with red
polygon(density(heart_disease), col = "red")

# Plot the density graph for "ever_married" variable
plot(density(ever_married), 
     main = "Density plot for ever_married", 
     ylab = "Frequency", xlab = "ever_married",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(ever_married), 2)))
# Fill in the area under the plot with red
polygon(density(ever_married), col = "red")

# Plot the density graph for "work_type" variable
plot(density(work_type), 
     main = "Density plot for work_type", 
     ylab = "Frequency", xlab = "work_type",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(work_type), 2)))
# Fill in the area under the plot with red
polygon(density(work_type), col = "red")

# Plot the density graph for "avg_glucose_level" variable
plot(density(avg_glucose_level), 
     main = "Density plot for avg_glucose_level", 
     ylab = "Frequency", xlab = "avg_glucose_level",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(avg_glucose_level), 2)))
# Fill in the area under the plot with red
polygon(density(avg_glucose_level), col = "red")

# Plot the density graph for "bmi" variable
plot(density(bmi), 
     main = "Density plot for bmi", 
     ylab = "Frequency", xlab = "bmi",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(bmi), 2)))
# Fill in the area under the plot with red
polygon(density(bmi), col = "red")

# Plot the density graph for "smoking_status" variable
plot(density(smoking_status), 
     main = "Density plot for smoking_status", 
     ylab = "Frequency", xlab = "smoking_status",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(smoking_status), 2)))
# Fill in the area under the plot with red
polygon(density(smoking_status), col = "red")

# Plot the density graph for "stroke" variable
plot(density(stroke), 
     main = "Density plot for stroke", 
     ylab = "Frequency", xlab = "stroke",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(stroke), 2)))
# Fill in the area under the plot with red
polygon(density(stroke), col = "red")

# Plot the density graph for "diabetes_categ" variable
plot(density(diabetes_categ), 
     main = "Density plot for diabetes_categ", 
     ylab = "Frequency", xlab = "diabetes_categ",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(diabetes_categ), 2)))
# Fill in the area under the plot with red
polygon(density(diabetes_categ), col = "red")

# Plot the density graph for "bmi_categ" variable 
plot(density(bmi_categ), 
     main = "Density plot for bmi_categ", 
     ylab = "Frequency", xlab = "bmi_categ",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(bmi_categ), 2)))
# Fill in the area under the plot with red
polygon(density(bmi_categ), col = "red")

# Plot the density graph for "overweight" variable 
plot(density(overweight), 
     main = "Density plot for overweight", 
     ylab = "Frequency", xlab = "overweight",
     sub = paste("Skewness: ", 
                 round(e1071::skewness(overweight), 2)))
# Fill in the area under the plot with red
polygon(density(overweight), col = "red")

# Displaying the result obtained for skewness through the density graph
paste("Skewness for Age: ", round(e1071::skewness(age), 2))
paste("Skewness for hypertension: ", round(e1071::skewness(hypertension), 2))
paste("Skewness for heart_disease: ", round(e1071::skewness(heart_disease), 2))
paste("Skewness for ever_married: ", round(e1071::skewness(ever_married), 2))
paste("Skewness for work_type: ", round(e1071::skewness(work_type), 2))
paste("Skewness for avg_glucose_level: ", round(e1071::skewness(avg_glucose_level), 2))
paste("Skewness for bmi: ", round(e1071::skewness(bmi), 2))
paste("Skewness for smoking_status: ", round(e1071::skewness(smoking_status), 2))
paste("Skewness for stroke: ", round(e1071::skewness(stroke), 2))
paste("Skewness for diabetes_categ: ", round(e1071::skewness(diabetes_categ), 2))
paste("Skewness for bmi_categ: ", round(e1071::skewness(bmi_categ), 2))
paste("Skewness for overweight: ", round(e1071::skewness(overweight), 2))

# Minimal skewness = -0.11 - slightly skewed 
# NB a skewness value <-1 or >1 = highly skewed 
# Skewness -1 to -0.5 and 0.5 to 1 = moderately skewed 
# And skewness -0.5 to 0.5 = approx symmetric

# Age = -0.12 - approx symmetric
# hypertension = 2.82 - highly skewed
# heart_disease = 4.15 - highly skewed
# ever_married = -0.64 - moderately skewed
# work_type = -0.9 - moderately skewed
# avg_glucose_level = 1.61 - highly skewed
# bmi = 1.05 - highly skewed
# smoking_status = 0.09 - approx symmetric
# stroke = 4.53 - highly skewed
# diabetes_categ = 2.34 - highly skewed
# bmi_categ = -0.45 - approx symmetric
# overweight = -0.74 - moderately skewed

# To check the normality of the data present in different variable we can also 
# use qqnorm() fucntion Histogram Visualization
opar <- par(no.readonly = TRUE)

# Defining the length of the graph to get them plot
par(mfrow = c(1,2)) # divide the graph area in 2 cols

# Visual analysis of the data using histogram
# also showing a line representing if the data is normally distributed or not
# using qqnorm() & qqline() function

# Visual representation for the attributed & analyzing the 
# normal distribution of the variables

# For Age
hist(age, 
     main = "Normality proportion of Age", 
     xlab = "Age (years)")

qqnorm(age)
qqline(age, col = "red")
# Applying Shapiro-Wilks normality test for age variable
shapiro.test(age)

par(mfrow = c(2,2))
# For gender
hist(gender, 
     main = "Normality proportion of gender", 
     xlab = "gender")

qqnorm(gender)
qqline(gender, col = "red")
# Applying Shapiro-Wilks normality test for gender variable
shapiro.test(gender)

# For hypertension
hist(hypertension, 
     main = "Normality proportion of hypertension", 
     xlab = "hypertension")

qqnorm(hypertension)
qqline(hypertension, col = "red")
# Applying Shapiro-Wilks normality test for hypertension variable
shapiro.test(hypertension)

# For heart_disease
hist(heart_disease, 
     main = "Normality proportion of heart_disease", 
     xlab = "heart_disease")

qqnorm(heart_disease)
qqline(heart_disease, col = "red")
# Applying Shapiro-Wilks normality test for heart_disease variable
shapiro.test(heart_disease)

# For ever_married
hist(ever_married, 
     main = "Normality proportion of ever_married", 
     xlab = "ever_married")

qqnorm(ever_married)
qqline(ever_married, col = "red")
# Applying Shapiro-Wilks normality test for ever_married variable
shapiro.test(ever_married)

# For work_type
hist(work_type, 
     main = "Normality proportion of work_type", 
     xlab = "work_type")

qqnorm(work_type)
qqline(work_type, col = "red")
# Applying Shapiro-Wilks normality test for work_type variable
shapiro.test(work_type)

# For avg_glucose_level
hist(avg_glucose_level, 
     main = "Normality proportion of avg_glucose_level", 
     xlab = "avg_glucose_level")

qqnorm(avg_glucose_level)
qqline(avg_glucose_level, col = "red")
# Applying Shapiro-Wilks normality test for avg_glucose_level variable
shapiro.test(avg_glucose_level)

# For BMI
hist(bmi, 
     main = "Normality proportion of bmi", 
     xlab = "bmi")

qqnorm(bmi)
qqline(bmi, col = "red")
# Applying Shapiro-Wilks normality test for bmi variable
shapiro.test(bmi)

# For smoking_status
hist(smoking_status, 
     main = "Normality proportion of smoking_status", 
     xlab = "smoking_status")

qqnorm(smoking_status)
qqline(smoking_status, col = "red")
# Applying Shapiro-Wilks normality test for smoking_status variable
shapiro.test(smoking_status)

# For stroke
hist(stroke,
     main = "Normality proportion of stroke", 
     xlab = "stroke")

qqnorm(stroke)
qqline(stroke, col = "red")
# Applying Shapiro-Wilks normality test for stroke variable
shapiro.test(stroke)

# For diabetes_categ
hist(diabetes_categ, 
     main = "Normality proportion of diabetes_categ", 
     xlab = "diabetes_categ")

qqnorm(diabetes_categ)
qqline(diabetes_categ, col = "red")
# Applying Shapiro-Wilks normality test for diabetes_categ variable
shapiro.test(diabetes_categ)

# For bmi_categ
hist(bmi_categ, 
     main = "Normality proportion of bmi_categ", 
     xlab = "bmi_categ")

qqnorm(bmi_categ)
qqline(bmi_categ, col = "red")
# Applying Shapiro-Wilks normality test for bmi_categ variable
shapiro.test(bmi_categ)

# For overweight
hist(overweight, 
     main = "Normality proportion of overweight", 
     xlab = "overweight")

qqnorm(overweight)
qqline(overweight, col = "red")
# Applying Shapiro-Wilks normality test for overweight variable
shapiro.test(overweight)

par <- opar

# Add the records id as a variable
new_stroke_data <- cbind(row_id = rownames(new_stroke_data), new_stroke_data)
head(new_stroke_data)

# Initial model
fit <- lm(age ~ 
            hypertension + heart_disease + bmi + avg_glucose_level + 
            stroke + ever_married + work_type_1 + work_type_2 + work_type_3+ 
            work_type_4 + smoking_status_1 + smoking_status_2 + 
            smoking_status_3, data = new_stroke_data)

summary(fit)

# Outliers
library("car")
attach(new_stroke_data)
par(mfrow = c(1,2))
# qqplot continuous variables
qqPlot(bmi,
       main = "BMI")
qqPlot(avg_glucose_level,
       main = "Avg_Glucose_Level")

# list outliers value
new_stroke_data[c(4030, 2020), c('age', 'bmi', 'avg_glucose_level')]
new_stroke_data[c(1145, 159), c('age', 'bmi', 'avg_glucose_level')]

# remove the clearly wrong collected data
# 4 rows are removed
new_stroke_data <- new_stroke_data[-c(4030, 2020, 1145, 159), ]

# Re-fit the model to improve it
fit <- lm(age ~ 
            hypertension + heart_disease + bmi + avg_glucose_level + 
            stroke + ever_married + work_type_1 + work_type_2 + work_type_3+
            work_type_4 + smoking_status_1 + smoking_status_2 + 
            smoking_status_3, data = new_stroke_data)

summary(fit)
# Remove bmi, smoking_status_2 and smoking_status_3

# Adding dummy variables for average glucose level and bmi and fit the model
fit <- lm(age ~ 
            hypertension + heart_disease + ever_married +
            diabetes_categ_1 + diabetes_categ_2 + stroke +
            work_type_1 + work_type_2 + work_type_3 + work_type_4 +
            smoking_status_1 + overweight,
          data = new_stroke_data)
summary(fit)

# Training & Testing dataset
set.seed(1)
no_rows_data <- nrow(new_stroke_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE) 

training_data <- new_stroke_data[sample, ]
testing_data <- new_stroke_data[-sample, ]

fit <- lm(age ~ 
            hypertension + heart_disease + ever_married +
            diabetes_categ_1 + diabetes_categ_2 + stroke +
            work_type_1 + work_type_2 + work_type_3 + work_type_4 +
            smoking_status_1 + overweight,
          data = training_data)

summary(fit)
confint(fit)

# Visualization
par(mfrow = c(1,1))
library(car)
qqPlot(fit, labels=row.names(new_stroke_data), 
       id.method="identify", 
       simulate=TRUE, 
       main="Q-Q Plot")

training_data["4526",]
training_data["433",]

fitted(fit)["4526"]
fitted(fit)["433"]

# Histogram of the studentized residuals and superimpose a normal curve, 
# kernel-density curve, and rug plot
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

legend("topright", legend = c("Normal curve", "Kernel Density Curve"), 
       lty = 1:2, col = c("blue", "red"), cex = 0.7)

library(car)
outlierTest(fit)

# Remove 4526th record from new_stroke_data dataset
new_stroke_data <- subset(new_stroke_data, new_stroke_data$row_id != "4526")

# training & testing dataset
set.seed(1)
no_rows_data <- nrow(new_stroke_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE) 

training_data <- new_stroke_data[sample, ]
testing_data <- new_stroke_data[-sample, ]

fit <- lm(age ~ 
            hypertension + heart_disease + ever_married +
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

fit <- lm(age ~ 
            hypertension + heart_disease + ever_married +
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

# Check for Linearity
crPlots(fit)

# Influential observations
cutoff <- 4/(nrow(training_data) - length(fit$coefficients) - 2)
plot(fit, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red")

library(car)
avPlots(fit, ask=FALSE)

# Influence plot
library(car)
influencePlot(fit, main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

# Homoscedasticity
ncvTest(fit)

spreadLevelPlot(fit)

# Most variables in the dataset have attributes irrelevant to children,
# like smoking_status and ever_married, 
# Create a subset of the dataset to exclude patients with age >= 18
str(new_stroke_data[new_stroke_data$age >= 18, ])

# Adjust the dataset
new_stroke_data <- new_stroke_data[new_stroke_data$age >= 18, ]
hist(new_stroke_data$age)

attach(new_stroke_data)
# Rebuild the model by taking square root
new_stroke_data$sqrt_age <- sqrt(new_stroke_data$age)
fit <- lm(sqrt_age ~ 
            hypertension + heart_disease +     
            ever_married + work_type_2 + work_type_3 + work_type_4 + 
            smoking_status_1 + overweight + stroke + 
            diabetes_categ_1 + diabetes_categ_2 ,
          data = new_stroke_data)

summary(fit)

# training & testing dataset
set.seed(1)
no_rows_data <- nrow(new_stroke_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE) 

training_data <- new_stroke_data[sample, ]
testing_data <- new_stroke_data[-sample, ]

# Multi-collinearity
library(car)
vif(fit)
# The VIF scores should be close to 1 but under 5 is fine and 10+ 
# indicates that the variable is not needed and can be removed from the model.

# We can check whether any of the variables indicate a multi-collinearity problem
# if the value > 2
sqrt(vif(fit)) > 2

#Transforming the age variable
library(car)
summary(powerTransform(new_stroke_data$sqrt_age))

# Comparing models using AIC
sqrt_transform_age <- sqrt(training_data$age)
training_data$age_sqrt <- sqrt_transform_age

fit_model1 <- lm(age ~ hypertension + heart_disease +     
                   ever_married + work_type_2 + work_type_3 + 
                   work_type_4 + smoking_status_1 + overweight + stroke + 
                   diabetes_categ_1 + diabetes_categ_2, data=training_data)

fit_model2 <- lm(age_sqrt ~ hypertension + heart_disease +     
                   ever_married + work_type_2 + work_type_3 + 
                   work_type_4 + smoking_status_1 + overweight + stroke + 
                   diabetes_categ_1 + diabetes_categ_2, data=training_data)

AIC(fit_model1,fit_model2)
spreadLevelPlot(fit_model2)

# Comparing multiple models
# STEPWISE-REGRESSION
library(MASS)
fit_test <- lm(age ~ hypertension + heart_disease +     
                 ever_married + work_type_2 + work_type_3 + 
                 work_type_4 + smoking_status_1 + overweight + stroke + 
                 diabetes_categ_1 + diabetes_categ_2, data=training_data)

stepAIC(fit_test, direction="backward")

#install.packages("leaps")
library(leaps)
leaps <-regsubsets(age ~ hypertension + heart_disease + ever_married + 
                     work_type_2 + work_type_3 + work_type_4 + 
                     smoking_status_1 + overweight + stroke + 
                     diabetes_categ_1 + diabetes_categ_2, 
                   data=training_data, nbest=4)

plot(leaps, scale="adjr2")

library(MASS)
fit_test <- lm(age_sqrt ~ hypertension + heart_disease + ever_married + 
                 work_type_2 + work_type_3 + work_type_4 + 
                 smoking_status_1 + overweight + stroke + 
                 diabetes_categ_1 + diabetes_categ_2, data=training_data)

stepAIC(fit_test, direction="backward")

#library(leaps)
leaps <-regsubsets(age_sqrt ~ hypertension + heart_disease + ever_married + 
                     work_type_2 + work_type_3 + work_type_4 + 
                     smoking_status_1 + overweight + stroke + 
                     diabetes_categ_1 + diabetes_categ_2, 
                   data=training_data, nbest=4)

plot(leaps, scale="adjr2")

# Examine predicted accuracy
fit_model <- lm(age ~ hypertension + heart_disease +     
                  ever_married + work_type_2 + work_type_4 + stroke +
                  smoking_status_1 + diabetes_categ_1, data=training_data)

fit_model_sqrt <- lm(age_sqrt ~ hypertension + heart_disease + ever_married + 
                       work_type_2 + work_type_4 + stroke +
                       smoking_status_1 + diabetes_categ_1, 
                     data=training_data)

predicted_age <- predict(fit_model, testing_data)
predicted_age_sqrt <- predict(fit_model_sqrt, testing_data)
converted_age_sqrt <- predicted_age_sqrt ^2

# Make actuals_predicted dataframe
actuals_predictions <- data.frame(cbind(actuals = testing_data$age,
                                        predicted = predicted_age))
head(actuals_predictions)

# Make actuals_predicted dataframe for sqrt(Murder)
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

# Residual Standard Error (RSE)
sigma(fit_model)/ mean(testing_data$age)

sigma(fit_model_sqrt)/ mean(testing_data$age)

# Run some outputs with the final model

df <- data.frame(hypertension = c(1), heart_disease = c(1), ever_married = c(1),
                 work_type_2 = c(1), work_type_4 = c(0), stroke = c(1),
                 smoking_status_1 = c(1), diabetes_categ_1 = c(1) )
predicted_age <- predict(fit_model, df)
predicted_age
# predicted age = 86
# Result shows that when the hypertension, heart disease is present and the 
# person is married having govt job with smoking habit and normal diabetic status,
# mostly the person's age would be around 86.

df <- data.frame(hypertension = c(1), heart_disease = c(1), ever_married = c(0),
                 work_type_2 = c(1), work_type_4 = c(0), stroke = c(1),
                 smoking_status_1 = c(1), diabetes_categ_1 = c(1) )
predicted_age <- predict(fit_model, df)
predicted_age
# predicted age = 70
# Result shows that with similar setting as previous test but the persons marital
# status is changed to unmarried then mostly these kind of people are in their 70s

df <- data.frame(hypertension = c(1), heart_disease = c(1), ever_married = c(0),
                 work_type_2 = c(1), work_type_4 = c(0), stroke = c(1),
                 smoking_status_1 = c(0), diabetes_categ_1 = c(1) )
predicted_age <- predict(fit_model, df)
predicted_age
# predicted age = 65
# Result shows that if the person is not married and working in govt firm also
# not having smoking habit but has hypertension,heart disease, normal diabetic
# status then statistics shows that these people fall in age group of around 65

df <- data.frame(hypertension = c(0), heart_disease = c(1), ever_married = c(0),
                 work_type_2 = c(1), work_type_4 = c(0), stroke = c(1),
                 smoking_status_1 = c(0), diabetes_categ_1 = c(1) )
predicted_age <- predict(fit_model, df)
predicted_age
# predicted age = 57
# Result shows that if the person is not married, never had smoking habit, has
# heart disease and normal glucose levels(0-140) Also the person got heart stroke
# in past, Falls under age group of around 57. Which clearly shows that marital
# status , hypertension and smoking habit is more in higher age group people
# which is above 57

df <- data.frame(hypertension = c(0), heart_disease = c(0), ever_married = c(0),
                 work_type_2 = c(1), work_type_4 = c(0), stroke = c(1), 
                 smoking_status_1 = c(0), diabetes_categ_1 = c(1) )
predicted_age <- predict(fit_model, df)
predicted_age
# predicted age = 44
# Result shows that person's age would be mostly around 44 when he/she is not 
# married, no smoking habit, never had heart disease, has normal diabetic status 
# but got heart stroke and working in govt company. This study shows that these 
# kind of cases are found in young people who are in their early 40s

df <- data.frame(hypertension = c(0), heart_disease = c(0), ever_married = c(0),
                 work_type_2 = c(1), work_type_4 = c(0), stroke = c(0), 
                 smoking_status_1 = c(0), diabetes_categ_1 = c(1) )
predicted_age <- predict(fit_model, df)
predicted_age
# predicted age = 33
# Lastly when result is obtained for the person who is not married with no heart 
# disease, no heart stroke, has normal diabetic status no smoking habits and also
# a working professional in govt firm, we can see that that these people are 
# mostly young people age is around 33

df <- data.frame(hypertension = c(0), heart_disease = c(0), ever_married = c(0),
                 work_type_2 = c(0), work_type_4 = c(1), stroke = c(0), 
                 smoking_status_1 = c(0), diabetes_categ_1 = c(1) )
predicted_age <- predict(fit_model, df)
predicted_age
# predicted age = 30
# With the same health status and marital status as previous test but work type 
# changed to private sector job. Then people of age 
# group 30 seems to work in private job more and living healthy life too.