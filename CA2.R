# Analyzing and dealing with the data related to the "Stroke"
# for visualizing and predicting the functioning & working 
# of the patients heart and implementing the model for the chances 
# of "Stroke" with the variables present.

# Importing & Installing the required packages & libraries
# wherever needed for analysing and visualizing

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

# Id and Date attributes 
# As these attributes were used to identify the patients
# records only and does not affect the prediction, 
# hence they can be dropped for further processing
stroke_data <- stroke_data[, c(2,3,4,5,6,7,8,9,10,11,12)]
str(stroke_data)

# Gender attribute:
# As there is only 1 entry with "Other" value, hence remove
# patients who were categorized as ‘Other’ in the gender column
stroke_data = filter(stroke_data, gender!= 'Other')
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

# Residence_type
# convert from Urban/Rural to 1/0
residence_type_col <- ifelse(stroke_data$Residence_type =='Urban', 1, 0)
stroke_data$Residence_type <- as.factor(residence_type_col)

# Create a new variable for diabetic status
# 1 Normal range of Glucose level <= 140 mmol/L
# 2 Pre-diabetes: 140 to 200 mmol/L
# 3 Diabetic range of Glucose level > 200 mmol/L
diabetes_col <- cut(stroke_data$avg_glucose_level,
                    breaks = c(0, 140, 199, Inf), 
                    labels = c(1, 2, 3), 
                    right = FALSE,
                    order = TRUE)
stroke_data$diabetes <- as.factor(diabetes_col)
str(stroke_data)
# Create a new variable indicating bmi level
# 1 Thin < 18.5
# 2 Healthy >=18.5
# 3 Fat >=25
# 4 Obese >= 30
bmi_col <- cut(stroke_data$bmi,
                     breaks = c(0, 18.5, 25, 30, Inf), 
                     labels = c(1, 2, 3, 4), 
                     right = TRUE,
                     order = TRUE)
stroke_data$bmi_level <- as.factor(bmi_col)

# Create a new variable for fat people
fat_col <- cut(stroke_data$bmi,
                      breaks = c(0, 25, Inf), 
                      labels = c(0, 1), 
                      right = TRUE,
                      order = FALSE)
stroke_data$fat <- as.factor(fat_col)

# Display the first six entries from the DF
head(stroke_data)
str(stroke_data)

# Structure of DF
str(stroke_data)

# Verifying that it is a DF
class(stroke_data)

# Displaying the number of rows and columns
nrow(stroke_data)
ncol(stroke_data)

# Display the summary of the data
summary(stroke_data)

# Rename the column names
#colnames(stroke_data)
#name <- c("ID","Gender", "Age", "Hypertension", "Heart_Disease", "Ever_Married", 
#          "Work_Type", "Residence_Type", "Avg_Glucose_Level", "BMI", "Smoking_Status", "Stroke", 
#          "Date")
#names(stroke_data) <- name

# Display the column-names of DF
colnames(stroke_data)

# Displaying the summary
summary(stroke_data)

# To check if any NA data present
incomplete_data <- stroke_data[!complete.cases(stroke_data),]
incomplete_data

# Display the missing data in rows
nrow(incomplete_data)


# No missing data present in the DF

# Checking if any NA is present in the DF
# FALSE represents no NA's in the DF
# TRUE represent there are NA's in the DF
any(is.na(stroke_data))

# converting the stroke data into numeric
all_numeric <- stroke_data
library(dplyr)
all_numeric <- all_numeric %>%
  mutate(gender = case_when(gender == "Male" ~1,
                            gender == "Female" ~0,
                            gender == "Other" ~2),
         ever_married = case_when(ever_married == "Yes" ~1,
                                  ever_married == "No" ~0),
         work_type = case_when(work_type == "children" ~0,
                               work_type == "Never_worked" ~1,
                               work_type == "Private" ~2,
                               work_type == "Govt_job" ~3,
                               work_type == "Self-employed" ~4),
         Residence_type = case_when(Residence_type == "Urban" ~1,
                                    Residence_type == "Rural" ~0),
         smoking_status = case_when(smoking_status == "never smoked" ~0,
                                    smoking_status == "formerly smoked" ~1,
                                    smoking_status == "smokes" ~2,
                                    smoking_status == "Unknown" ~3) 
  )
str(all_numeric)





# Converting the BMI attribute from character to numeric
new_stroke_data["bmi"] <- as.numeric(new_stroke_data$bmi)
str(new_stroke_data)


str(new_stroke_data)

# 4. Residence_type attribute:
# Renaming the variable
new_stroke_data <- new_stroke_data %>% rename("residence_type" = "Residence_type")
# Factoring the variable
new_stroke_data$residence_type = as.factor(new_stroke_data$residence_type)
str(new_stroke_data)

# Checking if any NA is present in the DF
any(is.na(new_stroke_data))

# Display the structure of DF
str(new_stroke_data)

# Display the column-names of DF
colnames(new_stroke_data)

# Installing the library 'psych'
# install.packages("psych")
library(psych)

# To visualize the distribution and correlation
pairs(new_stroke_data)

pairs.panels(new_stroke_data, 
             smooth = FALSE, # If TRUE, draws less smooths
             scale = FALSE, # If TRUE, scales the correlation text font
             density = TRUE, # If TRUE, adds density plots and histograms
             ellipses = FALSE, # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21, # pch symbol
             lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE, # If TRUE, reports correlations
             jiggle = FALSE, # If TRUE, data points are jitered
             factor = 2, # Jitering factor
             hist.col = 4, # Histograms color
             stars = TRUE, # If TRUE, adds significance level with stars
             ci = TRUE) # If TRUE, adds confidence intervals


######################## STROKE DATA ################################
# Stroke Dataset - Predicting the chances of getting a "Stroke"
# Using "Multiple Linear Regression"
####################################################################

# For further implementation we need to keep in mind about the
# dependent & independent variables for predicting the model
# DEPENDENT variable = "Stroke"
# INDEPENDENT variables = All others except 'Stroke'


# Including the variables we need to use for predicting a model


# Dependent var = X-axis
# Independent var = Y-axis

# Low correlation = value equal to zero
# High Correlation = straight line in plot (value > 0.60) 
# (positive & negative)
# No correlation = No correlation no line

attach(new_stroke_data)
str(new_stroke_data)

# For AGE
scatter.smooth(x = stroke, y = age, 
               main = "Stroke ~ Age", 
               xlab = "Chance of Stroke",
               ylab = "Age (Years)")
# The plot shows there is NO Correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(stroke, age)
# It is giving a correlation value = 0.23
# The correlation tests shows that the correlation between the Stroke & 
# age variable = 0.23 indicating a positive correlation.
# The AGE variable is of no use in predicting the model.

# For Gender
scatter.smooth(x = stroke, y = gender, 
               main = "Stroke ~ Gender", 
               xlab = "Chance of Stroke",
               ylab = "Gender")
# The plot shows there is    Correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(stroke, gender)
# It is giving a correlation value = 0.006
# The correlation tests shows that the correlation between the Stroke & 
# Gender variable = 0.006 indicating no correlation.
# The gender field can also be excluded for predicting a model.

# For hypertension
scatter.smooth(x = stroke, y = hypertension, 
               main = "Stroke ~ hypertension", 
               xlab = "Chance of Stroke",
               ylab = "hypertension")
# The plot shows there is    Correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(stroke, hypertension)
# It is giving a correlation value = 0.14

# For heart_diseases
scatter.smooth(x = stroke, y = heart_disease, 
               main = "Stroke ~ heart_disease", 
               xlab = "Chance of Stroke",
               ylab = "heart_disease")
# The plot shows there is    Correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(stroke, heart_disease)
# It is giving a correlation value = 0.13

# For Work_Type
scatter.smooth(x = stroke, y = work_type, 
               main = "Stroke ~ work_type", 
               xlab = "Chance of Stroke",
               ylab = "Work type")
# The plot shows there is 

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(stroke, work_type)
# It is giving a medium correlation value = 0.86
# The correlation tests shows that the correlation between the Stroke & 
# Work type variable = 0.86 indicating a high correlation.
# We can keep this variable for Prediction

# For BMI
scatter.smooth(x = stroke, y = bmi, 
               main = "Stroke ~ bmi", 
               xlab = "Chance of Stroke",
               ylab = "BMI")
# The plot shows there is very less correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(stroke, bmi)
# It is giving a medium correlation value = 0.042
# The correlation tests shows that the correlation between the Stroke & 
# bmi variable = 0.042 indicating a low correlation.
# We can exclude this variable for Prediction

# For Avg_Glucose_Level
scatter.smooth(x = stroke, y = avg_glucose_level, 
               main = "Stroke ~ GlucoseLevel", 
               xlab = "Chance of Stroke",
               ylab = "Avg Glucose Level")
# The plot shows there is 

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(stroke, avg_glucose_level)
# It is giving a less correlation value = 0.13
# The correlation tests shows that the correlation between the Stroke & 
# Glucose variable = 0.13 indicating a low correlation.
# We can decide to exclude this variable for prediction


# For Ever_Married
scatter.smooth(x = stroke, y = ever_married, 
               main = "Stroke ~ ever_maried", 
               xlab = "Chance of Stroke",
               ylab = "Married")
# The plot shows there is very less correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(stroke, ever_married)
# It is giving a low correlation value = 0.105
# The correlation tests shows that the correlation between the Stroke & 
# ECG variable = 0.13 indicating a less correlation.
# We can decide to keep this variable for prediction or not


# For residence_type
scatter.smooth(x = stroke, y = residence_type, 
               main = "stroke ~ residence_type", 
               xlab = "Chance of Stroke",
               ylab = "residence_type")
# The plot shows there is        correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(stroke, residence_type)
# It is giving a       correlation value = 
# The correlation tests shows that the correlation between the stroke & 
# residence_type variable =  indicating a       correlation.
# The residence_type variable is      in predicting the model.

# For Smoking_Status
scatter.smooth(x = stroke, y = smoking_status, 
               main = "stroke ~ smoking_status", 
               xlab = "Chance of Stroke",
               ylab = "Smoking_status")
# The plot shows there is      correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(stroke, smoking_status)
# It is giving a negative correlation value = -0.05
# The correlation tests shows that the correlation between the stroke & 
# smoking status variable = -0.05 indicating a weak negative correlation.


# To check the correlation for all the variables from the DF
paste("Correlation for the stroke & age:", cor(stroke, age))
paste("Correlation for the stroke & gender:", cor(stroke, gender))



# Structure of the DF
str(new_stroke_data)
head(new_stroke_data)
attach(new_stroke_data)
# Check the outliers
opar <- par(no.readonly = TRUE)

boxplot(age, 
        main = "Age", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(age)$out))
# There is no outlier

boxplot(gender, 
        main = "gender", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(gender)$out))
# There is no outlier

boxplot(stroke, 
        main = "stroke", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(stroke)$out))
# There is 1 outlier




par(opar)