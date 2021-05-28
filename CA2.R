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
colnames(stroke_data)[colnames(stroke_data) == "Residence_type"] <- "residence_type"

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

levels(stroke_data$smoking_status)

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
stroke_matrix <- cor (new_stroke_data[1:12])
# corrplot(stroke_matrix, method = "number")
library(corrplot)
corrplot(stroke_matrix, type = "upper")
# Initial model
fit <- lm(age ~ 
            hypertension + heart_disease +     
            ever_married +  
            bmi + stroke + diabetes_categ,
          data = new_stroke_data)

summary(fit)
confint(fit)
attach(new_stroke_data)
# outliers
library("car")
# qqplot continous variables
qqPlot(new_stroke_data$bmi,
       main = "BMI")

plot(fit, pch = 10, cex = 2, main="Influential observations ") 
abline(h = 4 * mean(fit, na.rm=T), col="red")  # add cutoff line
text(x = 1:length(fit) + 1, y = fit, 
     labels=ifelse(fit > 4 * mean(fit, na.rm = T), 
                   names(lm),""),col="red")
# list outliers' value
new_stroke_data[c(4030,2020, 181), c("bmi") ]

# remove the clearly wrong collected data
# two rows are removed
new_stroke_data <- new_stroke_data[-c(2020, 4030), ]

# the model improved a little
# replace the diabetes with dummy variables
fit <- lm(age ~ gender+ hypertension + heart_disease +     
            ever_married + work_type + residence_type+avg_glucose_level + 
            smoking_status + bmi + stroke ,
          data = new_stroke_data)
summary(fit)
# remove gender, residence_type and bmi

# replace the diabetes with dummy variables
fit <- lm(age ~ hypertension + heart_disease +     
            ever_married + work_type_1 + work_type_2 + work_type_3 + 
            work_type_4 + diabetes_categ + smoking_status_1 + 
            overweight + stroke,
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

fit <- lm(age ~ hypertension + heart_disease +     
            ever_married + work_type_1 + work_type_2 + work_type_3 + 
            work_type_4 + diabetes_categ + smoking_status_1 + 
            overweight + stroke,
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
training_data["3922",]

fitted(fit)["4526"]
fitted(fit)["3922"]



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

fit <- lm(age ~ hypertension + heart_disease +     
            ever_married + work_type_1 + work_type_2 + work_type_3 + 
            work_type_4 + diabetes_categ + smoking_status_1 + 
            overweight + stroke,
          data = training_data)

outlierTest(fit)

# Remove 3922nd record
# from new_stroke_data dataset
new_stroke_data <- subset(new_stroke_data, new_stroke_data$row_id != "3922")

# training & testing dataset
set.seed(1)
no_rows_data <- nrow(new_stroke_data)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE) 

training_data <- new_stroke_data[sample, ]
testing_data <- new_stroke_data[-sample, ]

fit <- lm(age ~ hypertension + heart_disease +     
            ever_married + work_type_1 + work_type_2 + work_type_3 + 
            work_type_4 + diabetes_categ + smoking_status_1 + 
            overweight + stroke,
          data = training_data)

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

crPlots(fit)
















#*************************************************************************
library(psych)
# pairs.panels(new_stroke_data, 
#              smooth = TRUE, # If TRUE, draws loess smooths  
#              scale = FALSE, # If TRUE, scales the correlation text font  
#              density = TRUE, # If TRUE, adds density plots and histograms  
#              ellipses = TRUE, # If TRUE, draws ellipses   
#              method = "spearman",# Correlation method (also "pearson" or "kendall") 
#              pch = 21, # pch symbol   
#              lm = FALSE, # If TRUE, plots linear fit rather than the LOESS (smoothed) fit 
#              cor = TRUE, # If TRUE, reports correlations
#              jiggle = FALSE, # If TRUE, data points are jittered  
#              factor = 2, # Jittering factor  
#              hist.col = 4, # Histograms color   
#              stars = TRUE,
#              ci = TRUE) # If TRUE, adds confidence intervals 

# correlation table
res <- cor(new_stroke_data)
round(res, 2)
# correlation table2
# install.packages("Hmisc")
library("Hmisc")
res2 <- rcorr(as.matrix(new_stroke_data))
res2
rcorr(as.matrix(new_stroke_data), type = c("pearson","spearman"))
res2$r
res2$P
# visualization the correlation
#install.packages("corrplot")
library(corrplot)
corrplot(res2, 
         type = "upper", 
         order = "hclust", 
         tl.col = "black", 
         tl.srt = 45)

# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")
# install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
# big & slow
# chart.Correlation(new_stroke_data, histogram=TRUE, pch=19)

boxplot(new_stroke_data[, c("age",  "bmi", "avg_glucose_level")])
# generate glucose value to diabetes status
# str(new_stroke_data)
# model
str(new_stroke_data)
model <- lm(age_level ~ gender + stroke + hypertension + 
              heart_disease + ever_married + 
              Residence_type + diabetes_0 + diabetes_1 + diabetes_2 + 
              bmi + work_type_1 + work_type_2 + 
              work_type_3 + work_type_4 + 
              smoking_status_1 + smoking_status_2 + smoking_status_3, 
            data = new_stroke_data)
summary(model)
# select variables
model <- lm(age_level ~ stroke + ever_married + 
              Residence_type + 
              bmi_level_0 + bmi_level_1 + bmi_level_2 + bmi_level_3, 
            data = new_stroke_data)
summary(model)
# train & test
# create training & testing data
set.seed(1)
no_rows_data <- nrow(new_stroke_data)
# use 70% and don't use it again
sample_data <- sample(1 : no_rows_data, size = round(0.7 * no_rows_data),
                      replace = FALSE)
training_data <- new_stroke_data[sample_data, ]
testing_data <- new_stroke_data[-sample_data, ]

fit <- lm(age_level ~ stroke + 
            bmi_level_0 + bmi_level_1 + bmi_level_2 + bmi_level_3, data=training_data)
summary(fit)
confint(fit)
# 
library(car)
qqPlot(fit, 
       labels=row.names(states), 
       id.method="identify", 
       simulate=TRUE, main="Q-Q Plot")
student_fit <- rstudent(fit)
hist(student_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")

rug(jitter(student_fit), col="brown")
curve(dnorm(x, mean=mean(student_fit), sd=sd(student_fit)), add=TRUE, col="blue", lwd=2)
lines(density(student_fit)$x, density(student_fit)$y, col="red", lwd=2, lty=2)
legend("topright", legend = c( "Normal Curve", "Kernel Density Curve"), lty=1:2, col=c("blue","red"), cex=.7)

outlierTest(fit)


# predict
prob <- predict(model, testing_data, type = "response")
# pred <- factor(prob > 0.5, levels = c(FALSE, TRUE))
lr.perf <- table(testing_data$age_level, prob, dnn = c("Actual", "Predicted"))
lr.perf
















# For gender
scatter.smooth(x = age, y = gender, 
               main = "Age ~ Gender", 
               xlab = "Age",
               ylab = "Gender")

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, gender)
# It is giving a correlation value = -0.30

# For hypertension
scatter.smooth(x = age, y = hypertension, 
               main = "Age ~ hypertension", 
               xlab = "Age",
               ylab = "hypertension")
# The plot shows there is    Correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, hypertension)
# It is giving a correlation value = 0.27

# For heart_diseases
scatter.smooth(x = age, y = heart_disease, 
               main = "Age ~ heart_disease", 
               xlab = "Age",
               ylab = "heart_disease")
# The plot shows there is    Correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, heart_disease)
# It is giving a correlation value = 0.25

# For Ever_Married
scatter.smooth(x = age, y = ever_married, 
               main = "Age ~ ever_maried", 
               xlab = "Age",
               ylab = "Ever_Married")
# The plot shows there is  correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, ever_married)
# It is giving a low correlation value = 0.68

# For Work_Type
scatter.smooth(x = age, y = work_type, 
               main = "Age ~ work_type", 
               xlab = "Age",
               ylab = "Work type")
# The plot shows there is 

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, work_type)
# It is giving a medium correlation value = 0.63

# For residence_type
scatter.smooth(x = age, y = residence_type, 
               main = "Age ~ residence_type", 
               xlab = "Age",
               ylab = "residence_type")
# The plot shows there is        correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, residence_type)
# It is giving a correlation value = 0.01

# For avg_glucose_level
scatter.smooth(x = age, y = avg_glucose_level, 
               main = "Age ~ avg_glucose_level", 
               xlab = "Age",
               ylab = "avg_glucose_level")

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, avg_glucose_level)
# It is giving a correlation value = 0.23

# For BMI
scatter.smooth(x = age, y = bmi, 
               main = "Age ~ bmi", 
               xlab = "Age",
               ylab = "BMI")
# The plot shows there is  correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, bmi)
# It is giving a medium correlation value = 0.33

# For Smoking_Status
scatter.smooth(x = age, y = smoking_status, 
               main = "Age ~ smoking_status", 
               xlab = "Age",
               ylab = "Smoking_status")
# The plot shows there is      correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, smoking_status)
# It is giving a negative correlation value = -0.30

# For stroke
scatter.smooth(x = age, y = stroke, 
               main = "Age ~ Stroke", 
               xlab = "Age",
               ylab = "Stroke")
# The plot shows there is      correlation

# Checking Correlation
# Values of -0.2 < x < 0.2 - Low Correlation
cor(age, stroke)
# It is giving a correlation value = 0.23


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

new_stroke_data <- subset(new_stroke_data, select = -c(residence_type))

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

boxplot(hypertension, 
        main = "hypertension", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(hypertension)$out))
# There is 1 outlier

boxplot(heart_disease, 
        main = "heart_disease", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(heart_disease)$out))
# There is 1 outlier

boxplot(ever_married, 
        main = "ever_married", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(ever_married)$out))
# There is no outlier

boxplot(work_type, 
        main = "work_type", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(work_type)$out))
# There is 0 outlier

boxplot(avg_glucose_level, 
        main = "avg_glucose_level", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(avg_glucose_level)$out))
# There is  outlier

boxplot(bmi, 
        main = "bmi", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(bmi)$out))
# There is  outlier

boxplot(smoking_status, 
        main = "smoking_status", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(smoking_status)$out))
# There is  outlier

boxplot(stroke, 
        main = "stroke", 
        sub = paste("Outlier rows: ", 
                    boxplot.stats(stroke)$out))
# There is 1 outlier

par(opar)