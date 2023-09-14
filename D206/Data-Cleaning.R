# Install necessary packages and libraries
install.packages(c("plyr, tidyverse", "naniar", "visdat", "factoextra", "mice", "readr", "dlookr", "outliers", "dplyr"))
library(plyr)
library(tidyverse)
library(naniar)
library(visdat)
library(factoextra)
library(mice)
library(readr)
library(dlookr)
library(outliers)
library(dplyr)

# Load the data, used the "import Dataset" button in RStudio -> from the textbook
medical_raw_data <- read_csv("medical_raw_data.csv")

# Detection
# Duplicates
which(duplicated(medical_raw_data) == TRUE) # Returns integer(0), there are no duplicated rows
which(duplicated(as.list(medical_raw_data)) == TRUE) # Second column shows up

# Detect Missing values
colSums(is.na(medical_raw_data))
# Output shows that Children, Age, Income, Soft_drink, Overweight, Anxiety, and Initial_Days columns have missings
vis_miss(medical_raw_data) # Visualization of the same data
vis_dat(medical_raw_data) # Shows which missingness and datatype in one visual

# Detect Outliers in numerical variables pertaining to patient information
#VitD_levels
summary(medical_raw_data$VitD_levels)
boxplot.stats(medical_raw_data$VitD_levels)$stats 
# comparing the summary to the boxplot stats hints at the presence of outliers because the values don't match
boxplot(medical_raw_data$VitD_levels,horizontal = TRUE, main="VitD_levels")
VitDlvl_out <- boxplot.stats(medical_raw_data$VitD_levels)$out # Vector of outlier values
hist(VitDlvl_out, main="VitD_levels Outlier Distribution") # visualizing where the outliers are
length(VitDlvl_out) # number of outliers is 534, large number, check with grubbs test
grubbs.test(medical_raw_data$VitD_levels, type=10)
grubbs.test(medical_raw_data$VitD_levels, opposite=TRUE, type=10) 
# highest value is an outlier, lowest value is not an outlier
VitDlvl_out

# Doc_visits
summary(medical_raw_data$Doc_visits)
boxplot.stats(medical_raw_data$Doc_visits)$stats
# the two sets of statistics are the same, no outliers
boxplot(medical_raw_data$Doc_visits,horizontal = TRUE, main="Doc_visits")
# Boxplot confirms that outliers are not needed

# Full_meals_eaten
summary(medical_raw_data$Full_meals_eaten)
boxplot.stats(medical_raw_data$Full_meals_eaten)$stats
# Difference in max values shows that there are likely outliers on the high end
boxplot(medical_raw_data$Full_meals_eaten, horizontal=TRUE, main="Full_meals_eaten")
FME_out <- boxplot.stats(medical_raw_data$Full_meals_eaten)$out
hist(FME_out, main="Full_meals_eaten Outlier Distribution")
length(FME_out) # number of outliers is 8
FME_out

# VitD_supp
summary(medical_raw_data$VitD_supp)
boxplot.stats(medical_raw_data$VitD_supp)$stats
# Difference in max values indicate outliers on the high end
boxplot(medical_raw_data$VitD_supp, horizontal = TRUE,main="VitD_supps")
Vitssupp_out <- boxplot.stats(medical_raw_data$VitD_supp)$out
hist(Vitssupp_out, main="VitD_supps Outlier Distribution")
length(Vitssupp_out) # 70 outliers, check with Grubbs test
grubbs.test(medical_raw_data$VitD_supp, type=10) # Grubbs test indicates that the highest value is an outlier
Vitssupp_out

# TotalCharge
summary(medical_raw_data$TotalCharge)
boxplot.stats(medical_raw_data$TotalCharge)$stats
# Difference in max values indicate outliers on the high end
boxplot(medical_raw_data$TotalCharge, horizontal = TRUE)
TC_out <- boxplot.stats(medical_raw_data$TotalCharge)$out
hist(TC_out, main="TotalCharges Outlier Distribution")
length(TC_out) # 466 outliers, check with Grubbs test
grubbs.test(medical_raw_data$TotalCharge, type=10) 
# Grubbs test indicates that the highest value is an outlier
TC_out

# Additional_charges
summary(medical_raw_data$Additional_charges)
boxplot.stats(medical_raw_data$Additional_charges)$stats
# Difference in max values indicate outliers on the high end
boxplot(medical_raw_data$Additional_charges, horizontal = TRUE)
AC_out <- boxplot.stats(medical_raw_data$Additional_charges)$out
length(AC_out) # 424 outliers, check with Grubbs test
grubbs.test(medical_raw_data$Additional_charges, type=10) 
# Grubbs test indicates that the highest value is NOT an outlier

# Children
summary(medical_raw_data$Children)
boxplot.stats(medical_raw_data$Children)$stats
# Difference in max values indicate outliers on the high end
boxplot(medical_raw_data$Children,horizontal = TRUE)
Children_out <- boxplot.stats(medical_raw_data$Children)$out
length(Children_out) # 303 outliers, check with Grubbs test
grubbs.test(medical_raw_data$Children, type=10)
# P-value is not significant, indicates that the highest value is not an outlier

# Age
summary(medical_raw_data$Age)
boxplot.stats(medical_raw_data$Age)$stats
# The values are the same which indicates that there are no outliers
boxplot(medical_raw_data$Age,horizontal=TRUE,main="Age")
# Boxplot also shows no outliers

# Income
summary(medical_raw_data$Income)
boxplot.stats(medical_raw_data$Income)$stats
# The difference in values indicates outliers on the high end
boxplot(medical_raw_data$Income, horizontal=TRUE, main="Income")
Income_out <- boxplot.stats(medical_raw_data$Income)$out
length(Income_out) # 252 outliers, check with Grubbs test
grubbs.test(medical_raw_data$Income, type = 10)
# p-value is significant, highest value is an outlier
hist(Income_out, main="Income Outlier distribution")
Income_out

#Initial_days
summary(medical_raw_data$Initial_days)
boxplot.stats(medical_raw_data$Initial_days)$stats
# Values are the same, indicating that there are no outliers
boxplot(medical_raw_data$Initial_days, horizontal=TRUE, main="Initial_days")
# Boxplot also shows no outliers
# Variables with outliers needing treatment: VitD_levels, Full_meals_eaten, VitD_supp, TotalCharge, Income

# Treatment

# Duplicates
# Drop the duplicated column
medical_data_undupe <- select(medical_raw_data, -...1)
# check by running detection agaain
which(duplicated(as.list(medical_data_undupe)) == TRUE) 
# Shows up as zero, so no duplicates

# Missing Values
# Method is multivariate imputation
# re-express the categorical variable soft_drink before imputation
medical_data_undupe$Soft_drink <- ifelse(medical_data_undupe$Soft_drink == "Yes",1,0)
table(medical_data_undupe$Soft_drink)

# Visualize data with histograms
hist(medical_data_undupe$Children, main="Children")
hist(medical_data_undupe$Age, main="Age")
hist(medical_data_undupe$Income, main="Income")
hist(medical_data_undupe$Soft_drink, main="Soft_drink")
hist(medical_data_undupe$Overweight, main="Overweight")
hist(medical_data_undupe$Anxiety, main="Anxiety")
hist(medical_data_undupe$Initial_days, main="Initial_days")

# Cast the categorical variables as factors
medical_data_undupe$Soft_drink=as.factor(medical_data_undupe$Soft_drink)
medical_data_undupe$Overweight=as.factor(medical_data_undupe$Overweight)
medical_data_undupe$Anxiety=as.factor(medical_data_undupe$Anxiety)

# Separate out variables for MICE, then impute
# Predictive mean modeling used for the numeric variables, and logistic regression used for the binary variables
medical_data_undupe_missvar <- medical_data_undupe %>% select(CaseOrder, Children, Age, Income, Soft_drink, Overweight, Anxiety, Initial_days)
medical_data_imp = mice(medical_data_undupe_missvar,m=5,method=c("","pmm","pmm","pmm","logreg","logreg","logreg","pmm"),maxit=20) 

# Choose the imputation with the mean closest to the original
summary(medical_data_undupe_missvar$Children)
summary(medical_data_imp$imp$Children)

medical_data_finalmice = complete(medical_data_imp, 1)

# Confirm there are no missings
vis_dat(medical_data_finalmice)

#Add the data sets back together
medical_data_nomiss <- medical_data_undupe %>% select(-Children,-Age,-Income,-Soft_drink,-Overweight,-Anxiety,-Initial_days)
medical_data_complete <-merge(medical_data_nomiss, medical_data_finalmice, by="CaseOrder")

# Confirm there are no missing values
vis_dat(medical_data_complete) 

# Outliers
# Create a data frame with the outliers imputed with the median, and one data frame where the outliers are left alone.
medical_data_with_outliers <- medical_data_complete

# Impute the outliers with the median of the data
medical_data_clean <- medical_data_complete %>%
  mutate(VitD_levels = imputate_outlier(medical_data_complete, VitD_levels, method = "median"),
         Full_meals_eaten = imputate_outlier(medical_data_complete, Full_meals_eaten, method = "median"),
         VitD_supp = imputate_outlier(medical_data_complete, VitD_supp, method = "median"),
         TotalCharge = imputate_outlier(medical_data_complete, TotalCharge, method = "median"),
         Income = imputate_outlier(medical_data_complete, Income, method = "median"))

# Compare the two sets of data
#VitD_levels
hist(medical_data_with_outliers$VitD_levels, main="VitD_levels with Outliers")
hist(medical_data_clean$VitD_levels, main="VitD_levels without Outliers")
summary(medical_data_clean$VitD_levels)

#Full_meals_eaten
hist(medical_data_with_outliers$Full_meals_eaten, main="Full_meals_eaten with Outliers")
hist(medical_data_clean$Full_meals_eaten, main="Full_meals_eaten without Outliers")
summary(medical_data_clean$Full_meals_eaten)

#VitD_supp
hist(medical_data_with_outliers$VitD_supp, main="VitD_supp with Outliers")
hist(medical_data_clean$VitD_supp, main="VitD_supp without Outliers")
summary(medical_data_clean$VitD_supp)

#TotalCharge
hist(medical_data_with_outliers$TotalCharge, main="TotalCharge with Outliers")
hist(medical_data_clean$TotalCharge, main="TotalCharge without Outliers")
summary(medical_data_clean$TotalCharge)

#Income
hist(medical_data_with_outliers$Income, main="Income with Outliers")
hist(medical_data_clean$Income, main="Income without Outliers")
summary(medical_data_clean$Income)

#Re-express Categorical Variables
#Visualize types
vis_dat(medical_data_clean)

# ReAdmis, HighBlood, Stroke, Arthritis, Diabetes, Hyperlipidemia, BackPain, Allergic_rhinitis, Reflux_esophagitis, Asthma are T/F. 
# Recategorization same as with Soft_drink above
medical_data_clean$ReAdmis <- ifelse(medical_data_clean$ReAdmis == "Yes",1,0)
table(medical_data_clean$ReAdmis)

medical_data_clean$HighBlood <- ifelse(medical_data_clean$HighBlood  == "Yes",1,0)
table(medical_data_clean$HighBlood)

medical_data_clean$Stroke <- ifelse(medical_data_clean$Stroke == "Yes",1,0)
table(medical_data_clean$Stroke)

medical_data_clean$Arthritis <- ifelse(medical_data_clean$Arthritis  == "Yes",1,0)
table(medical_data_clean$Arthritis)

medical_data_clean$Diabetes <- ifelse(medical_data_clean$Diabetes  == "Yes",1,0)
table(medical_data_clean$Diabetes)

medical_data_clean$Hyperlipidemia <- ifelse(medical_data_clean$Hyperlipidemia  == "Yes",1,0)
table(medical_data_clean$Hyperlipidemia)

medical_data_clean$BackPain <- ifelse(medical_data_clean$BackPain  == "Yes",1,0)
table(medical_data_clean$BackPain)

medical_data_clean$Allergic_rhinitis <- ifelse(medical_data_clean$Allergic_rhinitis  == "Yes",1,0)
table(medical_data_clean$Allergic_rhinitis)

medical_data_clean$Reflux_esophagitis <- ifelse(medical_data_clean$Reflux_esophagitis  == "Yes",1,0)
table(medical_data_clean$Reflux_esophagitis)

medical_data_clean$Asthma <- ifelse(medical_data_clean$Asthma  == "Yes",1,0)
table(medical_data_clean$Asthma)

# Complication_risk and Education are ordinal
Complication_risk.num <- revalue(x=medical_data_clean$Complication_risk, replace=c("Low" = 0, "Medium" = 1, "High" = 2))
medical_data_clean$Complication_risk_numeric <- as.numeric(Complication_risk.num)
table(medical_data_clean$Complication_risk_numeric)
medical_data_clean <- select(medical_data_clean, -Complication_risk)

# Check number of levels
unique(medical_data_clean$Education) # Education has 12 levels, but some can be combined
Education.num <- revalue(x=medical_data_clean$Education, replace=c("No Schooling Completed" = 0, "Nursery School to 8th Grade" = 1, "9th Grade to 12th Grade, No Diploma" = 2, 
                                                                   "GED or Alternative Credential" = 3, "Regular High School Diploma" = 3, "Some College, Less than 1 Year" = 4, 
                                                                   "Some College, 1 or More Years, No Degree" = 5, "Associate's Degree" = 7, "Bachelor's Degree" = 8, "Master's Degree" = 9,
                                                                   "Professional School Degree" = 9, "Doctorate Degree" = 10))
medical_data_clean$Education_numeric <- as.numeric(Education.num)
table(medical_data_clean$Education_numeric)
medical_data_clean <-select(medical_data_clean, -Education)

# CustomerID, UID, City, State, Area, Timezone, Job, Services, Marital, Gender,
# Initial_admin, Employment are nominal, check cardinality
unique(medical_data_clean$Customer_id) # too many groups, drop
unique(medical_data_clean$UID) # too many groups, drop
unique(medical_data_clean$City) # Too many groups, drop
unique(medical_data_clean$State) # Too many groups, drop
unique(medical_data_clean$Area) # three groups (Suburban, Urban, rural)
unique(medical_data_clean$Timezone) # Too many groups, drop
unique(medical_data_clean$Job) # Too many groups, drop
unique(medical_data_clean$Services) # Four groups (Blood Work, Intravenous, CT Scan, MRI)
unique(medical_data_clean$Marital) # Five groups (Divorced, Married, Widowed, Never Married, Separated)
unique(medical_data_clean$Gender) # Three groups (Male, Female, Prefer not to answer)
unique(medical_data_clean$Initial_admin) # Three groups (Emergency Admission, Elective Admission, Observation Admission)
unique(medical_data_clean$Employment) # Five groups (Full Time, Retired, Unemployed, Student, Part Time)

# Drop variables with too many groups
medical_data_clean <- select(medical_data_clean, -c("Customer_id", "UID", "City", "State", "Timezone", "Job", "Zip", "Lat", "Lng", "Population", "Interaction", "County"))

# One-hot encoding for remaining categorical variables
# Add caret package for dummyvars function
install.packages("caret")
library(caret)

# Define one-hot encoding function
dummy <- dummyVars("~.", data=medical_data_clean)

# Perform the encoding
final_medical_data <- data.frame(predict(dummy, newdata=medical_data_clean))

# Export the cleaned data
write_csv(final_medical_data, "final_medical_data.csv")
