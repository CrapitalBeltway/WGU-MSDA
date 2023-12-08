# Install needed libraries and packages
install.packages(c("tidyverse", "broom", "car"))
library(tidyverse)
library(broom)
library(car)

# Loading the cleaned data from previous class
final_medical_data <- read_csv("C:/Users/lexmm/Downloads/final_medical_data.csv")

# drop one level of each ordinal categorical variable in order to reduce cardinality
# "Soft_drink.0", "Overweight.0", "Anxiety.0", "AreaUrban", "EmploymentUnemployed", "MaritalNever.Married", "GenderPrefer.not.to.answer", "Initial_adminElective.Admission", "ServicesBlood.Work" 
prepared_data <- select(final_medical_data, -c("Soft_drink.0", "Overweight.0", "Anxiety.0", "AreaUrban", "EmploymentUnemployed", "MaritalNever.Married", "GenderPrefer.not.to.answer", "Initial_adminElective.Admission", "ServicesBlood.Work"))

# Research question: What causes larger Additional_charges?
#DV: final_medical_data$Additional_charges 
#IVs: VitD_levels, TotalCharge, Initial_days, Doc_visits, ReAdmis, 
# Initial_adminEmergency.Admission, Initial_adminObservation.Admission, ServicesCT.Scan, 
# ServicesIntravenous, ServicesMRI, Complication_risk_numeric, Arthritis, Stroke, HighBlood, 
# Diabetes, Hyperlipidemia, BackPain, Allergic_rhinitis, Reflux_esophagitis, 
# Asthma, Overweight.1, Anxiety.1

#Regression Model
model1 <- lm(Additional_charges ~ VitD_levels + TotalCharge + Initial_days + Doc_visits + ReAdmis + Initial_adminEmergency.Admission + Initial_adminObservation.Admission + ServicesCT.Scan + ServicesIntravenous + ServicesMRI + Complication_risk_numeric + Arthritis + Stroke + HighBlood + Diabetes + Hyperlipidemia + BackPain + Allergic_rhinitis + Reflux_esophagitis + Asthma + Overweight.1 + Anxiety.1, data = prepared_data)
summary(model1)

#metrics
model1_summ <- summary(model1) # R^2 of 0.433, Adj. R^2 of 0.431
model1_tidy <- tidy(model1)
sig_model1 <- filter(model1_tidy, model1_tidy$p.value <= 0.05)
vif1 <- tidy(vif(model1)) # Checking for collinearity, none of the values have VIF over 10, so we can keep them

# Calculate mean squared error
mean(model1_summ$residuals^2) #24286293


# Refine using backwards stepwide selection
model2 <- step(model1, direction = "backward", scope=formula(model1), trace=0)
model2_summ <- summary(model2) 
# Adj. R^2 is 0.432

#Metrics
model2_tidy <- tidy(model2)
sig_model2 <- filter(model2_tidy, model2_tidy$p.value <= 0.05)

# calculate mean squared error
mean(model2_summ$residuals^2) # 24309709

# plots
par(mfrow = c(2,2))
plot(model1, main="Initial Model")
par(mfrow = c(3,3))
avPlots(model1)

par(mfrow = c(2,2))
plot(model2, main="Reduced Model")
par(mfrow = c(3,3))
avPlots(model2)