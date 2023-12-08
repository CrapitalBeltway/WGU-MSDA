# We used the same prepared data from the other task.
# The DV we are going to use id REAdmis and the the IVs are Age, Income, SoftDrink, Overweight,
# Anxiety, Complication_risk, Gender, VitD_level, Doc_visits, Full_meals_eaten, VitD_supp,
# InitialAdmin_emergencu, InitialAdmin_Observation, HighBlood, Stroke, Arthritis, Diabetes, 

#Install libraries and packages
install.packages("tidyverse", "caret")
library(tidyverse)
library(caret)

# Making new dataframe with only the variables that will be used in the regression analysis
logreg <- select(prepared_data, CaseOrder, ReAdmis, GenderFemale, GenderMale, VitD_levels, Doc_visits, Full_meals_eaten, VitD_supp, Initial_adminEmergency.Admission, Initial_adminObservation.Admission, HighBlood, Stroke, Arthritis, Diabetes, Hyperlipidemia, BackPain, Allergic_rhinitis, Asthma, Age, Income, Soft_drink.1, Overweight.1, Anxiety.1, Complication_risk_numeric)

# Check the correlation between the DV and IVs
logreg_corr <- data.frame(cor(logreg[,2:23]))
# None of the IVs appear to have a strong correlation with the DV

# Create the initial model
formula1 <- ReAdmis ~ GenderFemale + GenderMale + VitD_levels + Doc_visits + Full_meals_eaten + VitD_supp + Initial_adminEmergency.Admission + Initial_adminObservation.Admission + HighBlood + Stroke + Arthritis + Diabetes + Hyperlipidemia + BackPain + Allergic_rhinitis + Asthma + Age + Income + Soft_drink.1 + Overweight.1 + Anxiety.1 + Complication_risk_numeric
logmodel1 <- glm(formula1, data = logreg, family = binomial(logit))

summary(logmodel1)
# Only VitD_supp is statistically significant
# ND = 13146, RD = 13123, AIC = 13169

# Reduced Model with Insignificant terms removed
logmodel2 <- glm(ReAdmis ~ VitD_supp, data = logreg, family = binomial(logit))

summary(logmodel2)
# ND: 13146, RD: 13140, AIC: 13144

# Using stepwise selection to reduce the model 
logmodel3 <- step(logmodel1, direction = "backward", scope=formula(logmodel1), trace=0)
summary(logmodel3)
# ND: 13146, RD: 13131, AIC: 13141

# Removing the insignificant terms from model3
logmodel4 <- glm(ReAdmis ~ VitD_supp + Initial_adminEmergency.Admission, data = prepared_data, family = binomial(logit))
summary(logmodel4)
# ND: 13146, RD: 13136, AIC: 13142
# These numbers are worse than model3, so we'll go with model3


# Confusion matrix for the reduced model
PredModel3 <- predict(logmodel3, logreg, type="response")
PredModel4 <- round(PredModel3)
ReducedMatrix <- confusionMatrix(factor(PredModel4, levels = 0:1), factor(prepared_data$ReAdmis, levels = 0:1))

ReducedMatrix