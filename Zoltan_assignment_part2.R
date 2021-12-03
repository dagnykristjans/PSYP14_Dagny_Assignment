library(tidyverse)
library(lm.beta)
library(gridExtra)
library(dplyr)
library(psych)
library(psych) # for describe 
library(car) # for residualPlots, vif, pairs.panels, ncvTest 
library(lmtest) # bptest 
library(sandwich) # for coeftest vcovHC estimator 
library(boot) # for bootstrapping 
library(lmboot)
library(caret)
library(leaps)

mydatafile = data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1") 

#Exclude errors
datafile1 <- mydatafile %>% 
  filter(!STAI_trait == 4.2, !pain == 55)

view(datafile1)


#Model
model_1 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight + IQ + household_income , data = datafile1)

model_1

#Model diagnostics

#Confidence Intervals for Model Parameters
confint(model_1)

#Standardized Coefficients
lm.beta(model_1)

#Plots
model_1 %>% plot(which = 5)

model_1 %>% plot(which = 4)

describe(residuals(model_1))

exclude_outliers <- datafile1 %>% slice(c(46, 64, 85))

model_1 %>% plot(which = 2)

view(exclude_outliers)

#Histograms
residuals_model_1 = enframe(residuals(model_1))
residuals_model_1 %>%
  ggplot() + aes(x = value) + geom_histogram()

#skew and kurtosis
describe(residuals(model_1))

#LINEARITY#
model_1 %>% residualPlots()

#Homoscedasticty#
model_1 %>% plot(which = 3) #-- looks funnel shaped which is not good

bptest(model_1)  # --- looks fine

ncvTest(model_1) # --- looks fine

#No multicollinearity#
model_1 %>% vif() # --- looks fine

summary(model_1) # -- STAI and SEX not significant 


#Backward regression

model1_backwards = step(model_1, direction = "backward")

summary(model1_backwards)

confint(model1_backwards)

#Standardized Coefficients
lm.beta(model1_backwards)

#ANOVA
anova(model1_backwards)
anova(model_1, model1_backwards)

#Adjusted R squared
summary(model_1)$adj.r.squared #0.504055

summary(model1_backwards)$adj.r.squared #0.5073085

#AIC
AIC(model_1) #484.0054

AIC(model1_backwards) #476.3015

#All of the above model comparison methods indicate that the backward regression model performs better. 

#Theory based model#
theory_based_model <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = datafile1)

#ANOVA
anova(model1_backwards, theory_based_model)

#AIC
AIC(model1_backwards)

AIC(theory_based_model)

#PART 2 OF ASSIGNMENT 2#
data_file_2 = read.csv("https://tinyurl.com/87v6emky")

view(data_file_2)

backward_model <- lm(pain ~ age + pain_cat + cortisol_serum + mindfulness, data = data_file_2)

#Backward regression

backward_model = step(model_data_file_2, direction = "backward")

summary(backward_model)

# calculate predicted values

pred_test <- predict(theory_based_model, data_file_2)
pred_test_back <- predict(backward_model, data_file_2)

View(data_file_2)
# now we calculate the sum of squared residuals

RSS_test = sum((data_file_2[, "pain"] -pred_test)^2)
RSS_test_back = sum((data_file_2[, "pain"] - pred_test_back)^2)
RSS_test
RSS_test_back


