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
library(lm.beta)
library(lme4)

library(psych) # for describe\t
library(tidyverse) # for tidy code and ggplot\t library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer library(MuMIn) # for r.squaredGLMM
library(MuMIn)

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

#Load data files
data_file_3 = data_sample_3 = read.csv("https://tinyurl.com/b385chpu") 

data_file_4 = data_sample_4 = read.csv("https://tinyurl.com/4f8thztv") 

#View data
view(data_file_3)

view(data_file_4)

#Filter error
data_file_3_filtered <- data_sample_3 %>% 
  filter(!household_income == -7884)

#Assign class to a grouping factor
data_file_3_filtered = data_file_3_filtered %>%
  mutate(hospital = factor(hospital))

view(data_file_3_filtered)

#Build a model
Model_1_part3 <- lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1 | hospital), data = data_file_3_filtered)

#Coefficients 
summary(Model_1_part3)

#Confidence intervals of the coefficients for all fixed effect predictors
confint(Model_1_part3)

lm.beta(Model_1_part3)

#Marginal R squared with confidence intervals
r2beta(Model_1_part3, method = "nsj", data = data_file_3_filtered)

#Marginal and conditional R squared values
r.squaredGLMM(Model_1_part3)

#Regression obtained on data file 3 to predict pain in data file 4
mod_reg_3_4 = lmer(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + (1 | hospital), data = data_file_4)

mod_reg_3_4

# calculate predicted values

pred_test_3_4 <- predict(mod_reg_3_4, data_file_4, allow.new.levels = TRUE)

# now we calculate the sum of squared residuals

RSS = sum((data_file_4[, "pain"] -pred_test_3_4)^2)
RSS

#RSS and TSS

mod_mean <- lm(pain ~ 1, data = data_file_4)

TSS = sum((data_file_4$pain - predict(mod_mean))^2)

TSS

1-(RSS/TSS) #Compare this R2 to the marginal and conditional R2 values computed for the model on data file 3

#Build a new linear mixed effects model on dataset 3 predicting pain. However, instead of including all predictors, you should only include the most influential predictor from the previous model.

New_model_mixedeffect <- lmer(pain ~ cortisol_serum + (1 | hospital), data = data_file_3_filtered)

New_model_mixedeffect

# Intercept and random slope
Intercept_random_slope <- lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = data_file_3_filtered)

Intercept_random_slope

summary(Intercept_random_slope)

Random_int_slope_opt = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), control = lmerControl(optimizer = "Nelder_Mead"), data = data_file_3_filtered)

Random_int_slope_opt

data_file_3_filtered <- data_file_3_filtered %>% mutate(prediction_slope = predict(Intercept_random_slope))

#Regression line
data_file_3_filtered %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 1) + geom_line(color = "red",
                                                       aes(y = prediction_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 5)
#Fix order
New_order <- c("hospital_1", "hospital_2", "hospital_3", "hospital_4", "hospital_5", "hospital_6", "hospital_7", "hospital_8", "hospital_9", "hospital_10")
library(plyr)
data_file_3_ordered  <- arrange(transform(data_file_3_filtered,
                                                hospital=factor(hospital,levels=New_order)),hospital)
#Regression line on ordered data
data_file_3_ordered %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 1) +		
  geom_line(color='red', aes(y=prediction_slope, x=cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)
