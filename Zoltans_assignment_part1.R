library(tidyverse)
library(psych) # for describe
library(tidyverse) # for tidy code and ggplot\t
library(lm.beta)
library(gridExtra)

library(psych)
library(psych) # for describe 
library(car) # for residualPlots, vif, pairs.panels, ncvTest 
library(lmtest) # bptest 
library(sandwich) # for coeftest vcovHC estimator 
library(boot) # for bootstrapping 
library(lmboot) # for wild bootsrapping 
library(tidyverse)


#DATA FILE#
mydatafile = read_csv("https://raw.githubusercontent.com/kekecsz/PSYP14-Advanced-Scientific-Methods/main/Home%20assignment/home_sample_1.csv")

view(mydatafile)

summary(mydatafile)

#errors in datafile"
#Pain level = 55 ..... scale was supposed to be from 1-10 so 55 does not make sense"
#STAI_trait = 4.20 .... looks very low, values are supposed to be from 20-80
#Household_income = 3628 and 7834 .... looks weird a small amount for yearly income

str(mydatafile)

mydatafile %>% summary()

#Now I need to remove the weird values
mydatafileclean <- mydatafile %>% 
  filter(!STAI_trait == 4.2, !pain == 55, !household_income == 3628, !household_income == 7834)

mydatafileclean %>% summary()

view(mydatafileclean)

mydatafileclean %>% ggplot() + aes(x = age) + geom_histogram()

mydatafileclean %>% ggplot() + aes(y = sex, x = age) + geom_point()

#MODEL1
mod1 <- lm(pain ~ sex + age, data = mydatafileclean)
mod1

#Identify underlying pattern with GGplot#
mydatafileclean %>%
  ggplot() + aes(x = age, y = pain) + geom_point() + geom_smooth(method = "lm", se = F)


mydatafileclean %>%
  ggplot() + aes(x = sex, y = pain) + geom_point() + geom_smooth(method = "lm", se = F)

#Scatter plots
plot1 = mydatafileclean %>%
  ggplot() + aes(x = age, y = pain) + geom_point() + geom_smooth(method = "lm")

 plot1

 plot2 = mydatafileclean %>% ggplot() + aes(x = sex, y = pain) + geom_point() + geom_smooth(method = "lm") 

 plot2 

 #Grid arrange
 grid.arrange(plot1, plot2, nrow = 1)
 
#Summary
 sm = summary(mod1)
 
 #Akaike information criterion (AIC) is a mathematical method for evaluating how well a model fits the data it was generated from. In statistics, AIC is used to compare different possible models and determine which one is the best fit for the data.
 AIC(mod1)
 
 #Confidence Intervals for Model Parameters
 confint(mod1)
 
 #Standardized Coefficients
 lm.beta(mod1)

#MODEL 2
 mod2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = mydatafileclean)

 mod2

 #the adj. R squared statistic to see how much variance is explained by the new and the old model.
 
 summary(mod1)$adj.r.squared #0.07293794
 
 summary(mod2)$adj.r.squared #0.5150577

#AIC both models
AIC(mod1)
AIC(mod2)

anova(mod1, mod2)


#Cook’s distance. Highly inﬂuential cases can be identiﬁed by looking at the scatterplot with the regression line, by plotting the residual - levarage plot, and by using Cook’s distance.
mydatafileclean %>%
  ggplot() + aes(x = household_income, y = pain) + geom_point() + geom_smooth(method = "lm")


mod1 %>% plot(which = 5)

mod2 %>% plot(which = 5)

mod1 %>% plot(which = 4)

mod2 %>% plot(which = 4)
mod1 %>% plot(which = 5)


#You should always look at the cases with relatively high cook’s distance values to ensure they are valid data and not the result of an error.
mydatafileclean %>% slice(c(7, 22, 45, 71, 83))


mod1 %>% plot(which = 2)

mod2 %>% plot(which = 2)


#CHECKING DATA WITH INCOME VARIBLES STILL IN IT#

mydatafileclean2 <- mydatafile %>% 
  filter(!STAI_trait == 4.2, !pain == 55)

view(mydatafileclean2)

mod1_income_still <- lm(pain ~ sex + age, data = mydatafileclean2)

mod1_income_still

sm = summary(mod1_income_still)
sm

anova(mod1_income_still)

#Akaike information criterion (AIC) is a mathematical method for evaluating how well a model fits the data it was generated from. In statistics, AIC is used to compare different possible models and determine which one is the best fit for the data.
AIC(mod1_income_still)

#Confidence Intervals for Model Parameters
confint(mod1_income_still)

#Standardized Coefficients
lm.beta(mod1_income_still)

#MODEL 2 WITH INCOME STILL IN
mod2_income_still <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data = mydatafileclean2)

mod2_income_still

sm2 = summary(mod2_income_still)
sm2

AIC(mod2_income_still)

confint(mod2_income_still)

lm.beta(mod2_income_still)

#Plots

mydatafileclean2 %>%
  ggplot() + aes(x = household_income, y = pain) + geom_point() + geom_smooth(method = "lm")

mod1_income_still %>% plot(which = 5)

mod2_income_still %>% plot(which = 5)

mod1_income_still %>% plot(which = 4)

mod2_income_still %>% plot(which = 4)

describe(residuals(mod1_income_still))

mydatafileclean2 %>% slice(c(8, 23, 46, 73, 85))

mod1_income_still %>% plot(which = 2)

#ANOVA"
anova(mod1_income_still, mod2_income_still)

AIC (mod1_income_still)

residuals_mod1_income_still = enframe(residuals(mod1_income_still)) 
residuals_mod1_income_still %>% ggplot() + aes(x = value) + geom_histogram()

residuals_mod1_income_still

describe(residuals(mod1_income_still))

#Histograms model 2
residuals_mod2_income_still = enframe(residuals(mod1_income_still)) 
residuals_mod2_income_still %>% ggplot() + aes(x = value) + geom_histogram()

residuals_mod2_income_still

#skew and kurtosis
describe(residuals(mod2_income_still))

#LINEARITY#
mod2_income_still %>% residualPlots()

#Homogeneity
mod2_income_still %>% plot(which = 3) #the shape is not great but the bptest and nvctest are not signifianct

bptest(mod2_income_still) 

ncvTest(mod2_income_still) 

#No multicollinearity#

mod2_income_still %>% vif()

summary(mod2_income_still)

#Making the final model while excluding one of the predictors; cortisol_saliva

mod3 <- lm(pain ~ sex + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = mydatafileclean2)
mod3 %>% vif()

summary(mod3)

#ASSUMPTIONS MODEL 3#
residuals_mod3 = enframe(residuals(mod3)) 
residuals_mod3 %>% ggplot() + aes(x = value) + geom_histogram()

residuals_mod3

describe(residuals(mod3))

#Histograms model 3
residuals_mod3 = enframe(residuals(mod3)) 
residuals_mod3 %>% ggplot() + aes(x = value) + geom_histogram()

residuals_mod3

#skew and kurtosis
describe(residuals(mod3))

#LINEARITY#
mod3 %>% residualPlots()

#Homogeneity.#
mod3 %>% plot(which = 3) #still does not look great but again test not significant

bptest(mod3) 

ncvTest(mod3) 

sm3 = summary(mod3)
sm3

AIC(mod3)

confint(mod3)

lm.beta(mod3)

#Anova on final models
anova(mod1_income_still, mod3)

