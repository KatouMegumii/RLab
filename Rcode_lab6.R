#R practical - Lab 6 - Regression Assumptions
#SOST70011 Introduction to Statistical Modelling - 22/23

#Some reference guides:
#https://data.library.virginia.edu/diagnostic-plots/
#https://www.ercankaradas.com/book/intro.html

install.packages("tidyverse")
install.packages("car")
install.packages("MASS")

library(MASS)
library(car)
library(tidyverse)

 
setwd("C:/Work") #Set this as your working directory if you are loading the data via R.
load("ESS_data2.Rdata") #Miss out if you have loaded the data into R directly.

reg4 <- lm(trstprl ~ trstun + gndr + agea + eduyrs + ctzcntr + pdwrk, data = data)
summary(reg4)
plot(reg4)

#Testing outliers
outlierTest(reg4)
#How many outliers does this test indicate?

qqPlot(reg4, main = "QQ Plot")
#Do you notice any outliers based on this graph?

leveragePlots(reg4)
#Do you notice any outliers based on these graphs?

#Influential Observations
influencePlot(reg4, id.method="identify")
#What is your conclusion based on these graphs?

#Non-normality
qqPlot(reg4)
#What is your conclusion based on this graph

#Histogram of the standardized residuals:
sresid <- studres(reg4)
hist(sresid, freq = FALSE)

#Non-constant Error Variance
ncvTest(reg4)
spreadLevelPlot(reg4)

#Multi-collinearity
vif(reg4)

#Nonlinearity
# Evaluate Nonlinearity
# component + residual plot
crPlots(reg4)

#Non-independence of Errors
durbinWatsonTest(reg4)

#Nonlinear Effects
#Relationship between trstprl and age in the data
ggplot(data, aes(agea, trstprl)) +
  geom_point(alpha = .1, shape = 1,
             position = position_jitter(width = 1, height = .5)) +
  geom_smooth(se = FALSE, size = 2)

#Modelled relationship between trstprl and age
pred <- predict(reg4, data)
data_pred <- cbind(data, pred)
ggplot(data_pred, aes(agea, pred)) +
  geom_point(alpha = .1, shape = 1) +
  geom_smooth(se = FALSE, size = 2)

#Create a new age variable that measures age in periods of 10 years.
data$age_cat <- cut(data$agea, c(13, 25, 35, 45, 55, 65, 120))
data$age_cat <- as.factor(data$age_cat)
table(data$agea, data$age_cat)

reg5 <- lm(trstprl ~ trstun + gndr + eduyrs + ctzcntr + pdwrk + age_cat, data = data)
summary(reg5)

#How do you interpret the coefficients for the age category?
#Do you think the relationship is linear?
#How does the R-square compare with the previous model? What does that mean?

pred <- predict(reg5, data)
data_pred <- cbind(data, pred)
ggplot(data_pred, aes(agea, pred)) +
  geom_point(alpha = .1, shape = 1) +
  geom_smooth(se = FALSE, size = 2)

#Add a quadratic effect for age
reg6 <- lm(trstprl ~ trstun + gndr + eduyrs + ctzcntr + pdwrk +
             agea + I(agea^2), data = data)
summary(reg6)

#Graph to show the predicted scores.
pred <- predict(reg6, data)
data_pred <- cbind(data, pred)
ggplot(data_pred, aes(agea, pred)) +
  geom_point(alpha = .1, shape = 1) +
  geom_smooth(se = FALSE, size = 2)

#Include a cubed effect for age
reg7 <- lm(trstprl ~ trstun + gndr + eduyrs + ctzcntr + pdwrk +
             agea + I(agea^2) + I(agea^3), data = data)
summary(reg7)

pred <- predict(reg7, data)
data_pred <- cbind(data, pred)

ggplot(data_pred, aes(agea, pred)) +
  geom_point(alpha = .1, shape = 1) +
  geom_smooth(se = FALSE, size = 2)

data <- data %>%
  mutate(agea = replace(agea, agea > 90, 90))

data <- data %>%
  mutate(eduyrs = replace(eduyrs, eduyrs > 27, 27))

reg8 <- lm(trstprl ~ trstun + gndr + ctzcntr + pdwrk + cntry +
             agea + I(agea^2) + eduyrs + I(eduyrs^2), data = data)
summary(reg8)

#Try to interpret the coefficients.
#Redo all the regression diagnositics using the new model. Is the model less problematic?
plot(reg8)
outlierTest(reg8)
qqPlot(reg8)
leveragePlots(reg8)
influencePlot(reg8, id.method = "identify")
sresid <- studres(reg8)
hist(sresid, freq = FALSE)
ncvTest(reg8)
spreadLevelPlot(reg8)
vif(reg8)
crPlots(reg8)
durbinWatsonTest(reg8)

#When and how to center a variable.
#The function scale() can be used to center a variable around its mean
reg4 <- lm(trstprl ~ trstun + gndr + scale(agea, scale = FALSE) + eduyrs + ctzcntr + pdwrk, data = data)
summary(reg4)
plot(reg4)

#Standardization
#After standardization, the variable means are all 0 and variances are all 1.
reg4 <- lm(trstprl ~ trstun + gndr + scale(agea) + eduyrs + ctzcntr + pdwrk, data = data)
summary(reg4)
plot(reg4)

#ANOVA and Regression Modelling
#https://bookdown.org/steve_midway/DAR/understanding-anova-in-r.html
#Run an ANOVA model that explains trust in parliament using gender
anova1 <- aov(trstprl ~ gndr, data = data)
summary(anova1)
coefficients(anova1)

reg2 <- lm(trstprl ~ gndr, data = data)
summary(reg2)

anova2 <- aov(trstprl ~ trstun + gndr, data = data)
summary(anova2)
coefficients(anova2)

reg3 <- lm(trstprl ~ trstun + gndr, data = data)
summary(reg3)



