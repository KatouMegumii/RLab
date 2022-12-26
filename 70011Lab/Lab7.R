
#install.packages("tidyverse")
#install.packages("car")
#install.packages("MASS")
#install.packages("pROC")
#install.packages("brant")
#install.packages("pscl")
#install.packages("arm")

library(pscl)
library(brant)
library(pROC)
library(MASS)
library(car)
library(tidyverse)
library(arm)

setwd("C:/Work")
load("ESS_data2.Rdata")

# PART I

## Basic logistic model

#Build a model explaining voting in the last elections
#Recode as 1 = vote and 0 no vote

table(data$vote)

data$vote <- 2 - data$vote
table(data$vote)

#Relationship between gender and voting
table(data$gndr, data$vote)

#Conditional probability.
tab <- table(data$gndr, data$vote)

prop_tab <- round(prop.table(tab, margin = 1), 2)

tab
prop_tab

#Calculate the relative risks

#Relative risk of not voting (for males vs females)
(rr1 <- prop_tab[1]/prop_tab[2]) #[a/(a+b)]/[c/(c+d)]

#Relative risk of voting (for males vs females)
(rr2 <- prop_tab[3]/prop_tab[4])

(odds <- rr1/rr2)

#Logit model using the two variables
lm1 <- glm(vote ~ gndr, data = data, family = "binomial")

summary(lm1)

#Is the relationship significant?
#On what scale do you think the regression coefficient is? Try to interpret it.

#calculate the odds ratios and the confidence intervals
exp(cbind(OR = coef(lm1), confint(lm1)))

#Try to interpret the coefficients

#Another variable we will be looking at is if respondents signed a petition in the last year:
table(data$sgnptit)
attributes(data$sgnptit)

#Recode it as 1 if they signed a petition and 0 otherwise
#Add to a new variable called 'sign'
data$sign <- 1
data$sign[data$sgnptit == 2] <- 0

#Recode the missing
data$sign[data$sgnptit > 2] <- NA

table(data$sign, data$sgnptit)

#Do a logistic regression explaining this variable using gndr

lma <- glm(sign ~ gndr, data = data, family = "binomial")

summary(lma)

#Are the intercept and slope significant?

#Calculate the odds ratios and the confidence intervals and interpret them
exp(cbind(OR = coef(lma), confint(lma)))

#Investigate the predicted probabilities to vote based on our model for male and females
test <- cbind(pred = predict(lm1, data, type = "response"), data)

ggplot(test, aes(gndr, pred)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean")

#Is the conclusion of the graph the same as the regression coefficients?

#Do a similar graph for the prediction of signing a petition. Interpret the graph and compare
#it to the findings in the regression.

test <- cbind(pred = predict(lma, data, type = "response"), data)

ggplot(test, aes(gndr, pred)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean")

## Multiple logistic models

#Develop the model and include a number of predictors we have used before

lm2 <- glm(vote ~ trstprl + trstun + gndr + ctzcntr + pdwrk + cntry + eduyrs +
             agea, data = data, family = "binomial")

summary(lm2)

#Which variables are significant and which are not?
#What variables have a positive effect and which have a negative effect?

#Calculate the odds ratios and the confidence intervals and interpret the coefficients
exp(cbind(OR = coef(lm2), confint(lm2)))

#Calculate a similar model explaining the signing of a petition. 
#Answer all the same questions as before.

lmb <- glm(sign ~ trstprl + trstun + gndr + ctzcntr + pdwrk + cntry + eduyrs +
             agea, data = data, family = "binomial")

summary(lmb)

exp(cbind(OR = coef(lmb), confint(lmb)))

#Create some graphs to help us understand the results

test <- cbind(pred = predict(lm2, data, type = "response"), data)

ggplot(test, aes(agea, pred, color = cntry)) + 
  geom_smooth()

ggplot(test, aes(agea, pred, color = ctzcntr)) + 
  geom_smooth()

#Do two similar graphs for the model explaining signing a petition

test <- cbind(pred = predict(lmb, data, type = "response"), data)

ggplot(test, aes(agea, pred, color = cntry)) + 
  geom_smooth()

ggplot(test, aes(agea, pred, color = ctzcntr)) + 
  geom_smooth()

## Binned Residual Plots 

binnedplot(fitted(lmb), 
           residuals(lmb, type = "response"), 
           nclass = NULL, 
           xlab = "Expected Values", 
           ylab = "Average residual", 
           main = "Binned residual plot", 
           cex.pts = 0.8, 
           col.pts = 1, 
           col.int = "gray")

#Interactions and non-linear effects

#Non-linear effects for age and education on likelihood to vote

lm3 <- glm(vote ~ trstprl + trstun + gndr + ctzcntr + pdwrk + cntry + 
             eduyrs + I(eduyrs^2) + I(eduyrs^3) + agea + I(agea^2) + I(agea^3),
           data = data, family = "binomial")

summary(lm3)

#Are the non-linear effects significant? 
#From their sign, what do you think the relationship looks like?

test <- cbind(pred = predict(lm3, data, type = "response"), data)

ggplot(test, aes(agea, pred)) + 
  geom_smooth()

ggplot(test, aes(eduyrs, pred)) + 
  geom_smooth()

#Interpret the non-linear relationship

#Do a similar model and tables for the signature of petitions.

lmc <- glm(sign ~ trstprl + trstun + gndr + ctzcntr + pdwrk + cntry + 
             eduyrs + I(eduyrs^2) + I(eduyrs^3) + agea + I(agea^2) + I(agea^3),
           data = data, family = "binomial")

summary(lmc)

test <- cbind(pred = predict(lmc, data, type = "response"), data)

ggplot(test, aes(agea, pred)) + 
  geom_smooth()

ggplot(test, aes(eduyrs, pred)) + 
  geom_smooth()

#Investigate if there is an interaction between gender and if they are citizens when explaining voting.

lm4 <- glm(vote ~ trstprl + trstun + gndr + ctzcntr + pdwrk + cntry + 
             eduyrs + agea + gndr:ctzcntr,
           data = data, family = "binomial")

summary(lm4)

#Interpret the coefficient. 

#Calculate the odds ratios and the confidence intervals and interpret them as well.
exp(cbind(OR = coef(lm4), confint(lm4)))

#Do a graph and see what the relationship looks like
test <- cbind(pred = predict(lm4, data, type = "response"), data)

ggplot(test, aes(gndr, pred, fill = ctzcntr)) + 
  geom_bar(position = "dodge", stat = "summary", fun = "mean")

#Interpret the results in the graphic

#Let’s see if the effect of age is different by country

lm5 <- glm(vote ~ trstprl + trstun + gndr + ctzcntr + pdwrk + cntry + 
             eduyrs + agea + gndr:ctzcntr + cntry:agea,
           data = data, family = "binomial")

summary(lm5)

#For what countries is the interaction significant? How do you interpret the coefficients?

test <- cbind(pred = predict(lm5, data, type = "response"), data)

ggplot(test, aes(agea, pred, color = cntry)) + 
  geom_smooth(se = F)

#How do you interpret the fact that the age line is different for different countries?

#Extend the model that that explains the signature of petitions using the cntry:agea interaction.
#Interpret the coefficients and do the two graphs.

lmd <- glm(sign ~ trstprl + trstun + gndr + ctzcntr + pdwrk + cntry + 
             eduyrs + agea + pdwrk:gndr + cntry:agea,
           data = data, family = "binomial")

summary(lmd)

test <- cbind(pred = predict(lmd, data, type = "response"), data)

ggplot(test, aes(agea, pred, color = cntry)) + 
  geom_smooth(se = F)

## Comparing models

anova(lm2 , lm3, test = "Chisq")
AIC(lm2, lm3)

#What is the conclusion based on the Chi-squared and the AIC? Which model should we choose?
#In a similar way compare “lm2” with “lm4” and “lm4” with “lm5”. 
#Which models would you select?

anova(lm2 , lm4, test = "Chisq")
AIC(lm2, lm4)

anova(lm4 , lm5, test = "Chisq")
AIC(lm4, lm5)

## Testing of performance

#ROC Curves

test <- cbind(pred = predict(lm5, data, type = "response"), data)

class <- roc(vote ~ pred, data = test)
class

#What do these indicators tell us about the fit?

outlierTest(lm5)

influencePlot(lm5,	id.method = "identify")

vif(lm5) 

durbinWatsonTest(lm5)

#Use the same statistics for the last model you ran that explains the likelihood of signing a petition.

test <- cbind(pred = predict(lmd, data, type = "response"), data)

class <- roc(sign ~ pred, data = test)
class

outlierTest(lmd)

influencePlot(lmd,	id.method = "identify")

vif(lmd) 

durbinWatsonTest(lmd)

## Probit regression

pm5 <- glm(vote ~ trstprl + trstun + gndr + ctzcntr + pdwrk + cntry + 
             eduyrs + agea + gndr:ctzcntr + cntry:agea,
           data = data, family = binomial(link = "probit"))

summary(pm5)

#Compare the results with “lm5”
