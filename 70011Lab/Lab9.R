rm(list = ls(all = TRUE))
#Some help on ordered logistic models: https://data.library.virginia.edu/fitting-and-interpreting-a-proportional-odds-model/

library(pscl)
library(brant)
library(pROC)
library(MASS)
library(car)
library(tidyverse)

setwd("C:/Work")

load("ESS_data2.Rdata")

table(data$polintr)

attributes(data$polintr)

data$polintr[data$polintr > 5] <- NA
data$polintr <- as.factor(4 - data$polintr)

table(data$polintr)

data_small <- filter(data, cntry == "GB")

mlm1 <- polr(polintr ~ trstprl + gndr + ctzcntr + pdwrk + 
               eduyrs + agea , data = data_small, Hess = TRUE)


summary(mlm1)

exp(cbind(OR = coef(mlm1), confint(mlm1)))

test <- cbind(pred = predict(mlm1, data_small, type = "probs"), data_small)
test$pred.3
ggplot(test, aes(eduyrs, pred.3, color = gndr)) + 
  geom_smooth(fun.y = "mean", se = F)

brant(mlm1)


data_small$fclcntr[data_small$fclcntr > 5] <- NA
data_small$fclcntr <- as.factor(4 - data_small$fclcntr)

table(data_small$fclcntr)

mlm2 <- polr(fclcntr ~ trstprl + gndr + ctzcntr + pdwrk + 
               eduyrs + agea , data = data_small, Hess = TRUE)


summary(mlm2)

exp(cbind(OR = coef(mlm2), confint(mlm2)))

brant(mlm2)

#Poisson Regression

p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")

p <- within(p, {
  prog <- factor(prog, levels = 1:3, 
                 labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})

summary(p)

ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")

m1 <- glm(num_awards ~ prog + math, 
          family = "poisson", data = p)

summary(m1)


exp(cbind(OR = coef(m1), confint(m1)))

with(m1, 
     cbind(res.deviance = deviance, df = df.residual,
           p = pchisq(deviance, df.residual, lower.tail = FALSE)))

m2 <- update(m1, . ~ . - prog)

anova(m2, m1, test="Chisq")

p$phat <- predict(m1, type = "response")

p <- p[with(p, order(prog, math)), ]


ggplot(p, aes(x = math, y = phat, colour = prog)) +
  geom_point(aes(y = num_awards), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1)

# Zero-inflated poisson

zinb <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
zinb <- within(zinb, {
  nofish <- factor(nofish)
  livebait <- factor(livebait)
  camper <- factor(camper)
})

summary(zinb)

ggplot(zinb, aes(count)) + geom_histogram() + scale_x_log10()

m1 <- zeroinfl(count ~ child + camper | persons, data = zinb)

summary(m1)

mnull <- update(m1, . ~ 1)

pchisq(2 * (logLik(m1) - logLik(mnull)), df = 3, lower.tail = FALSE)


p1 <- glm(count ~ child + camper, family = poisson, data = zinb)

summary(p1)


