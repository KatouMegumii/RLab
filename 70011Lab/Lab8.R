rm(list = ls(all = TRUE))

# package for data cleaning and visualization
library(tidyverse)
# package for multilevel modeling
library(lme4)

setwd("C:/Work")
load("ESS_data2.Rdata")

ess <- data
rm(data)

# find out how many countries and cases we have 
count(ess, cntry) %>% print(n = Inf)

# explore our outcome variable
ess %>% 
  group_by(cntry) %>% 
  summarise(mean = mean(imbgeco, na.rm = T), 
            SD = sd(imbgeco, na.rm = T), 
            miss = mean(is.na(imbgeco))) %>% 
  mutate_if(is.numeric, ~round(., 2)) %>% 
  print(n = 50)

# look at distribution by country
ess %>% 
  ggplot(aes(imbgeco)) + 
  geom_density() +
  facet_wrap(~cntry)
 
# look at education by country

ess %>% 
  ggplot(aes(eduyrs)) + 
  geom_density() +
  facet_wrap(~cntry)  

# empty multilevel model
m0 <- lmer(imbgeco ~ 1 + (1 | cntry), data = ess)
# details of results
summary(m0)

# calculate ICC
# use R as a calculator
0.4049/(5.5142 + 0.4049)

#use the variances from the model (slightly different answer as no rounding)
vars_m0 <- as.data.frame(VarCorr(m0))
(ICC_m0 <- vars_m0[1,4] / (vars_m0[1,4] + vars_m0[2,4]))

library(lattice)
qqmath(ranef(m0, condVar = TRUE))

# random intercept with a control variable
m1 <- lmer(imbgeco ~ 1 + eduyrs + (1 | cntry), data = ess)
# print results
summary(m1)

# model with random slope
m2 <- lmer(imbgeco ~ 1 + eduyrs + (1 + eduyrs | cntry), data = ess)
# print results
summary(m2)

# another way to see random effect
qqmath(ranef(m2, condVar = TRUE))

# yet another way to look at the random effects
# save coefficients
coefs_m2 <- coef(m2)
# print random effects and best line
coefs_m2$cntry %>%
  mutate(cntry = rownames(coefs_m2$cntry))  %>% 
  ggplot(aes(eduyrs, `(Intercept)`, label = cntry)) + 
  geom_point() + 
  geom_smooth(se = F, method = lm) +
  geom_label(nudge_y = 0.15, alpha = 0.5) +
  theme_bw() +
  labs(x = "Slope", y = "Intercept")
