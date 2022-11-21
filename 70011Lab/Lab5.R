library(tidyverse)
library(MASS)
library(car)
load("../ESS_data2.Rdata")

cor.test(data$trstun, data$trstprl, use = "complete.obs")

ggplot(data, aes(trstprl, trstun)) + geom_point(alpha = .1, shape = 1, position = position_jitter(width = 1, height = .5))

ggplot(data, aes(trstprl, trstun)) + geom_point(alpha = .1, shape = 1, position = position_jitter(width = 1, height = .5)) + geom_smooth(method = lm, se = FALSE, size = 2)

dplyr::select(data,imbgeco)

table(data$imbgeco)
attributes(data$imbgeco)
