library(tidyverse)
library(haven)
data=haven::read_dta("ESS7e02_1.dta")
table(data$trstprl)
attributes(data$trstprl)
data=data %>%
  mutate(trstprl = replace(trstprl, trstprl>70, NA))
summarise(data,sd_trstprl=sd(trstprl,na.rm=TRUE))
