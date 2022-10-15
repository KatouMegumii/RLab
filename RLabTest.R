library(nycflights13)
library(tidyverse)

filter(flights,month==1,day==1)

df <- tibble(x = c(1, NA, 3))

filter(df, x > 1)
filter(df, is.na(x) | x > 1)
