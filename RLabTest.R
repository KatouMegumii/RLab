library(nycflights13)
library(tidyverse)

test1=filter(flights, month == 11 | month == 12)
test2=filter(flights,month==11,month==12)
test3=filter(flights, month == 11&month == 12)

test4=filter(flights,month==1,day==1)
test5=filter(flights,month==1&day==1)

df <- tibble(x = c(1, NA, 3))
print(df)

filter(df, x > 1)
filter(df, is.na(x) | x > 1)

round(4.256,1)
ceiling(4.25)
floor(4.25)

1&0

filter(flights,is.na(dep_time))

NA * 0