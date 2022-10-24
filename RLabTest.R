library(dplyr)
library(nycflights13)
library(tidyverse)

filter(flights, month == 11 | month == 12)
filter(flights,month==11,month==12)
filter(flights, month == 11&month == 12)

filter(flights,month==1,day==1)
filter(flights,month==1&day==1)

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

by_group=group_by(flights,dest)
test1=arrange(by_group,year,month,day,.by_group = TRUE)
head(test1)
test2=arrange(by_group,year,month,day,.by_group = FALSE)
head(test2)

data=data.frame(x=c(1,2,NA))
arrange(data,desc(is.na(x)))
arrange(data,desc(is.na(x)),x)

flights[c("month","year")]

view(flights)

arr1=arrange(flights,dep_time,sched_dep_time,dep_delay)
arr2=arrange(flights,dep_time&sched_dep_time&dep_delay)
arr3=arrange(flights,dep_time|sched_dep_time|dep_delay)

select(flights,month,year)
select(flights,month|year)

x=c(4,4,6,10,10,14)
ex=sum(x^2)/6

x=matrix(c(3,6,7,10,15,19),nrow=1)
y=matrix(c(4,4,6,10,10,14),nrow=6)
xy=y%*%x
Exy=sum(xy)/36

n=20
c(1:n)