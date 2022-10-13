
#####################################################################3
# Title: "R practical - Lab 2 - Solutions"
#  "SOST70011 - Introduction to Statistical Modelling"
#
#Requires the packages tidyverse, nycflights13, focats

rm(list = ls(all = TRUE))

install.packages("tidyverse")
install.packages("nycflights13")
install.packages("forcats")

library("tidyverse")
library("nycflights13")
library("forcats")

##
flights
View(flights)


# *filter()* cases


filter(flights, month == 1, day == 1)

jan1 <- filter(flights, month == 1, day == 1)
print(jan1)


(dec25 <- filter(flights, month == 12, day == 25))

filter(flights, month == 1)


# Logical operators


nov_dec <- filter(flights, month %in% c(11, 12))
print(nov_dec)

# Missing values


NA > 5

10 == NA

NA + 10

NA / 2


x <- c(1,2,NA,3,4)
is.na(x)


df <- tibble(x = c(1, NA, 3))

filter(df, x > 1)

filter(df, is.na(x) | x > 1)

## Exercises 
#Using the flight data. Find all flights that:

# * Had an arrival delay of two or more hours

filter(flights, arr_delay >= 120)

# * Flew to Houston (IAH or HOU)

filter(flights, dest == "IAH" | dest == "HOU")

# * Were operated by United, American, or Delta

filter(flights, carrier %in% c("AA", "DL", "UA"))
 
# * Departed in summer (July, August, and September)

filter(flights, month >= 7, month <= 9)
 
#* Another useful dplyr filtering helper is between(). What does it do? Can you use it to simplify the code needed to answer the previous challenges?
  
#  The expression between(x, left, right) is equivalent to x >= left & x <= right.

#Of the answers in the previous question, we could simplify the statement of departed in summer (month >= 7 & month <= 9) using the between() function.

filter(flights, between(month, 7, 9))

# *arrange()*
 
arrange(flights, year, month, day)

 
arrange(flights, desc(arr_delay))


df <- tibble(x = c(5, 2, NA))
arrange(df, x)

arrange(df, desc(x))
 

## Exercises

#* How could you use arrange() to sort all missing values to the start? (Hint: use is.na()).


arrange(flights, desc(is.na(dep_time)), dep_time)

# The flights will first be sorted by desc(is.na(dep_time)).
# Since desc(is.na(dep_time)) is either TRUE when dep_time is missing, or 
# FALSE, when it is not, the rows with missing values of dep_time will 
# come first, since TRUE > FALSE.

# * Sort flights to find the most delayed flights. Find the flights that left earliest.

arrange(flights, desc(dep_delay))
 

# * Sort flights to find the fastest (highest speed) flights.

 
head(arrange(flights, air_time))
 

#* Which flights travelled the farthest? Which travelled the shortest?
  
 
head(arrange(flights, desc(distance / air_time)))
 


# *select()* columns


select(flights, year, month, day)


select(flights, year:day)


select(flights, -(year:day))

rename(flights, tail_num = tailnum)


select(flights, time_hour, air_time, everything())


# Exercises

#What happens if you include the name of a variable multiple times in a select() call?
  
#  The select() call ignores the duplication.
# Any duplicated variables are only included once, in the first location they appear.
# The select() function does not raise an error or warning or print any message 
#  if there are duplicated variables.

select(flights, year, month, day, year, year)


# Add new variables

 
flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)

print(flights_sml)

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
)

 
mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours
)


#If you only want to keep the new variables, use transmute():
 
transmute(flights,
          gain = arr_delay - dep_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours
)

 
# 
transmute(flights,
          dep_time,
          hour = dep_time %/% 100,
          minute = dep_time %% 100
)

 
#Logs: log(), log2(), log10(). Logarithms are an incredibly useful
#transformation for dealing with data that ranges across multiple orders of
#magnitude. They also convert multiplicative relationships to additive, a
#feature we’ll come back to in modelling.
 
(x <- 1:10)

lag(x)

lead(x)
 

#Cumulative and rolling aggregates: R provides functions for running sums,
#products, mins and maxes: cumsum(), cumprod(), cummin(), cummax(); and dplyr
#provides cummean() for cumulative means. 

 
cumsum(x)

cummean(x)


#Logical comparisons, <, <=, >, >=, !=, which you learned about earlier. If
#you’re doing a complex sequence of logical operations it’s often a good idea
#to store the interim values in new variables so you can check that each step
#is working as expected.

#Ranking: there are a number of ranking functions, but you should start with
#min_rank(). It does the most usual type of ranking (e.g. 1st, 2nd, 2nd, 4th).
#The default gives smallest values the small ranks; use desc(x) to give the
#largest values the smallest ranks.

 

y <- c(1, 2, 2, NA, 3, 4)

min_rank(y)

min_rank(desc(y))
 
