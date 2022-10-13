df <- tibble(x = c(1, NA, 3))

filter(df, x > 1)

filter(df, is.na(x) | x > 1)
