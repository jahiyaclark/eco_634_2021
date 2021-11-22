dpois(x = 7, lambda = 10.4)
?dpois
?dbinom
dbinom(x = 0, size = 6, prob = 2/3)
dpois(4, 6 , 2/3)
ppois(q = 7, lambda = 10.4)

pbinom(4, 6, 2/3)

1 - pbinom(3, 6, 2/3)



pnorm(7.5, mean = 10, sd = 3)
1- pnorm(7.5, mean = 10, sd = 3)

### Question 1 ###

dbinom(x = 3, size = 4, prob = 0.75)

##Question 2##
pbinom(3, 4, 0.75)

### Question 3 ##
1- pbinom(3, 5, 0.75)

## Question 4 ##

pnorm(1.2, mean = 2, sd = 2)


### Question 5 ##

1- pnorm(1.2, mean = 2, sd = 2)

###Question 6

ps <- pnorm(c(1.2, 3.2), mean = 2, sd = 2)
ps[1]- ps[2]

pnorm(3.2, mean = 2, sd = 2) - pnorm(1.2, mean = 2, sd = 2)

?pnorm


