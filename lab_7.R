# Create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN = 1, FUN = max)


###Mean values in each column
apply(dat, MARGIN = 2, FUN = mean)


moths = read.csv(here("data", "moths.csv"))
head(moths)

m = 10000

# numeric() creates an vector of length m with all values initialized to zero
result = numeric(m)
head(result)

for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace=TRUE))
}

mean(result)
quantile(result,c(0.025,0.975))

install.packages("boot")

require(boot)

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot = 
  boot(
    data = moths$anst,
    statistic = boot_mean,
    R = 10000)
print(myboot)

str(myboot)

mean(moths$anst)
myboot$t0
mean(myboot$t) - myboot$t0
sd(myboot$t)

##CI
quantile(
  myboot$t,
  c(0.025, 0.975))

moth_dat = moths[,-1]
head(moth_dat)

n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)

####   data[sample(moths$anst), replace=TRUE] ####

n = nrow(moth_dat) #number of rows or sample observations

m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)


# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix from the resampled rows.
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums of the new data matrix.
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)

### First Draft

rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)


## Second draft
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)


### check
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}
rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)

##building the rarefaction curve

rarefact = rarefaction_sampler(moths[,-1], 10000)
head(rarefact)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness',
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))


### Calculate a parametric 95% CI for mean bill length (in mm) for the 
### Gentoo penguins in the penguins dataset from package palmerpenguins 
### using your SSE function. For this calculation you should use Student’s 
### t-distribution to calculate the critical values.


sse_mean = function(x) 
{
  n= length(x) -  sum(as.numeric(is.na(x)))
  return(sd(x, na.rm = TRUE)/ sqrt(n))
}

sse_mean(dat_pen$bill_length_mm)

quantile(dat_pen$bill_length_mm,c(0.05,0.95), na.rm = TRUE)

dt(dat_pen$bill_length_mm, df = 123)
qt(0.95, 122)

dat_pen = subset(penguins, species == "Gentoo")
mean(dat_pen$bill_length_mm, na.rm = TRUE)

as.numeric(is.na(dat_pen$bill_length_mm))
sum(as.numeric(is.na(dat_pen$bill_length_mm)))
length(dat_pen$bill_length_mm) - 1

sd(dat_pen$bill_length_mm, na.rm = TRUE)
 
### SSE
((1.96 * sd(dat_pen$bill_length_mm, na.rm = TRUE))/sqrt(length(dat_pen$bill_length_mm) - 1)
 * qt(0.95, 122) )

mean(dat_pen$bill_length_mm, na.rm = TRUE) - 42.93
52.19 - 4.574878

quantile(dat_pen$bill_length_mm,c(0.025,0.975), na.rm = TRUE)
### Question 1 ###
dat_pen = subset(penguins, species == "Gentoo")
sum(as.numeric(is.na(dat_pen$bill_length_mm)))
n_pen <- length(dat_pen$bill_length_mm) - 1

##Question 2##
sd_pen <- sd(dat_pen$bill_length_mm, na.rm = TRUE)
sd_pen
### Question 3###

pen_crit <- qt(c(0.025, 0.975), df = n_pen-1)
pen_crit

### Question 4##
sse_pen <- (sd_pen)/sqrt(n_pen)
sse_pen

## Question 5 ##
radius_pen <- pen_crit * sse_pen
radius_pen
gen_pen <- mean(dat_pen$bill_length_mm, na.rm = TRUE)
gen_pen
ci_pen <-gen_pen + radius_pen 
ci_pen

47.50488 - 0.5500946
###Question 6 - 7
m = n_pen
result = numeric(m)


for(i in 1:m)
{
  result[i] = mean(sample(dat_pen$bill_length_mm, replace=TRUE), na.rm = TRUE)
}
result
pen_boot_mean <- mean(result, na.rm = TRUE)



quantile(result,c(0.025,0.975), df= n_pen-1)

print(quantile(result,c(0.025,0.975), df= n_pen-1))

### QUestion 7##

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

require(boot)
billboot = boot(
  data = dat_pen$bill_length_mm,
  statistic = boot_mean,
  R = 10000)

print(billboot)

str(billboot)

## Question 8 ##
quantile(
  billboot$t,
  c(0.025, 0.975))

### Question 9 ###

moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  for(i in 1:n_iterations)
  {
    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace=TRUE)
       
      t1 = input_dat[rows_j, ]
      
      t2 = apply(t1, 2, sum)
      
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}


### Question 10 

###	I wasn’t able to add shading to the confidence envelope.

### Question 11

rarefact = rarefaction_sampler(moths[,-1], 10000)

rarefact

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))


matplot(
  rare,
  type='l',
  xlab='Number of Sample sites',
  ylab='Number of Moth Species',
  main='Range of Number of Moth Speices 
  by Sampling Site',
  col = 6:8)


legend(
  'bottom',
  legend=c('mean','2.5%','97.5%'),
  lty=c(6,7,8),col=c(6,7,8), inset=c(.1,.1))

require(here)
png(
  filename = here("images", "rarefaction.png"),
  width = 1500, height = 1600, 
  res = 180)
dev.off()
