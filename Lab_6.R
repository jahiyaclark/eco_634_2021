rm(list = ls())

require(palmerpenguins)

sse_mean = function(x) 
{
  n= length(x) -  sum(as.numeric(is.na(x)))
  return(sd(x, na.rm = TRUE)/ sqrt(n))
  }

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)

dat_penguins <- data.frame(penguins)
require(palmerpenguins)
sse_mean(penguins$bill_depth_mm)
 ##length of vector x##
length(penguins$bill_depth_mm)
sd(penguins$bill_depth_mm)
?sd
is.na(penguins$bill_depth_mm)
as.numeric(is.na(penguins$bill_depth_mm))
sum(as.numeric(is.na(penguins$bill_depth_mm)))
?is.na()

boxplot(flipper_length_mm ~ species, data = penguins)

dat_pen = subset(penguins, species != "Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1, 2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}


# for reproducibility
set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1, 2))
boxplot(flipper_length_mm ~ species, data = penguins)
boxplot(flipper_shuffled ~ penguins$species, xlab = "species")


t.test(dat_pen$flipper_length_mm ~ dat_pen$species)

set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)

boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1

t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test

t_test$estimate


diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed


table(dat_pen$species)

n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

print(c(observed = diff_observed, simulated = diff_simulated))

x = dat_pen$flipper_length_mm
n_1 = 68
n_2 = 152

dat_1 = sample(x, n_1, replace = TRUE)
dat_2 = sample(x, n_2, replace = TRUE)

diff_simulated = 
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

two_group_resample = function(x, n_1, n_2) 
{
  d_1 = sample(x, n_1, replace = TRUE)
  d_2 = sample(x, n_2, replace = TRUE)
  diff_in_means = mean(d_1, na.rm = TRUE) - mean(d_2, na.rm = TRUE)
  
     return(diff_in_means)}
  

set.seed(54321)
two_group_resample(dat_penguins$flipper_length_mm, 68, 152)
str(two_group_resample)

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_penguins$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences, main = "N= 2000", xlab = "Mean Differences", col = "yellow")
View(mean_differences)


t_test = t.test(flipper_shuffled ~ dat_pen$species)

str(t_test)
t_test$estimate

sum(abs(mean_differences))
sum(abs(mean_differences) >= diff_observed)
dev.off()

boxplot(dat_pen$bill_depth_mm ~ dat_pen$species,
        xlab = "Species", ylab = "Bill Depth (mm)")

q8_agg_means = aggregate(
  bill_depth_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)

diff_crit = diff(q8_agg_means[, 2])
diff_crit

q9_test <- t.test(dat_pen$bill_depth_mm ~ dat_pen$species)
q9_test

q9_diff_observed = round(diff(q9_test$estimate), digits = 3)
print(q9_diff_observed, digits = 3)


table(dat_pen$species)

n_1 = 68
n_2 = 152

q10_dat_1 = sample(dat_pen$bill_depth_mm, n_1, replace = TRUE)
q10_dat_2 = sample(dat_pen$bill_depth_mm, n_2, replace = TRUE)

q10_diff_simulated = 
  mean(q10_dat_1, na.rm = TRUE) - mean(q10_dat_2, na.rm = TRUE)

print(c(q_observed = q9_diff_observed, q_simulated = q10_diff_simulated))

two_group_resample(dat_pen$bill_depth_mm, 68, 152)

n = 200
q10mean_differences = c()
for (i in 1:n)
{
  q10mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$bill_depth_mm, 68, 152)
  )
}
hist(q10mean_differences, main = "Histogram of Sim. Diff in Means",
     xlab = "Mean Differences", col = "#BCD2EE")


sum(abs(q10mean_differences) >= diff_crit)

require(here)
png(
  filename = here("images", "hist_of_sim_diff_means")
)

