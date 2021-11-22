ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}

curve(
  exp_fun(x, 0.3, (1/15)), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)

param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

#####
error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")

####

###

error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)

par(mfrow = c(1, 2))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed, main = "Exponentially Distributed Errors", xlab = "", ylab = "")

y_observed_3 <- y_pred + rexp(n = n_pts, rate = 1.2)

plot(x_sim, y_observed_3, main = "Exponentially Distributed Errors", xlab = "", ylab = "")

##
par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)

##

par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")


dev.off()
require(here)
dat_disp <- read.csv(here("data", "dispersal.csv"))
plot(dat_disp$dist.class, dat_disp$disp.rate.ftb)
View(dat_disp)

###
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 0.0087, (1/245)), add = TRUE)

View(dat_disp)
###
curve(
  exp_fun(x, 0.97, (1/405)), add = TRUE)

curve(line_point_slope(dat_disp, 500, 0.4, 0.5))

dev.off()
##### Question 1

exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}

curve(
  exp_fun(x, 1.2, 0.2), add = TRUE, col = "red", 
  from = 0, to = 20, ylim = c(0, 1.9), xlab = "", ylab = ""
)

curve(
  exp_fun(x, 1.9, 0.1), add = TRUE, col = "black")

curve(
  exp_fun(x, 1.9, 0.3), add = TRUE, col = "black", 
  lty = "dotted"
)

curve(
  exp_fun(x, 1.2, 0.4), add = TRUE, col = "red",
  lty = "dotted"
)

dev.off()
###Question 5

ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ricker_fun(x, 25, 0.2), 
  from = 0, to = 50, add = TRUE)

curve(
  ricker_fun(x, 20, 0.2), add = TRUE, lty = "dotted")
)

curve(
  ricker_fun(x, 10, 0.2), add = TRUE, lty = "dotted"
)

curve(
  ricker_fun(x, 75, 0.3), add = TRUE, col = "red"
)

curve(
  ricker_fun(x, 50, 0.3), add = TRUE, col = "red",
  lty = "dotted"
)

curve(
  ricker_fun(x, 40, 0.3), add = TRUE, col = "red",
  lty = "dotted"
)

dev.off()
###Question 8
png(
  filename = here("images", "lab_04_hist_01.png"))
  
plot(dat_disp$dist.class, dat_disp$disp.rate.ftb)
locator(1)

curve(line_point_slope(x, 797, 0.187, -0.0005961044), add = TRUE)


(0.000827-0.6527)/(1399.315-305.76)

###Question 10

plot(dat_disp$dist.class, dat_disp$disp.rate.ftb)

curve(
  exp_fun(x, 0.97, (1/405)), add = TRUE)

### Question 12

plot(dat_disp$dist.class, dat_disp$disp.rate.ftb)

curve(
  ricker_fun(x, 0.0087, (1/245)), add = TRUE)


#### Question 14

summary(dat_disp$disp.rate.ftb)
lm(dat_disp)
lm(dat_disp$disp.rate.ftb)
?lm

locator(15)

dat_disp$resids_ricker <- c(00.59611652, 0.69225830, 0.77321980, 0.75297943, 0.69225830,
                           0.61129680, 0.48479446, 0.39371277, 0.30263109, 0.19636912,
                          0.13058790, 0.09010715, 0.05468649, 0.03950621, 0.02432593)


dat_disp$resids_linear <- c( 0.672,  0.646, 0.606,  0.550, 0.489,  0.454,  0.393,  0.332,
                           0.241, 0.196, 0.150,  0.0850,
                            0.0496,  0.00408, 0.00331)


dat_disp$resids_exp <- c(0.78333999, 0.71249868, 0.63659727, 0.55563577, 0.43419352,
                         0.31781137, 0.22672968, 0.16600856, 0.13564799, 0.10022734,
                        0.06986678, 0.05468649, 0.03950621, 0.03444612, 0.021432)


View(dat_disp)
resid_linear <- c(dat_disp$disp.rate.ftb - dat_disp$resids_linear)
resid_exp <- c(dat_disp$disp.rate.ftb - dat_disp$resids_exp)
resid_ricker <- c(dat_disp$disp.rate.ftb - dat_disp$resids_ricker)


disp_resids <- data.frame(dat_disp$resids_linear, dat_disp$resids_exp, dat_disp$resids_ricker)

require(here)
png(
  filename = here("images", "lab_05_hist.png"))


par(mfrow = c(3, 1))
hist(resid_linear, main = "Histogram of Linear Resids", xlab = "", col = "light green", xlim = c(-0.5, 0.5))
hist(resid_exp, main = "Histogram of Exponential Resids", xlab = "", col = "sky blue", xlim = c(-0.5, 0.5))
hist(resid_ricker, main = "Histogram of Ricker Resids", xlab = "", col = "lavender", xlim = c(-0.7, 0.3))


dev.off()




