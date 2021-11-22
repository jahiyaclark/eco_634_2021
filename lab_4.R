# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)


require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")


mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

mean(penguins$body_mass_g, na.rm = FALSE)
sd(penguins$body_mass_g, na.rm = FALSE)
nrow(penguins)


dat_1 = rnorm(n = 344, mean = 4202, sd = 802)
dat_2 = rnorm(n = 344, mean = 4202, sd = 802)
dat_3 = rnorm(n = 344, mean = 4202, sd = 802)
dat_4 = rnorm(n = 344, mean = 4202, sd = 802)

par(mfrow = c(1, 1))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

set.seed(12)
dat_unif = runif(n = 1500, min = 0, max = 4)
hist(dat_unif)

set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)

set.seed(1)
View(set.seed(1))
head(set.seed(1))


set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)

curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

dev.off

n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

resids(line_point_slope(dat$x, guess_x, guess_y, guess_slope))


dat$residual <-  (dat$y_predicated - dat$y_observed)

resid 

dat$y_predicated <- c(0.1012500, -0.1241179, 0.2093763, -0.4074218, -0.1085965,  0.3864613,  0.3037460,  0.2978222,
                    -0.3424526, -0.3823739)

View(dat
     )

View(dat$residual
     )
head(dat)

resid(dat$residual)


summary(dat$residual)

summary(dat)

sum(dat$residual)

abs(dat$residual)


plot(dat$residual ~ dat$y_observed)


hist(dat$residual)

#my own work now##


set.seed(1)

pop_sd = 2.4
pop_mean = 10.4

norm_17 = rnorm(n = 17, mean = pop_mean, sd = pop_sd)
norm_30 = rnorm(n = 30, mean = pop_mean, sd = pop_sd)
norm_300 = rnorm(n = 300, mean = pop_mean, sd = pop_sd)
norm_3000 = rnorm(n = 3000, mean = pop_mean, sd = pop_sd)

par(mfrow = c(2, 2))

hist(norm_17, main = "Sample Size = 17", xlab = "17 Samples", col = 
       adjustcolor(col= "#FF69B4", alpha.f = 0.1),
     border = "pink")
hist(norm_30, main = "Sample Size = 30", xlab= "30 Samples", col = 
       adjustcolor(col= "#98F5FF", alpha.f = 0.1),
     border = "pink")
hist(norm_300, main = "Sample Size = 300", xlab = "300 Samples", col = 
       adjustcolor(col= "#8B7355", alpha.f = 0.1),
     border = "pink")
hist(norm_3000, main = "Sample Size = 3000", xlab = "3000 Samples", col = 
       adjustcolor(col= "#FFB90F", alpha.f = 0.1),
     border = "pink")

require(here)
png(
  filename = here("images", "lab_04_hist_01.png"),
  width = 1500, height = 1600, 
  res = 180)

par(mfrow = c(2, 2))

hist(norm_17, main = "Sample Size = 17", xlab = "17 Samples", col = 
       adjustcolor(col= "#FF69B4", alpha.f = 0.1),
     border = "pink")
hist(norm_30, main = "Sample Size = 30", xlab= "30 Samples", col = 
       adjustcolor(col= "#98F5FF", alpha.f = 0.1),
     border = "pink")
hist(norm_300, main = "Sample Size = 300", xlab = "300 Samples", col = 
       adjustcolor(col= "#8B7355", alpha.f = 0.1),
     border = "pink")
hist(norm_3000, main = "Sample Size = 3000", xlab = "3000 Samples", col = 
       adjustcolor(col= "#FFB90F", alpha.f = 0.1),
     border = "pink")

dev.off()



install.packages("pwr")
?pwr
require(pwr)
pwr.anova.test()

library(pwr)
delta=10
sigma=20
r= delta/sigma
pwr.t.test(d= 0.3, sig.level = 0.05, power = 0.20, type = "one.sample")

pwr.anova.test(n= null, d= 0.3, sig.level = 0.05, power = 0.20)

# Generate a vector of x-values
x = seq(-2, 25, length.out = 10000)
y = dnorm(x, mean = 10.4, sd = 2.4)

plot(x, y, main = "Standard Normal PDF: mean=10.4 and sd=2.4 ", type = "l", xlim = c(0, 20), ylim = c(0,0.2))
abline(h = 0)


norm_17 = rnorm(n = 17, mean = pop_mean, sd = pop_sd)


?dnorm
?svg

?pdf

pdf(
  file = here( "images", "norm_1.pdf"), 
  )


png(
  filename = here("images", "question_14.png"),
  width = 1700, height = 1600, 
  res = 180)



n_pts = 100
x_min = 0
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))
plot(dat, ylim= c(-0.75, 1.5), main = "Plot of Random Data", pch = 10, cex = 1.3, col = "#00FF7F")


line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

x= dat$x
x1= 5
y1 = 0.4
slope1= 0.2
curve(line_point_slope(x, x1, y1, slope1), add= TRUE)
line_point_slope(x, x1, y1, slope1)
dat$y_predicted= c(line_point_slope(x, x1, y1, slope1))

dat$resids= c(dat$y_predicted - dat$y_observed)

sum(dat)
summary(dat)

hist(y_observed= rnorm(n_pts))
view(y_observed)
View(y_observed)
hist(x, main = "Histogram of Random Data",
  col =  
  adjustcolor(col= "#E066FF", alpha.f = 0.1),
  border = "#4169E1")
boxplot(x, main= "Boxplot of Random Data", col = "#4169E1")
plot(dat$x, dat$y_observed, main = "Random Data")


resids(line_point_slope(dat$x, guess_x, guess_y, guess_slope)))

hist(dat$resids,main = "Histogram of Residuals",
     col =  
       adjustcolor(col= "#EED8AE", alpha.f = 0.1),
     border = "#EE5C42", xlab = "Residuals")

plot(dat$resids~ dat$y_predicted, main = "Plot of Predicted Values by Residual", 
     ylab = "Residuals",
     xlab = "Predicted", 
     col = "#27408B")

par(mfrow = c(1, 2))


dev.off()


