require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")
install.packages("simpleboot")
require("simpleboot")
?two.boot()

chin_pen = droplevels(subset(penguins, species == "Chinstrap"))

flip_off <- two.boot(
        sample1 = chin_pen$flipper_length_mm, 
         sample2 = dat_ade$flipper_length_mm, 
         FUN = mean, 
         R = 1000, 
         na.rm = TRUE)

hist(flip_off)

require(here)
tree_data = read.csv(here("data", "vegdata.csv"))


boxplot(pine ~ treatment, dat = tree_data)

dat_tree = droplevels(subset(tree_data, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = clip_and_control)

table(tree_data$treatment)

aggregate(pine ~ treatment, dat = tree_data, FUN = mean)

wilcox.test(x = 17.875, y = 1.875, alternative = "two.sided")

tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

# sum(tree_boot$t >= 0)
# sum(tree_boot$t < 0)
require(boot)
boot.ci(tree_boot)
hist(tree_boot$t, main = "Bootstrap sampling distribution")
quantile(tree_boot$t, c(0.025, 0.975))



#### Bird Data ##
rm(list = ls())
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_hab = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_hab,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

"s.sidi" %in% dat_all

names(dat_all)

# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

s_sidi_mean <- mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd <- sd(dat_all$s.sidi, na.rm = TRUE)
# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.

dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
dat_all$s.sidi.standardized <- (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd

mean(dat_all$b.sidi.standardized)

sd(dat_all$b.sidi.standardized)

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")


fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

m = 10000 
result = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  
  result[i] = coef(fit_resampled_i)[2]
} 

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

quantile(result, c(.05))
###aggregate(flipper_length_mm ~ species, data= penguins, FUN = function(x) shapiro.test(x)$pvalue

### Question 1- 4 ##
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

pen_boot=two.boot(
  subset(penguin_dat, species == "Adelie")$flipper_length_mm,
   subset(penguin_dat, species == "Chinstrap")$flipper_length_mm,
 
  FUN = mean,
  R = 10000,
  na.rm = TRUE)

hist(pen_boot$t, main = "Histogram of Stimulated Chinstrap and 
     Adelie Flipper Mean Diff", xlab = "", col = "#D0A9F5")
quantile(pen_boot$t, 0.025)

boot.ci(pen_boot)
mean(pen_boot$t, na.rm = TRUE) - (quantile(pen_boot$t, 0.025))
sd(pen_boot$t)
### Question 5
 ?ecdf
pen_ecdf <- ecdf(pen_boot$t)

## 6##
1 - pen_ecdf(-4.5)
### 7 ##
pen_ecdf(-8)
### 9 ###
require(here)
veg = read.csv(here("data", "vegdata.csv"))

dat_veg = droplevels(subset(veg, treatment %in% c("control", "clipped")))

wilcox.test(pine ~ treatment, data = dat_veg, alternative = "two.sided")

aggregate(pine ~ treatment, dat = dat_veg, FUN = mean)

wilcox.test(x = 17.875, y = 1.875, alternative = "two.sided")

##10 ##

tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

quantile(tree_boot$t, c(0.025, 0.975))

## 11##
clip_mean <- mean(subset(dat_tree, treatment == "clipped")$pine) 
con_mean <- mean(subset(dat_tree, treatment == "control")$pine)
clip_mean - con_mean

## 13 ##
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_hab = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird, 
  dat_hab,
  by = c("basin", "sub"))

s_sidi_mean <- mean(dat_all$s.sidi, na.rm = TRUE)
s_sidi_sd <- sd(dat_all$s.sidi, na.rm = TRUE)

dat_all$s.sidi.standardized <- (dat_all$s.sidi - s_sidi_mean)/s_sidi_sd


##14 ###

dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

m = 10000 
result = numeric(m) 

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  
  result[i] = coef(fit_resampled_i)[2]
} 
head(result)
hist(result, main = "Histogram of Monte Carlo 
resapmled slopes of
     Simpson's diversity indices", xlab = "Sampled slopes", col = "#A9F5D0")

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]

abline(v = slope_observed, col = "blue", lwd = 2)
abline(v = quantile(result, c(.05)), col = "red", lty = "dotted", lwd = 2)

quantile(result, c(.05)) 
print(slope_observed)


