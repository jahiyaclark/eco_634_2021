require(here)
x_observed = c(2, 6)
print(x_observed)


read.csv(here("data", "hab.sta.csv"))

dat_hab <- data.frame(read.csv(here("data", "hab.sta.csv")))
dat_bird <- data.frame(read.csv(here("data", "bird.sta.csv")))

dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)
dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)
wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)
prod(dpois(x = wiwa_counts, lambda = 4.5))

sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

dat_all <- merge(dat_bird, dat_hab)

summary(dat_all$WIWA)
hist(dat_all$WIWA)
hist(dat_all$WIWA, breaks = 0:7)


0:7 - 0.5
hist(dat_all$WIWA, breaks = 0:7 - .5)

par(mfrow = c(1, 2))
data_wiwa = dat_all$WIWA
hist(dat, breaks = 0:(max(data_wiwa) + 1) - 0.5, main = "Histogram of\nWilson's Warbler counts")

dat = dat_all$GRJA
hist(dat, breaks = 0:(max(dat) + 1) - 0.5, main = "Histogram of\nGray Jay counts")


sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))

wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 5.673)
sum(log(dpois(x = wiwa_counts, lambda = 3.998)))

sum(log(dpois(x = dat_all$WIWR, lambda = 1.63)))
hist(dat_all$WIWR, breaks = 0:7 -.5, main = "Histogram of Winter Wrens", 
     xlab = "Winter wren count")

length(dat_all$WIWR) 
?length
dbinom(x,size = 1046, prob= 1.0)
sum(as.numeric(is.na(dat_all$WIWR)))









