install.packages("psych")
require(psych)
here(dat_bird)
here(dat_hab)
pairs(iris)
pairs(iris[, c("Petal.Width", "Sepal.Width", "Sepal.Length")])
head(iris)
head(data_habitat)
hist(dat_bird$RUHU, 
     main = "Histogram of Rufous Hummingbird Abundance",
     xlab = "Number of birds counted",
     breaks = 0:7 - 0.5, 
     col = 
       adjustcolor(col= "#FF69B4", alpha.f = 0.1),
     border = "pink",
     ylim = c(0, 1000),
     xlim= c(0, 4))
pairs.panels(dat_hab[c("snag.dc1", "snag.dc2", "snag.dc4")])
pairs.panels(dat_hab[c(19, 20, 21)])
