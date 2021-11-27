require(palmerpenguins)
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218,
  alternative = "less"
)

t.test(flipper_length_mm ~ species, 
       data = subset(penguins, species != "Chinstrap"),
       alternative = "greater")

par(mfrow = c(1, 1))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass", xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")


require(palmerpenguins)
boxplot(body_mass_g ~ species, data = penguins)

dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = TRUE)

shapiro.test(dat_chinstrap$body_mass_g)

aggregate(body_mass_g ~ species, data = penguins, FUN = mean)

fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)

anova(fit_species)

fit_species = lm(body_mass_g ~ species, data = penguins)

boxplot(body_mass_g ~ species, data = penguins)

fit_additive = lm(body_mass_g ~ sex + species, data = penguins)

fit_interactive = lm(body_mass_g ~ sex * species, data = penguins)

anova(fit_interactive)

summary(fit_interactive)

require(palmerpenguins)

lm(bill_length_mm ~ body_mass_g, data = penguins)
boxplot(
  formula(fit_interactive),
  ylab = "Body Mass (g)",
  xlab = "",
  names = c("Female \nAdelie", "Female \nChinstrap", 
            "Female \nGentoo", "Male \nAdelie", "Male \nChinstrap",
            "Male \nGentoo"),las= 2,
  col = rainbow(6)
)


chin = subset(penguins, species == "Chinstrap", drop = TRUE) 
female <- subset(penguins, sex == "female", drop = TRUE)
mean(q_8$body_mass_g, na.rm = TRUE)


aggregate(q_8, sex)

?data.frame()
?subset
aggregate(chin, sex, r)

f_chin_means = aggregate(
  body_mass_g ~ sex, 
  data = chin, 
  FUN = "mean", 
  na.rm = TRUE)

f_chin_means


