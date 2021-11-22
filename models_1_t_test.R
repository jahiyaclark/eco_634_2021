require(palmerpenguins)
dat_ade = droplevels(subset(penguins, species == "Adelie"))
hist(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")
boxplot(dat_ade$body_mass_g, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")
boxplot(dat_ade$body_mass_g ~ dat_ade$sex, main = "Adelie Penguins: Body Mass", xlab = "body mass (g)")

pen_fem_sex <- droplevels(subset(dat_ade, sex != "male"))

pen_male_sex <- droplevels(subset(dat_ade, sex != "female"))
t.test( pen_fem_sex$body_mass_g,)
t.test(pen_male_sex$body_mass_g, mu= 4000, alternative = c("greater"(dat_ade$sex, dat_ade$body_mass_g)

                                                           
t.test(pen_fem_sex$body_mass_g, pen_male_sex$body_mass_g)                                                           
t.test(body_mass_g ~ sex, data = dat_ade)

