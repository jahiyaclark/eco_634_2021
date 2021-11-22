require("here")
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

### Reproductive Success and Failure
n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(n_success, n_years)

### Reproductive Catastrophe and Late Filling

late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

binom.test(
  n_success,
  n_years,
  p = normal_fill_rate) 

binom.test(
  n_success,
  n_years,
  p = normal_fill_rate,
  alternative ='less')

## t test and wilcox

t.test(n_success)

## F Distri ex.
veg = read.csv(here("data", "vegdata.csv"))
head(veg)

boxplot(pine ~ treatment, data = veg)

## variance test


var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))


## F tests assumes normality 

shapiro.test(veg$pine[veg$treatment=="control"])

shapiro.test(veg$pine[veg$treatment=="clipped"])

### non parametric variance test

fligner.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

bartlett.test(pine ~ treatment, data=veg)

fligner.test(pine ~ treatment, data = veg)

## Comparing two mean samples

t.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control' , 'clipped')
)

wilcox.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped'))

## tests for paired samples

control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

t.test(control, clipped, paired=TRUE)

wilcox.test(control, clipped, paired=TRUE)

## Marbled Salamander
require(here)
disp = read.csv(here("data", "dispersal.csv"))

disp
plot(disp$disp.rate.ftb, disp$disp.rate.eb)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs')


cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

## comparing two distributions


plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)
## comparing two or more proportions

prop.test(c(4,16),c(40,250))

prop.test(c(8, 32), c(80, 500))

## dependence of variables in a conting. table

    ## chi square test

owls = matrix(c(16, 9, 4, 11), nrow=2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)

fisher.test(owls)

### Bird habitat data 
birds   = read.csv(here("data", "bird.sta.csv"))
hab     = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by=c("basin", "sub", "sta"))

# Create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

prop.test(c(29, 314), c(173, 873), 
          alternative = c("less"))

?prop.test

chisq.test(br_creeper_table)

require(palmerpenguins)
fit_fl_sp = 
  lm(
    formula = flipper_length_mm ~ species,
    data = penguins)

fit_species <- 
  lm(
    formula = penguins$body_mass_g ~ penguins$species
  )

fit_sex <-  
  lm(
    data = penguins,
    formula = body_mass_g ~ sex
    )

fit_both <- 
  lm(
    formula = body_mass_g ~ species * sex,
    data = penguins
  )


boxplot(
  formula(fit_species),
  main = "Penguins by Body Mass",
  ylab = "Body Mass (g)",
  xlab = "",
  col = "pink"
)
head(penguins)

fit_sex <-  
  lm(
    formula = penguins$body_mass_g ~ penguins$sex)
 
boxplot(
  formula(fit_both),
  ylab = "Body Mass (g)",
  xlab = "",
  names = c("Female \nAdelie", "Female \nChinstrap", 
            "Female \nGentoo", "Male \nAdelie", "Male \nChinstrap",
            "Male \nGentoo"),las= 2,
  col = rainbow(6)
)

fit_both <- 
  lm(
    formula = penguins$body_mass_g ~ penguins$species * penguins$sex
  )

bartlett.test(penguins$body_mass_g ~ penguins$species)

bartlett.test(penguins$body_mass_g ~ penguins$sex)




qnorm(c(0.025, 0.975))
require(palmerpenguins)


sex_group = aggregate(
  body_mass_g ~ sex,
  data = penguins,
  FUN = c)
str(sex_group)


sp_group = aggregate(
  body_mass_g ~ species,
  data = penguins,
  FUN = c)

bartlett.test(sex_group$body_mass_g)

bartlett.test(sp_group$body_mass_g)


