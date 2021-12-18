rm(list = ls())

require(here)
rope <- read.csv(here("data", "rope.csv"))

rope$rope.type = factor(rope$rope.type)
levels(rope$rope.type)
                        
n_obs = nrow(rope)
n_groups = length(levels(rope$rope.type))             

ss_tot = sum((rope$p.cut - (mean(rope$p.cut)))^2)
df_tot = n_obs - 1

aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type), 
  FUN = mean)

aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) mean(x))

agg_resids = 
  aggregate(
  x = rope$p.cut,
  by = list(rope$rope.type),
  FUN = function(x) mean(x))

str(agg_resids)

agg_sq_resids = 
  aggregate(
    x = rope$p.cut,
    by = list(rope$rope.type),
    FUN = function(x) sum((x - mean(x))^2) )

str(agg_sq_resids)

ss_within = sum(agg_sq_resids$x)
df_within <- n_obs - n_groups

ss_among = ss_tot - ss_within
df_among <- n_groups -1

ms_among  =  ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)
                  
f_ratio = ms_among / ms_within
f_ratio
f_pval = 1 - pf(f_ratio, df_among, df_within)
f_pval

fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$"Sum Sq"


digits_check = 5

fit_1 = lm(p.cut ~ rope.type, data=rope)
anova(fit_1)
anova_fit_1 = anova(fit_1)

anova_fit_1$Df == c(df_among, df_within)

round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)

blaze <- (subset(rope, rope.type == "BLAZE"))

mean(blaze$p.cut)


dev.off()


fit_rope_1 = lm(p.cut ~ rope.type, data = rope)
summary(fit_rope_1)

  fit_rope_1$coefficient[[1]]
