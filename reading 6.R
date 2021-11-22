# Clear your R environment to make 
# sure there are no stray variables.

rm(list = ls())

pol_n_predation = 26
pol_n_no_predation = 184
pol_n_total = 210
pol_predation_rate = (pol_n_predation/pol_n_total)
  
psd_n_predation = 25
psd_n_no_predation = 706
psd_n_total = 731
psd_predation_rate = (psd_n_predation/psd_n_total)



print(
  paste0(
    "The seed predation rate for Polyscias fulva is: ",
    round(pol_predation_rate, digits = 3))) 

print(
  paste0(
    "The seed predation rate for Pseudospondias microcarpa is: ",
    round(psd_predation_rate, digits = 3)))


tab <- matrix(c(7, 5, 14, 19, 3, 2, 17, 6, 12), ncol=3, byrow=TRUE)
colnames(tab) <- c('colName1','colName2','colName3')
rownames(tab) <- c('rowName1','rowName2','rowName3')
tab <- as.table(tab)

View(tab)

seed <- matrix(c(pol_n_predation, pol_n_no_predation, pol_n_total, pol_predation_rate, psd_n_predation, psd_n_no_predation, psd_n_total, psd_predation_rate), 
               ncol = 2, byrow = FALSE)
colnames(seed) <- c("Polyscias fulva (pol)", "Pseudospondias microcarpa (psd)")

rownames(seed) <- c("Any Taken", "None Taken", "N", "Predation Rate")

seed <- as.table(seed)
head(seed)

require(here)
png(
  filename = here("images", "Seed Predation Table.png"),
  width = 1700, height = 1600, 
  res = 180)

dev.off()

