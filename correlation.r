install.packages("foreign")
library(foreign)

dat=read.dta("CARD.DTA")
min(dat$age)
median(dat$age)

# Die Effektstärke ist im Rahmen der Korrelation der Korrelationskoeffizient r selbst. Laut Cohen: Statistical Power Analysis for the Behavioral Sciences (1988), S. 79-81 sind die Effektgrenzen
#ab 0,1 (schwach),
#ab 0,3 (mittel) und
#ab 0,5 (stark).
# [0-0.1], [0.1, 0,3], [0.3, 0.5], [0.5, 1]

subset_cor = subset(dat, select = c(nearc2, nearc4, educ, age, fatheduc, motheduc, momdad14, sinmom14, step14, south66, smsa, south, smsa66, enroll, KWW, married, libcrd14, black, wage, IQ, exper, lwage, expersq), na.rm = TRUE)
korr_tab = cor(subset_cor)
korr_tab
write.csv(korr_tab, file = "Korrelationstabelle.csv", sep = ";")

# Zusammenhang zwischen Einkommen und den Faktoren Schwarz und Bildung
wage_from_black_and_educ = summary(lm(wage~black+educ, data = dat))
wage_from_black_and_educ
# -> Beides hoch signifikant