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


# Schwarze haben einen geringeren Stundenlohn als Weiße (black, wage)
wage_black = summary(lm(dat$wage~dat$black))
wage_black$coefficients
plot(dat$wage, dat$black)
cor(dat$wage, dat$black)
# -> Schwach negativ korreliert -> "Umso weniger ich verdiene, umso eher bin ich schwarz"

#- Hoher IQ bzw. KWW = hohes Einkommen
IQ_wage = summary(lm(dat$IQ~dat$wage))
IQ_wage
plot(dat$IQ, dat$wage)
cor(dat$IQ, dat$wage)
# -> keine Korrelation

#- many years of schooling = hohes Einkommen
educ_wage = summary(lm(dat$educ~dat$wage))
educ_wage
plot(dat$educ, dat$wage)
cor(dat$educ, dat$wage)
# -> Mittel positiv korreliert -> "Umso länger ich in der Schule war, umso mehr verdiene ich"

#- Hohe Bildung der Eltern (fatheduc, motheduc) = hohe years of schooling 
#- ältere Leute = erfahrener = höheres Einkommen
#- Kinder die bei beiden Elternteilen aufgewachsen sind = hohe years of schooling 
#- 4 Jahre College = hohes Einkommen
#- married = geringeres Einkommen wegen weniger Überstunden und keine Lust zu Reisen
#- Bücherei Karte = many years of schooling 