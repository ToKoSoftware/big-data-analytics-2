
install.packages("foreign")
library(foreign)


setwd("C:/Users/Karud/Documents/R")
dat=read.dta("CARD.DTA")

age =dat$age #Alter wird als Variable gespeichert

min(age)    #Minimum
max(age)    #Maximum
mean(age)   #Mittelwert
median(age) #Median
sd(age)     #Standardabweichung


KWW=dat$KWW #KWW wird als Variable gespeichert

length(KWW[!is.na(KWW)]) #Zählen wie viele Einträge von KWW nicht NA sind

min(KWW, na.rm =TRUE)    #Minimum
max(KWW, na.rm =TRUE)    #Maximum
mean(KWW, na.rm =TRUE)   #Mittelwert
median(KWW, na.rm =TRUE) #Median
sd (KWW, na.rm =TRUE)    #Standardabweichung




# Hypothese: Ausbildung wirkt sich (kausal) auf das Gehalt aus
#
# Modell: y = beta0 + beta1 * x + epsilon
#         y : Gehalt
#         x : Ausbildung
#         beta0, beta1 : zu schätzenden Koeffizienten
#         epsilon : Schätzfehler



#Automatisierte Berechung der linearen Regression: Funktion lm
#Start mit Gehalt und Ausbildung
model1 = summary(lm(dat$lwage~dat$educ))   
model1


#lwage um prozentuale Aussage treffen zu können
model2 = summary(lm(dat$lwage~dat$educ))   
model2


#Erfahrung wird hinzu genommen.
summary(lm(dat$wage~dat$educ+dat$exper,data=dat))  



install.packages("AER")   #Packages für Instrumentalvariableansatz installieren
library (AER)             #Library öffnen


#College-Nähe wird nun mit einbezogen. Instrumentalvariablenansatz, 
#da College-Nähe nur mit der Ausbildung zusammen hängt und nicht mit dem Gehalt
summary(ivreg(dat$wage~dat$educ+dat$exper|dat$nearc4+dat$exper),diagnostics=TRUE)


#Vermutung: Gehalt wird durch Ausbildung, Hautfarbe und Erfahrung beeinflusst.
#College-Nähe beeinflusst die Ausbildung
#Ausführung ergibt: Hautfarbe hat keinen signifikanten Einfluss auf das Gehalt
summary(ivreg(dat$wage~dat$educ+dat$black+dat$exper|dat$nearc4+dat$black+dat$exper),diagnostics=TRUE)


#Vermutung: Hautfarbe hat einen Einfluss auf die Ausbildung. (Deshalb nur rechts von der Pipe)
#College-Nähe und Hautfarbe beeinflussen die Ausbildung
#Ausführung ergibt: Hautfarbe hat keinen Einfluss auf das die Ausbildung 
summary(ivreg(dat$wage~dat$educ+dat$exper|dat$nearc4+dat$black+dat$exper),diagnostics=TRUE)

#Da die Hautfarbe keinen Einfluss hat, wird diese wieder aus dem Code entfernt


#lwage um prozentuale Aussage treffen zu können
summary(ivreg(dat$lwage~dat$educ+dat$exper|dat$nearc4+dat$exper),diagnostics=TRUE)





