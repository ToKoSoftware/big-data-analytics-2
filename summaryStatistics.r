

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




