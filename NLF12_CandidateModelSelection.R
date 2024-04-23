#leaps does 'exhaustive search' for us
library(leaps)

#set directory
setwd("C:/Users/chawl/OneDrive - University of Cincinnati/Prissha/SPRING24/PROB STAT 2")
dir()

#load data ACTIVITY 1

NFLData = read.table(file="NFL12_14.txt", header=TRUE)
dimnames(NFLData)

#make candidate models ACTIVITY 2
CandModels <- regsubsets( Wins ~ First + ThirdAtt + ThirdPct + RZAtt + RZPct + RatioRush_Pass + OppFirst + OppThirdMd, data=NFLData , nbest=1, nvmax=8 ) # CHECK the numbers for options "nbest" and "nvmax" by referring to its help documentation.  Type "? regsubsets"

summary(CandModels)

# Activity 3.

Rsq = summary(CandModels)$rsq
adjRsq = summary(CandModels)$adjr2
Cp = summary(CandModels)$cp
ModelNo = 1:length(Rsq)

round(rbind(ModelNo,Rsq,adjRsq,Cp),4)

par(mfrow=c(2,2))
plot(ModelNo, Rsq, xlab="Model No.", ylab="Coeff. of Determination", type="b", pch=20)
plot(ModelNo, adjRsq, xlab="Model No.", ylab="Adj. Coeff. of Determination", type="b", pch=20)
plot(ModelNo, Cp, xlab="Model No.", ylab="Cp statistic", type="b", pch=20)

# Activity 4

M3 <- lm( Wins ~ RZAtt+RZPct+RatioRush_Pass, data=NFLData )  
M4 <- lm( Wins ~ RZAtt + RZPct+RatioRush_Pass +OppFirst, data=NFLData ) 
M5 <- lm( Wins ~ RZAtt + RZPct+RatioRush_Pass +OppFirst+ThirdPct, data=NFLData)

summary(M3)
summary(M4)
summary(M5)

anova(M3,M4)
anova(M4,M5)
anova(M3,M5)
