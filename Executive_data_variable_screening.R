#EXAUSTIVE SEARCH VARIABLE SCREENING

#leaps does 'exhaustive search' for us
library(leaps)

#set directory
setwd("C:/Users/chawl/OneDrive - University of Cincinnati/Prissha/SPRING24/PROB STAT 2")
dir()

#load data
exsal = read.table(file="EXECSAL2.txt", header=TRUE)
ls()

dimnames(exsal)

Y=exsal[,"Y"]
X1=exsal[,"X1"]
X2=exsal[,"X2"]
X3=exsal[,"X3"]
X4=exsal[,"X4"]
X5=exsal[,"X5"]
X6=exsal[,"X6"]
X7=exsal[,"X7"]
X8=exsal[,"X8"]
X9=exsal[,"X9"]
X10=exsal[,"X10"]

#conducting the exhaustive search: we get 10 models
CandModels = regsubsets(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,data=exsal,nvmax=10,nbest=1)


# Compute r^2, adj r^2, cp for these 10 models
ModelSummary=summary(CandModels)
Rsq = ModelSummary$rsq
adjRsq = ModelSummary$adjr2
Cp=ModelSummary$cp

#display as table, round to 2 decimals
round(rbind(Rsq,adjRsq,Cp),2)

#build the 3 chosen models-4,5,6
best4pred = lm(Y~X1+X2+X3+X4,data=exsal)
best5pred = lm(Y~X1+X2+X3+X4+X5,data=exsal)
best6pred = lm(Y~X1+X2+X3+X4+X5+X9,data=exsal)

#conduct anova test for these models 4 and 5
anova(best4pred,best5pred)

#5 was better from last anova, so run anova for 5 and 6
anova(best5pred,best6pred)

#SO 5 IS MOST SIGNIFICANT, CHECK BETA VALUES
best5pred$coefficients

#Just for backup compare 5 and 6
anova(best5pred,best6pred)

#We confirm that 5 was more significant 


