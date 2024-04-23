#set directory
setwd()
dir()

#load data
del = read.table(file="EXPRESS.txt", header=TRUE)
ls()
dimnames(del)
del

#get vectors of variables
y = del[,"Cost"] #other way of writing: x1 = del[,1] meaning column 1 of data
x1 = del[,"Weight"]
x2 = del[,"Distance"]
n = length(Y)
n

#make vectors for remaining variables-square terms
x1sq = x1^2
x2sq = x2^2

#Fit 2 models
ReducedModel = lm(y~x1+x2+x1:x2)

CompleteModel = lm(y~x1+x2+x1sq+x2sq+x1:x2)

summary_R=summary(ReducedModel)
summary_C=summary(CompleteModel)

#degree of freedom

df_SSE_R = length(y)-3-1 # 3 because 3 predictors-x1,x2,x1:x2
df_SSE_C = length(y)-5-1

# Finding SSE (SSE is sigma^2 * df)

SSE_R = summary_R$sigma^2*df_SSE_R
SSE_C = summary_C$sigma^2*df_SSE_C

# Finding F stat

F_stat = (SSE_R-SSE_C)/(5-3)/(SSE_C/df_SSE_C)

# Finding p value

pf(F_stat, df1=2, df2=df_SSE_C, lower.tail = F) #Lower tail false because test is one sided

# Doing the comparison of models directly using code

anova(ReducedModel, CompleteModel)
