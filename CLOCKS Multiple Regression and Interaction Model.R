# Set the working directory to the desired path
setwd()
dir()


CLOCKS = read.table(file="GFCLOCKS.txt", header=TRUE)
ls()
dimnames(CLOCKS)
CLOCKS

Y = CLOCKS[,"PRICE"]
X1 = CLOCKS[,"AGE"]
X2 = CLOCKS[,"NUMBIDS"]
n = length(Y)

print(n)

plot(X1,Y,xlim=c(100,200),ylim=c(500,2200),xlab="Age of clock",ylab = "Price of clock")
plot(X2,Y,xlim=c(5,20),ylim=c(500,2200),xlab="No of bidders",ylab = "Price of clock")

summary(X1)
summary(X2)
summary(Y)

#Building the multiple regression

model= lm(Y~X1 + X2)

summary(model)


#pt and qt class 3/4
# qt gives t value of alpha/2 and df. Test is 2 sided so both tails will have same value. 
qt(0.05/2, df=29) #don't specify lower tail gives negative

qt(0.05/2,df = 29, lower.tail = F) #specify lower tail gives positive

#Finding CI for B1 (age of clocks) 
#lower tail false for t to be positive
CI95.age = c(12.74,12.74) #12.74 is B1cap value from previous output
CI95.age[1] = CI95.age[1] - qt(0.025,df=29,lower.tail=F)*0.9047 #0.9047 is SE of B1 from previous r output
CI95.age[2] = CI95.age[2] + qt(0.025,df=29,lower.tail=F)*8.73 #8.73 is SE of B2 from previous R output 
CI95.age

#interaction model

IntModel = lm(Y~X1+X2+X1:X2)
summary(IntModel) #Even though age is not significant (p=0.66) we keep it because interaction term x1:x2 is significant (p=1.35e-06)

