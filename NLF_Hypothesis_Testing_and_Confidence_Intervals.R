#ACTIVITY 1

dir()
NFLdata = read.table(file="NFL12_14.txt", header=TRUE)
ls()
NFLdata

Y = NFLdata[,"Wins"]
X = NFLdata[,"PassPct"]
n = length(Y)

print(n)

summary(X)
summary(Y)

#Check summary for ,=min and max values and set xlim and ylim

plot(X,Y,xlim=c(53,71),ylim=c(0.5,13.9),xlab="Percentage of completions per pass attempts (%)",ylab="Regular season wins")

#ACTIVITY 2

#1
xbar = mean(X)
ybar = mean(Y)

print(xbar)
print(ybar)
#2
XtimesY = X * Y
#3
sumXtimesY = sum(XtimesY)
print(sumXtimesY)
#4
SSxy = sumXtimesY - n * xbar * ybar
print(SSxy)
#5
Xsquared = X^2
#6
sumXsquared = sum(Xsquared)
print(sumXsquared)
#7
SSxx = sumXsquared - n * xbar^2
print(SSxx)
#8
beta1hat = SSxy / SSxx
beta0hat = ybar - beta1hat * xbar

print(beta0hat) 
print(beta1hat)

#ACTIVITY 3 

#1
Yhat = beta0hat + beta1hat * X
print(Yhat)
#2
epsilon_hat = Y - Yhat
print(epsilon_hat)
#3
SSE = sum(epsilon_hat^2)
print(SSE)
#4
MSE = SSE/(n-2)
print(MSE)
#5
s = sqrt( MSE )
print(s)
#6
CV = 100 * s / ybar
print(CV)

#ACTIVITY 4

#1
tstat = beta1hat / ( s/sqrt(SSxx) )
print(tstat)
#2
DF = n - 2
pvalue = 2 * pt(tstat,df=DF,lower.tail=F)
print(pvalue)

#Since p-value is 10^-7 so very smaller than alpha 0.05 and 0.02 (10^-2), we reject the null hypothesis.

#Activity 5

SEbeta1 = s / sqrt(SSxx)

t1 = qt(0.025,df=DF,lower.tail=F)  
CI1 = beta1hat + c(-1,1) * t1 * SEbeta1
print(CI1)

