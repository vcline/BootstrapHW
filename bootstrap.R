#Author:  Vishi Cline
#Date   :  10/20/2016
#Description: R code for generating bootstrap code to illustrate the Central Limit Theorem 
#             using a normal distribution with two different sample sizes and an exponential 
#             distribution with two different sample sizes.
############################################################################################

#declare sample size
size<-50
#declare population mean
popMean<-22
#declare population SD
popSD<-5

#Simple random sample using rnorm
#50 obsersvations , pop mean=22, pop sd=5
x<-rnorm(size,popMean,popSD)

#mean of the sample
#sample mean compared to the assumed true population mean of 22
xbar<-mean(x)
xbar

# number of replications
nsims<-1000

# initialize vector to hold the replicated bootstrap means
#generate 1000 bootstrp means from sample x, using 50 observations, and replacing the values
norm<-function(nsims,x, size){
  +	bootnorm<-numeric(nsims)
  +	for(i in 1:nsims){
    + 		temp<-sample(x,size, replace=TRUE)
    +		bootnorm[i]<-mean(temp)
    +		}
  +	bootnorm
}

#5 point summary.  mean is close to xbar
summary(bootnorm)

#sd of 1000 bootstrap samples of normal distribution of size 50
sd(bootnorm)

#sd of original sample/sqrt(size)
sd(x)/sqrt(size)

#Display histogram -normally distributed.
#Plot a line to show bootstrap mean, sample mean and population mean
hist(bootnorm)
abline(v=xbar,col="red",lwd=2)
abline(v=mean(bootnorm),col="blue",lwd=2,lty=2)
abline(v=22,col="cyan",lwd=3,lty=3)

#Repeat above steps for size=100
size<-100
bootnorm<-norm(nsims,x,size)
summary(bootnorm)
sd(bootnorm)
sd(x)/sqrt(size)
hist(bootnorm)
abline(v=xbar,col="red",lwd=2)
abline(v=mean(bootnorm),col="blue",lwd=2,lty=2)
abline(v=popMean,col="cyan",lwd=3,lty=3)

#Code for random exponential distribution
size<-50
x<-rexp(n=size)
xbar<-mean(x)
xbar
nsims<-1000
expon<-function(nsims,x, size){
  bootexp<-numeric(nsims)
  for(i in 1:nsims){
    temp<-sample(x,size, replace=TRUE)
    bootexp[i]<-mean(temp)
  }
  bootexp
}
bootexp<-expon(nsims,x,size)
summary(bootexp)
sd(bootexp)
sd(x)/sqrt(size)
hist(bootexp)
abline(v=xbar,col="red",lwd=2)
abline(v=mean(bootexp),col="blue",lwd=2,lty=2)