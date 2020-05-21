#################### Problem 1 ####################
require(e1071)
require(EnvStats)
setwd("/home/musto/Desktop/ALY 6050")

## Setting seed for reproducibility
set.seed(123)


## creating 1000 random numbers between 0 and 1
r <- runif(1000, min = 0, max = 1)

## Getting negative logarithmic values for r
x_1 <- -log(r)

## I will use  bins for these values - x
bins_1 <- seq(0,8,by = 1)

## seperating x vector into 8 bins
x_1.cut <- cut(x_1,bins_1,right = FALSE)
x_1.cut

## making frequency table by combining x.cut into table
x_1.freq <- table(x_1.cut)
barplot(x_1.freq, main = "Frequency Distribution of X values")

## Calculating Relative frequency by dividing 1000 (total numbers)
x_1.r_freq <- x_1.freq/1000
barplot(x_1.r_freq, main = "Relative Freq. Dist. of X values")


## Creating Probability plot
y_1 <- rexp(1000)
qqPlot(y_1,x_1,plot.type = "Q-Q",add.line = TRUE,main = "Probability Plot for X",xlab = "Quantiles of Exp. values",ylab = "Quantiles of X values")



##Goodness of fit test : 
## Here :   H0 : There is no significane difference between the observed and the expected values
##          HA : There is a significant difference between the observed and the expected values
##Significance level = 0.05

x_1.exp <- c(pexp(1),pexp(2)-pexp(1),pexp(3)-pexp(2),pexp(4)-pexp(3),pexp(5)-pexp(4),pexp(6)-pexp(5),pexp(7)-pexp(6),1- pexp(7))
x_1.exp
sum(x_1.exp)

res_1 <- chisq.test(x_1.freq,p = x_1.exp)
res_1


#Chi-squared test for given probabilities

#data:  x.freq
#X-squared = 2.3143, df = 7, p-value = 0.9404



#################### Problem 2 ####################

r1 <- runif(10000, min = 0, max = 1)
r2 <- runif(10000, min = 0, max = 1)
r3 <- runif(10000, min = 0, max = 1)

x_2 <- -log(r1*r2*r3)
bins_2 <- seq(0,14,by = 1)
bins_2

x_2.cut <- cut(x_2,bins_2,right = FALSE)
x_2.cut

## making frequency table by combining x.cut into table
x_2.freq <- table(x_2.cut)
barplot(x_2.freq, main = "Frequency Distribution of X values for P2 ")

## Calculating Relative frequency by dividing 1000 (total numbers)
x_2.r_freq <- x_2.freq/10000
barplot(x_2.r_freq, main = "Relative Freq. Dist. of X values for P2")


## Creating Probability plot
y_2 <- rgamma(10000,shape = 3)

qqPlot(x_2,y_2,add.line = TRUE,main = "Probability Plot for X values in P2",xlab = "Quantiles of Gamma Values",ylab = "Quantiles of X values")

##Goodness of fit test : 
## Here :   H0 : There is no significane difference between the observed and the expected values
##          HA : There is a significant difference between the observed and the expected values
##Significance level = 0.05

exp_2 <- c(pgamma(shape = 3,1)-pgamma(shape = 3,0),pgamma(shape = 3,2)-pgamma(shape = 3,1),pgamma(shape = 3,3)-pgamma(shape = 3,2),pgamma(shape = 3,4)-pgamma(shape = 3,3),pgamma(shape = 3,5)-pgamma(shape = 3,4),pgamma(shape = 3,6)-pgamma(shape = 3,5),pgamma(shape = 3,7)-pgamma(shape = 3,6),pgamma(shape = 3,8)-pgamma(shape = 3,7),pgamma(shape = 3,9)-pgamma(shape = 3,8),pgamma(shape = 3,10)-pgamma(shape = 3,9),pgamma(shape = 3,11)-pgamma(shape = 3,10),pgamma(shape = 3,12)-pgamma(shape = 3,11),pgamma(shape = 3,13)-pgamma(shape = 3,12),1-pgamma(shape = 3,13))
sum(exp_2)


res_2 <- chisq.test(x=x_2.freq,p=exp_2)
res_2

#Chi-squared test for given probabilities

#data:  x_2.freq
#X-squared = 14.807, df = 13, p-value = 0.3195


#################### Problem 3 ####################

r_3_1 <- runif(1000)
r_3_2 <- runif(1000)

x_3_1 <- -log(r_3_1)
x_3_2 <- -log(r_3_2)

Y <- vector()
Y

k <- ((x_3_1 - 1)^2)/2

for (val in seq(1000)) {
  
  if(x_3_2[val] >= k[val]){
    r <- runif(1)
    
    if (r > 0.5){
      Y <- append(Y,x_3_1[val])
    }
    else{
      Y <-append(Y,-x_3_1[val])
    }
  }
  
}

N <- length(Y)
Y

bins_3 <- seq(-6,6,by = 1)
bins_3

Y.cut <- cut(Y,bins_3,right = FALSE)
Y.cut

## making frequency table by combining x.cut into table
Y.freq <- table(Y.cut)
barplot(Y.freq, main = "Frequency Distribution of Y values")


## Calculating Relative frequency by dividing N (total numbers)
Y.r_freq <- Y.freq/N
barplot(Y.r_freq, main = "Relative Freq. Dist. of Y values")

## Creating Probability plot

qqPlot(Y,rnorm(N),add.line = TRUE,main = "Probability plot For Y values",xlab = "Quantiles of Y Values",ylab="Quantiles of Normal Values")

##Goodness of fit test : 
## Here :   H0 : There is no significane difference between the observed and the expected values
##          HA : There is a significant difference between the observed and the expected values
##Significance level = 0.05

norm_3 <- c(pnorm(-5),pnorm(-4)-pnorm(-5),pnorm(-3)-pnorm(-4),pnorm(-2)-pnorm(-3),pnorm(-1)-pnorm(-2),pnorm(0)-pnorm(-1),pnorm(1)-pnorm(0),pnorm(2)-pnorm(1),pnorm(3)-pnorm(2),pnorm(4)-pnorm(3),pnorm(5)-pnorm(4),1 - pnorm(5))
sum(norm_3)

res_3 <- chisq.test(x=Y.freq,p=norm_3)
res_3

#Chi-squared test for given probabilities

#data:  Y.freq
#X-squared = 6.2201, df = 11, p-value = 0.8583


#################### Problem 4 ####################
set.seed(123)

to_test <- c(10,20,30,40,50,60,70,80,90,100,200,300,400,500,600,700,800,900,1000)
W <- vector()

## Algorithm Function
for (num in to_test){
  r_4_1 <- runif(num)
  r_4_2 <- runif(num)

  x_4_1 <- -log(r_4_1)
  x_4_2 <- -log(r_4_2)

  Y_2 <- vector()
  Y_2

  k_2 <- ((x_4_1 - 1)^2)/2

  for (val in seq(num)) {
  
   if(x_4_2[val] >= k_2[val]){
      r <- runif(1)
    
     if (r > 0.5){
       Y_2 <- append(Y_2,x_4_1[val])
     }
      else{
      Y_2 <-append(Y_2,-x_4_1[val])
    }
    }
  }
  
  N <- length(Y_2)
  W <- append(W,num/N)
}


## Expected value and standard deviation
mean(W)
sd(W)

## Lets Normalize the W values
norm_W <- (W-mean(W))/sd(W)

## Seperating to bins
bins_4 <- seq(-5,5,by = 1)
bins_4

W.cut <- cut(norm_W,bins_4,right = FALSE)
W.cut

## making frequency table by combining x.cut into table
W.freq <- table(W.cut)
barplot(W.freq, main = "Frequency Distribution of W values")


## Calculating Relative frequency by dividing N (total numbers)
W.r_freq <- W.freq/N
barplot(W.r_freq, main = "Relative Freq. Dist. of W values")

## Probability plot for W values

qqPlot(W,rnorm(length(W)),add.line = TRUE,main = "Probability Plot for W",ylab = "Quantiles of Normal values")

##Goodness of fit test : 
## Here :   H0 : There is no significane difference between the observed and the expected values
##          HA : There is a significant difference between the observed and the expected values
##Significance level = 0.5

W_norm <- c(pnorm(-4),pnorm(-3)-pnorm(-4),pnorm(-2)-pnorm(-3),pnorm(-1)-pnorm(-2),pnorm(0)-pnorm(-1),pnorm(1)-pnorm(0),pnorm(2)-pnorm(1),pnorm(3)-pnorm(2),pnorm(4)-pnorm(3),1-pnorm(4))
sum(W_norm)

res_4 <- chisq.test(x = W.freq, p = W_norm)
res_4

#Chi-squared test for given probabilities

#data:  W.freq
#X-squared = 6.3506, df = 9, p-value = 0.7044
#
## Finding the limit
plot(to_test,W,type = "l",main = "W vs M values",xlab = "M values",ylab = "W values")


#################### Answer to Questions Posted ####################

# P1 - If r is a standard uniform random variable, then -LN(r) has the Exponential Probability Distribution
# P2 - The sum of three independent and identically distributed Exponential random Variables has the Gamma Probability Distribution
# P3 - The output of the algorithm of problem 3 has a Standard Normal probability Distribution
# P4 - In step 2 of the algorith of Problem 3, random variables X1 and X2, each of whose probability distribution is Exponential are used to generate a random value Y that has the Standard Normal Probability Distribution
# P5 - The Random value W that was discussed in Problem 4, has the Normal Probability Distribution. The Expected value of W is about 1.30.

