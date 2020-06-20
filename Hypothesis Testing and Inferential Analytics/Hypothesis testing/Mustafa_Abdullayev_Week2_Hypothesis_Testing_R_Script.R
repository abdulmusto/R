# Necessary Packages

setwd("/home/musto/Desktop/ALY 6015/R")
require(MASS)

######## PART B ########## 

# H0 is like default choice and HA is something that goes against it
# Here H0 : copper per million <= 1 part
#      HA : copper per million  > 1 part 

# I will use 0.05 as a significance of my test.
# In my test, my mu value is 1 and my test is right-sided.i.e greater

t.test(chem,mu = 1,alternative = "greater")

#data:  chem
#t = 3.0337, df = 23, p-value = 0.002952
#alternative hypothesis: true mean is greater than 1
#95 percent confidence interval:
#  2.427162      Inf
#sample estimates:
#  mean of x 
#4.280417

### As a result, my p-value is 0.002952. So since is it smaller than
### 0.05 (significance level), it means it is higly unlikely that 
### test result is due to luck. So I reject the null hypothesis 
### staying that capper per million is smaller than 1. Answer is Yes :
### Flour production company produces whole meail flour which has
### more than 1 part per million copper in it.


######### PART C ##########

# Here : 
#    H0 : means are equal :       m1 = m2
#    HA : means are not equal :   m1 != m2
# alpha = 0.05 (95% confidence)
# my test is two sided

male <- cats[which(cats$Sex=="M"),]$Bwt
female <- cats[which(cats$Sex=="F"),]$Bwt

t.test(male,female,alternative = "two.sided")

#data:  male and female
#t = 8.7095, df = 136.84, p-value = 8.831e-15
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.4177242 0.6631268
#sample estimates:
#  mean of x mean of y 
#2.900000  2.359574 

### my p-value is extremely small, so I can reject the null hypothesis :
### I reject the null hypothesis staying that male and female
### cats have the same body weight with 95% confidence


####### PART D ##########

# Here : 
#    H0 : material B is not worse than A : difference <= 0
#    HA : material A is better than B    : difference > 0 
# alpha = 0.05 (95% confidence)
# my test is one sided

A <- shoes$A
B <- shoes$B

t.test(A,B,paired = TRUE,alternative = "greater")

# data:  A and B
# t = -3.3489, df = 9, p-value = 0.9957
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   -0.6344264        Inf
# sample estimates:
#   mean of the differences 
# -0.41 

### my p-value is big (almost 1). So I am unable to reject null hypothesis
### I have to accept default choice : Material B is better than material A
### with 95% confidence



########## PART E #######

# Here : 
#      H0 : proportions are equal (no effect)      : p1 = p2
#      H1 : proportions are not equal (yes effect) : p1 != p2  
#significance level = 0.05
# this is two sided test (since question asks is there effect)

# I only need first 2 columns to find placebo and active group members count 
mydata <- as.data.frame(bacteria[1:2])

# Number of people in placebo group
size_p_all <- nrow(mydata[which(mydata$ap == "p"),])

# Number of people in active group
size_a_all <- nrow(mydata[which(mydata$ap == "a"),])

# Number of people in placebo group who has bacteria (success)
size_p_bacteria <- nrow(mydata[which(mydata$ap == "p" & mydata$y == "y"),])
# Number of people in active group who has bacteria(success)
size_a_bacteria <- nrow(mydata[which(mydata$ap == "a" & mydata$y == "y"),])

prop.test(x = c(size_a_bacteria,size_p_bacteria),n = c(size_a_all,size_p_all))

#2-sample test for equality of proportions with
#continuity correction

#data:  c(size_a_bacteria, size_p_bacteria) out of c(size_a_all, size_p_all)
#X-squared = 4.6109, df = 1, p-value = 0.03177
#alternative hypothesis: two.sided
#95 percent confidence interval:
#  -0.23516294 -0.01483706
#sample estimates:
#  prop 1 prop 2 
#0.750  0.875 

### p-value of this test is 0.03177 which is smaller than 0.05 (significance level)
### So, I can reject the null hypothesis. With 95% confidence,
### drug treatment have a significant effect of the precence of the bacteria

########## PART F #######

# Here : 
#    H0 : variances are equal :     v1 = v2
#    HA : variances are not equal : v1 != v2 
# alpha = 0.05 (95% confidence)
# my test is two sided

var.test(male,female,alternative = "two.sided")

#F test to compare two variances

#data:  male and female
#F = 2.9112, num df = 96, denom df = 46, p-value = 0.0001157
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  1.723106 4.703057
#sample estimates:
#  ratio of variances 
#2.911196 


### p-value is extremely small. So, I reject null hypothesis : ,
### I am 95% confident that male and female cats' bodyweights do not have
### same variances






