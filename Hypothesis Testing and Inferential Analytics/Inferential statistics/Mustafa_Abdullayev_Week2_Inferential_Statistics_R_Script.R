# Necessary Packages

setwd("/home/musto/Desktop/ALY 6015/R")
require(MASS)

cabbages

c39 <- cabbages[which(cabbages$Cult == "c39"),]
c52 <- cabbages[which(cabbages$Cult == "c52"),]
        

  ########## PART A ########
# T-test : Do 2 different cultivars of Cabbages have same Head weights ?
#   H0 : they are same      : M1 = M2
#   HA : they are not  same : M1 != M2
# significance = 0.05 (alpha) 
# my test is two sided

t.test(c39$HeadWt,c52$HeadWt,alternative = "two.sided")

#Welch Two Sample t-test

#data:  c39$HeadWt and c52$HeadWt
#t = 2.9209, df = 57.777, p-value = 0.004972
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  0.1971677 1.0561656
#sample estimates:
#  mean of x mean of y 
#2.906667  2.280000 

### my p-value is extremely small (0.004972).
###I can reject the null hypothesis stating that head weight of two different cultivars of cabbages are the same with the 95% confidence

########## PART B #########

# F-Test : are the variances of Ascorbic acid content same for two different cultivars of the cabbage?
#     H0 : they are same      : v1 = v2
#     H1 : they are not same  : v1 != v2
# significance level = 0.05 (95% Confidence)
# my test is two sided

var.test(c39$VitC,c52$VitC,alternative = "two.sided")

#F test to compare two variances

#data:  c39$VitC and c52$VitC
#F = 0.70977, num df = 29, denom df = 29, p-value = 0.3613
#alternative hypothesis: true ratio of variances is not equal to 1
#95 percent confidence interval:
#  0.3378266 1.4912287
#sample estimates:
#  ratio of variances 
#0.7097723

### p-value of test is 0.3613 which is bigger than my alpha (0.05).
### So, with 95% confidence, I am unable to reject my null hypothesis.
### I accept default choice,H0,stating that variance in the content of Ascorbic acid for 2 different cultivars of cabbage are statistically same. 