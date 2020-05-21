getwd()
setwd("/home/musto/Desktop/ALY 6050/Week 3 - Price Prediction")

require(forecast)
require(MLmetrics)
require(Hmisc)
require(EnvStats)

set.seed(123)

#############################################################################################33
###############################################################################################
###################### Problem 1 ###########################################################

PEP <- read.csv("PEP_Train.csv")
train_PEP <- ts(PEP$Close,frequency=21)
test_PEP <- read.csv("PEP_Test.csv")$Close

AAPL <- read.csv("AAPL_Train.csv")
train_AAPL <- ts(AAPL$Close,frequency = 21)
test_AAPL <- read.csv("AAPL_Test.csv")$Close

DIS <- read.csv("DIS_Train.csv")
train_DIS <- ts(DIS$Close,frequency = 21)
test_DIS <- read.csv("DIS_Test.csv")$Close

HD <- read.csv("HD_Train.csv")
train_HD <- ts(HD$Close, frequency = 21)
test_HD <- read.csv("HD_Test.csv")$Close

MSFT <- read.csv("MSFT_Train.csv")
train_MSFT <- ts(MSFT$Close,frequency = 21)
test_MSFT <- read.csv("MSFT_Test.csv")$Close

alpha_values = c(0.15,0.35,0.55,0.75)
beta_values = c(0.15,0.25,0.45,0.85)


######## Pepsi ###########
for (val in alpha_values){
  
  holt <- HoltWinters(train_PEP,alpha = val,seasonal = "multiplicative")
  prediction <-forecast(holt,h=5)
  pred <- prediction$mean
  actual <- test_PEP
  
  print(MSE(pred,actual))
}


######## Apple ###########
for (val in alpha_values){
  
  holt <- HoltWinters(train_AAPL,alpha = val,seasonal = "multiplicative")
  prediction <-forecast(holt,h=5)
  pred <- prediction$mean
  actual <- test_AAPL
  
  print(MSE(pred,actual))
}


######## Microsoft ###########
for (val in alpha_values){
  
  holt <- HoltWinters(train_MSFT,alpha = val,seasonal = "multiplicative")
  prediction <-forecast(holt,h=5)
  pred <- prediction$mean
  actual <- test_MSFT

  
  print(MSE(actual,pred))
}

######## Disney ###########
for (val in alpha_values){
  
  holt <- HoltWinters(train_DIS,alpha = val,seasonal = "multiplicative")
  prediction <-forecast(holt,h=5)
  pred <- prediction$mean
  actual <- test_DIS
  
  
  print(MSE(actual,pred))
}


######## Home Depot ###########
for (val in alpha_values){
  
  holt <- HoltWinters(train_HD,alpha = val,seasonal = "multiplicative")
  prediction <-forecast(holt,h=5)
  pred <- prediction$mean
  actual <- test_HD
  
  
  print(MSE(actual,pred))
}


################################################################################################################
################################################################################################################
#################################### Problem 2 #################################################################

######## Pepsi ###########
for (val in beta_values){
  
  holt <- HoltWinters(train_PEP,alpha = 0.75,beta = val,seasonal = "multiplicative")
  prediction <-forecast(holt,h=5)
  pred <- prediction$mean
  actual <- test_PEP
  
  print(MSE(actual,pred))
}


######## Apple ###########
for (val in beta_values){
  
  holt <- HoltWinters(train_AAPL,alpha = 0.75,beta=val,seasonal = "multiplicative")
  prediction <-forecast(holt,h=5)
  pred <- prediction$mean
  actual <- test_AAPL
  
  print(MSE(actual,pred))
}

######## Microsoft ###########
for (val in beta_values){
  
  holt <- HoltWinters(train_MSFT,alpha = 0.75,beta = val,seasonal = "multiplicative")
  prediction <-forecast(holt,h=5)
  pred <- prediction$mean
  actual <- test_MSFT
  
  
  print(MSE(actual,pred))
}

######## Disney ###########
for (val in beta_values){
  
  holt <- HoltWinters(train_DIS,alpha = 0.75,beta = val,seasonal = "multiplicative")
  prediction <-forecast(holt,h=5)
  pred <- prediction$mean
  actual <- test_DIS
  
  
  print(MSE(actual,pred))
}


######## Home Depot ###########
for (val in beta_values){
  
  holt <- HoltWinters(train_HD,alpha =0.75,beta = val,seasonal = "multiplicative")
  prediction <-forecast(holt,h=5)
  pred <- prediction$mean
  actual <- test_HD
  
  
  print(MSE(actual,pred))
}



#####################################################################################################################
#####################################################################################################################
################### Problem 3 ################################################3#####################################


############### Pepsi ############

PEP_lag0 <- Lag(PEP$Close,shift = 0)
PEP_lag1 <- Lag(PEP$Close,shift = 1)
PEP_lag2 <- Lag(PEP$Close,shift = 2)
PEP_lag3 <- Lag(PEP$Close,shift = 3)

PEP_lag <- data.frame(PEP_lag0,PEP_lag1,PEP_lag2,PEP_lag3)
names(PEP_lag) <- c("lag0","lag1","lag2","lag3")


PEP_test_lag1 <- Lag(test_PEP,shift = 1)
PEP_test_lag2 <- Lag(test_PEP,shift = 2)
PEP_test_lag3 <- Lag(test_PEP,shift = 3)

PEP_test_lag <- data.frame(PEP_test_lag1,PEP_test_lag2,PEP_test_lag3)
names(PEP_test_lag) <- c("lag1","lag2","lag3")

model <- lm(lag0 ~ lag1 + lag2 + lag3,data = PEP_lag)

## Coefficient of Determination
summary(model)

## Coefficient of Correlation
cor_PEP <-cor(PEP_lag, use = "pairwise.complete.obs")
print(round(cor_PEP,2))

## Actual Prediction and MSE
pred_PEP_3 <- predict(model,newdata = PEP_test_lag)
pred_PEP_3[4:5]

mse_3 <- MSE(pred_PEP_3[4:5],test_PEP[4:5])
print(mse_3)

## Histogram and Probability fplot for residuals
hist(model$residuals, xlab = "bins",ylab = "residuals", main = "Distribution of residuals")
qqPlot(x=rnorm(length(model$residuals)),y=model$residuals,add.line = TRUE,xlab = "Standard Normal Values",ylab = "Residuals", main = "Probability Plot")


## Chi Square Test
PEP_test <- chisq.test(x = model$residuals, y = rnorm(length(model$residuals)))
PEP_test

## Residuals vs Time
plot(x = as.Date(PEP$Date[4:543]),y = model$residuals,main = "Residuals vs Time for Pepsi",xlab = "Time",ylab = "Residuals for Pepsi")
abline(0,0,col="red")

## Residuals vs Stock Price
plot(x=PEP$Close[4:543],y = model$residuals,xlab = "Stock Prices",ylab = "residuals",main = "Residuals vs Stock Prices")
abline(0,0,col = "red")

############## Apple #############3

AAPL_lag0 <- Lag(AAPL$Close,shift = 0)
AAPL_lag1 <- Lag(AAPL$Close,shift = 1)
AAPL_lag2 <- Lag(AAPL$Close,shift = 2)
AAPL_lag3 <- Lag(AAPL$Close,shift = 3)

AAPL_lag <- data.frame(AAPL_lag0,AAPL_lag1,AAPL_lag2,AAPL_lag3)
names(AAPL_lag) <- c("lag0","lag1","lag2","lag3")


AAPL_test_lag1 <- Lag(test_AAPL,shift = 1)
AAPL_test_lag2 <- Lag(test_AAPL,shift = 2)
AAPL_test_lag3 <- Lag(test_AAPL,shift = 3)

AAPL_test_lag <- data.frame(AAPL_test_lag1,AAPL_test_lag2,AAPL_test_lag3)
names(AAPL_test_lag) <- c("lag1","lag2","lag3")

AAPL_model <- lm(lag0 ~ lag1 + lag2 + lag3,data = AAPL_lag)

## Coefficient of Determination
summary(AAPL_model)

## Coefficient of Coreelation
cor_AAPL <-cor(AAPL_lag, use = "pairwise.complete.obs")
print(round(cor_AAPL,2))

## Actual Prediction and MSE
pred_AAPL_3 <- predict(AAPL_model,newdata = AAPL_test_lag)
pred_AAPL_3[4:5]

mse_3 <- MSE(pred_AAPL_3[4:5],test_AAPL[4:5])
mse_3

## Histogram and Probability fplot for residuals
hist(AAPL_model$residuals,xlab = "bins",ylab = "residuals", main = "Distribution of residuals")
qqPlot(x=rnorm(length(AAPL_model$residuals)),y=AAPL_model$residuals,add.line = TRUE,xlab = "Standard Normal Values",ylab = "Residuals", main = "Probability Plot")


## Chi Square Test
AAPL_test <- chisq.test(x = AAPL_model$residuals, y = rnorm(length(AAPL_model$residuals)))
AAPL_test

## Residuals vs Time
plot(x = as.Date(AAPL$Date[4:543]),y = AAPL_model$residuals,main = "Residuals vs Time for Apple",xlab = "Time",ylab = "Residuals")
abline(0,0,col="red")

## Residuals vs Stock Price
plot(x=AAPL$Close[4:543],y = AAPL_model$residuals,xlab = "Stock Prices",ylab = "residuals",main = "Residuals vs Stock Prices")
abline(0,0,col = "red")



############## Microsoft #############3

MSFT_lag0 <- Lag(MSFT$Close,shift = 0)
MSFT_lag1 <- Lag(MSFT$Close,shift = 1)
MSFT_lag2 <- Lag(MSFT$Close,shift = 2)
MSFT_lag3 <- Lag(MSFT$Close,shift = 3)

MSFT_lag <- data.frame(MSFT_lag0,MSFT_lag1,MSFT_lag2,MSFT_lag3)
names(MSFT_lag) <- c("lag0","lag1","lag2","lag3")


MSFT_test_lag1 <- Lag(test_MSFT,shift = 1)
MSFT_test_lag2 <- Lag(test_MSFT,shift = 2)
MSFT_test_lag3 <- Lag(test_MSFT,shift = 3)

MSFT_test_lag <- data.frame(MSFT_test_lag1,MSFT_test_lag2,MSFT_test_lag3)
names(MSFT_test_lag) <- c("lag1","lag2","lag3")

MSFT_model <- lm(lag0 ~ lag1 + lag2 + lag3,data = MSFT_lag)

## Coefficient of Determination
summary(MSFT_model)

## Coefficient of Coreelation
cor_MSFT <-cor(MSFT_lag, use = "pairwise.complete.obs")
print(round(cor_MSFT,2))

## Actual Prediction and MSE
pred_MSFT_3 <- predict(MSFT_model,newdata = MSFT_test_lag)
pred_MSFT_3[4:5]

mse_3 <- MSE(pred_MSFT_3[4:5],test_MSFT[4:5])
mse_3

## Histogram and Probability fplot for residuals
hist(MSFT_model$residuals, xlab = "bins",ylab = "residuals", main = "Distribution of residuals")
qqPlot(x=rnorm(length(MSFT_model$residuals)),y=MSFT_model$residuals,add.line = TRUE,xlab = "Standard Normal Values",ylab = "Residuals", main = "Probability Plot")


## Chi Square Test
MSFT_test <- chisq.test(x = MSFT_model$residuals, y = rnorm(length(MSFT_model$residuals)))
MSFT_test

## Residuals vs Time
plot(x = as.Date(MSFT$Date[4:543]),y = MSFT_model$residuals,main = "Residuals vs Time for Microsoft",xlab = "Time",ylab = "Residuals")
abline(0,0,col="red")

## Residuals vs Stock Price
plot(x=MSFT$Close[4:543],y = MSFT_model$residuals,xlab = "Stock Prices",ylab = "residuals",main = "Residuals vs Stock Prices")
abline(0,0,col = "red")



############## Disney #############3

DIS_lag0 <- Lag(DIS$Close,shift = 0)
DIS_lag1 <- Lag(DIS$Close,shift = 1)
DIS_lag2 <- Lag(DIS$Close,shift = 2)
DIS_lag3 <- Lag(DIS$Close,shift = 3)

DIS_lag <- data.frame(DIS_lag0,DIS_lag1,DIS_lag2,DIS_lag3)
names(DIS_lag) <- c("lag0","lag1","lag2","lag3")


DIS_test_lag1 <- Lag(test_DIS,shift = 1)
DIS_test_lag2 <- Lag(test_DIS,shift = 2)
DIS_test_lag3 <- Lag(test_DIS,shift = 3)

DIS_test_lag <- data.frame(DIS_test_lag1,DIS_test_lag2,DIS_test_lag3)
names(DIS_test_lag) <- c("lag1","lag2","lag3")

DIS_model <- lm(lag0 ~ lag1 + lag2 + lag3,data = DIS_lag)

## Coefficient of Determination
summary(DIS_model)

## Coefficient of Coreelation
cor_DIS <-cor(DIS_lag, use = "pairwise.complete.obs")
print(round(cor_DIS,2))

## Actual Prediction and MSE
pred_DIS_3 <- predict(DIS_model,newdata = DIS_test_lag)
pred_DIS_3[4:5]

mse_3 <- MSE(pred_DIS_3[4:5],test_DIS[4:5])
mse_3

## Histogram and Probability fplot for residuals
hist(DIS_model$residuals, xlab = "bins",ylab = "residuals", main = "Distribution of residuals")
qqPlot(x=rnorm(length(DIS_model$residuals)),y=DIS_model$residuals,add.line = TRUE,xlab = "Standard Normal Values",ylab = "Residuals", main = "Probability Plot")


## Chi Square Test
DIS_test <- chisq.test(x = DIS_model$residuals, y = rnorm(length(DIS_model$residuals)))
DIS_test

## Residuals vs Time
plot(x = as.Date(DIS$Date[4:543]),y = DIS_model$residuals,main = "Residuals vs Time for Disney",xlab = "Time",ylab = "Residuals")
abline(0,0,col="red")

## Residuals vs Stock Price
plot(x=DIS$Close[4:543],y = DIS_model$residuals,xlab = "Stock Prices",ylab = "residuals",main = "Residuals vs Stock Prices")
abline(0,0,col = "red")



############## Home Depot #############3

HD_lag0 <- Lag(HD$Close,shift = 0)
HD_lag1 <- Lag(HD$Close,shift = 1)
HD_lag2 <- Lag(HD$Close,shift = 2)
HD_lag3 <- Lag(HD$Close,shift = 3)

HD_lag <- data.frame(HD_lag0,HD_lag1,HD_lag2,HD_lag3)
names(HD_lag) <- c("lag0","lag1","lag2","lag3")


HD_test_lag1 <- Lag(test_HD,shift = 1)
HD_test_lag2 <- Lag(test_HD,shift = 2)
HD_test_lag3 <- Lag(test_HD,shift = 3)

HD_test_lag <- data.frame(HD_test_lag1,HD_test_lag2,HD_test_lag3)
names(HD_test_lag) <- c("lag1","lag2","lag3")

HD_model <- lm(lag0 ~ lag1 + lag2 + lag3,data = HD_lag)

## Coefficient of Determination
summary(HD_model)

## Coefficient of Coreelation
cor_HD <-cor(HD_lag, use = "pairwise.complete.obs")
print(round(cor_HD,2))

## Actual Prediction and MSE
pred_HD_3 <- predict(HD_model,newdata = HD_test_lag)
pred_HD_3[4:5]

mse_3 <- MSE(pred_HD_3[4:5],test_HD[4:5])
mse_3

## Histogram and Probability fplot for residuals
hist(HD_model$residuals, xlab = "bins",ylab = "residuals", main = "Distribution of residuals")
qqPlot(x=rnorm(length(HD_model$residuals)),y=HD_model$residuals,add.line = TRUE,xlab = "Standard Normal Values",ylab = "Residuals", main = "Probability Plot")


## Chi Square Test
HD_test <- chisq.test(x = HD_model$residuals, y = rnorm(length(HD_model$residuals)))
HD_test

## Residuals vs Time
plot(x = as.Date(HD$Date[4:543]),y = HD_model$residuals,main = "Residuals vs Time for Home Depot",xlab = "Time",ylab = "Residuals")
abline(0,0,col="red")

## Residuals vs Stock Price
plot(x=HD$Close[4:543],y = HD_model$residuals,xlab = "Stock Prices",ylab = "residuals",main = "Residuals vs Stock Prices")
abline(0,0,col = "red")



########################################################################################################################
########################################################################################################################
################### Problem 4 ################################################3########################################

########### Pepsi ###########

names <- c("PEP","AAPL","MSFT","DIS","HD")

together <- cbind(PEP$Close,AAPL$Close,MSFT$Close,DIS$Close,HD$Close)
together <- data.frame(together)
names(together) <- names

together_test = cbind(test_AAPL,test_DIS,test_HD,test_MSFT)
together_test <- data.frame(together_test)
names(together_test) <- names[2:5]


tog_model <- lm(data=together, PEP ~ AAPL + MSFT + DIS + HD)

## Coefficient of Determination
summary(tog_model)

# Coefficient of Correlation
cor_together <- cor(together,use= "pairwise.complete.obs")
print(round(cor_together,2))

## Histogram and probability plot of resisuals
hist(tog_model$residuals, xlab = "bins",ylab = "residuals", main = "Distribution of residuals")
qqPlot(x=rnorm(length(tog_model$residuals)), y = tog_model$residuals, xlab = "Residuals",ylab = "Standard Normal Values", main = "Probability Plot",add.line = TRUE)

## Chi Square Test
tog_test <- chisq.test(x = tog_model$residuals, y = rnorm(length(tog_model$residuals)))
tog_test


## Residuals vs Time
plot(x = as.Date(PEP$Date),y = tog_model$residuals,main = "Residuals vs Time ",xlab = "Time",ylab = "Residuals")
abline(0,0,col="red")

## Residuals vs Stock Price
plot(x=together$PEP,y = tog_model$residuals,xlab = "Stock Prices",ylab = "residuals",main = "Residuals vs Stock Prices")
abline(0,0,col = "red")

#### Actual Prediction
pred_linear <- predict(tog_model, new = together_test)
test_PEP

mse_linear <- MSE(pred_linear,test_PEP)
print(mse_linear)



