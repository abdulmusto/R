## Loading necessary libraries
library(glmnet)
library(dplyr)
library(mice)
library(ggcorrplot)


## Directory set up
getwd()
setwd("/home/musto/Desktop/ALY 6015/R")


## Reading data from computer
data <- read.csv("housetrain.csv")


## In order to make things simple, I will only consider numeric values.
## I will drop columns with factors
to_drop <- colnames(data %>% select(which(sapply(.,is.factor))))
my_data <- data[,!names(data) %in% to_drop]


## Lets check the correlation plot 
attach(my_data)
ggcorrplot(cor(my_data[,-c(38)]))
detach(my_data)


## Lets define a function to check the percentage of missing values for each column
pMiss <- function(x){
  sum(is.na(x))/length(x)*100
                    }
apply(my_data,2,pMiss)


## Filling missing values with mean values
my_data <- mice(my_data,m=5,maxit=50,meth='pmm',seed=500)
my_data <- complete(my_data,1)
summary(my_data)



## Seperating data into training and test data
## Since I will use k-fold validation, I do not need validation subset
set.seed(1)

spec <- c(train = .7, test = .3)

g <- sample(cut(
  seq(nrow(my_data)), 
  nrow(my_data)*cumsum(c(0,spec)),
  labels = names(spec)
))


res <- split(my_data, g)

my_data_train <- res$train
my_data_test <- res$test



## Setting my lambda values to check
lambdas <- 10^seq(3, -2, by = -.1)
  

## Lets train my lasso model
## I will use matrix form since glmnet does not accept data frames
attach(my_data_train)

x <- as.matrix(my_data_train[, !(colnames(my_data_train) %in% c("SalePrice"))])
y <- as.matrix(my_data_train$SalePrice)

find_lambda <- cv.glmnet(x,y,alpha=1,lambda = lambdas)
plot(find_lambda)


## I choose my lambda to be 100
my_lambda = 100

## Defining my model
lasso_reg <- glmnet(x,y,alpha= 1,lambda = my_lambda,intercept = TRUE)
summary(lasso_reg)

lasso_reg$beta

detach(my_data_train)

## Lets Calculate RMSE - Square root of MSE
attach(my_data_test)

MSE_lasso <- mean((predict(lasso_reg,as.matrix(my_data_test[,-c(38)])) - my_data_test[,38])^2)		
RMSE_reg <- sqrt(MSE_lasso)
print(RMSE_reg)

detach(my_data_test)




## Now, Lets define an ordinalr least squares regression to see the difference
attach(my_data_train)

reg_ols <- lm(SalePrice ~ MSSubClass + LotFrontage + LotArea + OverallQual + OverallCond + YearBuilt + YearRemodAdd + MasVnrArea + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + TotalBsmtSF +
                          X1stFlrSF + X2ndFlrSF + LowQualFinSF + GrLivArea + BsmtFullBath + BsmtHalfBath + FullBath + HalfBath + BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd +Fireplaces + 
                          GarageYrBlt + GarageCars + GarageArea + WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + PoolArea + MiscVal + MoSold + YrSold)


summary(reg_ols)
anova(reg_ols)

detach(my_data_train)


## Calculatin RMSE for OLS Model
attach(my_data_test)	

MSE_OLS <- mean((predict(reg_ols,my_data_test[,-c(38)]) - my_data_test[38])^2)					
RMSE_ols <- sqrt(MSE_OLS)
print(RMSE_ols)

detach(my_data_test)


