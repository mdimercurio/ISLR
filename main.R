library("ISLR")
library("MASS")
attach(Boston)

# 3.6.2 Simple linear regression =============================
# p109

fix(Boston)
?Boston

lm.fit = lm(medv ~ lstat)

lm.fit
summary(lm.fit)

names(lm.fit)

coefficients(lm.fit)
#residuals(lm.fit)
plot(lstat, residuals(lm.fit))

# Confidence intervals
confint(lm.fit)

# Predict with confidence intervals
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="confidence")

# Predict with prediction intervals
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="prediction")

# Plot the fit
plot(lstat,medv, pch="+")
abline(lm.fit, lwd=3, col="red")

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(1,1))
plot(predict (lm.fit), residuals (lm.fit))
plot(predict (lm.fit), rstudent (lm.fit))

# Plot leverage
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# 3.6.3 Multiple regression =====================================

lm.fit = lm(medv ~ lstat+age, data=Boston)
summary(lm.fit)

lm.fit = lm(medv~., data=Boston)
summary(lm.fit)

# R squarred
summary(lm.fit)$r.sq
# RSE
summary(lm.fit)$sigma
# Variance Inflation Factor (higher than 5-10 indicates multi-collinearity)
library(car)
vif(lm.fit)

# Correlation matrix
cor(Boston)

# Exclude on predictor
lm.fit1 = lm(medv~.-age, data=Boston)
summary(lm.fit1)

# 3.6.4 Interaction terms =======================================
lm.fitInt = lm(medv~lstat*age, data=Boston)
summary(lm.fitInt)

# 3.6.5 Non linear transformations of the predictors ===========
lm.fit2 = lm(medv~lstat + I(lstat^2), data=Boston)
summary(lm.fit2)

# Anova: Compare two fits
lm.fit =lm(medv~lstat)
anova(lm.fit ,lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

lm.fit5 = lm(medv~poly(lstat, 5), data=Boston)
summary(lm.fit5)

## Qualitative predictors

# 3.6.6 Qualitative predictors 

# 3.6.7 Writing functions =================================

LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

LoadLibraries()

# 3.8 =====================================================

pairs(Auto)

cor(Auto[,-9])

lm.fit <- lm(mpg ~ .-name, data=Auto)
summary(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)

set.seed(1)
x1=runif(100)
x2=0.5*x1+rnorm(100)/10
y=2+2* x1 +0.3* x2+rnorm (100)

cor(x1, x2)

plot(x1, x2)

lm.fit = lm(y ~ x1 + x2)
summary(lm.fit)

lm.fit1 = lm(y ~ x1)
summary(lm.fit1)

lm.fit2 = lm(y ~ x2)
summary(lm.fit2)

x1=c(x1, 0.1)
x2=c(x2, 0.8)
y=c(y, 6)

lm.fitB = lm(y ~ x1 + x2)
summary(lm.fitB)
plot(lm.fitB)

lm.fit1B = lm(y ~ x1)
summary(lm.fit1B)
plot(lm.fit1B)

lm.fit2B = lm(y ~ x2)
summary(lm.fit2B)
plot(lm.fit2B)

# 4.6.1 The stock market data =============================
# p154
names(Smarket)

summary(Smarket)

dim(Smarket)

fix(Smarket)

pairs(Smarket)

cor(Smarket[,-9])

attach(Smarket)
plot(Volume)

# 4.6.2 Logistic regression ===============================

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family= binomial, data=Smarket)

summary(glm.fit)

coef(glm.fit)

glm.probs = predict(glm.fit, type="response")
head(glm.probs)
# Use contrasts to know which response is predicted
# Up in our case
contrasts(Direction)

## Make a prediction
# Create a vector of 1250 elements
glm.pred = rep("Down", 1250)
# Predict Up if the probability is higher than 0.5
glm.pred[glm.probs > 0.5] = "Up"

# Create a confusion matrix
table(glm.pred, Direction)

mean(glm.pred == Direction)

## Cross validation

train = (Year<2005)
Smarket.2005 = Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family= binomial, data=Smarket, subset=train)
glm.probs = predict(glm.fit, Smarket.2005, type="response")

glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

# Prediction using only Lag1 and Lag2
glm.fit = glm(Direction ~ Lag1 + Lag2, family= binomial, data=Smarket, subset=train)
glm.probs = predict(glm.fit, Smarket.2005, type="response")

glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)


predict(glm.fit, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8) ), type="response")

## 4.6.3 Linear discriminant analysis (LDA) ===============

library(MASS)

lda.fit = lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
lda.fit

plot(lda.fit)

lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[, 1] >=.5)
sum(lda.pred$posterior[, 1] <.5)

lda.pred$posterior[1:20, 1]
lda.class[1:20]
# Modify the classification threshold
sum(lda.pred$posterior[, 1] > .9)

## 4.6.4 Quadratic discriminant analysis (QDA) ============

qda.fit = qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)

qda.fit

qda.class = predict(qda.fit, Smarket.2005)$class

table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

## 4.6.5 K-nearest neighbors (KNN) ===========================

library(class)

train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

# When using KNN, R breaks the ties randomly so we need to set the seed 
set.seed(1)
# K = 1
knn.pred = knn(train.X, test.X, train.Direction, k=1)

table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

# K = 3
knn.pred = knn(train.X, test.X, train.Direction, k=3)

table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

## 4.6.6 [KNN] An application to caravan insurance data =========

dim(Caravan)

attach(Caravan)

summary(Purchase)

# Standardize data, using mean and stev by default
standardized.X = scale(Caravan[,-86])

var(Caravan[,1])
var(Caravan[,2])

var(standardized.X[,1])
var(standardized.X[,2])

test = 1:1000
train.X = standardized.X[-test, ]
test.X = standardized.X[test, ]
train.Y = Purchase[-test]
test.Y = Purchase[test]

set.seed(1)
# K=1
knn.pred = knn(train.X, test.X, train.Y, k=1)
# Comparing the knn prediction to always predicting No 
mean(test.Y!=knn.pred)
mean(test.Y!="No")
# Error rate among customers that where predicted to buy insurance
table(knn.pred, test.Y)
9/(68+9)

# K=3
knn.pred = knn(train.X, test.X, train.Y, k=3)
# Comparing the knn prediction to always predicting No 
mean(test.Y!=knn.pred)
mean(test.Y!="No")
# Error rate among customers that where predicted to buy insurance
table(knn.pred, test.Y)
5/(21+5)

# K=5
knn.pred = knn(train.X, test.X, train.Y, k=5)
# Comparing the knn prediction to always predicting No 
mean(test.Y!=knn.pred)
mean(test.Y!="No")
# Error rate among customers that where predicted to buy insurance
table(knn.pred, test.Y)
4/(11+4)

# Using logistic regression

glm.fit = glm(Purchase ~., data=Caravan, family=binomial, subset=-test)

glm.probs = predict(glm.fit, Caravan[test, ], type="response")

# Threshold = 0.5
glm.pred = rep("No", 1000)
glm.pred[glm.probs > .5] = "Yes"
table(glm.pred, test.Y)

# Threshold = 0.25
glm.pred = rep("No", 1000)
glm.pred[glm.probs > .25] = "Yes"
table(glm.pred, test.Y)
11/(22+11)

## 4.7 Exercises  ======================---------
# a
library(ISLR)
attach(Weekly)
names(Weekly)
summary(Direction)
# b
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family= binomial, data=Weekly)
# c
summary(glm.fit)

glm.probs <- predict(glm.fit, type="response")

glm.pred = rep("Down", nrow(Weekly))

glm.pred[glm.probs > 0.5] = "Up"

table(glm.pred, Direction)

mean(glm.pred == Direction)
# d
train <- (Year<'2009')

testSet <- Weekly[!train,]
  
glm.fit = glm(Direction ~ Lag2, family= binomial, data=Weekly, subset=train)

summary(glm.fit)

glm.probs <- predict(glm.fit, testSet, type="response")

glm.pred = rep("Down", nrow(testSet))
glm.pred[glm.probs > 0.5] = "Up"

table(glm.pred, testSet$Direction)
mean(glm.pred == test$Direction)

# e LDA

lda.fit = lda(Direction ~ Lag2, data=Weekly, subset=train)
lda.fit

plot(lda.fit)

lda.pred = predict(lda.fit, testSet)

table(lda.pred$class, testSet$Direction)
mean(lda.pred$class == testSet$Direction)

# f qda

qda.fit = qda(Direction ~ Lag2, data=Weekly, subset=train)

qda.fit

qda.pred = predict(qda.fit, testSet)

table(qda.pred$class, testSet$Direction)
mean(qda.pred$class == testSet$Direction)

# g KNN
train <- Weekly$Year < 2009

train.X = data.frame((Weekly$Lag2)[train])
test.X = data.frame((Weekly$Lag2)[!train])
train.Direction = Weekly$Direction[train]

# When using KNN, R breaks the ties randomly so we need to set the seed 
set.seed(1)
# K = 1
knn.pred = knn(train.X, test.X, train.Direction, k=1)

table(knn.pred, testSet$Direction)
mean(knn.pred == testSet$Direction)

# 11.a

names(Auto)

attach(Auto)
mpg01 <- ifelse(mpg > median(mpg), 1, 0)

myAuto <- data.frame(Auto, mpg01)

# 11.b


train <- myAuto[1:300,]
test <- myAuto[301:nrow(myAuto),]

lda.fit = lda(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration+year, data=train)

lda.pred = predict(lda.fit, test)

table(lda.pred$class, test$mpg01)
mean(lda.pred$class == test$mpg01)

qda.fit = qda(mpg01 ~ cylinders+displacement+horsepower+weight+acceleration+year, data=train)

qda.pred = predict(qda.fit, test)

table(qda.pred$class, test$mpg01)
mean(qda.pred$class == test$mpg01)

## 5.3.1 The validation set approach ============
attach(Auto)
set.seed(1)

train = sample(392,196)

lm.fit = lm(mpg ~ horsepower, data=Auto, subset=train)

mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data=Auto, subset=train)

mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data=Auto, subset=train)

mean((mpg-predict(lm.fit3, Auto))[-train]^2)

## 5.3.2 Leave-One-Out Cross-Validation =========

library(boot)

glm.fit = glm(mpg ~ horsepower, data=Auto)

cv.err = cv.glm(Auto, glm.fit)
cv.err$delta

cv.error = rep(0, 5)

for(i in 1:5){
  glm.fit = glm(mpg ~ poly(horsepower, i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}

cv.error

## 5.3.3 k-Fold Cross-Validation ================

set.seed(17)

cv.error.10 = rep(0, 10)

for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data=Auto)
  cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}

cv.error.10

## 5.3.4 The bootstrap ==========================

alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X, Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)

set.seed(1)

alpha.fn(Portfolio, sample(100, 100, replace=T))

boot(Portfolio, alpha.fn, R=1000)

# Estimating the accuracy of a linear regression model

boot.fn=function(data, index){
  return(coef(lm(mpg~horsepower, data=data, subset=index)))
}

boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))

boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower, data=Auto))$coef

boot.fn=function(data, index){
  return(coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index)))
}

set.seed(1)
boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))