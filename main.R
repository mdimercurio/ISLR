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
