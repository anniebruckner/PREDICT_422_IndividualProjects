# Individual Project 1
# Andrea Bruckner
# Predict 422, Sec 56

# Load the diabetes data
install.packages("lars")
library(lars)
data(diabetes)
data.all <- data.frame(cbind(diabetes$x, y = diabetes$y))

# Get overview of data
summary(data.all) # no missing data
str(data.all) # all numeric
head(data.all)
class(data.all) # data.frame
names(data.all)
nrow(data.all) # 442 rows
ncol(data.all) # 11 variables

# Partition the patients into two groups: training (75%) and test (25%)
n <- dim(data.all)[1] # sample size = 442
set.seed(1306) # set random number generator seed to enable

# repeatability of results
test <- sample(n, round(n/4)) # randomly sample 25% test
data.train <- data.all[-test,]
data.test <- data.all[test,]

# (Use these starting in Models 3-5)
x <- model.matrix(y ~ ., data = data.all)[,-1] # define predictor matrix
# excl intercept col of 1s
x.train <- x[-test,] # define training predictor matrix
x.test <- x[test,] # define test predictor matrix
y <- data.all$y # define response variable
y.train <- y[-test] # define training response variable
y.test <- y[test] # define test response variable
n.train <- dim(data.train)[1] # training sample size = 332
n.test <- dim(data.test)[1] # test sample size = 110

# 1.) Least squares regression model using all ten predictors (R function lm).

lm.fit <- lm(y~., data = data.train)
summary(lm.fit)
par(mfrow = c(2, 2))
plot(lm.fit)

# Coefficient Estimates
coef(lm.fit)

# Calculate MSE == 3111.265
mean((data.test$y - predict(lm.fit, data.test))^2) # 3111.265

lm.pred <- predict(lm.fit, data.test)
mean((data.test$y - lm.pred)^2)

# SE of MSE == 361.0908
sd((lm.pred - data.test$y)^2)/sqrt(length((lm.pred - data.test$y)^2)) # 361.0908

# 2.) Apply best subset selection using BIC to select the number of predictors (R function regsubsets in package leaps).
# applying best subset using either 8 or 10 variables results in the same best model

library(leaps)
fit.bestsub <- regsubsets(y ~ ., data = data.train, nvmax=10) # fit the model
fit.bestsub.sum <- summary(fit.bestsub)
names(fit.bestsub.sum)
fit.bestsub.sum$bic # -201.1269 is lowest BIC, M6
summary(fit.bestsub)
plot(fit.bestsub)

which.min(fit.bestsub.sum$bic) # 6

# resets plots window
par(mfrow = c(1, 1))

plot(fit.bestsub.sum$bic,xlab="Number of Variables",ylab="BIC",type="l")
points (6, fit.bestsub.sum$bic [6], col = "red",cex = 2, pch = 20)

# Coefficient Estimates
coef(fit.bestsub,6) # plot bottoms out at 6/has smallest BIC at 6 variables

# Calculate MSE == 3095.483
predict.regsubsets = function (object, newdata, id ,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi )
  mat[,xvars]%*% coefi
}

bestsub.pred <- predict(fit.bestsub,data.test,id=6)
mean((data.test$y-bestsub.pred)^2) # 3095.483
# I think I can also use y.test instead of data.test$y

# SE of MSE == 369.7526
sd((bestsub.pred - data.test$y)^2)/sqrt(length((bestsub.pred - data.test$y)^2)) # 369.7526
sd((bestsub.pred - data.test$y)^2)/sqrt(n.test) # 369.7526


# 3.) Apply best subset selection using 10-fold cross-validation to select the number of predictors (R function regsubsets in package leaps). [Use a random number seed of 1306 before entering the command: folds <- sample(1:k, nrow(data.train), replace = TRUE).]

k=10
set.seed(1306)
folds <- sample(1:k, nrow(data.train), replace = TRUE)
cv.errors<-matrix (NA,k,10, dimnames=list(NULL, paste(1:10)))

for(j in 1:k){
  cv10.fit<-regsubsets(y ~ ., data = data.train[folds !=j,],
                       nvmax =10)
  for(i in 1:10) {
    pred=predict(cv10.fit, data.train[folds==j,], id=i)
    cv.errors[j,i]=mean((data.train$y[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors, 2, mean)
mean.cv.errors # M6 has smallest error

par(mfrow =c(1,1))
plot(mean.cv.errors, type='b') # 6 variable model is best

cv10.best <- regsubsets(y ~ ., data = data.train, nvmax=10)

# Coefficient Estimates
coef(cv10.best, 6)
#coef(cv10.best, 10)

# Calculate MSE == 3095.483 --- same as # 2 MSE
cv10.pred <- predict(cv10.best, data.test, id=6)
mean((cv10.pred-y.test)^2)
mean((cv10.pred-data.test$y)^2)

# SE of MSE == 369.7526 --- same as # 2 MSE
sd((cv10.pred-data.test$y)^2)/sqrt(n.test) # 369.7526

# 4.) Ridge regression modeling using 10-fold cross-validation to select the largest value of λ such that the cross-validation error is within 1 standard error of the minimum (R functions glmnet and cv.glmnet in package glmnet). [Use a random number seed of 1306 immediately before entering the command: cv.out <- cv.glmnet(x.train, y.train, alpha = 0).]
# pages 266-269

grid = 10^seq(10,-2, length = 100)
ridge.fit0 = glmnet(x.train, y.train, alpha = 0, lambda = grid)
plot(ridge.fit0,xvar="lambda",label=T)
# is grid even necessary? -- not when you use cv.glmnet

library (glmnet)
k=10
set.seed(1306)
cv.out <- cv.glmnet(x.train, y.train, alpha = 0)
names(cv.out)
plot(cv.out)
#plot(cv.out, sign.lambda = -1)
# example of how to interpret this plot: http://gerardnico.com/wiki/r/ridge_lasso
#plot(cv.out$glmnet.fit) # not sure what to make of this...
names(cv.out)

largelam <- cv.out$lambda.1se
largelam # lambda = 41.67209

ridge.fit <- glmnet(x.train,y.train,alpha=0,lambda=41.67209)
#plot(ridge.fit, xvar="lambda", label=T)
# the plot does not look right...

# Coefficient Estimates
coef(ridge.fit)

ridge.pred <- predict(ridge.fit,newx = x.test)

# Calculate MSE == 3070.87
mean((ridge.pred-y.test)^2) # 3070.87


# SE of MSE == 350.5467
sd((ridge.pred-y.test)^2)/sqrt(n.test) # 350.5467


# 5.) Lasso model using 10-fold cross-validation to select the largest value of λ such that the cross-validation error is within 1 standard error of the minimum (R functions glmnet and cv.glmnet in package glmnet). [Use a random number seed of 1306 immediately before entering the command: cv.out <- cv.glmnet(x.train, y.train, alpha = 1).]

library (glmnet)
k=10
set.seed(1306)
cv.out <- cv.glmnet(x.train, y.train, alpha = 1)
plot(cv.out)
names(cv.out)

largelam.lasso <- cv.out$lambda.1se
largelam.lasso # 4.791278

lasso.fit <- glmnet(x.train,y.train,alpha=1,lambda=4.791278)
#plot(lasso.fit) # doesn't look right...

# Coefficient Estimates
coef(lasso.fit)

# Calculate MSE == 2920.041
lasso.pred <- predict(lasso.fit,newx=x.test)
mean((lasso.pred-y.test)^2) # 2920.041

# SE of MSE == 346.2248
sd((lasso.pred-y.test)^2)/sqrt(n.test) # 346.2248


### DISCARDED CODE ###

# 1
#confint(lm.fit)
#lm.fit2 <- lm(y.train ~ x.train)
#summary(lm.fit2)
#confint(lm.fit2)
#par(mfrow = c(2, 2))
#plot(lm.fit2)
# These are the same...so we can run the models either way?

# 2
#fit.bestsub.all <- regsubsets(y ~ ., data = data.train, nvmax=10) # fit the model using all the variables
#fit.bestsub.all.sum <- summary(fit.bestsub.all)
#fit.bestsub.all.sum$bic # -201.1269 is lowest BIC, M6
#summary(fit.bestsub.all)
#plot(fit.bestsub.all.sum$bic,xlab="Number of Variables",ylab="BIC",type="l")
#coef(fit.bestsub.all,6)
#test.mat <- model.matrix(y ~ ., data = data.test)

#val.errors = rep(NA ,10)
#for(i in 1:10){
#coefi=coef(fit.bestsub,id=i)
#pred=test.mat[,names(coefi)]%*% coefi
#val.errors[i]=mean((data.test$y-pred)^2)
#}

#val.errors
#which.min(val.errors )
#coef(regfit.best ,10)

# The code above kept resulting in this:
# Error in s$which[id, , drop = FALSE] : subscript out of bounds

# 4
#folds <- sample(1:k, nrow(data.train), replace = TRUE)
#cv.errors<-matrix (NA,k,10, dimnames=list(NULL, paste(1:10)))

#grid = 10^seq(10,-2, length = 100)
#ridge.fit = glmnet(x.train, y.train, alpha = 0, lambda = grid)
#plot(ridge.fit,xvar="lambda",label=T)
# is grid even necessary?
#ridge.fit2 <- glmnet(x.train, y.train, alpha = 0, lambda = largelam)
#coef(ridge.fit2)
#ridge.pred2 <- predict(ridge.fit, s=largelam, newx=x.test)
#mean((ridge.pred2-y.test)^2) # 3070.636