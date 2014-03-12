library(MASS);
library(leaps);
library(glmnet);
library(pls);

#setwd('C:\\Users\\Julia\\Documents\\GitHub\\stats315a')

wine.data <- read.csv("wine.train.csv", header=TRUE)
N <- nrow(wine.data);
# split <- floor(N*0.8);
# wine.train <- wine.data[1:split,]
# wine.tune <- wine.data[(split+1):N,]
wine.train <- wine.data[sample(nrow(wine.data)),];
  
#wine.train[2:12] <- scale(wine.train[2:12])

cv.error <- function(model.fn, num.folds, train.data, predict.fn) {
  N = nrow(train.data);
  fold.size <- floor(N/num.folds);
  error <- 0.0;
  
  fold.begin <- 1; fold.end <- floor(N/num.folds);
  for (i in 1:num.folds) {
    model.i <-  model.fn(train.data[-(fold.begin:fold.end),]);
    
    # add in i^th fold training error
    predict.i <- predict.fn(model.i, train.data[fold.begin:fold.end,]);
    
    error <- error + sum((predict.i-train.data[fold.begin:fold.end,]$quality)^2)/fold.size; #rss
    #error <- error + mean(round(predict.i)!=train.data[fold.begin:fold.end,]$quality); #accuracy
    
    # update fold.begin and fold.end
    fold.begin <- fold.end + 1;
    fold.end <- fold.begin + fold.size - 1;
  }
  
  return(error/num.folds);
}

# OLS
print('OLS')
print(cv.error(function(x) {return(lm(quality ~ ., x))}, 10, wine.train, function(model, x) predict(model, x[,1:12])))
# print(cv.error(function(x) {return(lm(quality ~ color + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol, x))}, 10, wine.train, function(model, x) predict(model, x[,1:12])))

# 
# Subset selection
best.subset.regression <- function(formula, train.data, subset.size, method) {
  leaps <- summary(regsubsets(formula, data=train.data,nvmax=subset.size,method=method,really.big=TRUE));
  #print(subset.size);
  #print(leaps$which)
  best.subset <- leaps$which[subset.size,2:13];
  return(lm(quality ~ ., data = train.data[,c(best.subset,TRUE)]));
}

print('BEST SUBSET')
subset.scores <- lapply(1:12, function(y) {cv.error(function(x) {return(best.subset.regression(quality ~ ., x, y, "exhaustive"))}, 10, wine.train, function(model, x) predict(model, x[,1:12]))})
print(subset.scores)
# 
# subset.scores <- lapply(1:12, function(y) {cv.error(function(x) {return(best.subset.regression(x, y, "forward"))}, 10, wine.train, function(model, x) predict(model, x[,1:12]))})
# print(subset.scores)
# 
# subset.scores <- lapply(1:12, function(y) {cv.error(function(x) {return(best.subset.regression(x, y, "backward"))}, 10, wine.train, function(model, x) predict(model, x[,1:12]))})
# print(subset.scores)

# Shrinkage
# Lasso
print('LASSOOOOO')
lambda <- c(1,0.1,0.01,0.001,0.0001,0.00001);
glmnet.lasso <- function(x) {return(glmnet(model.matrix(~ ., x[,1:12]),x[,13], lambda=lambda))}
glmnet.predict <- function(model, x, l) {predict(model, model.matrix(~ .,x[,1:12]),s=c(l))}
print(lapply(lambda, function(l) {cv.error(glmnet.lasso, 10, wine.train, function(model, x) {glmnet.predict(model, x, l)})}))
# Ridge
print('RIDGE REGRESSION')
lambda <- c(1,0.1,0.01,0.001,0.0001,0.00001);
glmnet.lasso <- function(x) {return(glmnet(model.matrix(~ ., x[,1:12]),x[,13], lambda=lambda, alpha=0))}
glmnet.predict <- function(model, x, l) {predict(model, model.matrix(~ .,x[,1:12]),s=c(l))}
print(lapply(lambda, function(l) {cv.error(glmnet.lasso, 10, wine.train, function(model, x) {glmnet.predict(model, x, l)})}))


# Basis expansion
# Interactions
print('INTERACTION MODEL')
print(cv.error(function(x) {return(lm(quality ~ .^2, x))}, 10, wine.train, function(model, x) predict(model, x[,1:12])))

print('INTERACTIONS WITH LASSO')
lambda <- c(1,0.1,0.01,0.001,0.0001,0.00001);
glmnet.lasso <- function(x) {return(glmnet(model.matrix(~ .^2, x[,1:12]),x[,13], lambda=lambda))}
glmnet.lasso.predict <- function(model, x, l) {predict(model, model.matrix(~ .^2,x[,1:12]),s=c(l))};
print(lapply(lambda, function(l) {cv.error(glmnet.lasso, 10, wine.train, function(model, x) {glmnet.lasso.predict(model, x, l)})}))

print('INTERACTIONS WITH RIDGE')
lambda <- c(1,0.1,0.01,0.001,0.0001,0.00001);
glmnet.lasso <- function(x) {return(glmnet(model.matrix(~ .^2, x[,1:12]),x[,13], lambda=lambda, alpha=0))}
glmnet.lasso.predict <- function(model, x, l) {predict(model, model.matrix(~ .^2,x[,1:12]),s=c(l))};
print(lapply(lambda, function(l) {cv.error(glmnet.lasso, 10, wine.train, function(model, x) {glmnet.lasso.predict(model, x, l)})}))

# print("INTERACTIONS WITH FORWARD SUBSET SELECTION")
# subset.scores <- lapply(1:78, function(y) {print(y); return(cv.error(function(x) {return(best.subset.regression(quality ~ .^2, x, y, "forward"))}, 10, wine.train, function(model, x) predict(model, x[,1:12])))})
# print(subset.scores)

log.no.fail <- function(x) {
  if (x <= 0) {
    return(log(0.00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001))
  } else {
    return(log(x));
  }
}

logb <- function(x) {
  return(sapply(x, log.no.fail));
}

print('POLYNOMIAL REGRESSION')
formula <- quality ~ color + poly(fixed.acidity, 2, raw=TRUE) + poly(volatile.acidity, 2, raw=TRUE) + poly(citric.acid, 2, raw=TRUE) + poly(residual.sugar, 2, raw=TRUE) + poly(chlorides, 2, raw=TRUE) + poly(free.sulfur.dioxide, raw=TRUE) + poly(total.sulfur.dioxide, 2, raw=TRUE) + poly(density, 2, raw=TRUE) + poly(pH, 2, raw=TRUE) + poly(sulphates, 2, raw=TRUE) + poly(alcohol, 2, raw=TRUE)
print(cv.error(function(x) {return(lm(formula, x))}, 10, wine.train, function(model, x) predict(model, x[,1:12])))

print('POLYNOMIAL WITH LASSO')
lambda <- c(1,0.1,0.01,0.001,0.0001,0.00001);
#formula <- ~ color + logb(fixed.acidity) + logb(volatile.acidity) + logb(citric.acid) + logb(residual.sugar) + logb(chlorides) + logb(free.sulfur.dioxide) + logb(total.sulfur.dioxide) + logb(density) + logb(pH) + logb(sulphates) + logb(alcohol) + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol
#formula <- ~ color + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol
#formula <- ~ color + logb(fixed.acidity) + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol
formula <- ~ color + poly(fixed.acidity, 2, raw=TRUE) + poly(volatile.acidity, 2, raw=TRUE) + poly(citric.acid, 2, raw=TRUE) + poly(residual.sugar, 2, raw=TRUE) + poly(chlorides, 2, raw=TRUE) + poly(free.sulfur.dioxide, raw=TRUE) + poly(total.sulfur.dioxide, 2, raw=TRUE) + poly(density, 2, raw=TRUE) + poly(pH, 2, raw=TRUE) + poly(sulphates, 2, raw=TRUE) + poly(alcohol, 2, raw=TRUE)

glmnet.lasso <- function(x) {return(glmnet(model.matrix(formula, x[,1:12]),x[,13], lambda=lambda))}
glmnet.lasso.predict <- function(model, x, l) {predict(model, model.matrix(formula,x[,1:12]),s=c(l))};
print(lapply(lambda, function(l) {cv.error(glmnet.lasso, 10, wine.train, function(model, x) {glmnet.lasso.predict(model, x, l)})}))

print('LOG AUGMENTED')
formula <- quality ~ color + logb(fixed.acidity) + logb(volatile.acidity) + logb(citric.acid) + logb(residual.sugar) + logb(chlorides) + logb(free.sulfur.dioxide) + logb(total.sulfur.dioxide) + logb(density) + logb(pH) + logb(sulphates) + logb(alcohol) + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol
print(cv.error(function(x) {return(lm(formula, x))}, 10, wine.train, function(model, x) predict(model, x[,1:12])))

print('LOG AUGMENTED WITH LASSO')
lambda <- c(1,0.1,0.01,0.001,0.0001,0.00001, 0.000001,0.0000001);
formula <- ~ color + logb(fixed.acidity) + logb(volatile.acidity) + logb(citric.acid) + logb(residual.sugar) + logb(chlorides) + logb(free.sulfur.dioxide) + logb(total.sulfur.dioxide) + logb(density) + logb(pH) + logb(sulphates) + logb(alcohol) + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol
glmnet.lasso <- function(x) {return(glmnet(model.matrix(formula, x[,1:12]),x[,13], lambda=lambda))}
glmnet.lasso.predict <- function(model, x, l) {predict(model, model.matrix(formula,x[,1:12]),s=c(l))};
print(lapply(lambda, function(l) {cv.error(glmnet.lasso, 10, wine.train, function(model, x) {glmnet.lasso.predict(model, x, l)})}))

print('Spline')
formula <- quality ~ color + ns(fixed.acidity, df=4, intercept=TRUE) + ns(volatile.acidity, df=4, intercept=TRUE) + ns(citric.acid, df=4, intercept=TRUE) + ns(residual.sugar, df=4, intercept=TRUE) + ns(chlorides, df=4, intercept=TRUE) + ns(free.sulfur.dioxide, df=4, intercept=TRUE) + ns(total.sulfur.dioxide, df=4, intercept=TRUE) + ns(density, df=4, intercept=TRUE) + ns(pH, df=4, intercept=TRUE) + ns(sulphates, df=4, intercept=TRUE) + ns(alcohol, df=4, intercept=TRUE) + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol
print(cv.error(function(x) {return(lm(formula, x))}, 10, wine.train, function(model, x) predict(model, x[,1:12])))

# print('PCR')
# ncomps <- 1:12
# print(lapply(ncomps, function(n) {cv.error(function(x) {pcr(quality ~ ., n, data=x)}, 10, wine.train, function(model, x) {predict(model, x[,1:12])})}))

# models to try:
# ordinary linear regression (quality ~ .; make sure to treat color as a factor)
#   regularization
#     V subset methods
#       V all subsets
#       V forward/backward/both stepwise/stagewise
#     shrinkage methods
#       V ridge
#       V lasso
#       mix
#       least angle
#       elastic net
#     *pca
#     partial least squares
# basis expansion
#   V ordinary: polynomial, log, norm, binning, etc
#   V splines
#   locally weighted regression ++ kernels/knn
#   varying coefficient models
# *combine
# * scale data to have mean 0, var 1
# other measures of relative importance
#  *    A better approach would be to refit the model with the variable omitted, and use the change in the training error as a measure of importance.
