library(MASS);
library(leaps);
library(glmnet);

#setwd('C:\\Users\\Julia\\Documents\\GitHub\\stats315a')

wine.data <- read.csv("wine.train.csv", header=TRUE)
N <- nrow(wine.data);
split <- floor(N*0.8);
wine.train <- wine.data[1:split,]
wine.tune <- wine.data[(split+1):N,]
  


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
#     pca
#     partial least squares
# basis expansion
#   ordinary: polynomial, log, norm, binning, etc
#   splines
#   locally weighted regression ++ kernels/knn
#   varying coefficient models
# other measures of relative importance


# OLS
print('OLS')
print(cv.error(function(x) {return(lm(quality ~ ., x))}, 10, wine.train, function(model, x) predict(model, x[,1:12])))
# 
# Subset selection
best.subset.regression <- function(formula, train.data, subset.size, method) {
  leaps <- summary(regsubsets(formula, data=train.data,nvmax=subset.size,method=method,really.big=TRUE));
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

# print("INTERACTIONS WITH BEST SUBSET (this is really slow- don't try to run it until fixed)")
# subset.scores <- lapply(1:12, function(y) {cv.error(function(x) {return(best.subset.regression(quality ~ .^2, x, y, "exhaustive"))}, 10, wine.train, function(model, x) predict(model, x[,1:12]))})
# print(subset.scores)
