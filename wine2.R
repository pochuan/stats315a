library(MASS);
library(leaps);

wine.data <- read.csv("wine.train.csv", header=TRUE)
N <- nrow(wine.data);
split <- floor(N*0.8);
wine.train <- wine.data[1:split,]
wine.tune <- wine.data[(split+1):N,]
  
cv.error <- function(model.fn, num.folds, train.data) {
  N = nrow(train.data);
  fold.size <- floor(N/num.folds);
  error <- 0.0;
  
  fold.begin <- 1; fold.end <- floor(N/num.folds);
  for (i in 1:num.folds) {
    # train model on all-but-i^th fold
    model.i <-  model.fn(train.data[-(fold.begin:fold.end),]);
    
    # add in i^th fold training error
    #print(model.i);
    predict.i <- predict(model.i, train.data[fold.begin:fold.end,1:12]);

    error <- error + sum((predict.i-train.data[fold.begin:fold.end,]$quality)^2)/fold.size; #rss
    #error <- error + mean(round(predict.i)!=train.data[fold.begin:fold.end,]$quality); #accuracy
    # update fold.begin and fold.end
    fold.begin <- fold.end + 1;
    fold.end <- fold.begin + fold.size - 1;
  }
  
  return(error/num.folds);
}

cv.error.glmnet <- function(model.fn, num.folds, train.data, lambda) {
  N = nrow(train.data);
  fold.size <- floor(N/num.folds);
  error <- 0.0;
  
  fold.begin <- 1; fold.end <- floor(N/num.folds);
  for (i in 1:num.folds) {
    model.i <-  model.fn(train.data[-(fold.begin:fold.end),]);
    
    # add in i^th fold training error
    predict.i <- predict(model.i, model.matrix(~ .,train.data[fold.begin:fold.end,1:12]),s=c(lambda));
    
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
#       ridge
#       lasso
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
print(cv.error(function(x) {return(lm(quality ~ ., x))}, 10, wine.train))
# 
# Subset selection
best.subset.regression <- function(train.data, subset.size, method) {
  leaps <- summary(regsubsets(quality ~ ., data=train.data,nvmax=subset.size,method=method));
  best.subset <- leaps$which[subset.size,2:13];
  return(lm(quality ~ ., data = train.data[,c(best.subset,TRUE)]));
}

print('BEST SUBSET')
subset.scores <- lapply(1:12, function(y) {cv.error(function(x) {return(best.subset.regression(x, y, "exhaustive"))}, 10, wine.train)})
print(subset.scores)
# 
# subset.scores <- lapply(1:12, function(y) {cv.error(function(x) {return(best.subset.regression(x, y, "forward"))}, 10, wine.train)})
# print(subset.scores)
# 
# subset.scores <- lapply(1:12, function(y) {cv.error(function(x) {return(best.subset.regression(x, y, "backward"))}, 10, wine.train)})
# print(subset.scores)

# Shrinkage
# Lasso
print('LASSOOOOO')
lambda <- c(1,0.1,0.01,0.001,0.0001,0.00001);
glmnet.lasso <- function(x) {return(glmnet(model.matrix(~ ., x[,1:12]),x[,13], lambda=lambda))}
print(lapply(lambda, function(l) {cv.error.glmnet(glmnet.lasso, 10, wine.train, l)}))
# Ridge
print('RIDGE REGRESSION')
lambda <- c(1,0.1,0.01,0.001,0.0001,0.00001);
glmnet.lasso <- function(x) {return(glmnet(model.matrix(~ ., x[,1:12]),x[,13], lambda=lambda, alpha=0))}
print(lapply(lambda, function(l) {cv.error.glmnet(glmnet.lasso, 10, wine.train, l)}))



