# load data with: wine.train <- read.csv("wine.train.csv", header=TRUE)

cv.error <- function(model.fn, formula, num.folds, train.data) {
  N = nrow(train.data);
  fold.size <- floor(N/num.folds);
  error <- 0.0
  
  fold.begin <- 1; fold.end <- floor(N/num.folds);
  for (i in 1:num.folds) {
    # train model on all-but-i^th fold
    model.i <- ifelse(identical(model.fn,glmnet),
                      model.fn(formula,family="gaussian",weights = rep(1,nrow(train.data[-(fold.begin:fold.end),1:12])),x=train.data[-(fold.begin:fold.end),1:12], y = train.data[-(fold.begin:fold.end),]$quality),
                      model.fn(formula, train.data[-(fold.begin:fold.end),]));
    
    # add in i^th fold training error
    predict.i <- predict(model.i, train.data[fold.begin:fold.end,1:12]);
    if (class(predict.i) == "list") {
      # used when model is LDA
      # TODO: maybe incorporate LDA class posterior probabilities into prediction instead of using MAP
      # assignments? Seems fairer in comparison to lm that way.
      predict.i <- as.numeric(predict.i$class);
    }
    error <- error + sum(abs(predict.i-train.data[fold.begin:fold.end,]$quality));
    # update fold.begin and fold.end
    fold.begin <- fold.end + 1;
    fold.end <- fold.begin + fold.size - 1;
  }
  
  return(error);
}

# models to try:
# ordinariy lm (quality ~ .; make sure to treat color as a factor)
# factor response for quality (how to do this?)
# LDA/QDA
# kNN
# regularization? maybe LASSO for variable selection? or, forward step-wise with AIC?
# Trees

# uncomment the following to test ordinary linear model, LDA respectively
# cv.error(lm, quality ~ ., 10, wine.train)
# cv.error(lda, as.factor(quality) ~ ., 10, wine.train)