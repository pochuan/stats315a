# Classification Tree with rpart
library(rpart)

wine.train <- read.csv("wine.train.csv", header=TRUE)
# grow tree 
fit <- rpart(quality ~.,
             method="class", data=wine.train, control=rpart.control(minsplit=30, cp=0.005))

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Wine")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

cv.error <- function(model.fn, formula, num.folds, train.data) {
  train.data<-train.data[sample(nrow(train.data)),]
  N = nrow(train.data);
  fold.size <- floor(N/num.folds);
  error <- 0.0
  
  fold.begin <- 1; fold.end <- floor(N/num.folds);
  for (i in 1:num.folds) {
    # train model on all-but-i^th fold
    model.i <- model.fn(formula, method="class", control=rpart.control(minsplit=30, cp=0.001), data=train.data[-(fold.begin:fold.end),]);
    plot(model.i, uniform=TRUE, 
         main="Classification Tree for Wine")
    # add in i^th fold training error
    predict.i <- predict(model.i, type = "class", train.data[fold.begin:fold.end,1:12]);
    if (class(predict.i) == "factor") {
      predict.i <- as.numeric(predict.i);
      if(i == 1) {print(predict.i);}
    }
    error <- error + sum((predict.i-train.data[fold.begin:fold.end,]$quality)^2);
    # update fold.begin and fold.end
    fold.begin <- fold.end + 1;
    fold.end <- fold.begin + fold.size - 1;
  }
  
  return(sqrt(error));
}