library(readr)
library(dplyr)
library(tidyverse)
library(corrplot)
library(moments)
library(car)
library(ggplot2)
library(ggrepel)
library(gridExtra)
#Libraries from exploratory analysis

library(cvTools)
library(MASS)
#Libraries for this script

red <- read_csv("winequality-red.csv")

sum(is.na(red))
red <- na.omit(red)
#Reading in the data

dfr <- as.data.frame(red)

#Creating dataframe to be used

ALL.results <- data.frame(matrix(ncol=2,nrow=0, 
                dimnames=list(NULL, c("Model", "Classification Accuracy %"))))

#Empty dataframe for results

set.seed(100)

test <- sample(1:nrow(dfr), size = nrow(dfr)/5)
train <- (-test)


dfr.train <- dfr[train,]
dfr.test <- dfr[test,]

#dfr train/test set

k <- 10 #number of folds

folds <- cvFolds(nrow(dfr), K=k)

#folds setup for cross-validation

r.mlm.train <- lm(
  quality ~.,
  data = dfr.train
)
#Create linear regression with all predictors using the training dataset

r.mlm.predict <- predict(r.mlm.train, newdata = dfr.test)
r.mlm.predict.rounded <- round(r.mlm.predict, digits = 0)

#Round the predicted to a integer so it can be compared to the test set for
# classification

(con_mat <- table(r.mlm.predict.rounded, dfr.test$quality))

r.mlm.acc <- round(mean(r.mlm.predict.rounded==dfr.test$quality)*100,digits = 2)

#Create the confusion matrix and calculate the proportion correct

ALL.results[1,] <- c("dfr MLR w/ Train/Test", r.mlm.acc)

#Record results in results matrix

r.mlm.cv.class <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))

for(i in 1:k){
  tr.mlr <- dfr[folds$subsets[folds$which != i],]
  te.mlr <- dfr[folds$subsets[folds$which == i],]
  
  r.mlm <- lm(quality~., data = tr.mlr)
  r.mlm.pred <- predict(r.mlm, newdata = te.mlr)
  
  r.mlm.cv.class[i] <- mean(round(r.mlm.pred, digits = 0)==te.mlr$quality)
}

r.mlm.cv.class

r.mlm.cv.class <- mean(r.mlm.cv.class)
print(paste("The average outputs correctly predicted is",
            round(r.mlm.cv.class*100,digits =2),"%",sep=" "))

ALL.results[2,] <- c("dfr MLR w/ 10-fold CV", round(r.mlm.cv.class*100, 
                                                    digits=2))

#dfr MLR with 10-fold cross-validation

dfr$quality <- factor(dfr$quality, ordered = TRUE)

dfr.train <- dfr[train,]
dfr.test <- dfr[test,]

r.olr <- polr(quality~., data = dfr.train, Hess = TRUE)

r.olr.pred <- predict(r.olr, newdata = dfr.test)

r.olr.pred <- as.numeric(as.character(unlist(r.olr.pred)))
dfr.test$quality <- as.numeric(as.character(unlist(dfr.test$quality)))

r.olr.class <- mean(r.olr.pred == dfr.test$quality)

ALL.results[3,] <- c("dfr OLR w/ Training/Test", round(r.olr.class*100, 
                                                       digits=2))

#dfr OLR with training/test set

r.olr.cv.class <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))

for(i in 1:k){
  tr.olr <- dfr[folds$subsets[folds$which != i],]
  te.olr <- dfr[folds$subsets[folds$which == i],]
  
  r.olr.cv <- polr(quality~., data = tr.olr, Hess = TRUE)
  r.olr.cv.pred <- predict(r.olr.cv, newdata = te.olr)
  
  r.olr.cv.pred <- as.numeric(as.character(unlist(r.olr.cv.pred)))
  te.olr$quality <- as.numeric(as.character(unlist(te.olr$quality)))
  
  r.olr.cv.class[i] <- mean(r.olr.cv.pred==te.olr$quality)
}

r.olr.cv.class

r.olr.cv.class <- mean(r.olr.cv.class)
print(paste("The average outputs correctly predicted is",
            round(r.olr.cv.class*100,digits =2),"%",sep=" "))

ALL.results[4,] <- c("dfr OLR w/ 10-fold CV", round(r.olr.cv.class*100, 
                                                    digits=2))

#dfr OLR with 10-fold cross validation

ALL.results
### END OF REGRESSION SECTION ###

library(cvTools)
library(rpart)
library(rpart.plot)
library(rpartScore)
#Libraries for this section

set.seed(100)

test <- sample(1:nrow(dfr), size = nrow(dfr)/5)
train <- (-test)

dfr <- as.data.frame(red)

dfr.train <- dfr[train,]
dfr.test <- dfr[test,]

#Quickly resetting the dataframes

r.tree <- rpart(quality~., data = dfr.train, method = "class", cp = 0.000001)

r.tree.pred <- predict(r.tree, newdata = dfr.test, type = "class")

r.tree.class <- mean(r.tree.pred == dfr.test$quality)

ALL.results[5,] <- c("dfr CART w/ Train/Test", round(r.tree.class*100,
                                                      digits = 2))

#dfr simple Classifcation Tree

rpart.plot(r.tree, extra = 2, digits = 2)

#plot dfr simple classification tree

cp <- data.frame(r.tree$cptable)
min.cp <- which.min(cp$xerror)
cp <- cp$CP[min.cp]

#Find cp with minimum relative error for dfr classification tree

r.tree.prun <- prune(r.tree, cp = cp)

r.tree.pred <- predict(r.tree.prun, newdata = dfr.test, type = "class")

r.tree.class <- mean(r.tree.pred == dfr.test$quality)

ALL.results[6,] <- c("dfr pruned CART w/ Train/Test", round(r.tree.class*100,
                                                             digits = 2))

rpart.plot(r.tree.prun, extra = 2, digits = 2)

#Plot pruned dfr classification tree

k <- 10 #number of folds

folds <- cvFolds(nrow(dfr), K=k)

r.tree.cv.class <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))

#Setting up for cross-fold validation

for(i in 1:k){
  tr.tree <- dfr[folds$subsets[folds$which != i],]
  te.tree <- dfr[folds$subsets[folds$which == i],]
  
  r.tree.cv <- rpart(quality~., data = tr.tree, method = "class", 
                     cp = 0.000001)
  
  cp.cv <- data.frame(r.tree.cv$cptable)
  min.cp.cv <- which.min(cp.cv$xerror)
  cp.cv <- cp.cv$CP[min.cp.cv]
  
  r.tree.prun.cv <- prune(r.tree.cv, cp = cp.cv)
  
  r.tree.pred.cv <- predict(r.tree.prun.cv, newdata = te.tree, type = "class")
  
  r.tree.cv.class[i] <- mean(r.tree.pred.cv == te.tree$quality)
  
}

r.tree.cv.class

r.tree.cv.class <- mean(r.tree.cv.class)
print(paste("The average outputs correctly predicted is",
            round(r.tree.cv.class*100,digits =2),"%",sep=" "))

ALL.results[7,] <- c("dfr CART w/ 10-fold CV", round(r.tree.cv.class*100, 
                                                      digits=2))

#dfr CART with 10-fold cross validation

r.ordtree <- rpartScore(quality~., data = dfr.train,prune = "mr",cp= 0.000001)

r.ordtree.pred <- predict(r.ordtree, newdata = dfr.test)

r.ordtree.class <- mean(r.ordtree.pred == dfr.test$quality)

ALL.results[8,] <- c("dfr Ordinal Tree w/ Train/Test", 
                      round(r.ordtree.class*100,digits = 2))

rpart.plot(r.ordtree, extra = 1, digits = 2)

#dfw simple ordinal tree with training/test set

ordcp <- data.frame(r.ordtree$cptable)
min.ordcp <- which.min(ordcp$xerror)
ordcp <- ordcp$CP[min.ordcp]

#Find cp with minimum relative error for dfw ordinal tree

r.ordtree.prun <- prune(r.ordtree, cp = ordcp)

r.ordtree.pred <- predict(r.ordtree.prun, newdata = dfr.test)

r.ordtree.class <- mean(r.ordtree.pred == dfr.test$quality)

ALL.results[9,] <- c("dfr pruned Ordinal Tree w/ Train/Test", 
                      round(r.ordtree.class*100,digits = 2))

rpart.plot(r.ordtree.prun, extra = 1, digits = 2)

#dfr pruned ordinal tree with training/test set

r.ordtree.class.cv <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))

for(i in 1:k){
  tr.ord <- dfr[folds$subsets[folds$which != i],]
  te.ord <- dfr[folds$subsets[folds$which == i],]
  
  r.ordtree.cv <- rpartScore(quality~., data = tr.ord,prune = "mr",
                             cp=0.000001)
  
  ordcp.cv <- data.frame(r.ordtree.cv$cptable)
  min.ordcp.cv <- which.min(ordcp.cv$xerror)
  ordcp.cv <- ordcp.cv$CP[min.ordcp.cv]
  
  r.ordtree.prun.cv <- prune(r.ordtree.cv, cp = ordcp.cv)
  
  r.ordtree.pred.cv <- predict(r.ordtree.prun.cv, newdata = te.ord)
  
  r.ordtree.class.cv[i] <- mean(r.ordtree.pred.cv == te.ord$quality)
  
}

r.ordtree.class.cv

r.ordtree.class.cv <- mean(r.ordtree.class.cv)
print(paste("The average outputs correctly predicted is",
            round(r.ordtree.class.cv*100,digits =2),"%",sep=" "))

ALL.results[10,] <- c("dfr pruned ordinal tree w/ 10-fold CV", 
                       round(r.ordtree.class.cv*100,digits=2))

#dfr pruned ordinal tree with cross-validation

ALL.results

### END OF TREE SECTION ###

library(cvTools)
library(randomForest)
library(ordinalForest)
#Libraries for this section

set.seed(100)

test <- sample(1:nrow(dfr), size = nrow(dfr)/5)
train <- (-test)

dfr <- as.data.frame(red)

dfr$quality <- as.factor(dfr$quality)
names(dfr) <- make.names(names(dfr))

dfr.train <- dfr[train,]
dfr.test <- dfr[test,]

#Quickly resetting the dataframes

r.rf <- randomForest(quality~., data = dfr.train, mtry =11, ntree = 500,
                     importance = TRUE)

r.rf.pred <- predict(r.rf, newdata = dfr.test)

r.rf.class <- mean(r.rf.pred == dfr.test$quality)

ALL.results[11,] <- c("dfr random forest w/ training/test set", 
                    round(r.rf.class*100,digits=2))

importance(r.rf)
varImpPlot(r.rf)

#dwr random forest with training/test set

k <- 10 #number of folds

folds <- cvFolds(nrow(dfr), K=k)

r.rf.cv.class <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))

for(i in 1:k){
  tr.rf <- dfr[folds$subsets[folds$which != i],]
  te.rf <- dfr[folds$subsets[folds$which == i],]
  
  r.rf.cv <- randomForest(quality~., data = tr.rf, mtry =11, ntree = 500,
                          importance = TRUE)
  
  r.rf.pred.cv <- predict(r.rf.cv, newdata = te.rf)
  
  r.rf.cv.class[i] <- mean(r.rf.pred.cv == te.rf$quality)
  
}

r.rf.cv.class

r.rf.cv.class <- mean(r.rf.cv.class)
print(paste("The average outputs correctly predicted is",
            round(r.rf.cv.class*100,digits =2),"%",sep=" "))

ALL.results[12,] <- c("dfr Random Forest w/ 10-fold CV",
                    round(r.rf.cv.class*100,digits=2))

#dfr random forest with cross-validation

r.ordfor <- ordfor("quality", data = dfr.train, mtry = 11,
                   nsets = 100, ntreeperdiv = 10, ntreefinal = 500,
                   nbest = 1, npermtrial = 50)

r.ordfor.pred <- predict(r.ordfor, newdata = dfr.test)

r.ordfor.class <- mean(r.ordfor.pred$ypred==dfr.test$quality)

ALL.results[13,] <- c("dfr Ordinal Forest w/ training/test set",
                    round(r.ordfor.class*100,digits=2))


head(sort(r.ordfor$varimp, decreasing = TRUE), 4)

#dfr ordinal forest with training/test set

r.ordfor.cv.class <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))

for(i in 1:k){
  tr.of <- dfr[folds$subsets[folds$which != i],]
  te.of <- dfr[folds$subsets[folds$which == i],]
  
  r.ordfor.cv <- ordfor("quality", data = tr.of, mtry = 11,
                        nsets = 100, ntreeperdiv = 10, ntreefinal = 500,
                        nbest = 1, npermtrial = 50)
  
  r.ordfor.pred.cv <- predict(r.ordfor.cv, newdata = te.of)
  
  r.ordfor.cv.class[i] <- mean(r.ordfor.pred.cv$ypred == te.of$quality)
  
}

r.ordfor.cv.class

r.ordfor.cv.class <- mean(r.ordfor.cv.class)
print(paste("The average outputs correctly predicted is",
            round(r.ordfor.cv.class*100,digits =2),"%",sep=" "))

ALL.results[14,] <- c("dfw Ordinal Forest w/ 10-fold CV",
                    round(r.ordfor.cv.class*100,digits=2))

#dfr ordinal forest with 10-fold cross validation

ALL.results



