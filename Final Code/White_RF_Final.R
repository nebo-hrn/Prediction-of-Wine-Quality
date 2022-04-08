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
library(randomForest)
library(ordinalForest)
#Libraries for this script
options(warn=-1)
#Supresses warnings added in after code was complete, code would not compile
#due to the warnings from ordinalForest

white <- read_csv("winequality-white.csv")

sum(is.na(white))
white <- na.omit(white)
#Reading in the data

dfw <- as.data.frame(white)

dfw <- dfw[-2782,]
dfw2 <- subset(dfw, select = -density)

dfw$quality <- as.factor(dfw$quality)
dfw2$quality <- as.factor(dfw$quality)

names(dfw) <- make.names(names(dfw))
names(dfw2) <- make.names(names(dfw2))

#Creating the dataframes to be used

RF.results <- data.frame(matrix(ncol=2,nrow=0, 
                  dimnames=list(NULL, c("Model", "Classification Accuracy %"))))

#Empty data frame for results

set.seed(100)

test <- sample(1:nrow(dfw), size = nrow(dfw)/5)
train <- (-test)


dfw.train <- dfw[train,]
dfw.test <- dfw[test,]

#Training and Test sets for dfw

w.rf <- randomForest(quality~., data = dfw.train, mtry =11, ntree = 500,
                     importance = TRUE)

w.rf.pred <- predict(w.rf, newdata = dfw.test)

w.rf.class <- mean(w.rf.pred == dfw.test$quality)

RF.results[1,] <- c("dfw random forest w/ training/test set", 
                       round(w.rf.class*100,digits=2))

importance(w.rf)
varImpPlot(w.rf)


#dwf random forest with training/test set

dfw.train2 <- dfw2[train,]
dfw.test2 <- dfw2[test,]

#Training and Test sets for dfw2

w.rf2 <- randomForest(quality~., data = dfw.train2, mtry =10, ntree = 500,
                     importance = TRUE)

w.rf.pred2 <- predict(w.rf2, newdata = dfw.test2)

w.rf.class2 <- mean(w.rf.pred2 == dfw.test2$quality)

RF.results[2,] <- c("dfw2 random forest w/ training/test set", 
                    round(w.rf.class2*100,digits=2))

importance(w.rf2)
varImpPlot(w.rf2)

#dwf2 random forest with training/test set

k <- 10 #number of folds

folds <- cvFolds(nrow(dfw), K=k)
folds2 <- cvFolds(nrow(dfw2), K=k)

w.rf.cv.class <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))
w.rf.cv.class2 <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))

#preparing both datasets for cross-validation

for(i in 1:k){
  tr.rf <- dfw[folds$subsets[folds$which != i],]
  te.rf <- dfw[folds$subsets[folds$which == i],]
  
  w.rf.cv <- randomForest(quality~., data = tr.rf, mtry =11, ntree = 500,
                          importance = TRUE)
  
  w.rf.pred.cv <- predict(w.rf.cv, newdata = te.rf)
  
  w.rf.cv.class[i] <- mean(w.rf.pred.cv == te.rf$quality)
  
}

w.rf.cv.class

w.rf.cv.class <- mean(w.rf.cv.class)
print(paste("The average outputs correctly predicted is",
            round(w.rf.cv.class*100,digits =2),"%",sep=" "))

RF.results[3,] <- c("dfw Random Forest w/ 10-fold CV",
                      round(w.rf.cv.class*100,digits=2))

#dfw random forest with cross-validation

for(i in 1:k){
  tr.rf <- dfw2[folds2$subsets[folds2$which != i],]
  te.rf <- dfw2[folds2$subsets[folds2$which == i],]
  
  w.rf.cv2 <- randomForest(quality~., data = tr.rf, mtry =10, ntree = 500,
                          importance = TRUE)
  
  w.rf.pred.cv2 <- predict(w.rf.cv2, newdata = te.rf)
  
  w.rf.cv.class2[i] <- mean(w.rf.pred.cv2 == te.rf$quality)
  
}

w.rf.cv.class2

w.rf.cv.class2 <- mean(w.rf.cv.class2)
print(paste("The average outputs correctly predicted is",
            round(w.rf.cv.class2*100,digits =2),"%",sep=" "))

RF.results[4,] <- c("dfw Random Forest w/ 10-fold CV",
                    round(w.rf.cv.class2*100,digits=2))

#dfw2 random forest with cross-validation

w.ordfor <- ordfor("quality", data = dfw.train, mtry = 11,
                   nsets = 100, ntreeperdiv = 10, ntreefinal = 500,
                   nbest = 1, npermtrial = 50)

w.ordfor.pred <- predict(w.ordfor, newdata = dfw.test)

w.ordfor.class <- mean(w.ordfor.pred$ypred==dfw.test$quality)

RF.results[5,] <- c("dfw Ordinal Forest w/ training/test set",
                    round(w.ordfor.class*100,digits=2))


head(sort(w.ordfor$varimp, decreasing = TRUE), 4)

#dfw ordinal forest with training/test set

w.ordfor2 <- ordfor("quality", data = dfw.train2, mtry = 10,
                    nsets = 100, ntreeperdiv = 10, ntreefinal = 500,
                    nbest = 1, npermtrial = 50)

w.ordfor.pred2 <- predict(w.ordfor2, newdata = dfw.test2)

w.ordfor.class2 <- mean(w.ordfor.pred2$ypred==dfw.test2$quality)

RF.results[6,] <- c("dfw2 Ordinal Forest w/ training/test set",
                    round(w.ordfor.class2*100,digits=2))

head(sort(w.ordfor2$varimp, decreasing = TRUE), 4)

#dfw2 ordinal forest with training/test set

w.ordfor.cv.class <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))
w.ordfor.cv.class2 <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))

#preparing both datasets for cross-validation

for(i in 1:k){
  tr.of <- dfw[folds$subsets[folds$which != i],]
  te.of <- dfw[folds$subsets[folds$which == i],]
  
  w.ordfor.cv <- ordfor("quality", data = tr.of, mtry = 11,
                        nsets = 100, ntreeperdiv = 10, ntreefinal = 500,
                        nbest = 1, npermtrial = 50)
  
  w.ordfor.pred.cv <- predict(w.ordfor.cv, newdata = te.of)
  
  w.ordfor.cv.class[i] <- mean(w.ordfor.pred.cv$ypred == te.of$quality)
  
}

w.ordfor.cv.class

w.ordfor.cv.class <- mean(w.ordfor.cv.class)
print(paste("The average outputs correctly predicted is",
            round(w.ordfor.cv.class*100,digits =2),"%",sep=" "))

RF.results[7,] <- c("dfw Ordinal Forest w/ 10-fold CV",
                    round(w.ordfor.cv.class*100,digits=2))

#dfw ordinal forest with 10-fold cross validation

for(i in 1:k){
  tr.of <- dfw2[folds2$subsets[folds2$which != i],]
  te.of <- dfw2[folds2$subsets[folds2$which == i],]
  
  w.ordfor.cv2 <- ordfor("quality", data = tr.of, mtry = 10,
                         nsets = 100, ntreeperdiv = 10, ntreefinal = 500,
                         nbest = 1, npermtrial = 50)
  
  w.ordfor.pred.cv2 <- predict(w.ordfor.cv2, newdata = te.of)
  
  w.ordfor.cv.class2[i] <- mean(w.ordfor.pred.cv2$ypred == te.of$quality)
  
}

w.ordfor.cv.class2

w.ordfor.cv.class2 <- mean(w.ordfor.cv.class2)
print(paste("The average outputs correctly predicted is",
            round(w.ordfor.cv.class2*100,digits =2),"%",sep=" "))

RF.results[8,] <- c("dfw2 Ordinal Forest w/ 10-fold CV",
                    round(w.ordfor.cv.class2*100,digits=2))

#dfw2 ordinal forest with 10-fold cross validation

RF.results