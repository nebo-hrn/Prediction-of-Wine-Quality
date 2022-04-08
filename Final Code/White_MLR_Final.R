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

white <- read_csv("winequality-white.csv")

sum(is.na(white))
white <- na.omit(white)
#Reading in the data

dfw <- as.data.frame(white)

dfw <- dfw[-2782,]
dfw2 <- subset(dfw, select = -density)

#Creating the dataframes to be used

MLR.results <- data.frame(matrix(ncol=2,nrow=0, 
                 dimnames=list(NULL, c("Model", "Classification Accuracy %"))))

#Empty data frame for results

set.seed(100)

test <- sample(1:nrow(dfw), size = nrow(dfw)/5)
train <- (-test)

#Training and Test sets for dfw

dfw.train <- dfw[train,]
dfw.test <- dfw[test,]

#Create a test and training data set, 20/80 split

w.mlm.train <- lm(
  quality ~.,
  data = dfw.train
)
#Create linear regression with all predictors using the training dataset

w.mlm.predict <- predict(w.mlm.train, newdata = dfw.test)
w.mlm.predict.rounded <- round(w.mlm.predict, digits = 0)

#Round the predicted to a integer so it can be compared to the test set for
# classification

(con_mat <- table(w.mlm.predict.rounded, dfw.test$quality))

w.mlm.acc <- round(mean(w.mlm.predict.rounded==dfw.test$quality)*100,digits = 2)

#Create the confusion matrix and calculate the proportion correct

MLR.results[1,] <- c("dfw MLR w/ Train/Test", w.mlm.acc)

#dfw train/test multi linear regression

test <- sample(1:nrow(dfw2), size = nrow(dfw2)/5)
train <- (-test)

dfw2.train <- dfw2[train,]
dfw2.test <- dfw2[test,]

#Create a test and training data set, 20/80 split

w.mlm.train2 <- lm(
  quality ~.,
  data = dfw2.train
)
#Create linear regression with all predictors using the training dataset

w.mlm.predict2 <- predict(w.mlm.train2, newdata = dfw2.test)
w.mlm.predict.rounded2 <- round(w.mlm.predict2, digits = 0)

#Round the predicted to a integer so it can be compared to the test set for
# classification

(con_mat <- table(w.mlm.predict.rounded2, dfw2.test$quality))

w.mlm.acc2 <- round(mean(w.mlm.predict.rounded2==dfw2.test$quality)*100,
                    digits = 2)

#Create the confusion matrix and calculate the proportion correct

MLR.results[2,] <- c("dfw2 MLR w/ Train/Test", w.mlm.acc2)

#dfw2 train/test multi linear regression

k <- 10 #number of folds

folds <- cvFolds(nrow(dfw), K=k)
folds2 <- cvFolds(nrow(dfw2), K=k)

w.mlm.cv.class <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))
w.mlm.cv.class2 <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))

#Preparing both datasets for cross-validation

for(i in 1:k){
  tr.mlr <- dfw[folds$subsets[folds$which != i],]
  te.mlr <- dfw[folds$subsets[folds$which == i],]
  
  w.mlm <- lm(quality~., data = tr.mlr)
  w.mlm.pred <- predict(w.mlm, newdata = te.mlr)
  
  w.mlm.cv.class[i] <- mean(round(w.mlm.pred, digits = 0)==te.mlr$quality)
}

w.mlm.cv.class

w.mlm.cv.class <- mean(w.mlm.cv.class)
print(paste("The average outputs correctly predicted is",
            round(w.mlm.cv.class*100,digits =2),"%",sep=" "))

MLR.results[3,] <- c("dfw MLR w/ 10-fold CV", round(w.mlm.cv.class*100, 
                                                    digits=2))

#dfw cross-validated Multiple Linear Regression

for(i in 1:k){
  tr.mlr2 <- dfw2[folds2$subsets[folds2$which != i],]
  te.mlr2 <- dfw2[folds2$subsets[folds2$which == i],]
  
  w.mlm2 <- lm(quality~., data = tr.mlr2)
  w.mlm.pred2 <- predict(w.mlm2, newdata = te.mlr2)
  
  w.mlm.cv.class2[i] <- mean(round(w.mlm.pred2, digits = 0)==te.mlr2$quality)
}

w.mlm.cv.class2

w.mlm.cv.class2 <- mean(w.mlm.cv.class2)
print(paste("The average outputs correctly predicted is",
            round(w.mlm.cv.class2*100,digits =2),"%",sep=" "))

MLR.results[4,] <- c("dfw2 MLR w/ 10-fold CV", round(w.mlm.cv.class2*100, 
                                                    digits=2))

#dfw2 cross-validated Multiple Linear Regression

dfw$quality <- factor(dfw$quality, ordered = TRUE)
dfw2$quality <- factor(dfw2$quality, ordered = TRUE)

#Making the response variable a factor for ordinal logistic regression

test <- sample(1:nrow(dfw), size = nrow(dfw)/5)
train <- (-test)

dfw.train <- dfw[train,]
dfw.test <- dfw[test,]

test <- sample(1:nrow(dfw2), size = nrow(dfw2)/5)
train <- (-test)

dfw2.train <- dfw2[train,]
dfw2.test <- dfw2[test,]

#Re-create training and test set with new factored response variable

w.olr <- polr(quality~., data = dfw.train, Hess = TRUE)

w.olr.pred <- predict(w.olr, newdata = dfw.test)

w.olr.pred <- as.numeric(as.character(unlist(w.olr.pred)))
dfw.test$quality <- as.numeric(as.character(unlist(dfw.test$quality)))

w.olr.class <- mean(w.olr.pred == dfw.test$quality)

MLR.results[5,] <- c("dfw OLR w/ Training/Test", round(w.olr.class*100, 
                                                     digits=2))

#dfw OLR w/ Training/Test Set

w.olr2 <- polr(quality~., data = dfw2.train, Hess = TRUE)

w.olr.pred2 <- predict(w.olr2, newdata = dfw2.test)

w.olr.pred2 <- as.numeric(as.character(unlist(w.olr.pred2)))
dfw2.test$quality <- as.numeric(as.character(unlist(dfw2.test$quality)))

w.olr.class2 <- mean(w.olr.pred2 == dfw2.test$quality)

MLR.results[6,] <- c("dfw2 OLR w/ Training/Test", round(w.olr.class2*100, 
                                                       digits=2))

#dfw2 OLR w/ Training/Test Set

folds <- cvFolds(nrow(dfw), K=k)
folds2 <- cvFolds(nrow(dfw2), K=k)

w.olr.cv.class <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))
w.olr.cv.class2 <- matrix(NA,k,1, dimnames=list(NULL, paste(1)))

#Re-create the folds and empty classification matrix for OLR

for(i in 1:k){
  tr.olr <- dfw[folds$subsets[folds$which != i],]
  te.olr <- dfw[folds$subsets[folds$which == i],]
  
  w.olr.cv <- polr(quality~., data = tr.olr, Hess = TRUE)
  w.olr.cv.pred <- predict(w.olr.cv, newdata = te.olr)
  
  w.olr.cv.pred <- as.numeric(as.character(unlist(w.olr.cv.pred)))
  te.olr$quality <- as.numeric(as.character(unlist(te.olr$quality)))
  
  w.olr.cv.class[i] <- mean(w.olr.cv.pred==te.olr$quality)
}

w.olr.cv.class

w.olr.cv.class <- mean(w.olr.cv.class)
print(paste("The average outputs correctly predicted is",
            round(w.olr.cv.class*100,digits =2),"%",sep=" "))

MLR.results[7,] <- c("dfw OLR w/ 10-fold CV", round(w.olr.cv.class*100, 
                                                    digits=2))

#dfw OLR w/ 10-fold CV

for(i in 1:k){
  tr.olr2 <- dfw2[folds2$subsets[folds2$which != i],]
  te.olr2 <- dfw2[folds2$subsets[folds2$which == i],]
  
  w.olr.cv2 <- polr(quality~., data = tr.olr2, Hess = TRUE)
  w.olr.cv.pred2 <- predict(w.olr.cv2, newdata = te.olr2)
  
  w.olr.cv.pred2 <- as.numeric(as.character(unlist(w.olr.cv.pred2)))
  te.olr2$quality <- as.numeric(as.character(unlist(te.olr2$quality)))
  
  w.olr.cv.class2[i] <- mean(w.olr.cv.pred2==te.olr2$quality)
}

w.olr.cv.class2

w.olr.cv.class2 <- mean(w.olr.cv.class2)
print(paste("The average outputs correctly predicted is",
            round(w.olr.cv.class2*100,digits =2),"%",sep=" "))

MLR.results[8,] <- c("dfw2 OLR w/ 10-fold CV", round(w.olr.cv.class2*100, 
                                                    digits=2))

#dfw2 OLR w/ 10-fold CV


MLR.results