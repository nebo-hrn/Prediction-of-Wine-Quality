library(dplyr)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(leaps)
library(glmnet)
library(coefplot)
library(ggfortify)
library(readr)
library(car)
library(moments)
library(ggpubr)
library(ggrepel)
library(qqplotr)
library(MASS)
library(ordinal)
library(caret)
library(rpart)
library(rpart.plot)
library(rpartScore)
library(DMwR2)
library(randomForest)


white <- read_csv("winequality-white.csv")

sum(is.na(white))
white <- na.omit(white)


dfw <- as.data.frame(white)


SEED <- 5864
set.seed(SEED)

test <- sample(1:nrow(dfw), size = nrow(dfw)/5)
train <- (-test)

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

#Using the training model, predict the response variable using the "test set"

w.mlm.predict.rounded <- round(w.mlm.predict, digits = 0)

#Round the predicted to a integer so it can be compared to the test set for
# classification

(con_mat <- table(w.mlm.predict.rounded, dfw.test$quality))
mean(w.mlm.predict.rounded==dfw.test$quality)
mean(w.mlm.predict.rounded!=dfw.test$quality)

#Create the confusion matrix and calculate the proportion correct and incorrect
#This model was correct ~52.91113% of the time

correctness <- rep(NA, 100)

for(j in 1:100){
tol_value <- j*0.01
tolx <- w.mlm.predict+(w.mlm.predict*tol_value)
tolz <- w.mlm.predict-(w.mlm.predict*tol_value)
yorn <- 0
for(i in 1:nrow(dfw.test)){
  yorn[i]<-between(dfw.test$quality[i],tolz[i],tolx[i])
}
yorn
correctness[j] <- sum(yorn)/length(yorn)
}
correctness <- correctness*100
tolerance_percentage <- 1:100
plot(tolerance_percentage,correctness, 
     main = "Classification Rate vs Tolerance")


#This is basically MSE proof, at .50 so withing a range of 1 we have a 95%
#confidence interval assuming confidence interval

w.glm.train <- glm(quality~.,data=dfw.train, family = "poisson")
w.glm.predict <- predict(w.glm.train, newdata = dfw.test, type = "response")

w.glm.predict.rounded <- round(w.glm.predict, digits = 0)

con_mat.glm <- table(w.glm.predict.rounded, dfw.test$quality)
mean(w.glm.predict.rounded==dfw.test$quality)
mean(w.glm.predict.rounded!=dfw.test$quality)

#Tried poisson regression, it is only marginally better at 50.97%

#Use a validation set to choose among models
w.best <- regsubsets(quality~., data = dfw.train, nvmax=11)
#Create a best subsets model with all the training data and variables

test.mat=model.matrix(quality~., data = dfw.test)
w.val.errors <- rep(NA,11)
classif <- rep(NA,11)
inclass <- rep(NA,11)
for(i in 1:11){
  coefi <- coef(w.best, id=i)
  pred <- test.mat[,names(coefi)]%*%coefi
  w.val.errors[i] <- mean((dfw.test$quality-round(pred,digits=0))^2)
  classif[i] <- mean(round(pred,digits=0)==dfw.test$quality)
  inclass[i] <- mean(round(pred,digits=0)!=dfw.test$quality)
}
w.val.errors
classif

which.min(w.val.errors)
which.max(classif)

round(coef(w.best, 9),3)
round(coef(w.best, 6),3)

w.best2 <- regsubsets(quality~., data = dfw, nvmax=11)
round(coef(w.best2, 9),3)
round(coef(w.best2, 6),3)

#Found the model that has the highest classification rate, val.error doesnt tell
# us much here other than if the prediction errors are normally distributed
# then 95% of the time it will be within 1 of the correct value

#Use cross validation to choose among models

predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form,newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[,xvars] %*% coefi
}

k <- 10
n <- nrow(dfw)
set.seed(SEED)
folds=sample(rep(1:k, length=n))

w.cv.errors=matrix(NA,k,11, dimnames=list(NULL, paste(1:11)))
classif.kfold <- matrix(NA,k,11, dimnames=list(NULL, paste(1:11)))

for(j in 1:k){
  best.fit=regsubsets(quality~.,data=dfw[folds!=j,],nvmax=11) 
  for(i in 1:11){
    pred=predict(best.fit,dfw[folds==j,], id=i)
    w.cv.errors[j,i] <- mean( (dfw$quality[folds==j] - round(pred,digits =0))^2)
    classif.kfold[j,i] <- mean(round(pred,digits=0)==dfw$quality[folds==j])
    }
}

mean.w.cv.errors <- rep(0, 11) 
for (i in 1:11) {
  mean.w.cv.errors[i] <- mean(w.cv.errors[,i])
}

mean.classif.kfold <- rep(0, 11) 
for (i in 1:11) {
  mean.classif.kfold[i] <- mean(classif.kfold[,i])
}

#Best-plot function
best.plot <- function(varName, varLabel, minmax=" ") {
  gg <- ggplot(data.frame(varName), aes(x=seq_along(varName),
                                        y=varName)) + 
    geom_line() +
    labs(x="Number of variables",
         y=varLabel, title="Best subsets")
  
  if (minmax=="min") {
    gg <-  gg + geom_point(aes(x=which.min(varName), y=min(varName)),
                           color="red") +
      geom_vline(aes(xintercept=which.min(varName)),linetype="dotted")
  }
  if (minmax=="max") {
    gg <- gg + geom_point(aes(x=which.max(varName), y=max(varName)),
                          color="red") +
      geom_vline(aes(xintercept=which.max(varName)), linetype="dotted")
  } 
  return(gg)
}

#End of best-plot function

best.plot(mean.w.cv.errors, "Cross-validated MSE", "min")
best.plot(mean.classif.kfold, "Cross-validated Mean Correct Classification", 
          "max")

mean.w.cv.errors[3]

mean.classif.kfold
which.max(mean.classif.kfold)

w.best2 <- regsubsets(quality~., data = dfw, nvmax=11)
round(coef(w.best2, 3),3)

#Use lasso regression as a potential for variable selection

library(dplyr)
library(tidyverse)
library(gridExtra)
library(corrplot)
library(leaps)
library(glmnet)
library(coefplot)
library(ggfortify)
library(readr)
library(car)
library(moments)
library(ggpubr)
library(ggrepel)
library(qqplotr)
library(MASS)
library(ordinal)
library(caret)
library(rpart)
library(rpart.plot)
library(rpartScore)
library(DMwR2)
library(randomForest)

white <- read_csv("winequality-white.csv")

sum(is.na(white))
white <- na.omit(white)


dfw <- as.data.frame(white)

dfw$quality <- as.factor(dfw$quality)

SEED <- 5864
set.seed(SEED)

test <- sample(1:nrow(dfw), size = nrow(dfw)/5)
train <- (-test)

dfw.train <- dfw[train,]
dfw.test <- dfw[test,]

w.clm <- clm(quality~.,data=dfw.train)
w.clm.predict <- predict(w.clm, newdata=dfw.test, type = "class")

w.clm.predict <- as.numeric(as.character(unlist(w.clm.predict)))
dfw.test$quality <- as.numeric(as.character(unlist(dfw.test$quality)))

mean(w.clm.predict==dfw.test$quality)

summary(w.clm)


convergence(w.clm)

tim <- cbind(dfw.test, predict(w.clm, newdata = dfw.test, type = "class")$fit)

head(do.call("cbind", predict(w.clm, se.fit=TRUE, interval=TRUE)))