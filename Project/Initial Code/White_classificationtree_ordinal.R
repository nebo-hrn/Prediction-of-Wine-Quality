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
names(dfw) <- make.names(names(dfw))

SEED <- 5864
set.seed(SEED)

test <- sample(1:nrow(dfw), size = nrow(dfw)/5)
train <- (-test)

dfw.train <- dfw[train,]
dfw.test <- dfw[test,]

#ordinaltree <- rpartScore(quality~., data = dfw.train)

#ordinaltree.predict <- predict(ordinaltree, newdata= dfw.test)

#mean(ordinaltree.predict==dfw.test$quality)
#Did not work so commented out

rpart.dfw <- rpart(quality~.,data=dfw,method ="class", cp=0.0000000000001)
plotcp(rpart.dfw)
printcp(rpart.dfw)

#indicates a value around 100 has the lowest xerror and xstd, when we look
#its a value of tree of size 126, we can get this with a cp of 1.3333e-03

rpart.dfw.min <- prune(rpart.dfw, cp=1.3333e-03)

rpart.plot(rpart.dfw.min, extra = 2)

rpart.dfw.min2 <- rt.prune(rpart.dfw, se=0)
printcp(rpart.dfw.min2)
plotcp(rpart.dfw.min2)

#Best value shown at tree size of 53 and a cp of 0.0015

rpart.dfw.1se <- rt.prune(rpart.dfw, se=1)
printcp(rpart.dfw.1se)
plotcp(rpart.dfw.1se)

rpart.plot(rpart.dfw.1se, extra = 2)

test <- sample(1:nrow(dfw), size = nrow(dfw)/5)
train <- (-test)

dfw.train <- dfw[train,]
dfw.test <- dfw[test,]

rpart.dfw.train <- rpart(quality~., dfw.train, method = "class", cp = .0015)
rpart.dfw.pred <- predict(rpart.dfw.train, dfw.test, type = "class")

table(rpart.dfw.pred, dfw.test$quality)

plotcp(rpart.dfw.train)
mean(rpart.dfw.pred==dfw.test$quality)
rpart.plot(rpart.dfw.train, extra =2)

#Training-test shows a classification rate of 53%

M <- 11


bag.dfw <- randomForest(quality~., data =dfw.train,mtry=11, importance = TRUE)

bag.dfw
plot(bag.dfw, main = "Bagged trees, mtry = 11, ntrees = 500")

bag.dfw.pred <- predict(bag.dfw, newdata = dfw.test)

mean(bag.dfw.pred==dfw.test$quality)

importance(bag.dfw)

varImpPlot(bag.dfw)

#Training-test shows a classification of ~65% for bagged random forest

rf.dfw <- randomForest(quality~., data=dfw.train,mtry=5, importance = TRUE)
rf.dfw.pred <- predict(rf.dfw, newdata = dfw.test)

mean(rf.dfw.pred==dfw.test$quality)

rf.dfw
importance(rf.dfw)
varImpPlot(rf.dfw)


#Training-test shows a classification of ~65-66% for random forest 

dfw.train2 <- dplyr::select(dfw.train, quality, alcohol, volatile.acidity,
                            density,free.sulfur.dioxide)
dfw.test2 <- dplyr::select(dfw.test, quality, alcohol, volatile.acidity,
                           density,free.sulfur.dioxide)

rf.dfw2 <- randomForest(quality~., data=dfw.train2, mtry=2, importance=TRUE)
rf.dfw.pred2 <- predict(rf.dfw2, newdata = dfw.test2)

mean(rf.dfw.pred2==dfw.test2$quality)

rf.dfw2


#Four best from MDI, gives a classification rate of ~64.65%

dfw.train3 <- dplyr::select(dfw.train, quality, alcohol, volatile.acidity,
                            pH,free.sulfur.dioxide)
dfw.test3 <- dplyr::select(dfw.test, quality, alcohol, volatile.acidity,
                           pH,free.sulfur.dioxide)

rf.dfw3 <- randomForest(quality~., data=dfw.train3, mtry=2, importance=TRUE)
rf.dfw.pred3 <- predict(rf.dfw3, newdata = dfw.test3)

mean(rf.dfw.pred3==dfw.test3$quality)

rf.dfw3

#Four best from MDA, gives a classifiation rate of ~62.9





