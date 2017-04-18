suppressWarnings(suppressMessages(library('tidyverse')))  # loads tons of packages
suppressWarnings(suppressMessages(library('dplyr')))
suppressWarnings(suppressMessages(library('randomForest')))
suppressWarnings(suppressMessages(library('caret')))
suppressWarnings(suppressMessages(library('ggthemes')))
suppressWarnings(suppressMessages(library('mice')))  ## imputations
suppressWarnings(suppressMessages(library('Amelia'))) ## stuff for missing values
suppressWarnings(suppressMessages(library('rmarkdown')))
suppressWarnings(suppressMessages(library('gridExtra')))
suppressWarnings(suppressMessages(library('Matrix')))
library('reshape2',quietly=TRUE)

setwd("~/Dropbox/data science/data projects/kaggle - house prices")


## load clean data 

train <- read.csv('trainClean.csv')
test <- read.csv('testClean.csv')

train$SalePrice <- log(train$SalePrice) ## fix skewness


##### PREDICTIONS ########

## Split the data to train and CV sets
set.seed(42)
splitIndex<-createDataPartition(train$Id, p=.8, list = F, times=1)
dev<-train[splitIndex,]
val<-train[-splitIndex,]

###### RANDOM FOREST #####
 rfControl = trainControl(method = "repeatedcv", # Use cross-validation
                                         number = 10, # Use 10 folds for cross-validation
                                         repeats = 3, search="grid")
rfGrid <- expand.grid(.mtry=sqrt(ncol(dev)))#(.mtry=ncol(train))#c(1:sqrt(ncol(dev))))
rf.model<-train(SalePrice~.,data = dev,
                method = "rf",
                importance=T,
                metric = 'RMSE',
                tuneGrid = rfGrid, trControl=rfControl)
print(rf.model)
# mtry  RMSE       Rsquared
# 1     0.3138272  0.7711145
# 2     0.2319047  0.8075637
# 3     0.1988847  0.8314919
# 4     0.1824158  0.8479245
# 5     0.1723779  0.8585380
# 6     0.1659131  0.8652593
# 7     0.1615833  0.8703370
# 8     0.1580716  0.8742074

plot(rf.model)
importance(rf.model$finalModel) ## variable importance

summary(rf.model)
imp <- varImp(rf.model)
imp
# Overall
# GrLivArea         100.00
# TotalBsmtSF        96.10
# OverallQual        89.71
# LotArea            89.10
# X2ndFlrSF          88.18
# LotFrontage        85.45
# TotRmsAbvGrd       82.89
# Fireplaces         80.07
# MSSubClass         78.38
# GarageCars         77.58

## check on hold-out validation set
rf.predict<-predict(rf.model, val)
postResample(pred = rf.predict, obs = val$SalePrice) ## R2 and RSME
# RMSE  Rsquared
# 0.1415392 0.8845851

##re-run on the whole training set
rf.predict<-predict(rf.model, test)
Final<-data.frame(Id = test$Id, SalePrice = exp(rf.predict))
head(Final)
write.csv(Final, file = 'houses_subm_rf1.csv', row.names = F)
# public score 0.14584




######## XGBOOST ########
suppressWarnings(suppressMessages(library('xgboost')))
set.seed(42)
labels <- dev$SalePrice## labels vector
dev$SalePrice <-NULL
dev$Id <-NULL
testID<-val$Id
val$Id <-NULL
dim(train)
dim(test)

dtrain <- xgb.DMatrix(data = as.matrix(dev),label = labels)
dtest <- xgb.DMatrix(data = as.matrix(val),label=val$SalePrice)

params <- list(booster = "gbtree", objective = 'reg:linear', eta=0.3, gamma=0, 
               max_depth=6, min_child_weight=1, 
               subsample=1, colsample_bytree=1)

xgbcv <- xgb.cv( params = params, data = dtrain, 
                 nrounds = 100, nfold = 5, 
                 showsd = T, stratified = T,
                 print_every_n = 10, 
                 early_stopping_rounds = 20, maximize = F)

xgb1 <- xgb.train(params = params, data = dtrain, nrounds = 38, 
                  watchlist = list(val=dtest,train=dtrain), 
                  print_every_n = 10, early_stopping_rounds = 20, 
                  maximize = F)

xgbpred <- predict (xgb1,dtest)
postResample(pred = xgbpred, obs = val$SalePrice)
# RMSE  Rsquared 
# 0.1334464 0.8757026 
mat <- xgb.importance (feature_names = colnames(dev),model = xgb1)
xgb.plot.importance (importance_matrix = mat[1:20])


## TUNING
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())

cv.ctrl = trainControl(method = "repeatedcv", repeats = 1,number = 4,
                       allowParallel=T)
xgbGrid <- expand.grid(nrounds = c(100,1000),
                       eta = c(0.01,0.05,0.1, 0.3), #0.05
                       max_depth = c(2,4,6), ##4
                       gamma = c(0, 0.01),
                       colsample_bytree = c(0.3, 0.4, 0.5, 0.6, 1), #0.3
                       min_child_weight = c(0.3, 0.5, 0.6, 1), #0.3
                       subsample = c(0.5, 1))

xgb_tune = train(as.matrix(dev),
                 labels,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgbGrid,
                 verbose=T,
                 metric="RMSE",
                 nthread =3)
xgb_tune
# RMSE was used to select the optimal model using  the smallest value.
# The final values used for the model were nrounds = 1000, max_depth = 2, eta
# = 0.05, gamma = 0, colsample_bytree = 0.3, min_child_weight = 0.5 and
# subsample = 0.5.
xgbpred <- predict (xgb_tune,dtest)
postResample(pred = xgbpred, obs = val$SalePrice)
# RMSE  Rsquared
# 0.1182556 0.9014886

######## WHOLE SET ######
labels<- train$SalePrice
train$SalePrice <-NULL
train$Id <-NULL
testID<-test$Id
test$Id <-NULL
dtrain1 <- xgb.DMatrix(data = as.matrix(train),label = labels)
dtest1 <- xgb.DMatrix(data = as.matrix(test) )

params <- list(booster = "gbtree", objective = 'reg:linear', eta=0.05, gamma=0, 
               max_depth=2, min_child_weight=0.5, 
               subsample=0.5, colsample_bytree=0.3)

xgbcv <- xgb.cv( params = params, data = dtrain, 
                 nrounds = 100, nfold = 5, 
                 showsd = T, stratified = T,
                 print_every_n = 10, 
                 early_stopping_rounds = 20, maximize = F)


## TUNING
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())

cv.ctrl = trainControl(method = "repeatedcv", repeats = 1,number = 4,
                       allowParallel=T)
xgbGrid <- expand.grid(nrounds = c(1000),
                       eta = c(0.01,0.05,0.1), #0.05
                       max_depth = c(2,4,6), ##4
                       gamma = c(0, 0.01),
                       colsample_bytree = c(0.3, 0.4, 0.5, 0.6, 1), #0.3
                       min_child_weight = c(0.3, 0.5, 0.6, 2), #0.3
                       subsample = c(0.5, 1))

xgb_tune = train(as.matrix(train),
                 labels,
                 method="xgbTree",
                 trControl=cv.ctrl,
                 tuneGrid=xgbGrid,
                 verbose=T,
                 metric="RMSE",
                 nthread =3)

xgb_tune$bestTune

params <- list(booster = "gbtree", objective = 'reg:linear', 
               eta=0.01, gamma=0.01, 
               max_depth=6, min_child_weight=2, 
               subsample=0.5, colsample_bytree=0.3)

xgb1 <- xgb.train(params = params, data = dtrain1, nrounds = 1000 
                  #watchlist = list(val=dtest,train=dtrain), 
)
xgbpred <- predict (xgb1,dtest1)
head(exp(xgbpred))
Final<-data.frame(Id = testID, SalePrice = exp(xgbpred))
head(Final)
write.csv(Final, file = 'houses_subm_xgb1.csv', row.names = F)
# score 0.12265