suppressWarnings(suppressMessages(library('tidyverse')))  # loads tons of packages
suppressWarnings(suppressMessages(library('dplyr')))
suppressWarnings(suppressMessages(library('randomForest')))
suppressWarnings(suppressMessages(library('caret')))
suppressWarnings(suppressMessages(library('ggthemes')))
suppressWarnings(suppressMessages(library('mice')))  ## imputations
suppressWarnings(suppressMessages(library('Amelia'))) ## stuff for missing values
suppressWarnings(suppressMessages(library('rmarkdown')))
suppressWarnings(suppressMessages(library('gridExtra')))
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
rfGrid <- expand.grid(.mtry=c(1:sqrt(ncol(dev))))
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






######################################################
###### GBM #######
suppressWarnings(suppressMessages(library('gbm')))
gbmGrid <- expand.grid(interaction.depth = c(10, 20),
                       n.trees = (10:50)*50, 
                       shrinkage = seq(.001, 0.01),
                       n.minobsinnode = 10) #seq(.001))
gbmControl <- trainControl(method = "cv",
                           number=10,
                           repeats = 3#, classProbs = TRUE
)
mod.gbm <- train(SalePrice ~ ., data = dev,
                 #dist = "gaussian", 
                 method = "gbm",
                 trControl = gbmControl,
                 verbose = FALSE, 
                 tuneGrid = gbmGrid
)
# interaction.depth  n.trees  RMSE       Rsquared 
# 20                 2500     0.1380013  0.8962095

summary(mod.gbm) ##gives graph of feature importance
# OverallQual                   OverallQual 4.671094e+01
# GrLivArea                       GrLivArea 1.746358e+01
# TotalBsmtSF                   TotalBsmtSF 6.550783e+00
# GarageCars                     GarageCars 4.610815e+00
# BsmtFinSF1                     BsmtFinSF1 3.097085e+00
# YearBuilt                       YearBuilt 2.767574e+00
# CentralAirY                   CentralAirY 2.236760e+00
# LotArea                           LotArea 1.881119e+00
# OverallCond                   OverallCond 1.765352e+00
# YearRemodAdd                 YearRemodAdd 1.662115e+00
# Fireplaces                     Fireplaces 1.413450e+00
# ExterQualTA                   ExterQualTA 7.868708e-01
# LotFrontage                   LotFrontage 6.238139e-01
# MSZoningRM                     MSZoningRM 5.548262e-01
# X2ndFlrSF                       X2ndFlrSF 4.663291e-01

gbm.perf(mod.gbm$finalModel)
plot(mod.gbm) ##cool graph!!!
plot(mod.gbm, plotType = "level")
mod.gbm$finalModel$fit

gbm.predict<-predict(mod.gbm, val)

mod.gbm
postResample(pred = gbm.predict, obs = val$SalePrice)
# RMSE  Rsquared 
# 0.1299798 0.8932267 
save.image()

