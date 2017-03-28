suppressWarnings(suppressMessages(library('tidyverse')))  # loads tons of packages
suppressWarnings(suppressMessages(library('dplyr')))
suppressWarnings(suppressMessages(library('randomForest')))
suppressWarnings(suppressMessages(library('caret')))
suppressWarnings(suppressMessages(library('ggthemes')))
suppressWarnings(suppressMessages(library('mice')))  ## imputations
suppressWarnings(suppressMessages(library('Amelia'))) ## stuff for missing values
library('rmarkdown')
suppressWarnings(suppressMessages(library('gridExtra')))
library('reshape2',quietly=TRUE)
library('anytime',quietly=TRUE)

setwd("~/Dropbox/data science/data projects/kaggle - house prices")



train <- read.csv('train.csv', stringsAsFactors = FALSE,
                     strip.white = TRUE, na.strings = c("NA",""))
test <- read.csv('test.csv', stringsAsFactors = FALSE,
                    strip.white = TRUE, na.strings = c("NA",""))

#summary(train)



train[1,!sapply(train, is.numeric)]
nbhd<-unique(train$Neighborhood)
sum(is.na(train$Neighborhood)) ##no NA in train$Neighborhood
train$BsmtCond<- as.numeric(as.factor(train$BsmtCond))
train$BsmtQual<- as.numeric(as.factor(train$BsmtQual))
summary(train$BsmQualFactor)
train$BsmtCond[is.na(train$BsmtCond)]<-0
train$BsmtQual[is.na(train$BsmtQual)]<-0


##turns out neighborhood doesn;t influence the price much
plotSalePrice.vs.x <- function(xval){
  ggplot(train, aes(x=xval, y=train$SalePrice))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90))
}##!!!boxplot gives better info than bar- with outliers and quantiles!!!

plotSalePrice.vs.x(as.factor(train$Neighborhood))
plotSalePrice.vs.x(as.factor(train$BsmtQual))



####### CORRELATION MATRIX CALCULATION AND GRAPH #############

train$GarageCond <- as.numeric(as.factor(train$GarageCond))
train$GarageQual <- as.numeric(as.factor(train$GarageQual))
train$BsmtQual <- as.numeric(as.factor(train$BsmtQual))
train$BsmtCond <- as.numeric(as.factor(train$BsmtCond))
train$GarageType <- as.numeric(as.factor(train$GarageType))



cor.matr<-cor(train[,sapply(train,is.numeric)],use="pairwise.complete.obs")
cor.matr<-cor.matr %>%
  round(2)%>%
  melt()
  

#upper triangular matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
cor.matr.up<- cor.matr%>%
  get_upper_tri()%>%
  melt(na.rm=TRUE)


##analyze predictors
# png('correlation_heatmap.png')
ggplot(data=cor.matr, aes(x=Var1,y=Var2, fill=value))+
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation")+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_equal()
# dev.off()
#### end of correlation


#train$YrBltFactor<-cut(train$YearBuilt,c(1872,1900,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010))


p1<-ggplot(train, aes(x = SalePrice)) +
      geom_histogram()
#p2<-ggplot(train, aes(x = YearBuilt)) +
#      geom_histogram()
# p2<-ggplot(train, aes(x = YrBltFactor)) +
#   geom_bar(aes(fill=as.factor(OverallQual)))

p3<-ggplot(train, aes(x = OverallQual)) +
      geom_bar()
p4<-ggplot(train, aes(x = GarageCars)) +
      geom_bar()
grid.arrange(p1,p3, p4, ncol=2)

pYB<-ggplot(train, aes(x = as.factor(YearBuilt),#BedroomAbvGr,
                     y=SalePrice))+
  theme(axis.text.x = element_text(angle = 90))

##factor by OverallQual
pYB + geom_bar(aes(fill=as.factor(OverallQual)),
           stat="identity",
           position="dodge")
  # scale_fill_brewer(name="Number of bedrooms", palette = "Dark2")+
  # labs(title="Bar plot Sale Price vs Year Sold by number of Bedrooms", x="Year Sold",
  #      y="Sale Price")

pYB + geom_bar(stat="identity")

pYB + geom_boxplot()



# #multiple graphs on one page
# grid.arrange(p1, p2, ncol=1) 


## Density graphs
# d1<-ggplot(train, aes(x=LotArea, color=as.factor(BedroomAbvGr))) +
#   geom_density()+
#   scale_x_log10()+
#   scale_color_discrete(name="Bedrooms")+
#   ggtitle("Lot area, by bedrooms")
# 
# d2<-ggplot(train, aes(x=SalePrice, color=as.factor(BedroomAbvGr))) +
#   geom_density()+
#   scale_x_log10()+
#   scale_color_discrete(name="Bedrooms")+
#   ggtitle("Sale price, by bedrooms")
# 
# grid.arrange(p1, d1, p2, d2, ncol=2) 
# 
# 
# 
# mm<-ggplot(train, aes(x=LotArea, y=SalePrice))
# mm+geom_density2d()+
#   scale_x_log10()+scale_y_log10()

#############################

#Important features
# train$GarageArea, train$GarageCars, train$FullBath, train$GrLivArea
# train$TotalBsmtSF, train$TotalBsmtSF, train$OverallQual
#train$TotRmsAbvGrd


###MISSING VALUES###
ratio.is.na<-function(dataset){
  col.na<-colMeans(is.na(dataset))
 col.na<-col.na[col.na>0]*100
 col.na
}

ratio.is.na(train)
ratio.is.na(test)
names(which(colSums(is.na(test))>0)) ## names of columns with NA
## we'll drop all Bsmt* variables apart from TotalBsmtSF (main info) and 
## BsmtQual that looks correlated to SalePrice. On the other hand: BsmtQual~~BsmtCond,
## At the same time BsmtFinSF1 is uncorrelated. 
## BsmtFullBath"  "BsmtHalfBath" seem to be not much correlated

## uncorrelated: X3SsnPorch, BsmtHalfBath, LowQualFinSF, BsmtFinSF2

##Twin variables: GarageArea, GarageCars,  
##                  X1stFlrSF, TotalBsmtSF


train$isTrain<-TRUE
test$isTrain<-FALSE
test$SalePrice<-NA
testID<-test$Id
ds<-rbind(train, test)
## right away exclude the features with missing values >15%
drop.fea.15 <-c("Alley", "MiscFeature", "Fence", "FireplaceQu","PoolQC","LotFrontage")
ds <- ds[!names(ds) %in% drop.fea.15]
ratio.is.na(ds)


## Let's work further with missing features
## There are some twin variables like GarageArea and GarageCars, GarageYrBlt. Let's drop GarageArea
##X1stFlrSF looks like twin of TotalBsmtSF, let's drop it
##also let's drop GarageCond, GarageQual (correlated with area, not much with sale price)
##with this for now i'll drop all other Garage
## BsmtCond doesn't have too much to say about final price, unlike BsmtQual
drop.fea <-c("GarageArea", "X1stFlrSF", "GarageYrBlt", "GarageCond", "GarageQual", 
             "TotRmsAbvGrd", "GarageType","GarageFinish", "BsmtCond",
             "BsmtFinSF1", "BsmtFinSF2","BsmtFinType2", "BsmtFinType1", 
             "BsmtHalfBath", "MasVnrArea", "MasVnrType", "BsmtUnfSF")
ds <- ds[!names(ds) %in% drop.fea]
ds$GarageCars[is.na(ds$GarageCars)]<-0


## features that are character and are missing
colnames(ds[,sapply(ds,is.character) & colSums(is.na(ds))>0])


ds$BsmtQual[is.na(ds$BsmtQual)] <- "None"
ds$BsmtQual <- as.factor(ds$BsmtQual)
ds$BsmtExposure[is.na(ds$BsmtExposure)] <- "NONE"
ds$BsmtExposure <- as.factor(ds$BsmtExposure)
ds$BsmtFullBath[is.na(ds$BsmtFullBath)] <- 0
ds$BsmtFullBath <- as.factor(ds$BsmtFullBath)


colSums(is.na(ds)>0)

##what's with kitchen and utilities
sum(is.na(ds$KitchenQual))
ds$OverallCond[is.na(ds$KitchenQual)] ## 3 - Fair
ds$KitchenQual[is.na(ds$KitchenQual)] <- "Fair"
ds$KitchenQual <- as.factor(ds$KitchenQual)

sum(is.na(ds$Utilities))
ds$Neighborhood[is.na(ds$Utilities)] ##"IDOTRR"  "Gilbert"
ds$Utilities[ds$Neighborhood=="IDOTRR"] ##AllPub
ds$Utilities[ds$Neighborhood=="Gilbert"]
ds$Utilities[is.na(ds$Utilities)] <- "AllPub"
ds$Utilities <- as.factor(ds$Utilities)

summary(as.factor(ds$Electrical))
ds$Electrical[is.na(ds$Electrical)] <- "SBrkr"
ds$Electrical <- as.factor(ds$Electrical)

# ds$MasVnrArea[is.na(ds$MasVnrArea[is.na(ds$MasVnrType)])] <- 0
# ds$MasVnrType[is.na(ds$MasVnrType)]<-"NONE"

ds$Exterior1st[is.na(ds$Exterior1st)] <- "VinylSd"
ds$Exterior2nd[is.na(ds$Exterior2nd)] <- "VinylSd"
ds$TotalBsmtSF[is.na(ds$TotalBsmtSF)] <- 0

summary(as.factor(ds$SaleType)) ## majority "WD"
ds$SaleType[is.na(ds$SaleType)] <- "WD"

summary(as.factor(ds$Functional)) ## majority "Typ"
ds$Functional[is.na(ds$Functional)] <- "Typ"

summary(as.factor(ds$MSZoning)) ## majority "RL"
ds$MSZoning[is.na(ds$MSZoning)] <- "RL"

# ds$YearBuilt <- anydate(ds$YearBuilt)
# ds$YrSold <- anydate(ds$YrSold)
# ds$YearRemodAdd <- anydate(ds$YearRemodAdd)
# ds$GarageYrBlt <- anydate(ds$GarageYrBlt)

train <- ds %>% filter(isTrain == T)
test <- ds %>% filter(isTrain == F)

test$SalePrice <- NULL
train$isTrain <- NULL
test$isTrain <- NULL

sum(is.na(train))
sum(is.na(test))








##########################################################


# 
# ## now lets do some analysis, so far something stupid
# ## try regression, random forests and gradient boosting
# 
# lr.model<-train(SalePrice ~ LotArea+BedroomAbvGr,
#                 data=train_sh,
#                 method = "lm"
# )
# summary(lr.model)
# 
# ggplot(train_sh, aes(x=LotArea, y=SalePrice)) +
#   geom_point(aes(color=as.factor(BedroomAbvGr)))+
#   scale_color_brewer(name="Number of bedrooms", palette = "Dark2")+
#   geom_smooth(method='lm')+
#   scale_x_log10()
# 
# 
# preProc<-preProcess(train_sh[,5:6], method = c("scale"))
# train_pp<-predict(preProc,train_sh[,5:6])
# head(train_sh)
# 
# train_new<-select(train_sh,1:4)
# train_new<-bind_cols(train_new,train_pp)
# head(train_new)
# 
# lr.model.bd<-train(SalePrice ~ LotArea+BedroomAbvGr+MoSold+YrSold,
#                 data=train_new,
#                 method = "lm"
# )
# summary(lr.model.bd)
# res<-residuals(lr.model.bd)
# predicted<-predict(lr.model.bd)
# plot(train_sh$SalePrice,predicted)
# plot(train_sh$SalePrice,res)
