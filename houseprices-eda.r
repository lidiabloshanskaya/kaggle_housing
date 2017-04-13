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

## First let's get the feeling and intuition about the data, 
##the feature significance and correllation with simple EDA

####### CORRELATION MATRIX CALCULATION AND GRAPH #####
cor.matr<-cor(train[,sapply(train,is.numeric)],use="pairwise.complete.obs")
cor.matr<-cor.matr %>%
  round(2)%>%
  melt()


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

##LotFrontage highly correlated with LotArea, LotFrontage looks like more significant variable, 
#but it has many missing values, we''ll stick to LotArea
## Looks like we have the set of important features, let's get a closer look at them 

important.features <- c(#"SaleCondition",
                        "OpenPorchSF", # ~ WoodDeckSF
                        "GarageCars",
                       # "Fireplaces",
                        "FullBath",
                        "GrLivArea", # ~Bedroom
                        "X2ndFlrSF",
                        "TotalBsmtSF",
                       # "Foundation",
                       # "ExterQual",
                        "YearBuilt",  #YearRemodAdd~
                        "OverallQual",
                        "LotArea", 
                        "LotFrontage")


cor.matr.imp<-cor(train[,c(important.features,"SalePrice")],use="pairwise.complete.obs")
cor.matr.imp<-cor.matr.imp %>%
  round(2)%>%
  melt()

##detailed correlation matrix for most important features
ggplot(data=cor.matr.imp, aes(x=Var1,y=Var2, fill=value))+
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation")+
  geom_text(aes(label = round(value, 1))) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_equal()

## Our favourites are "OverallQual", "GarageCars", "FullBath", "LotFrontage", "GrLivArea", "TotalBsmtSF"

### Let's first look at Sale Price and log-transform it to correct skewness
p1<-ggplot(train, aes(x = SalePrice)) +
  geom_histogram()
p2<-ggplot(train, aes(x = log(SalePrice))) +
  geom_histogram()
grid.arrange(p1,p2, ncol=2)

## Further Exploer Features 



## SalePrice vs YearBuilt
pYB<-ggplot(train, aes(x = as.factor(YearBuilt),
                     y=SalePrice))+
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position = "bottom")
pYB1 <- pYB + geom_boxplot()
##factor by OverallQual
pYB2 <- pYB + geom_bar(aes(fill=as.factor(OverallQual)),
           stat="identity",
           position="dodge")+
  theme(legend.position = "bottom")
grid.arrange(pYB1,pYB2, ncol=1)
## newer built houses are more expensive, unless they are really old... and really good


###pairwise distros - need to check log transform
suppressWarnings(suppressMessages(library('car')))
spm(train[, c("SalePrice","OverallQual", "GarageCars", "FullBath", "LotFrontage", "GrLivArea", "TotalBsmtSF")])



###################################
#####      MISSING VALUES       #####
train$isTrain<-TRUE
test$isTrain<-FALSE
test$SalePrice<-NA
testID<-test$Id
ds<-rbind(train, test)


ratio.is.na<-function(dataset){
  col.na<-colMeans(is.na(dataset))
 col.na<-col.na[col.na>0]*100
 col.na
}

ratio.is.na(ds)
## right away exclude the features with missing values >15%
##but will keep "LotFrontage", since it seems to have high correlation with SalePrice

drop.fea.15 <-c("Alley", "MiscFeature", "Fence", "FireplaceQu","PoolQC")
ds <- ds[!names(ds) %in% drop.fea.15]
ratio.is.na(ds)

## Impute the "LotFrontage" as a linear regression from LotArea and GarageArea

ds.LotFrontage <- ds %>% select(LotFrontage, LotArea, GarageArea)
ds.LotFrontage <- ds.LotFrontage[complete.cases(ds.LotFrontage),]

na.LotFrontage <- data.frame("LotArea" = ds$LotArea[is.na(ds$LotFrontage)],
                               "GarageArea" = ds$GarageArea[is.na(ds$LotFrontage)] )

lm.LotFrontage <- lm(LotFrontage ~ LotArea + GarageArea, data=ds.LotFrontage)
ds$LotFrontage[is.na(ds$LotFrontage)] <- predict(lm.LotFrontage, newdata=na.LotFrontage)

sum(is.na(ds$LotFrontage)) ##just chekin'

## Also will drop "GarageArea" now which seems to be twin to GarageCars
## and "X1stFlrSF" looks like twin of TotalBsmtSF
## GarageYrBlt is heavily related to YearBuilt 

ds <- ds[!names(ds) %in% c("GarageArea","X1stFlrSF", "GarageYrBlt", "BsmtHalfBath")]

## Let's work further with missing features

## features that are character and are missing
colnames(ds[,sapply(ds,is.character) & colSums(is.na(ds))>0])


ds$BsmtQual[is.na(ds$BsmtQual)] <- "None"
ds$BsmtQual <- as.factor(ds$BsmtQual)
ds$BsmtExposure[is.na(ds$BsmtExposure)] <- "NONE"
ds$BsmtExposure <- as.factor(ds$BsmtExposure)
ds$BsmtFullBath[is.na(ds$BsmtFullBath)] <- 0
ds$BsmtFullBath <- as.factor(ds$BsmtFullBath)
ds$GarageCars[is.na(ds$GarageCars)]<-0
ds$TotalBsmtSF[is.na(ds$TotalBsmtSF)] <- 0

#colSums(is.na(ds)>0)

##what's with kitchen and utilities
sum(is.na(ds$KitchenQual))
ds$OverallCond[is.na(ds$KitchenQual)] ## 3 - Fair
ds$KitchenQual[is.na(ds$KitchenQual)] <- "Fair"
ds$KitchenQual <- as.factor(ds$KitchenQual)

sum(is.na(ds$Utilities))
ds$Neighborhood[is.na(ds$Utilities)] ##"IDOTRR"  "Gilbert"
# ds$Utilities[ds$Neighborhood=="IDOTRR"] ##AllPub
# ds$Utilities[ds$Neighborhood=="Gilbert"]
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
ds$BsmtFinType1[is.na(ds$BsmtFinType1)] <- "Unf"
ds$BsmtFinType2[is.na(ds$BsmtFinType2)] <- "Unf"
ds$MasVnrType[is.na(ds$MasVnrType)] <- "NONE"
ds$MasVnrArea[is.na(ds$MasVnrArea)] <- 0

ds$BsmtQual[is.na(ds$BsmtQual)] <- "TA"
ds$BsmtCond[is.na(ds$BsmtCond)] <- "TA"
ds$BsmtFinSF1[is.na(ds$BsmtFinSF1)] <- 0
ds$BsmtFinSF2[is.na(ds$BsmtFinSF2)] <- 0
ds$GarageQual[is.na(ds$GarageQual)] <- "NONE"
ds$GarageCond[is.na(ds$GarageCond)] <- "NONE"
ds$GarageType[is.na(ds$GarageType)] <- "NONE"
ds$GarageFinish[is.na(ds$GarageFinish)] <- "NONE"
ds$BsmtUnfSF[is.na(ds$BsmtUnfSF)] <- 0


ratio.is.na(ds) ##checkin' all's good



train <- ds %>% filter(isTrain == T)
test <- ds %>% filter(isTrain == F)

test$SalePrice <- NULL
train$isTrain <- NULL
test$isTrain <- NULL


#########      OUTLIERS      #########


##Outliers in data can distort predictions and affect the accuracy, 
##if we don’t detect and handle them appropriately especially in regression models.
## * Extreme Value Analysis - univariate,  scatterplots, histograms and box and whisker plots
## * Proximity Methods - clustering, k-means
## * Projection Methods - PCA etc.
## * Multivariate - Cook's distance http://r-statistics.co/Outlier-Treatment-With-R.html

## ok, let's go back to my scatterplots and boxplots and kick those outliers
## SalePrice vs OverallQual - discrete
pQual<-ggplot(train, aes(x = as.factor(OverallQual), y=SalePrice))+
  geom_boxplot()
## SalePrice vs GrLivArea - contininuous
pLivAr<-ggplot(train, aes(x = GrLivArea,y=SalePrice)) +
  geom_point()
## SalePrice vs LotArea - contininuous
pLotAr<-ggplot(train, aes(x = LotArea, y=SalePrice))+
  geom_point()
pBsmSF<-ggplot(train, aes(x = TotalBsmtSF,y=SalePrice))+
  geom_point()
grid.arrange(pQual,pLivAr, pLotAr, pBsmSF, ncol=2)

## we already can see the outliers
outliers.GrLivArea<-train$Id[train$GrLivArea > 4000 & train$SalePrice < 200000 ]##those two on the bottom
outliers.GrLivArea
#[1]  524 1299
train$TotalBsmtSF[train$Id==1299] # also takes care of that Basement weirdo
## removing those two rows:
train<-train[-outliers.GrLivArea,]

## checking other stuff:
# suppressWarnings(suppressMessages(library('outliers')))
# set.seed(42)
# outlier(train$SalePrice)




## but i also want to try Cook's distance
## * Multivariate - Cook's distance http://r-statistics.co/Outlier-Treatment-With-R.html
#scaled.SalePrice <- scale(train$SalePrice) ## standardized data
lmod <- lm(SalePrice ~ ., data=train)
cooksd <- cooks.distance(lmod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
#text(x=1:length(cooksd)+1, y=cooksd, 
 #    labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red") 
car::outlierTest(lmod)




## LotArea also looks fishy, but it doesn't look like outliers, 
## just few observations with large LotArea. On top of that the 
## SalePrice doesn't depend that heavily on LotArea, so will let it be
## And now for....

