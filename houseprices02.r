suppressWarnings(suppressMessages(library('tidyverse')))  # loads tons of packages
suppressWarnings(suppressMessages(library('dplyr')))
suppressWarnings(suppressMessages(library('randomForest')))
suppressWarnings(suppressMessages(library('caret')))
suppressWarnings(suppressMessages(library('ggthemes')))
suppressWarnings(suppressMessages(library('mice')))  ## imputations
suppressWarnings(suppressMessages(library('Amelia'))) ## stuff for missing values
library('rmarkdown')
suppressWarnings(suppressMessages(library('gridExtra')))

setwd("~/Dropbox/data science/data projects/kaggle - house prices")



train <- read.csv('train.csv')
test <- read.csv('test.csv')

test$SalePrice<-NA

train$isTrain<-TRUE
test$isTrain<-FALSE

testID<-test$Id

ds<-rbind(train, test)
ds$Id<-NULL

summary(ds)

## DATA CLEANING

## SOME EDA

train_sh<-select(train,Id, BedroomAbvGr, MoSold, YrSold,LotArea, SalePrice)
#train_sh$BedroomAbvGr<-as.factor(train_sh$BedroomAbvGr)
#summary(train_sh)



##analyze predictors
ggplot(train_sh, aes(x = SalePrice)) +
  geom_histogram()


ggplot(train_sh, aes(x = YrSold,
                     y=SalePrice,
                     color=as.factor(BedroomAbvGr)))+
  geom_point(position="dodge")#+  geom_smooth()
##each YrSold will have a distinct color

ggplot(train_sh, aes(x = YrSold,#BedroomAbvGr,
                     y=SalePrice))+
  geom_bar(aes(fill=as.factor(BedroomAbvGr)),
           stat="identity",
           position="dodge")+
  scale_fill_brewer(name="Number of bedrooms", palette = "Dark2")+
  labs(title="Bar plot Sale Price vs Year Sold by number of Bedrooms", x="Year Sold",
       y="Sale Price")

##boxplot
p1<-ggplot(train_sh, aes(x = as.factor(BedroomAbvGr), y=SalePrice))+
   geom_boxplot()+
  labs(title="Sale Price vs Bedrooms", x="Bedrooms",
       y="Sale Price")
  # scale_y_log10()

p2<-ggplot(train_sh, aes(x = as.factor(BedroomAbvGr), y=LotArea))+
  geom_boxplot()+scale_y_log10()+
  labs(title="Lot Area vs Bedrooms", x="Bedrooms",
       y="Lot Area in log ")

# #multiple graphs on one page
# grid.arrange(p1, p2, ncol=1) 


## Density graphs
d1<-ggplot(train_sh, aes(x=LotArea, color=as.factor(BedroomAbvGr))) +
  geom_density()+
  scale_x_log10()+
  scale_color_discrete(name="Bedrooms")+
  ggtitle("Lot area, by bedrooms")

d2<-ggplot(train_sh, aes(x=SalePrice, color=as.factor(BedroomAbvGr))) +
  geom_density()+
  scale_x_log10()+
  scale_color_discrete(name="Bedrooms")+
  ggtitle("Sale price, by bedrooms")

grid.arrange(p1, d1, p2, d2, ncol=2) 



mm<-ggplot(train_sh, aes(x=LotArea, y=SalePrice))
mm+geom_density2d()+
  scale_x_log10()+scale_y_log10()


## Price vs Area
ggplot(train_sh, aes(x=LotArea, y=SalePrice)) +
  geom_point(aes(color=as.factor(BedroomAbvGr)))+
  scale_color_brewer(name="Number of bedrooms", palette = "Dark2")+
  geom_smooth()+
  scale_x_log10()+
  labs(title="Sale Price vs Lot Are by number of Bedrooms", x="Lot Area",
       y="Sale Price")

##facets
ggplot(train_sh, aes(x=LotArea, y=SalePrice)) +
  geom_point(aes(color=as.factor(BedroomAbvGr)))+
  scale_color_brewer(name="Number of bedrooms", palette = "Dark2")+
  geom_smooth()+
  scale_x_log10()+
  facet_wrap(~YrSold)+
  labs(title="Sale Price vs Lot Are by number of Bedrooms depending on the Year", x="Lot Area",
       y="Sale Price")

## now lets do some analysis, so far something stupid
## try regression, random forests and gradient boosting

lr.model<-train(SalePrice ~ LotArea+BedroomAbvGr,
                data=train_sh,
                method = "lm"
)
summary(lr.model)

ggplot(train_sh, aes(x=LotArea, y=SalePrice)) +
  geom_point(aes(color=as.factor(BedroomAbvGr)))+
  scale_color_brewer(name="Number of bedrooms", palette = "Dark2")+
  geom_smooth(method='lm')+
  scale_x_log10()


preProc<-preProcess(train_sh[,5:6], method = c("scale"))
train_pp<-predict(preProc,train_sh[,5:6])
head(train_sh)

train_new<-select(train_sh,1:4)
train_new<-bind_cols(train_new,train_pp)
head(train_new)

lr.model.bd<-train(SalePrice ~ LotArea+BedroomAbvGr+MoSold+YrSold,
                data=train_new,
                method = "lm"
)
summary(lr.model.bd)
res<-residuals(lr.model.bd)
predicted<-predict(lr.model.bd)
plot(train_sh$SalePrice,predicted)
plot(train_sh$SalePrice,res)
