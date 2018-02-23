#final_pred413
#Author:Nitin Gaonkar

library("jpeg")
library("MASS")
library("Party")
library("randomForest")
library("xgbosst")
library("Metrics")
library("lars")
library("psych")
library("mice")
library("dplyr")
library("plyr")
library("Amelia")

#load the data
setwd("/Users/ngaonkar/Desktop/Ames_housing_data")
train=read.csv("train.csv", stringsAsFactors=T) #read in the raw data
test=read.csv("test.csv", stringsAsFactors=T) #read in the raw data

str(train) # see the data structure


#EDA & data imputation

numeric<-sapply(train, is.numeric)
cormatrix<-cor(x=train[, numeric],use='complete')
par(mfrow=c(1,1))
corrplot(cormatrix,  method="color",mar=c(1,1,1,1),rect.col="black", tl.col="black")


a=rep(0,length(ncol(train)))
for(i in 1:ncol(train)){a[i]=sum(is.na(train[,i]))}
a
train=train[, c(-4,-7,-58, -73,-74,-75)]


test=read.csv("test.csv", stringsAsFactors=T) #read in the raw data
str(test) # see the data structure
test$SalePrice=as.integer(rep("",length(test$Id)))
test=test[, c(-4,-7,-58, -73,-74,-75)]

final=rbind(train,test)
a=rep(0,length(ncol(total)))
for(i in 1:ncol(total)){a[i]=sum(is.na(total[,i]))}
a

describe(final) #get an initial thought about outliers..
hist(total$SalePrice)


#Visualize missing values
missmap(final, main="Missing Map")

imputation=mice(final[,c(-75)],m=3,method="cart") #excludes price
finalset=complete(imputation)
missmap(finalset)


#Models, below are the various models submitted:
fit <- lm(log(finalset$SalePrice) ~ sqrt(finalset$MasVnrArea) + log(finalset$LotArea) 
          + sqrt(finalset$TotalBsmtSF) + sqrt(finalset$X1stFlrSF) + log(finalset$GrLivArea) + 
            sqrt(finalset$GarageArea) + (finalset$YrSold) + finalset$MoSold + sqrt(finalset$OverallQual) + 
            finalset$YearBuilt + finalset$YearRemodAdd + finalset$GarageCars + finalset$TotRmsAbvGrd   , data=finalset[1:1460,])

fit <- lm(log(SalePrice) ~ sqrt(MasVnrArea) + log(LotArea) + sqrt(TotalBsmtSF) +
            sqrt(X1stFlrSF) + log(GrLivArea) + sqrt(GarageArea) + YrSold + MoSold + sqrt(OverallQual) + 
            YearBuilt + YearRemodAdd + GarageCars + TotRmsAbvGrd + Neighborhood + Fireplaces , data=finalset[1:1460,])


fit <- lm (log(SalePrice) ~ MSSubClass + MSZoning + LotArea + Street + 
               +     LandContour + Utilities + LotConfig + LandSlope + Neighborhood + 
               +     Condition1 + Condition2 + BldgType + OverallQual + OverallCond + 
               +     YearBuilt + YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + 
               +     MasVnrType + MasVnrArea + ExterQual + BsmtQual + BsmtCond + 
               +     BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + 
               +     X1stFlrSF + X2ndFlrSF + FullBath + BedroomAbvGr + KitchenAbvGr + 
               +     KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + GarageCars + 
               +     GarageArea + GarageQual + ScreenPorch + PoolArea + SaleCondition, 
             data = finalset[1:1460, ])

#model with best rmse
fit <- lm (log(totalimp$SalePrice[1:1460]) ~ MSSubClass + MSZoning + LotArea + Street +
       LandContour + Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + 
       Condition2 + BldgType + OverallQual + OverallCond + YearBuilt + YearRemodAdd + 
         RoofStyle + RoofMatl + Exterior1st + MasVnrType + MasVnrArea + ExterQual + 
         BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + 
         BsmtUnfSF + X1stFlrSF + X2ndFlrSF + FullBath + BedroomAbvGr + KitchenAbvGr + 
         KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + GarageCars + 
         GarageArea + GarageQual + ScreenPorch + PoolArea + SaleCondition, 
         data = finalset[1:1460, ])

mypred=predict(fit,finalset[1461:2919,])
write.csv(mypred,"model1.csv")

################
# Random forest
###############

#Random forest:

Model2 <- randomForest(finalset$SalePrice[1:1460]~., data=finalset[1:1460,], mtry=9, ntree=500)

#predict

forestsubmit <- predict(model2,finalset[1461:2919,])

write.csv(forestsubmit,"randomforest.csv")


##############
#XGBOOST
###############

#load the data
setwd("/Users/ngaonkar/Desktop/Ames_housing_data")
training=read.csv("train.csv", stringsAsFactors=T) #read in the raw data
testing=read.csv("test.csv", stringsAsFactors=T) #read in the raw data

trainD<- as.matrix(training, rownames.force=NA) 
testD<- as.matrix(testing, rownames.force=NA)

train2 <- as(trainD, "sparseMatrix") 
test2 <- as(testD, "sparseMatrix")

vars <- c(1:40, 41:81)

sparse<- xgb.cv(data = trainD, nrounds = 600, min_child_weight = 0, max_depth = 10, eta = 0.02, subsample = .7, 
                colsample_bytree = .7, booster = "gbtree", eval_metric = "rmse", verbose = TRUE, 
                print_every_n = 50, nfold = 4,
                nthread = 2, objective="reg:linear")

#choose parameters:

parameters <- list(colsample_bytree = .7, subsample = .7, booster = "gbtree", max_depth = 10, eta = 0.02, 
                   eval_metric = "rmse", objective="reg:linear")

#model
Model3 <- xgb.train(params = parameters, data = trainData, nrounds = 600, 
                    watchlist = list(train = trainData), verbose = TRUE, print_every_n = 50, nthread = 2)

#predict:

testData <- xgb.DMatrix(data = test2[,vars])
prediction <- predict(Model3, testData)
test <- as.data.frame(as.matrix(test2)) 
prediction <- as.data.frame(as.matrix(prediction)) 
colnames(prediction) <- "prediction" 
finaloutput <- cbind(test3, prediction)



