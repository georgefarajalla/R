library(data.table)
library(ggplot2)
library(caret)
library(dplyr)
library(corrplot)
library(randomForest)
library(mltools)


path <- "C:/Users/PC/Desktop/IE/Machine Learning 2/Assignments/Assignment 1/Data"

# original test and train data sets
original_test <- as.data.frame(fread(file.path(path, "test.csv")))
dim(original_test)
original_train <- as.data.frame(fread(file.path(path, "train.csv")))
dim(original_train)

original_test$SalePrice <- NA

# add sale price column to be able to combine both train and test into one dataset
original_dataset <- rbind(original_train, original_test)
dim(original_dataset)
str(original_dataset)

labels <- original_dataset$Id # stored for later use
original_dataset$Id <- NULL



# to obtain the number of unique values for each variable
sapply(original_dataset, function(x){length(unique(x))})

# precentage of missing values for each variable
sapply(original_dataset, function(x){100*sum(is.na(x))/length(x)})

sapply(original_dataset, function(x){sum(is.na(x))})

# copy the original dataset
dataset <- as.data.frame(original_dataset)



dataset$GarageYrBlt[is.na(dataset$GarageYrBlt)] <- dataset$YearBuilt[is.na(dataset$GarageYrBlt)]

# this vector contains the names of the columns whose NAs are to be filled by 'none':

fill_none <- c('Alley', 'MasVnrType', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2',
               'FireplaceQu', 'GarageType', 'GarageFinish', 'GarageQual', 'GarageCond', 'PoolQC', 'MiscFeature',
               'Fence')


## This for loop fills the above columns with 'none':

for (i in 1:ncol(dataset[fill_none])){
  
  for(j in 1:nrow(dataset[fill_none])){
    
    if(is.na(dataset[fill_none][j, i])) {dataset[fill_none][j, i] = 'None'
    }
    
  }
  
}


# this vector contains the names of the columns whose NAs will be replaced by the mode:

categorical <- c('BsmtFullBath', 'BsmtHalfBath','HalfBath', 'GarageCars', 'MSZoning', 'Utilities', 
                 'Exterior1st','Exterior2nd', 'Electrical', 'KitchenQual', 'Functional', 'SaleType')


## and here we replace those values by the mode

for(i in 1:ncol(dataset[categorical])){
  
  for(j in 1:nrow(dataset[categorical])){
    
    if(is.na(dataset[categorical][j,i])){
      
      dataset[categorical][j,i] <- names(sort(table(dataset[,categorical][,i]), decreasing = T))[1]
        #names(sort(table(dataset[,categorical][,i]), decreasing = T))[1]
      print(dataset[categorical][j,i])
      
    }
    
  }
  
}

dataset$BsmtFinSF1[is.na(dataset$BsmtFinSF1)] <- 0
dataset$BsmtFinSF2[is.na(dataset$BsmtFinSF2)] <- 0
dataset$BsmtUnfSF[is.na(dataset$BsmtUnfSF)] <- 0
dataset$TotalBsmtSF[is.na(dataset$TotalBsmtSF)] <- 0
dataset$BsmtFullBath[is.na(dataset$BsmtFullBath)] <- 0
dataset$BsmtHalfBath[is.na(dataset$BsmtHalfBath)] <- 0

class(dataset)


# imputing the numerical variables
dataset$LotFrontage[is.na(dataset$LotFrontage)] <- as.integer(round(median(dataset$LotFrontage, na.rm = T)))
dataset$GarageArea[is.na(dataset$GarageArea)] <- as.integer(round(median(dataset$GarageArea, na.rm = T)))
dataset$MasVnrArea[is.na(dataset$MasVnrArea)] <- as.integer(round(median(dataset$MasVnrArea, na.rm = T)))




dataset$Remodeled <- ifelse(dataset$YearBuilt==dataset$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
dataset$HouseAge <- as.numeric(dataset$YrSold)-dataset$YearRemodAdd
dataset$IsNew <- ifelse(dataset$YrSold==dataset$YearBuilt, 1, 0)
dataset$HasBsmt <- ifelse(dataset$TotalBsmtSF==0, 0, 1)  #0= No Basement, 1= Has a Basement
dataset$HasSecFloor <- ifelse(dataset$`2ndFlrSF`==0, 0, 1)  #0= No Second Floor, 1= Has a Second Floor


dataset$RtGrLot <-  ifelse(dataset$LotArea==0, 0, round(dataset$GrLivArea/dataset$LotArea,3));
dataset$RtBedGr <-  ifelse(dataset$GrLivArea==0, 0, round(dataset$Bedroom/dataset$GrLivArea,3));
dataset$RtFullBathGr <-  ifelse(dataset$GrLivArea==0, 0,round(dataset$FullBath/dataset$GrLivArea,3));
dataset$RtFullBathBed <-  ifelse(dataset$FullBath==0, 0,round(dataset$Bedroom/dataset$FullBath,3));
dataset$RtHalfBathBed <-  ifelse(dataset$HalfBath==0, 0,round(dataset$Bedroom/dataset$HalfBath,3));
dataset$TotBathGr <-  dataset$FullBath + dataset$HalfBath;
dataset$TotFullBath <-  as.numeric(dataset$FullBath) + as.numeric(dataset$BsmtFullBath);
dataset$TotHalfBath <-  as.numeric(dataset$HalfBath) + as.numeric(dataset$BsmtHalfBath);
dataset$TotSqr <-  dataset$`1stFlrSF` + dataset$`2ndFlrSF` + dataset$TotalBsmtSF;
dataset$RtGrTotSqr <-  ifelse(dataset$TotSqr==0, 0,round(dataset$GrLivArea/dataset$TotSqr,3));
dataset$Utilities <- NULL
dataset$TotalSqFeet <- dataset$GrLivArea + dataset$TotalBsmtSF


dataset$GroupNeigh <- ifelse(dataset$Neighborhood %in% c("MeadowV","IDOTRR","BrDale","OldTown","Edwards","BrkSide", "Sawyer", "Blueste", "SWISU", "NAmes", "NPkVill"),
                             1,
                             ifelse(dataset$Neighborhood %in% c("Mitchel", "SawyerW", "Gilbert", "NWAmes", "Blmngtn", "CollgCr"),
                                    2,
                                    ifelse(dataset$Neighborhood %in% c("ClearCr", "Crawfor", "Veenker", "Somerst", "Timber"),
                                           3,4)))

dataset <- dataset[-c(524, 1299),]

cols_factor <- c('BsmtFullBath', 'BsmtHalfBath',
                 'FullBath', 'HalfBath', 'BedroomAbvGr', 'KitchenAbvGr', 'TotRmsAbvGrd', 'Fireplaces',
                 'GarageCars', 'MSSubClass', 'MSZoning', 'Utilities', 'Exterior1st',
                 'Exterior2nd', 'Electrical', 'KitchenQual', 'Functional', 'SaleType',
                 'Alley', 'MasVnrType', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1',
                 'BsmtFinType2', 'GarageType', 'GarageFinish', 'GarageQual', 'GarageCond', 'PoolQC',
                 'MiscFeature', 'Fence', 'YrSold',
                 'MoSold')



# transform columns to factor:
dataset[,cols_factor] <- lapply(dataset[,cols_factor],as.factor)
sapply(dataset, class)


# new dataset where the missing values have been imputed
sapply(dataset, function(x){sum(is.na(x))}) # no missing values

# Standardization and Box-Cox transformation:

sale <- dataset$SalePrice
dataset$SalePrice <- NULL


pr_std <- preProcess(dataset, method = c("center", "scale"))
dataset <- predict(pr_std, dataset)

dataset$SalePrice <- sale

num_vars <- which(sapply(dataset, is.numeric))
num_cols <- dataset[,num_vars]

pre_bc <- preProcess(dataset, method = "BoxCox")
dataset <- predict(pre_bc, dataset)


## correlations between numeric variables
num_cor <- cor(num_cols, use="pairwise.complete.obs")
cor_sorted <- as.matrix(sort(num_cor[,'SalePrice'], decreasing = TRUE))
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.05)))
num_cor <- num_cor[CorHigh, CorHigh]
cor_sorted <- as.matrix(sort(num_cor[,'SalePrice'], decreasing = TRUE))



num_names <- names(which(apply(cor_sorted, 1, function(x) abs(x)<0.5)))
temp <- dataset
temp <- temp[, !(colnames(temp) %in% num_names)]
dataset <- temp

# this vector contains the names of the columns to be transformed to factor:


dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 
              'TotalRmsAbvGrd', 'BsmtFinSF1')

dataset <- dataset[,!(names(dataset) %in% dropVars)]



dataset=dataset %>% mutate_if(is.character, as.factor)
temp <- dataset
temp$SalePrice <- NULL

set.seed(2018)
quick_RF <- randomForest(x =  temp[1:1458,], y = dataset$SalePrice[1:1458], ntree = 100, importance = TRUE)
imp_RF <- importance(quick_RF) ## increase in MSE
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]


ggplot(imp_DF[1:20,], aes(x = reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + 
  labs(y = '% increase MSE if variable is randomly permuted') + coord_flip() 

imp_DF$Variables <- NULL
n <- names(which(apply(imp_DF, 1, function(x) x<1)))
temp <- dataset
temp <- temp[, !(colnames(temp) %in% n)]
dataset <- temp

qqnorm(dataset$SalePrice)
qqline(dataset$SalePrice)

dataset$SalePrice <- log(dataset$SalePrice)

qqnorm(dataset$SalePrice)
qqline(dataset$SalePrice)

train1 <- dataset[!is.na(dataset$SalePrice),]
test1 <- dataset[is.na(dataset$SalePrice),]

lambdas <- 10^seq(-3, 0, by = .05)

set.seed(121)
train_control_config <- trainControl(method = "repeatedcv", 
                                     number = 5, 
                                     repeats = 1,
                                     returnResamp = "all")

# Train the model using all the data
final.model <- train(SalePrice ~ ., data = train1, 
                     method = "glmnet", 
                     metric = "RMSE",
                     trControl=train_control_config,
                     tuneGrid = expand.grid(alpha = 0, lambda = lambdas))

# Predict the prices for the test data (i.e., we use the exp function to revert the log transformation that we applied to the target variable)
final.pred <- as.numeric(exp(exp(predict(final.model, test1))))
final.pred[is.na(final.pred)]
hist(final.pred, main="Histogram of Predictions", xlab = "Predictions")

lasso_submission <- data.frame(Id = original_test$Id, SalePrice= (final.pred))
colnames(lasso_submission) <-c("Id", "SalePrice")
write.csv(lasso_submission, file = "submission.csv", row.names = FALSE) 


