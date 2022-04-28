
rm(list = ls())

## @@@@@@@@  Load data
setwd("~/Dropbox/2c-projects/Santander_CustomerSatisfaction-Kaggle")

train=read.csv("train.csv")
test=read.csv("santander_test.csv")


## @@@@@@@@   Profile data before cleaning
summary(train)
str(train)


## @@@@@@@@   Removing IDs
train$ID <- NULL
test.id <- test$ID
test$ID <- NULL

## @@@@@@@@  Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}


## @@@@@@@@   Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}


#remove features

feature.names <- setdiff(names(train), toRemove)

train <- train[, feature.names]
test <- test[, feature.names]


## @@@@@@@@   Profile data after cleaning
summary(train)
str(train)


## @@@@@@@@  Sample the data at 10% 
set.seed(113)  #15071
spl = sample(nrow(train), .1*nrow(train))
Strain = train[spl,]


## @@@@@@@@  Preliminary feature engineering using Clustering
#K-means

#create set of number of clusters from 1 to 12
wss<-numeric(12)

#find wss (within-cluster sum of squares) for each kmeans operation for i clusters
for (i in 1:12) wss [i] <-sum(kmeans(train, centers=i)$withinss)

#plot 
plot(1:12, wss, type="b", xlab="No. of clusters", ylab="Within groups sum of squares")

#k-means
km<-kmeans(train,3,100)

#review output
km
km$size


# Kmeans for Train and test

#km<-kmeans(train,3,100)
train$cluster <-km$cluster

km<-kmeans(test,3,100)
test$cluster <-km$cluster

# convert #cluster var to numeric
train$cluster<-as.numeric(train$cluster)
test$cluster<-as.numeric(test$cluster)


## @@@@@@@@  log and e^x variables (optional)

# from  adding log and e^x
StrainEX<-Strain$TARGET

# New data if N/A's exist

#log Nan exist (MLtrain) @@
MLtrain[is.na(MLtest)] <- 0
MLtest[is.na(MLtest)] <- 0

# if Nan present @@ feature engineered set

FETrain[is.na(FETrain)] <- 0
FETest[is.na(FETest)] <- 0


## @@@@@@@@  normalize data

library(caret)

#normalize train
str(train) 
preprocTrain = preProcess(train)              # $ var15 : int  23 34 23 37 39 23 27 26 45 25 ...
TrainNorm = predict(preprocTrain, train)
str(TrainNorm)                               # $ var15 : num  -0.7882 0.0608 -0.7882 0.2923 0.4467 ...                         


#normalize test
str(test) 
preprocSSTest = preProcess(test)   #$ var15 : int  32 35 23 24 23 43 39 29 53 37 ..
TestSSNorm = predict(preprocSSTest, test)
str(TestSSNorm)        #  $ var15 : num  -0.0881 0.1439 -0.784 -0.7067 -0.784 ...


# add back un-normalized TARGET variable

str(TrainNorm$TARGET)
TrainNorm$TARGET<-train$TARGET
str(TrainNorm$TARGET)

## @@@@@@@@ create training and test sets

# split data set into train & test
set.seed(113)  #15071
spl = sample(nrow(TrainNorm), 0.8*nrow(TrainNorm))
SStrain = TrainNorm[spl,]
SStest = TrainNorm[-spl,]


str(SStrain)
str(SStest)


## @@@@@@@@ XGBoost

# Load Modules
require(xgboost)
## Loading required package: xgboost
require(methods)
## Loading required package: methods
require(data.table)
## Loading required package: data.table
require(magrittr)
## Loading required package: magrittr

y=SStrain$TARGET
y
SStrain$TARGET<-NULL

str(SStrain)   # 60816 obs. of  307 variables
str(y)         # int [1:60816]


# Convert both datasets (training and test) in numeric Matrix format.

#Matrix
trainMatrix <- as.matrix(SStrain)
testMatrix <- as.matrix(SStest)


#data.frame is not a format supported natively by XGBoost.
trainMatrix <- SStrain[,lapply(.SD,as.numeric)] %>% as.matrix
testMatrix <- SStest[,lapply(.SD,as.numeric)] %>% as.matrix


#xgb.DMatrix 
trainMatrix<-xgb.DMatrix(SStrain)
testMatrix<-xgb.DMatrix(SStest)


str(trainMatrix)

# Use the cross validation to evaluate the our error rate

#Log los – minimize
#Eta – learning rate
#Max.depth – tree depth

bst.cv = xgb.cv(data = as.matrix(trainMatrix), 
                label = y, 
                booster = "gbtree", 
                max.depth = 7, 
                eta=0.125, 
                nrounds = 100,
                nthread = 2, 
                objective = "binary:logistic",
                eval_metric = "auc",
                nfold = 10,
                early.stop.round=NULL
                )

numberOfClasses <- max(y) + 1

#option#1

numberOfClasses <- max(y) + 1

param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc", #"mlogloss",
              "num_class" = numberOfClasses)

cv.nround <- 100
cv.nfold <- 2

bst.cv = xgb.cv(param=param, data = as.matrix(trainMatrix), label = y, 
                nfold = cv.nfold, nrounds = cv.nround)


#option#2

numberOfClasses <- max(y) + 1

watchlist <- list(train=trainMatrix)

param <- list(objective = "binary:logistic",
              booster = "gbtree",
              eval_metric = "auc", #"mlogloss"
              eta = 0.0202048,
              max.depth = 6,
              subsample           = 0.6815,
              #stratified = TRUE,                # sample is unbalanced; use stratified sampling
              colsample_bytree    = 0.701,
              verbose             = 1,
              watchlist           = watchlist,
              "num_class" = numberOfClasses)

cv.nround <- 50
cv.nfold <- 3

bst.cv = xgb.cv(param=param, data = as.matrix(trainMatrix), label = y, 
                nfold = cv.nfold, nrounds = cv.nround, early.stop.round=NULL)

str(bst.cv)
nrow(bst.cv)



# Plot xgb.cv (logloss)
plot(log(bst.cv$test.mlogloss.mean),type="l") 

# plot (AUC)
plot((bst.cv$test.auc.mean),type="l")

# plot the AUC for the training and testing samples
library(ggplot2)
library(reshape)

dst<-bst.cv
dst<dst$ID

for (i in 1:nrow(dst)) {
    dst$ID [i]<-i
}
summary(dst)
str(dst)
auc.long <- melt(dst, id = "ID", measure = c("test.auc.mean", "train.auc.mean"))
ggplot(data=auc.long, aes(ID,value,color = variable)) + 
  geom_line() + 
  theme_bw()


# @@ Train model with Optimum # of rounds (i.e. # of trees)

#Thread=no. of cores

# version#1
bst <- xgboost(data = trainMatrix, label = y, booster = "gbtree", max.depth = 6, eta=0.01, 
               nrounds = 563,
               nthread = 2, objective = "binary:logistic",gamma=1, eval_metric = "auc")


# version#2
watchlist <- list(train=trainMatrix)

bst2<-xgboost(data = trainMatrix, label = y, booster = "gbtree", 
                             objective           = "binary:logistic", 
                             eval_metric         = "auc",
                             eta                 = 0.0202050, #0.0202048
                             max_depth           = 6, #6,
                             subsample           = 0.6815,
                             colsample_bytree    = 0.701,
                             nrounds             = 520,
                             verbose             = 1,
                             watchlist           = watchlist,
                             maximize            = FALSE,
                             nthread = 2
             )
#time:  ____ ; 7:50- ; nrounds=34

#predict
preds=predict(bst,testMatrix)
preds2=predict(bst2,testMatrix)


#predict on test data
testMatrix2 <- as.matrix(Stest)
preds=predict(bst,testMatrix2)
preds2=predict(bst2,testMatrix2)

str(preds)
preds[1:10]
str(preds2)
preds2[1:10]


#AUC
#install.packages("AUC")
library(cvAUC)
auc <- AUC(preds, SStest$TARGET)
auc
auc2 <- AUC(preds2, SStest$TARGET)

str(preds2)
SStest$TARGET
auc2

[1] 0.8737374 kaggle n=10 , 0.8, no false 
[1] 0.7440476 n=10 0.5 kaggle
[1] 0.6206897 =500   kaggle
[1] 0.7011494 =420    
[1] 0.6436782 =350
[1] 0.7126437 =300
[1] 0.6321839 =200
[1] 0.6206897 =100   kaggle 0.5 data
[1] 0.7479675 =420
[1] 0.7658537 =415
[1] 0.7642276 =400
[1] 0.4837398  0.13  kaggle script int+log rounds=5
[1] 0.8173664  0.13  Kaggle script int+log data nrounds=5
[1] 0.8182556                         50 rounds
[1] 0.8109487                         47 rounds
[1] 0.8119732                         45 rounds
[1] 0.7795948   0.13 integer+log numer 12 rounds
[1] 0.685364    5 rounds Kaggle script new data
[1] 0.8386474  30 rounds
[1] 0.8260051  10 rounds
[1] 0.8201059  new data 5 folds highest auc param

# Kaggle  nrounds=530
# Kaggle 0.820489 nrounds=30 [1] 0.8403631  0.13 Train; 0.8 split, label = y, booster = "gbtree", 
objective           = "binary:logistic", 
eval_metric         = "auc",
eta                 = 0.0202048,
max_depth           = 5,
subsample           = 0.6815,
colsample_bytree    = 0.701,
nrounds             = 530, 
verbose             = 1,
watchlist           = watchlist,
maximize            = FALSE



# Kaggle [1] 0.8407319    seed=113, 0.13 Train; 0.8 split, no gamma
# [1] 0.8407319    seed=113, 0.13 Train; 0.8 split, gamma=0.005
# [1] 0.8406772   seed=113, 0.13 Train; 0.8 split, below + gamma=0.015
# [1] 0.839234    seed=113, 0.13 Train; 0.8 split, below + gamma=0.025
# [1] 0.8405293   seed=113, 0.13 Train; 0.8 split, label = y, booster = "gbtree", max.depth = 7, eta=0.125, nrounds = 30, nthread = 2, objective = "binary:logistic",eval_metric = "auc", gamma=0.025)

str(Stest$TARGET)

#Accuracy

table(SStest$TARGET, preds2 > 0.5)
str(predAcc)
(14600+0)/(14600+0+1+603)
=TN+TP/(TN+TP+FN+FP)



#Log loss
print(-mean(log(preds2)*SStest$TARGET+log(1-preds2)*(1-SStest$TARGET)))



#Generate trees                          
Trees = xgb.model.dt.tree(dimnames(Strain$data)[[2]], model = bst)
Trees = xgb.model.dt.tree(dimnames(Strain$names)[[2]], model = bst)
                             
#View(Trees)

#Yes->0-1 #(ID# - leaf/feature)
#N/A’s – missing values/robust to missing values
                                       
#View model.Each line is a branch
                                       
model <- xgb.dump(bst, with.stats = T)
model[1:10]
                                       
importance_matrix[1:10]#<-based on importance matrix
                                       
names <- dimnames(SStrain)[[2]] #names <- dimnames(SStrain$data)[[2]]
importance_matrix<-xgb.importance(names,model =bst)
xgb.plot.importance(importance_matrix[1:2])

str(importance_matrix)
#importMatrix <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(importance_matrix, "Imatrix-cluster-best.csv")
                                       
# Plot the tree

n_first_tree = 2
(plot the first two trees)
xgb.plot.tree(feature_name = names,model =bst, n_first_tree = 2)

# Get the feature real names
names <- dimnames(XTrain$data)[[2]]
importance_matrix <- xgb.importance(names, model = bst2)
xgb.plot.importance(importance_matrix[1:100])
xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       
                                       