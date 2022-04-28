# XGBoost

set.seed(1982)

# load in the agaricus dataset
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
dtrain <- xgb.DMatrix(data = agaricus.train$data, label = agaricus.train$label)
dtest <- xgb.DMatrix(data = agaricus.test$data, label = agaricus.test$label)

param <- list(max.depth=2, eta=1, silent=1, objective='binary:logistic')
nround = 4

# training the model for two rounds
bst = xgb.train(params = param, data = dtrain, nrounds = nround, nthread = 2)

# Model accuracy without new features
accuracy.before <- sum((predict(bst, agaricus.test$data) >= 0.5) == agaricus.test$label) / length(agaricus.test$label)

# by default, we predict using all the trees

pred_with_leaf = predict(bst, dtest, predleaf = TRUE)
head(pred_with_leaf)

create.new.tree.features <- function(model, original.features){
  pred_with_leaf <- predict(model, original.features, predleaf = TRUE)
  cols <- list()
  for(i in 1:length(trees)){
    # max is not the real max but it s not important for the purpose of adding features
    leaf.id <- sort(unique(pred_with_leaf[,i]))
    cols[[i]] <- factor(x = pred_with_leaf[,i], level = leaf.id)
  }
  cBind(original.features, sparse.model.matrix( ~ . -1, as.data.frame(cols)))
}

# Convert previous features to one hot encoding
new.features.train <- create.new.tree.features(bst, agaricus.train$data)
new.features.test <- create.new.tree.features(bst, agaricus.test$data)

# learning with new features
new.dtrain <- xgb.DMatrix(data = new.features.train, label = agaricus.train$label)
new.dtest <- xgb.DMatrix(data = new.features.test, label = agaricus.test$label)
watchlist <- list(train = new.dtrain)
bst <- xgb.train(params = param, data = new.dtrain, nrounds = nround, nthread = 2)

# Model accuracy with new features
accuracy.after <- sum((predict(bst, new.dtest) >= 0.5) == agaricus.test$label) / length(agaricus.test$label)

# Here the accuracy was already good and is now perfect.
cat(paste("The accuracy was", accuracy.before, "before adding leaf features and it is now", accuracy.after, "!\n"))

Status API Training Shop Blog About 

# @@@@@@@@@@@@@@@@   make more data  @@@@@@@@@@@@@@@@

more data script


# @@@@@@@@@@@@@@@@   Segmentation/Outliers: K-Means Clustering  @@@@@@@@@@@@@@@@

km<-kmeans(Strain,9,100)
#review output
km
km$cluster

#create set of number of clusters from 1 to 15
wss<-numeric(10)

#find wss (within-cluster sum of squares) for each kmeans operation for i clusters
for (i in 1:8) wss [i] <-sum(kmeans(Strain, centers=i)$withinss)

#plot 
plot(1:10, wss, type="b", xlab="No. of clusters", ylab="Within groups sum of squares")

Strain$cluster <-km$cluster

## @@@@

km<-kmeans(Stest,9,100)
#review output
km
km$cluster

#create set of number of clusters from 1 to 15
wss<-numeric(10)

#find wss (within-cluster sum of squares) for each kmeans operation for i clusters
for (i in 1:10) wss [i] <-sum(kmeans(Stest, centers=i)$withinss)

#plot 
plot(1:10, wss, type="b", xlab="No. of clusters", ylab="Within groups sum of squares")

Stest$cluster <-km$cluster


Xtrain<-Strain
Xtest<-Stest

Xtrain$TARGET<-NULL
Xtrain$ID<-NULL
str(XTrain)
str(XTest)
library(dplyr)

XTrain<-NULL
XTest<-NULL

XTrain<-select(Strain, 
       ID, TARGET)
XTest<-select(Stest, 
               ID)

XTrain$ID<-NULL

km<-kmeans(Strain,9,100)
XTrain$cluster <-km$cluster
km8<-kmeans(Strain,8,100)
XTrain$cluster8 <-km8$cluster
km7<-kmeans(Strain,7,100)
XTrain$cluster7 <-km7$cluster
km6<-kmeans(Strain,6,100)
XTrain$cluster6 <-km6$cluster
km5<-kmeans(Strain,5,100)
XTrain$cluster5 <-km5$cluster
km4<-kmeans(Strain,4,100)
XTrain$cluster4 <-km4$cluster
km3<-kmeans(Strain,3,100)
XTrain$cluster3 <-km3$cluster
km2<-kmeans(Strain,2,100)
XTrain$cluster2 <-km2$cluster
km1<-kmeans(Strain,1,100)
XTrain$cluster1 <-km1$cluster

km<-kmeans(Stest,9,100)
XTest$cluster <-km$cluster
km8<-kmeans(Stest,8,100)
XTest$cluster8 <-km8$cluster
km7<-kmeans(Stest,7,100)
XTest$cluster7 <-km7$cluster
km6<-kmeans(Stest,6,100)
XTest$cluster6 <-km6$cluster
km5<-kmeans(Stest,5,100)
XTest$cluster5 <-km5$cluster
km4<-kmeans(Stest,4,100)
XTest$cluster4 <-km4$cluster
km3<-kmeans(Stest,3,100)
XTest$cluster3 <-km3$cluster
km2<-kmeans(Stest,2,100)
XTest$cluster2 <-km2$cluster
km1<-kmeans(Stest,1,100)
XTest$cluster1 <-km1$cluster





# @@@@@@@@@  Dimensinality reduction @@@@@@@@




# @@@@@@@@@  Principal Component Analysis  @@@@@@@@@@@@



# @@@@@@@@@  Random Forest


# @@@@@@@@@  xgboost



# @@@@@@@@@  Logit LASSO



# @@@@@@@@@  Bestsubset 



# @@@@@@@@@  Subset  










