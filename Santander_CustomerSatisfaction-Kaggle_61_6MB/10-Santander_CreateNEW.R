# @@@@@@@@@@@@@@@@   Cleaning  @@@@@@@@@@@@@@@@


#Sparse.model.matrix

train <- sparse.model.matrix(TARGET ~ ., data = train)

dtrain <- xgb.DMatrix(data=train, label=train.y)
watchlist <- list(train=dtrain)



##### Removing IDs
TrainB$ID <- NULL
TestB.id <- Stest$ID
TestB$ID <- NULL

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(TrainB)) {
  if (length(unique(TrainB[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    TrainB[[f]] <- NULL
    TestB[[f]] <- NULL
  }
}

##### Removing identical features
features_pair <- combn(names(TrainB), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(TrainB[[f1]] == TrainB[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}


# Convert
feature.train.names <- names(orig.train)[-1]
for (f in feature.train.names) {
  if (class(merged[[f]]) == "numeric") {
    merged[[f]] <- merged[[f]] / max(merged[[f]])
  } else if (class(merged[[f]]) == "integer") {
    u <- unique(merged[[f]])
    if (length(u) == 1) {
      merged[[f]] <- NULL
    } else if (length(u) < 200) {
      merged[[f]] <- factor(merged[[f]])
    }
  }
}

#log transformation
cat("\n## Start log transformation: \n")
for (i in 1:dim(train)[2]){
  train[,i]=log(1+train[,i])
  test[,i]=log(1+test[,i])
}
train[1:5,1:10]
test[1:5,1:10]
cat("\n## End log transformation.\n")
train[is.na(train)] <- -999
test[is.na(test)] <- -999
summary(train[,1:10])
summary(test[,1:10])



## 0.83 with 36 features

import pandas as pd
import numpy as np
import csv
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import ExtraTreesClassifier
from sklearn import ensemble
import xgboost as xgb
from sklearn.cross_validation import train_test_split



## # Feature selection   @@ Python @@
clf = ExtraTreesClassifier(random_state=1729)
selector = clf.fit(X_train, y_train)
# clf.feature_importances_ 
fs = SelectFromModel(selector, prefit=True)

X_train = fs.transform(X_train)
X_test = fs.transform(X_test)
test = fs.transform(test)

print(X_train.shape, X_test.shape, test.shape)


