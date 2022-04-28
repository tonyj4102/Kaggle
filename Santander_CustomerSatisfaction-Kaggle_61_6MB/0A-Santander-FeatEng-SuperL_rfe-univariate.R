

#read in data
setwd("~/Dropbox/2b-Projects/Santander_CustomerSatisfaction-Kaggle")

train=read.csv("train.csv")
test=read.csv("santander_test.csv")


#### New feature for 0 count per line
count0 <- function(x) {
  return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)

### make y
y=train$TARGET
y=factor(y)
str(y)
train$TARGET<-NULL

#### feature selection
colmkeep<-c(rfWithFilter$optVariables)
train <- train[,colmkeep]
str(train)


##### Removing IDs
train$ID <- NULL
test.id <- test$ID
test$ID <- NULL

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}


##### Removing identical features
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

str(train$TARGET)
# take only 0.13
set.seed(113)  #15071
spl = sample(nrow(train), 0.10*nrow(train))
Subtrain = train[spl,]


#transform variables

train$var38 <- log(train$var38)
test$var38 <- log(test$var38)

train$saldo_medio_var5_hace3<-log(abs(train$saldo_medio_var5_hace3))
test$saldo_medio_var5_hace3<-log(abs(test$saldo_medio_var5_hace3))

#subset rows by K-means

#K-means

km<-kmeans(STrain10k,5,100)
#review output
km
km$size

#create set of number of clusters from 1 to 15
wss<-numeric(8)

#find wss (within-cluster sum of squares) for each kmeans operation for i clusters
for (i in 1:8) wss [i] <-sum(kmeans(STrain10k, centers=i)$withinss)

#plot 
plot(1:8, wss, type="b", xlab="No. of clusters", ylab="Within groups sum of squares")

# Kmeans for Train and test

km<-kmeans(Strain,9,100)
train$cluster <-km$cluster

km<-kmeans(Stest,9,100)
test$cluster <-km$cluster

#Subset by cluster

# using subset function
newdata <- subset(mydata, age >= 20 | age < 10,
                  select=c(ID, Weight)) 


#set feature names

feature.names <- setdiff(names(train), toRemove)

train <- train[, feature.names]
test <- test[, feature.names]


### 

# XGBoost and data prep
library(xgboost)
library(Matrix)

#train <- sparse.model.matrix(train.y ~ ., data = train)

dtrain <- xgb.DMatrix(data=as.matrix(train), label=as.matrix(y))
watchlist <- list(train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = "auc",
                eta                 = 0.0202048,
                max_depth           = 5,
                subsample           = 0.6815,
                colsample_bytree    = 0.701
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 560, 
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)
# nrounds             = 560, 



#superlearner

# @@@@@@@@@@@@@@@@   Model:   Ensemble - SUPERLEARNER   @@@@@@@@@@@@@@@@
[1] "SL.bayesglm"         "SL.caret"            "SL.caret.rpart"      "SL.cforest"         
[5] "SL.earth"            "SL.gam"              "SL.gbm"              "SL.glm"             
[9] "SL.glm.interaction"  "SL.glmnet"           "SL.ipredbagg"        "SL.knn"             
[13] "SL.leekasso"         "SL.loess"            "SL.logreg"           "SL.mean"            
[17] "SL.nnet"             "SL.nnls"             "SL.polymars"         "SL.randomForest"    
[21] "SL.ridge"            "SL.rpart"            "SL.rpartPrune"       "SL.step"            
[25] "SL.step.forward"     "SL.step.interaction" "SL.stepAIC"          "SL.svm"             
[29] "SL.template"        

install.packages("gbm")


install.packages("devtools")
library("devtools")
install_github("ecpolley/SuperLearner")
install_github("ledell/subsemble")
install.packages("h2o")
install_github("h2oai/h2o/R/ensemble/h2oEnsemble-package")

#Set up the ensemble
library(SuperLearner)

str(y)

SL.library <- c("SL.knn",
                "SL.randomForest",
                "SL.nnet"
                #"SL.ridge",
                #"SL.glmnet",
                #"SL.gbm"
)

method <- "method.NNLS"
family <- "binomial"

SSTrainY<-(SSTrain$TARGET)  #Target vector Y
typeof(SSTrainY)
SSTrain$TARGET<-NULL
SSTestY<-(SSTest$TARGET)
SSTest$TARGET<-NULL

#the outcome Y must be a numeric vector
y<-c(y)
is.vector(y) 

#How to train & test
fitSL2 <- SuperLearner(y, train,newX = test,
                       family = family,
                       SL.library = SL.library,
                       method = method)



pred=predict(fitSL2,test)


#Submission script 

pred <-predict(clf,as.matrix(test))

submission54 <- data.frame(ID=test.id, TARGET=pred)
cat("saving the submission file\n")
write.csv(submission54, "submission#54.csv", row.names = F)

#55 remove id const +0 col, superlearner "SL.knn",
"SL.randomForest",
"SL.nnet"
# 54 log x2 remove id cont,+0 feature  0.715120
# 53 filter fe only = params  0.347174
# 52 remove id, cont,+0 +filter FE  Kaggle-AUC #0.649299 



#feature selection

library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)


subsets <- c(1:5, 10, 50, 100, 300)


#helper function
rfRFE <-  list(summary = defaultSummary,
               fit = function(x, y, first, last, ...){
                 library(randomForest)
                 randomForest(x, y, do.trace=TRUE, importance = first, ...)
               },
               pred = function(object, x)  predict(object, x),
               rank = function(object, x, y) {
                 vimp <- varImp(object)
                 vimp <- vimp[order(vimp$Overall,decreasing = TRUE),,drop = FALSE]
                 vimp$var <- rownames(vimp)
                 vimp
               },
               selectSize = pickSizeBest,
               selectVar = pickVars)

ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv", #10-fold cross-validation
                   repeats = 5,
                   verbose = FALSE)

ctrl$functions <- rfRFE
ctrl$returnResamp <- "all"

set.seed(10)

start.time <- Sys.time()
rfProfile <- rfe(Subtrain, y, sizes = subsets, rfeControl = ctrl)
end.time <- Sys.time()
time.taken <- end.time - start.time


rfProfile

#plot(ldaProfile, type = c("o", "g"))

#postResample(predict(ldaProfile, test), testClass)

#lmProfile_S


time.taken

#Feature Selection using Univariate Filters

start.time <- Sys.time()

filterCtrl <- sbfControl(functions = rfSBF,
                         method = "repeatedcv", repeats = 5)
set.seed(10)
rfWithFilter <- sbf(Subtrain, y, sbfControl = filterCtrl)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


str(rfWithFilter)



##


var15
.. .. ..$ imp_op_var39_comer_ult1
.. .. ..$ imp_op_var40_comer_ult1
.. .. ..$ imp_op_var40_efect_ult1
.. .. ..$ imp_op_var40_efect_ult3,
.. .. ..$ imp_op_var40_ult1,
.. .. ..$ imp_op_var41_efect_ult1,
.. .. ..$ imp_op_var41_efect_ult3,
.. .. ..$ imp_op_var41_ult1,
.. .. ..$ imp_op_var39_efect_ult1,
.. .. ..$ imp_op_var39_efect_ult3,
.. .. ..$ imp_op_var39_ult1,
.. .. ..$ ind_var1,
.. .. ..$ ind_var5_0,
.. .. ..$ ind_var5,
.. .. ..$ ind_var8_0,
.. .. ..$ ind_var8,
.. .. ..$ ind_var12_0,
.. .. ..$ ind_var12,
.. .. ..$ ind_var13_0,
.. .. ..$ ind_var13_corto_0,
.. .. ..$ ind_var13_corto,
.. .. ..$ ind_var13,
.. .. ..$ ind_var24_0,
.. .. ..$ ind_var24,
.. .. ..$ ind_var25_cte,
.. .. ..$ ind_var26_0                  : num 0
.. .. ..$ ind_var26_cte                : num 0
.. .. ..$ ind_var26                    : num 0
.. .. ..$ ind_var25_0                  : num 0
.. .. ..$ ind_var25                    : num 0
.. .. ..$ ind_var30                    : num 0
.. .. ..$ ind_var39_0                  : num 0
.. .. ..$ ind_var40                    : num 0
.. .. ..$ ind_var41_0                  : num 0
.. .. ..$ ind_var39                    : num 0
.. .. ..$ num_var1                     : num 0
.. .. ..$ num_var4                     : num 0
.. .. ..$ num_var5_0                   : num 0
.. .. ..$ num_var5                     : num 0
.. .. ..$ num_var8_0                   : num 0
.. .. ..$ num_var8                     : num 0
.. .. ..$ num_var12_0                  : num 0
.. .. ..$ num_var12                    : num 0
.. .. ..$ num_var13_0                  : num 0
.. .. ..$ num_var13_corto_0            : num 0
.. .. ..$ num_var13_corto              : num 0
.. .. ..$ num_var13                    : num 0
.. .. ..$ num_var24_0                  : num 0
.. .. ..$ num_var24                    : num 0
.. .. ..$ num_var26_0                  : num 0
.. .. ..$ num_var26                    : num 0
.. .. ..$ num_var25_0                  : num 0
.. .. ..$ num_var25                    : num 0
.. .. ..$ num_op_var40_ult1            : num 0
.. .. ..$ num_op_var40_ult3            : num 0
.. .. ..$ num_var30_0                  : num 0
.. .. ..$ num_var30                    : num 0
.. .. ..$ num_var35                    : num 0
.. .. ..$ num_var40                    : num 0
.. .. ..$ num_var41_0                  : num 0
.. .. ..$ num_var39                    : num 0
.. .. ..$ num_var42                    : num 0
.. .. ..$ saldo_var1                   : num 0
.. .. ..$ saldo_var13_corto            : num 0
.. .. ..$ saldo_var13                  : num 0
.. .. ..$ saldo_var26                  : num 0
.. .. ..$ saldo_var25                  : num 0
.. .. ..$ saldo_var30                  : num 0
.. .. ..$ saldo_var40                  : num 0
.. .. ..$ var36                        : num 0
.. .. ..$ imp_aport_var13_hace3        : num 0
.. .. ..$ ind_var43_recib_ult1         : num 0
.. .. ..$ num_aport_var13_hace3        : num 0
.. .. ..$ num_var22_ult1               : num 0
.. .. ..$ num_var22_ult3               : num 0
.. .. ..$ num_med_var22_ult3           : num 0
.. .. ..$ num_meses_var5_ult3          : num 0
.. .. ..$ num_meses_var8_ult3          : num 0
.. .. ..$ num_meses_var12_ult3         : num 0
.. .. ..$ num_meses_var13_corto_ult3   : num 0
.. .. ..$ num_op_var40_comer_ult1      : num 0
.. .. ..$ num_op_var40_efect_ult1      : num 0
.. .. ..$ num_op_var40_efect_ult3      : num 0
.. .. ..$ num_op_var41_efect_ult1      : num 0
.. .. ..$ num_op_var41_efect_ult3      : num 0
.. .. ..$ num_op_var39_efect_ult1      : num 0
.. .. ..$ num_op_var39_efect_ult3      : num 0
.. .. ..$ saldo_medio_var13_corto_hace2: num 0
.. .. ..$ saldo_medio_var13_corto_ult1 : num 0
.. .. ..$ saldo_medio_var13_corto_ult3 : num 0
.. .. ..$ var38                        : num 0
.. .. ..$ n0                           : num 0


str(rfWithFilter$optVariables)
fit 
forest 
xlevels


#######

