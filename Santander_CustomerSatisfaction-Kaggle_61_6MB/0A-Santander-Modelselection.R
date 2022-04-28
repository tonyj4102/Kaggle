# Project:          Santander Bank/Kaggle
# Author:           Anthony James

# Goals:          

#1 identify dissatisfied customers early in their relationship
#2 take proactive steps to improve a customer's happiness before it's too late

# ranking metric:   AUC


# @@@@@@@@@@@@@@@@   Data aquisition  @@@@@@@@@@@@@@@@

setwd("~/Dropbox/Projects/Santander_CustomerSatisfaction-Kaggle")

Strain=read.csv("train.csv")
Stest=read.csv("santander_test.csv")


# @@@@@@@@@@@@@@@@   Profiling  @@@@@@@@@@@@@@@@

summary(Strain)
str(Strain)
str(Strain, list.len=ncol(Strain))

#'data.frame':	76020 obs. of  371 variables:

# dependent variables:    Target
# independent variables:  Integers, Numbers

colnames(Strain) 

#anomalized

head(Strain,n=100)
tail(___,n=10)

#Sparse


# @@@@@@@@@@@@@@@@   Exploration: Numeric Variables  @@@@@@@@@@@@@@@@

$ imp_ent_var16_ult1           : num  0 0 0 0 0 0 0 0 0 0 ...    [1] 210000 
$ imp_op_var39_comer_ult1      : num  0 0 0 195 0 0 0 0 0 0 ...[1] 12888.03
$ imp_op_var39_comer_ult3      : num  0 0 0 195 0 0 0 0 0 0 ...[1] 21024.81
$ imp_op_var40_comer_ult1      : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_op_var40_comer_ult3      : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_op_var40_efect_ult1      : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_op_var40_efect_ult3      : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_op_var40_ult1            : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_op_var41_comer_ult1      : num  0 0 0 195 0 0 0 0 0 0 ...
$ imp_op_var41_comer_ult3      : num  0 0 0 195 0 0 0 0 0 0 ...
$ imp_op_var41_efect_ult1      : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_op_var41_efect_ult3      : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_op_var41_ult1            : num  0 0 0 195 0 0 0 0 0 0 ...
$ imp_op_var39_efect_ult1      : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_op_var39_efect_ult3      : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_op_var39_ult1            : num  0 0 0 195 0 0 0 0 0 0 ...
$ saldo_var1                   : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var5                   : num  0 0 3 70.6 0 ...
$ saldo_var6                   : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var8                   : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var12                  : num  0 0 0 0 135003 ...
$ saldo_var13_corto            : num  0 300 0 0 0 0 0 0 0 0 ...
$ saldo_var13_largo            : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var13                  : num  0 300 0 0 0 0 0 0 0 0 ...
$ saldo_var14                  : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var17                  : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var20                  : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var24                  : num  0 0 0 0 135003 ...
$ saldo_var26                  : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var25                  : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var29                  : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var30                  : num  0 300 3 70.6 135003 ...
$ saldo_var31                  : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var32                  : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var33                  : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var37                  : num  0 0 0 35 0 ...
$ saldo_var40                  : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_var42                  : num  0 0 3 70.6 135003 ...
$ saldo_var44                  : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_amort_var18_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_amort_var34_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_aport_var13_1y3    : num  0 -1 0 0 0 0 0 0 0 0 ...
$ delta_imp_aport_var17_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_aport_var33_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_compra_var44_1y3   : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_reemb_var13_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_reemb_var17_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_reemb_var33_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_trasp_var17_in_1y3 : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_trasp_var17_out_1y3: num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_trasp_var33_in_1y3 : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_trasp_var33_out_1y3: num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_imp_venta_var44_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_num_aport_var13_1y3    : num  0 -1 0 0 0 0 0 0 0 0 ...
$ delta_num_aport_var17_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_num_aport_var33_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_num_compra_var44_1y3   : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_num_reemb_var13_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_num_reemb_var17_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_num_reemb_var33_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_num_trasp_var17_in_1y3 : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_num_trasp_var17_out_1y3: num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_num_trasp_var33_in_1y3 : num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_num_trasp_var33_out_1y3: num  0 0 0 0 0 0 0 0 0 0 ...
$ delta_num_venta_var44_1y3    : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_amort_var18_ult1         : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_amort_var34_ult1         : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_aport_var13_hace3        : num  0 300 0 0 0 0 0 0 0 0 ...
$ imp_aport_var13_ult1         : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_aport_var17_hace3        : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_aport_var17_ult1         : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_var7_emit_ult1           : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_var7_recib_ult1          : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_compra_var44_hace3       : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_compra_var44_ult1        : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_reemb_var13_ult1         : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_reemb_var17_hace3        : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_reemb_var17_ult1         : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_var43_emit_ult1          : num  0 0 0 0 135003 ...
$ imp_trans_var37_ult1         : num  0 0 0 0 270003 ...
$ imp_trasp_var17_in_hace3     : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_trasp_var17_in_ult1      : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_trasp_var17_out_ult1     : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_trasp_var33_in_hace3     : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_trasp_var33_in_ult1      : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_venta_var44_hace3        : num  0 0 0 0 0 0 0 0 0 0 ...
$ imp_venta_var44_ult1         : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var5_hace2       : num  0 0 3 186 3 ...
$ saldo_medio_var5_hace3       : num  0 88.89 0.18 0 0.3 ...
$ saldo_medio_var5_ult1        : num  0 0 3 91.6 40501.1 ...
$ saldo_medio_var5_ult3        : num  0 0 2.07 138.84 13501.47 ...
$ saldo_medio_var8_hace2       : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var8_hace3       : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var8_ult1        : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var8_ult3        : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var12_hace2      : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var12_hace3      : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var12_ult1       : num  0 0 0 0 85502 ...
$ saldo_medio_var12_ult3       : num  0 0 0 0 85502 ...
$ saldo_medio_var13_corto_hace2: num  0 300 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var13_corto_hace3: num  0 122 0 0 0 ...
$ saldo_medio_var13_corto_ult1 : num  0 300 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var13_corto_ult3 : num  0 241 0 0 0 ...
$ saldo_medio_var13_largo_hace2: num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var13_largo_hace3: num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var13_largo_ult1 : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var13_largo_ult3 : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var13_medio_hace2: num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var13_medio_ult3 : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var17_hace2      : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var17_hace3      : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var17_ult1       : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var17_ult3       : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var29_hace2      : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var29_hace3      : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var29_ult1       : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var29_ult3       : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var33_hace2      : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var33_hace3      : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var33_ult1       : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var33_ult3       : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var44_hace2      : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var44_hace3      : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var44_ult1       : num  0 0 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var44_ult3       : num  0 0 0 0 0 0 0 0 0 0 ...
$ var38                        : num  39205 49278 67334 64008 117311 ...



# @@@@@@@@@@@@@@@@   Cleaning  @@@@@@@@@@@@@@@@ (Optional)


##### Removing IDs
Strain$ID <- NULL
Stest.id <- Stest$ID
Stest$ID <- NULL

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(Strain)) {
  if (length(unique(Strain[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    Strain[[f]] <- NULL
    Stest[[f]] <- NULL
  }
}

##### Removing identical features
features_pair <- combn(names(Strain), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(Strain[[f1]] == Strain[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}


# @@@@@@@@@@@@@@@@   Preprocessing: Subsetting  @@@@@@@@@@@@@@@@

# Random subet  ~10k

set.seed(15071)
spr = sample(nrow(Strain), 0.13*nrow(Strain))
StrainR10P = Strain[spr,]
StrainR90P = Strain[-spr,]
str(StrainR10P)
str(StrainR90P)


# OPTIONAL- Subset 10,000, and 1,000 rows
STrainALL<-Strain
STrain10k<- Strain[1:10000,]
STrain1k <-Strain[1:1000,]


# @@@@@@@@@@@@@@@@   Modeling-Preprocessing: Train and Test Set  @@@@@@@@@@@@@@@@


set.seed(113)  #15071
spl = sample(nrow(StrainR10P), 0.8*nrow(StrainR10P))
SSTrain = StrainR10P[spl,]
SSTest = StrainR10P[-spl,]



# @@@@@@@@@@@@@@@@   Preprocessing: Normalizing (OPTIONAL)  @@@@@@@@@@@@@@@@
install.packages("caret")
library(caret)

# take out response variable-label must be in [0,1] for logistic regression

SSTrainY<-SSTrain$TARGET
SSTrain$TARGET<=NULL

SSTestY<-SSTest$TARGET
SSTest$TARGET<=NULL

#normalize train split for train
str(SSTrain) #76020 obs. of  371 variables:
preprocTrain = preProcess(SSTrain)              #: int  1 3 4 8 10 13 14 18 20 23 ...
TrainNorm = predict(preprocTrain, SSTrain)
str(TrainNorm)  #$ ID                           #: num  -1.74 -1.73 -1.73 -1.73 -1.73 ...                         

#normalize train split for test
str(SSTest) #76020 obs. of  371 variables:
preprocSSTest = preProcess(SSTest)
TestSSNorm = predict(preprocSSTest, SSTest)
str(TestSSNorm)  # $ ID  

#normalize test set
str(Stest) #76020 obs. of  371 variables:
preprocTest = preProcess(Stest)
TestNorm = predict(preprocTest, Stest)
str(TestNorm)  # $ ID                           : num  -1.73 -1.73 -1.73 -1.73 -1.73 ...

#model variables
TrainNorm
SSTrainY
TestSSNorm
SSTestY
TestNorm

# @@@@@@@@@@@@@@@@   Segmentation/Outliers: K-Means Clustering  @@@@@@@@@@@@@@@@

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



# @@@@@@@@@@@@@@@@   Model: Logistic Regression (glm)  @@@@@@@@@@@@@@@@

SSTest
SSTrain

#modeling
glm.Sant=glm(TARGET~.,data=SSTrain,family=gaussian)
summary(glm.Sant)
glm.predTest=predict(glm.Sant,newdata=SSTest, type="response")   # predict using model
glm.predTest
summary(glm.predTest)
head(glm.predTest)

#accuracy
table(SSTest$TARGET, glm.predTest > 0.18)
=TN+TP/(TN+TP+FN+FP)
(2852+5)/(2852+5+36+107)
[1] 0.9333333 #1k @ 0.5
[1] 0.9523333 #10k @0.2



# @@@@@@@@@@@@@@@@   Model: Classification and Regression Trees (CART)  @@@@@@@@@@@@@@@@


install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

Tree.Sant = rpart(TARGET~.,data=SSTrain, method="class", minbucket=200)
prp(Tree.Sant)
Predict.Sant = predict(Tree.Sant, newdata = SSTest, type = "class")  
summary(Predict.Sant)
table(Predict.Sant)
table(SSTest$TARGET)
      
#accuracy
table(SSTest$TARGET, Predict.Sant )
=TN+TP/(TN+TP+FN+FP)
(283)/(283+17)

[1] 0.9433333
 0 10k 
 
 
 # @@@@@@@@@@@@@@@@   Model:   Random Forest   @@@@@@@@@@@@@@@@

 
install.packages("randomForest")
library(randomForest)
StevensForest = randomForest (TARGET~.,data=SSTrain,ntree=200, nodesize=25)   #9:20
summary(StevensForest)
str(StevensForest)

# predict
PredictForest = predict(StevensForest, newdata = SSTest)

#accuracy
table(SSTest$TARGET, PredictForest > 0.55)
=TN+TP/(TN+TP+FN+FP)
(2886+1)/(2886+1+2+111)

#accuracy results
[1] 0.9623333  #10k @0.55   (2886+1)/(2886+1+2+111)
[1] 0.9613333  #10k @0.5 
[1] 0.9433333  #1k  @.55
[1] 0.9273333  #10k @0.2


# @@@@@@@@@@@@@@@@   Model:   Nueral Networks   @@@@@@@@@@@@@@@@

#load library
install.packages("nnet")
library(nnet)

#model variables
SSTrain
SSTest
TrainNorm
SSTrainY
TestSSNorm
SSTestY
TestNorm

#train model
ir.nn2 <- nnet(TARGET~.,data=SSTrain, size = 2, rang = 0.7,decay = 5e-4, maxit = 500)

#table(ird$species[-samp]
NN.Sander<-predict(ir.nn2, newdata=SSTest, type = "raw")
summary(NN.Sander)
str(NN.Sander)

NN.SanderAdj= NN.Sander+0.34     # =0.34

#accuracy
table(SSTest$TARGET, NN.Sander > 0.16)
table(SSTest$TARGET, NN.SanderAdj > 0.5)
=TN+TP/(TN+TP+FN+FP)
(2364+63)/(2364+63+49+524)

[1] 0.9553333 @0.25  final  value 262.099980  rang=0.5, maxit=500 
[1] 0.809 0.13 final  value 250.854307 rang =0.7, maxit=500


#logloss
print(-mean(log(NN.Sander)*SSTest$TARGET+log(1-NN.Sander)*(1-SSTest$TARGET)))
[1] 0.1463533 All size = 2, rang = 0.7,decay = 5e-4, maxit = 500)

#AUC
auc <- AUC(NN.Sander, SSTest$TARGET)
auc 
#[1] 0.7348126 All size = 2, rang = 0.7,decay = 5e-4, maxit = 500)

#submit

NN.Sander<-predict(ir.nn2, newdata=Stest, type = "raw")
NN.SanderAdj<-NN.Sander+0.34

# @@@@@@@@@@@@@@@@   Model:   Naive Bayes   @@@@@@@@@@@@@@@@

install.packages("e1071")
library(e1071)

modelNB <- naiveBayes(TARGET~.,data=SSTrain, laplace = 0)
summary(modelNB)
str(predNB)
predNB <- predict(modelNB, newdata = SSTest,threshold=0.0001, eps=0, type="class")
predNB
table(SSTest$TARGET)
      table(predNB)
#accuracy
table(SSTest$TARGET, predNB > 0.55)
=TN+TP/(TN+TP+FN+FP)


# @@@@@@@@@@@@@@@@   Model:   Support Vector Machine (SVM)   @@@@@@@@@@@@@@@@

m <- svm(TARGET~.,data=SSTrain, gamma = 0.5)
predSVM=predict (m, newdata = SSTest)
table(SSTest$TARGET)
table(predSVM >0.1305)
#accuracy
table(SSTest$TARGET, predSVM > 0.55)
=TN+TP/(TN+TP+FN+FP)

# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)


# @@@@@@@@@@@@@@@@   Model:   XGBoost   @@@@@@@@@@@@@@@@

require(xgboost)
## Loading required package: xgboost
require(methods)
## Loading required package: methods
require(data.table)
## Loading required package: data.table
require(magrittr)

#see XGBoost-R-working

## AUC

install.packages("AUC")
library(cvAUC)
auc <- AUC(preds, SSTest$TARGET)
auc


[1] 0.8407319 booster = "gbtree", max.depth = 7, eta=0.125, nrounds = 30,nthread = 2, objective = "binary:logistic")
[1] 0.8406507 eta=0.125  booster = "gbtree", max.depth = 6, eta=0.125, nrounds = 30,nthread = 2, objective = "binary:logistic")
[1] 0.839723 eta=0.13
[1] 0.8397855 eta-0.1275
[1] 0.8395799 eta=0.123
[1] 0.8386147 eta =0.12
[1] 0.8384881 0.11
# [1] 0.8383691 eta=0.14
[1] 0.8377377 eta 0.10
# [1] 0.837006 eta =0.15
#[1] 0.8363173 eta = 0.09
[1] 0.835191 eta=0.2
[1] 0.8350584 eta = 0.3
# [1] 0.834858 eta = 0.07
# 0.8316046 booster = "gbtree", max.depth = 6, eta = 0.06, nrounds = 30,nthread = 2, objective = "binary:logistic")
#[1] 0.8271798 eta=0.045
#[1] 0.8227453 eta=0.03
[1] 0.8211869 eta 0.015
# [1] 0.8164179 eta=0.01
# 0.82209 booster = "gbtree", max.depth = 6, eta = 0.023, nrounds = 30,nthread = 2, objective = "binary:logistic")
# [1] 0.8237612  NORMALIZE max depth=3, seed113, split=0.8, ALL /cv depth=3
# [1] 0.828262# max depth=3, seed113, split=0.8, ALL /cv depth=3
# [1] 0.8220357 max depth=3, seed113, split=0.8, ALL
# [1] 0.8096479 depth=5
#     0.8125579 depth =4
#[1] 0.8005493 - depth =7
#[1] 0.8262403 all data seed=113 nrounds =14
#[1] 0.776624 @0.5, 13% Data
# [1] 0.8204681, all DATA

TestPredictionBinaryXGB1=ifelse(preds >0.24,1,0) #nrounds=10,ALL
auc025_10 <- AUC(TestPredictionBinaryXGB1, SSTest$TARGET)
auc025_10

#first 10k
[1] 0.8128297
[1] 0.8201511     @0.24 # 0.9570289    #nrounds=10,ALL
0.1->   0.614
0.01->  0.69
0.05->  0.7097
0.025-> 0.729
0.035-> 0.733
0.03-> 0.7504

#logloss
print(-mean(log(preds)*SSTest$TARGET+log(1-preds)*(1-SSTest$TARGET)))
[1] 0.1846436 booster = "gbtree", max.depth = 6, eta = 0.06, nrounds = 30,nthread = 2, objective = "binary:logistic")
[1] 0.1403402 NORMALIZE max depth=3, seed113, split=0.8, ALL /cv depth=3
[1] 0.1372096 max depth=3, seed113, split=0.8, ALL /cv depth=3
[1] 0.1396965 max depth=3, seed113, split=0.8, ALL
[1] 0.1473972 max depth=5/not 2
[1] 0.1673639 max depth=10
[1] 0.1439073 max depth=1
[1] 0.1425649 max depth=3

#[1] 0.1370622  Random 9k (0.13)
#[1] 0.1505324 no rounds =5, 10k
#[1] 0.1421849 no rounds=10, ALL


#accuracy

preds[1:10]
table(preds > 0.5)

table(SSTest$TARGET, preds > 0.5)
=TN+TP/(TN+TP+FN+FP)


#test accuracy results 
[1] 0.9603394 (14597+4)/(14597+4+599+4)  NORMALIZE max depth=3, seed113, split=0.8, ALL /cv depth=3
#[1] 0.9599448(14584+11)/(14584+11+17+592) # max depth=3, seed113, split=0.8, ALL /cv depth=3
# 0.9596817(14580+11)/(14580+11+21+592)  max depth=3, seed113, split=0.8, ALL
(21879+13)/(21879+13+42+872)# seed 113 #rounds 14 All
(21870+2)/(21870+2+3+931) - random 13%
[1] 0.9590459     depth=1 , all data, 11 rounds
#  0.9409781       (2759+31)/(2759+31+98+77) @ 0.21  random 13%
#  0.9629005       (2853+2)/(2853+2+4+106)  @ 0.5 random 13%
#test accuracy - fist 10k
(21804+22)/(21804+22+69+911) @0.24 # 0.9570289   #nrounds=10,ALL
(21800+23)/(21800+23+910+73) @0.25 #0.9568973 #nrounds=10,ALL
(2861+2)/(125+12+2861+2) 0.95433 @0.25 #nrounds=5,10k
TestPredictionBinaryXGB1=ifelse(preds >0.25,1,0) #nrounds=5,10k
(2754+36)/(2754+36+91+119) 0.93 @0.243 #nrounds=5,10k
(2754+36)/(2754+36+119+91)  0.93 @0.2  #nrounds=5,10k
(2881+3)/(2881+3+7+109) [1] 0.9613333 




#pred for Submittal (test set)
preds=predict(bst,as.matrix(Stest))


#Trees
trees = xgb.model.dt.tree(dimnames(trainMatrix)[[2]],model = bst)

# view trees
View(trees)

# view models
model <- xgb.dump(bst, with.stats = T)
model[1:10]

# get the feature real names
names <- dimnames(trainMatrix)[[2]]
importance_matrix<-xgb.importance(names,model =bst)
xgb.plot.importance(importance_matrix[1:10])

# plot the tree #n_first_tree = 2, #(plot the first two trees)

xgb.plot.tree(feature_name = names,model =bst, n_first_tree = 2)


##Evaluation using ROC Curve

# Install and load ROCR package
install.packages("ROCR")
library(ROCR)

# Prediction function  			
ROCRpred = prediction(preds, SSTest$TARGET)

# Performance function			 #DEFINES WHAT WE’D LIKE TO PLOT
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))




# @@@@@@@@@@@@@@@@   Model:   Ensemble - MBOOST   @@@@@@@@@@@@@@@@


SSTrainFACT<-SSTrain
SSTrainFACT$TARGET<-as.factor(SSTrainFACT$TARGET)

install.packages("mboost")
library(mboost)
m.boost <-glmboost(TARGET~var15+imp_op_var40_comer_ult1+imp_op_var40_comer_ult3+imp_op_var40_efect_ult3+imp_op_var40_ult1+imp_sal_var16_ult1+ind_var30+num_var43_emit_ult1,family=Binomial(),data=SSTrainFACT) # Binomial-needed for classification
coef(m.boost)

# (Intercept)               var15            ind_var30    num_var43_emit_ult1 
# -0.04950741          0.00925420         -0.37900555          0.04199390 

plot(m.boost, ylim=range(coef(m.boost,which=c("var15", "ind_var30", "num_var43_emit_ult1 "))))

mboostpred=predict(m.boost,newdata=SSTest, type="response")   # predict using model
summary(mboostpred)
head(mboostpred)

#accuracy
table(SSTest$TARGET, mboostpred > 0.10)
=TN+TP/(TN+TP+FN+FP)
(279+1)/(279+1+16+4)     
[1] 0.9333333

cv.boost <-cvrisk(m.boost)
mstop(cv.boost)

plot(cv.boost, main="Cross-validated estimates of empirical risk")

# @@@@@@@@@@@@@@@@   Model:   Ensemble - GAMBoost   @@@@@@@@@@@@@@@@ 

mb.boost <-gamboost(TARGET~var15+imp_op_var40_comer_ult1+imp_op_var40_comer_ult3+imp_op_var40_efect_ult3+imp_op_var40_ult1+imp_sal_var16_ult1+ind_var30+num_var43_emit_ult1,family=Binomial(),data=SSTrainFACT,dfbase = 2,control = boost_control(mstop = 50))
# needed for classification 
mb.boost

gamboostpredict=predict(mb.boost,newdata=SSTest, type="response")   # predict using model
summary(gamboostpredict)
head(gamboostpredict)

#accuracy
table(SSTest$TARGET, glm.predTest > 0.09)
=TN+TP/(TN+TP+FN+FP)
(271+6)/(271+6+11+12)   [1] 0.9233333   0.09  


# @@@@@@@@@@@@@@@@   Model:   Ensemble - ADABOOST   @@@@@@@@@@@@@@@@

install.packages("ada")
library(ada)
m.ada <-ada(TARGET~var15+imp_op_var40_comer_ult1+imp_op_var40_comer_ult3+imp_op_var40_efect_ult3+imp_op_var40_ult1+imp_sal_var16_ult1+ind_var30+num_var43_emit_ult1,data=SSTest,iter=50)

m.ada.test <-addtest(m.ada,test.x=SSTest,SSTest$TARGET)

m.ada.test

plot(m.ada.test, test=TRUE)
varplot(m.ada.test, max.var.show=5)# first 5 variables


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

SL.library <- c("SL.knn",
                "SL.glm",
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

#How to train & test
fitSL2 <- SuperLearner(SSTrainY, SSTrain,newX = SSTest,
        family = family,
        SL.library = SL.library,
        method = method)


predSL=predict(fitSL2,Stest)

# 12:20 - 1:25  10k  ;"SL.knn", "SL.glm", "SL.randomForest"
# 11:17-11:20   0.013; "SL.glm", "SL.randomForest"  There were 28 warnings glm issues
# 11:35-1255   0.013: "SL.glm", "SL.randomForest"  "gbm" 50 or more warnings variable X has no variation
fitSL2$SL.predict [1:10]
str(fitSL2$SL.predict)
str(Stest)

#AUC
auc <- AUC(fitSL2$SL.predict, SSTestY)
auc

# [1] 0.7644577  "SL.knn","SL.glm","SL.randomForest","SL.nnet"
# [1] 0.7831011 "SL.glm","SL.randomForest","SL.nnet"
# [1] 0.7804648   "SL.randomForest","SL.nnet"   0.13
#[1] 0.875 "SL.glm", "SL.randomForest"  "gbm"   0.013
#[1] 0.882732  "SL.randomForest","SL.nnet", no warnings          0.013
#[1] 0.8298969 "method.AUC" 30 warnings         0.013
#[1] 0.8659794 "method.NNloglik 28 warnings     0.013
#[1] 0.8485825, "method.NNLS2" 29 warnings      0.013
#[1] 0.8782216  #same below, but "SL.glm","SL.randomForest"      0.013                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    [1] 0.8782216
#[1] 0.8685567  #0.013 random "SL.knn","SL.glm","SL.randomForest" (Warnings(deficience, converge,0/1)), 0.8split method <- "method.NNLS"
SL.glm","SL.randomForest"


#accuracy
table(SSTestY, fitSL2$SL.predict > 0.5)
=TN+TP/(TN+TP+FN+FP)
[1] 0.9600405 (1897+1)/(1897+1+2+77) 0.13  "SL.randomForest","SL.nnet" 
[1] 0.969697 (192+0)/(192+0+4+2) #0.013 random "SL.knn","SL.glm","SL.randomForest" (Warnings(deficience, converge,0/1)) "SL.knn","SL.glm","SL.randomForest", 0.8split method <- "method.NNLS"


#logloss
print(-mean(log(fitSL2$SL.predict)*SSTestY+log(1-fitSL2$SL.predict)*(1-SSTestY)))

[1] 0.1000386  #0.013 random "SL.knn","SL.glm","SL.randomForest" (Warnings(deficience, converge,0/1)) 0.8split method <- "method.NNLS"



#Result:  Superlearner ("SL.knn","SL.glm", "SL.randomForest")  1k  0.5  Accuracy =1.0 

#Evaluation using ROC Curve

# Install and load ROCR package
install.packages("ROCR")
library(ROCR)

# Prediction function  			
ROCRpred = prediction(fitSL$SL.predict, SSTest$TARGET)

# Performance function			 #DEFINES WHAT WE’D LIKE TO PLOT
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)

# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

10k
install.packages("AUC")
library(cvAUC)
auc <- AUC(fitSL$SL.predict, SSTest$TARGET)
auc

# @@@@@@@@@@@@@@@@   Model:   Ensemble - Subemsemble   @@@@@@@@@@@@@@@@    

library(subsemble)

y<-as.vector(SSTrain$TARGET)   #Target vector Y
x<-SSTrain

#Set up the ensemble
learner <- c("SL.knn",
             "SL.glm",
             "SL.randomForest")
metalearner <- "SL.nnls"
family <- "binomial"
subsets <- 1

#How to train & test
fit <- subsemble(x = x, y = y,newx = SSTest,
                 family = family,
                 learner = learner,
                 metalearner = metalearner,
                 subsets = subsets)

fit$pred



# @@@@@@@@@@@@@@@@   Model:   Ensemble - Version 2   @@@@@@@@@@@@@@@@

# Load some example data.

library(subsemble)
library(cvAUC)  # >= version 1.0.1
#data(admissions)


# Training data.
#x <- subset(admissions, select = -c(Y))[1:400,]
#y <- admissions$Y[1:400]

y <-as.double(SSTrain$TARGET)
SSTrain$TARGET<-NULL
x <-SSTrain
typeof(x)
typeof(y)


# Test data.
#newx <- subset(admissions, select = -c(Y))[401:500,]
#newy <- admissions$Y[401:500]

newy <-as.double(SSTest$TARGET)
SSTest$TARGET<-NULL
newx <-SSTest
typeof(x)
typeof(y)
typeof(newx)
typeof(newy)

# Set up the Subsemble.
learner <- c("SL.knn",
             "SL.glm",
             "SL.randomForest")
#learner <- c("SL.randomForest", "SL.glm")
metalearner <- "SL.glm"
subsets <- 1

# Train and test the model.
# With learnControl$multiType="crossprod" (the default), 
# we ensemble 4 models (2 subsets x 2 learners).

fit <- subsemble(x = x, y = y, newx = newx, family = binomial(), 
                 learner = learner, metalearner = metalearner,
                 subsets = subsets)

stopCluster(cl)

# AUC#1 - Evaulate the model by calculating AUC on the test set.
typeof(x) x <-SSTrain
typeof(y) y <-as.double(SSTrain$TARGET)  no TARGET
typeof(newx) SSTest no TARGET
typeof(newy) as.double(SSTest$TARGET)
Stest

#AUC
library(cvAUC)

auc <- AUC(predictions = fit$pred, labels = newy)
print(auc) 
[1] 0.7818037

#print(auc)  # Test set AUC is: 0.937


# accuracy
fit$pred
y
y=as.vector(y)
SSTest$TARGET<-y
str(SSTest$TARGET)

table(SSTest$TARGET, fit$pred > 0.5)
=TN+TP/(TN+TP+FN+FP)
summary(fitSL)

# logloss
#logloss
print(-mean(log(preds)*SSTest$TARGET+log(1-preds)*(1-SSTest$TARGET)))


# We can also use the predict method to generate predictions on new data afterwards.
typeof(Stest)
pred <- predict(fit, Stest)



# @@@@@@@@@@@@@@@@   Model:   Ensemble - H20   @@@@@@@@@@@@@@@@


### Ensemble - H20   ###  To review with AWS

library(h2o)  # First install from CRAN
localH2O <- h2o.init()

# h2o.shutdown()            #shutdown
# h2o.init(nthreads = -1)  #restart

# Data directly into H2O cluster (avoids R)
train <- h2o.importFile(localH2O, path = "train.csv")
# Data into H2O from R data.frame
train <- as.h2o(localH2O, SSTrain)

#How to train & test

y <- "Class"
x <- setdiff(names(train), y)
fit <- h2o.deeplearning(x = x, y = y, data = SSTrain)  #error
pred <- h2o.predict(fit, test)
                    
#Set up the super learner ensemble

learner <- c("h2o.glm.1",
             "h2o.glm.2",
             "h2o.randomForest.1",
             "h2o.deeplearning.1")
metalearner <- "SL.glm"
family <- "binomial"

#Create base learners

h2o.glm.1 <- function(..., alpha = 1.0) {
  h2o.glm.wrapper(..., alpha = alpha)
}
h2o.glm.2 <- function(..., alpha = 0.5) {
  h2o.glm.wrapper(..., alpha = alpha)
}

#How to train & test

fit <- h2o.ensemble(x = x, y = y, data = SSTrain,
                    family = family,
                    learner = learner,
                    metalearner = metalearner)

pred <- predict(object = fit, newdata = test)


## 20 Superlearner ("SL.knn","SL.glm", "SL.randomForest")  1k  Threshold=0.5  Accuracy =1.0
## 21 Subsemble (""SL.knn","SL.glm","SL.randomForest") subsets <- 2  Threshold=0.5  Accuracy =1.0


# @@@@@@@@@@@@@@@@   Kaggle-Submittals   @@@@@@@@@@@@@@@@


#### Submittal #1: RANDOM FOREST All, 0.5 Threshold ####

install.packages("randomForest")
library(randomForest)
StevensForest = randomForest (TARGET~.,data=Strain,ntree=200, nodesize=25)   #9:20
summary(StevensForest)
str(StevensForest)
# predict
PredictForest = predict(StevensForest, newdata = Stest)
str(Strain)
#Submit

TestPredictionBinaryRF=ifelse(PredictForest >0.5,1,0)
submissionS <- data.frame(ID=Stest$ID, TARGET=TestPredictionBinaryRF)
cat("saving the submission file\n")
write.csv(submissionS, "Santander1.csv")

#### Submittal #2: RANDOM FOREST All, 0.5 Threshold ####

StevensForest2 = randomForest (TARGET~.,data=SSTrain,ntree=200, nodesize=25)
PredictForest2 = predict(StevensForest2, newdata = Stest)
TestPredictionBinaryRF2=ifelse(PredictForest2 >0.5,1,0)
submissionS2 <- data.frame(ID=Stest$ID, TARGET=TestPredictionBinaryRF2)
cat("saving the submission file\n")
write.csv(submissionS2, "Santander2.csv")


#### Submittal #2: XGB, 0.01 Threshold ####

TestPredictionBinaryRF3=ifelse(preds >0.01,1,0)
submissionS3 <- data.frame(ID=Stest$ID, TARGET=TestPredictionBinaryRF3)
cat("saving the submission file\n")
write.csv(submissionS3, "Santander3.csv")

#### Submittal #3: XGB,  ####   0.826387 You just moved up 709 positions on the leaderboard

preds #nrounds=10,ALL
submissionS3 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS3, "Santander3.csv")

#### Submittal #4: XGB, 0.01 Threshold ####
TestPredictionBinaryXGB1=ifelse(preds >0.024,1,0) #nrounds=10,ALL
submissionS4 <- data.frame(ID=Stest$ID, TARGET=TestPredictionBinaryXGB1)
cat("saving the submission file\n")
write.csv(submissionS4, "Santander4.csv")


#### Submittal #5: Superlearner - train 10,000 
fitSL2$SL.predict [1:10]
submissionS5 <- data.frame(ID=Stest$ID, TARGET=fitSL2$SL.predict)
cat("saving the submission file\n")
write.csv(submissionS5, "Santander5.csv")

#### Submittal #6: Subensemble - train 10,000 
fit$pred [1:10]
summary(fit$pred)
str(fit$pred)
submissionS6 <- data.frame(ID=Stest$ID, TARGET=fit$pred)
cat("saving the submission file\n")
write.csv(submissionS6, "Santander6.csv")


#### Submittal #7: XGB, 0.05 Threshold ####  0.805382
preds #nrounds=5, 13% random
submissionS7 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS7, "Santander7.csv")


#### Submittal #8: XGB, 0.05 Threshold ####  ALl data 
preds #nrounds=11, depth=3, all data
submissionS8 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS8, "Santander8.csv")


#### Submittal #9: XGB, 0.05 Threshold ####  ALl data 
preds #depth=3, all data, norounds =15
submissionS9 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS9, "Santander9.csv")


#### Submittal #10: XGB,  ####  All data 
preds #depth=4, 0.8 split, all data, norounds=13
submissionS10 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS10, "Santander10.csv")


#### Submittal #11: XGB,  ####  All data 
preds #depth=3, 0.8 split, all data, norounds=13
submissionS11 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS11, "Santander11.csv")


#### Submittal #12: XGB,  ####  All data 
preds nrounds=12, depth=3, all, seed=113, split=0.8, depth=3 (cv)
submissionS12 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS12, "Santander12.csv")

#### Submittal #13: XGB,  ####  All data 
preds nrounds=7, depth=3, all, seed=113, split=0.8, depth=3 (cv)
submissionS13 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS13, "Santander13.csv")



#### Submittal #14: NN,  ####  All data  NN.SanderAdj

submissionS14 <- data.frame(ID=Stest$ID, TARGET=NN.SanderAdj)
cat("saving the submission file\n")
write.csv(submissionS14, "Santander14.csv")


#### Submittal #15: XGB,  ####  All data 
submissionS15 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS15, "Santander15.csv")]]

#### Submittal #16: XGB,  ####  All data 
submissionS16 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS16, "Santander16.csv")

#### Submittal #17: Superlearner - nnet, RanF 0.013,  ####  All data 
submissionS17 <- data.frame(ID=Stest$ID, TARGET=fitSL2$SL.predict)
cat("saving the submission file\n")
write.csv(submissionS17, "Santander17.csv")

#### Submittal #18: Superlearner - nnet, RanF 0.13,  ####  All data 
submissionS18 <- data.frame(ID=Stest$ID, TARGET=predSL)
cat("saving the submission file\n")
write.csv(submissionS18, "Santander18.csv")

#### Submittal #19: XGBOOST - ####  All data 
submissionS19 <- data.frame(ID=Stest.id, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS19, "Santander19.csv")

#### Submittal #20: XGBOOST - ####  All data 
submissionS20 <- data.frame(ID=Stest.id, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS20, "Santander20.csv")

#### Submittal #21: XGBOOST - ####  All data KAGGLE
submissionS21 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS21, "Santander21.csv")


#### Submittal #22: XGBOOST - ####  All data BEST
submissionS22 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS22, "Santander22.csv")


#### Submittal #23: XGBOOST - ####  All data KAGGLE = nrounds=30
submissionS23 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS23, "Santander23.csv")


#### Submittal #24: XGBOOST - ####  All data KAGGLE = nrounds=30
submissionS25 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS25, "Santander25.csv")


#### Submittal #26: XGBOOST - ####  New data KAGGLE = nrounds=30
submissionS26 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS26, "Santander26.csv")


#### Submittal #27: XGBOOST - ####  New data/new columns KAGGLE = nrounds=30
submissionS27 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS27, "Santander27.csv")

#### Submittal #28: XGBOOST - ####  New data/new columns KAGGLE = nrounds=30
submissionS28 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS28, "Santander28.csv")

#### Submittal #29: XGBOOST - ####  New data/new columns KAGGLE  
submissionS29 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS29, "Santander29.csv")


#### Submittal #30: XGBOOST - ####  New data/new columns KAGGLE  
submissionS30 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS30, "Santander30.csv")


#### Submittal #31: XGBOOST - ####  New data/new columns KAGGLE  
submissionS31 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS31, "Santander31.csv")

#### Submittal #32: XGBOOST - ####  New data/new columns KAGGLE  
submissionS32 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS32, "Santander32.csv")

#### Submittal #33: XGBOOST - ####  New data/new columns KAGGLE  
submissionS33 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS33, "Santander33.csv")

#### Submittal #34: XGBOOST - ####  New data/new columns KAGGLE  
submissionS34 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS34, "Santander34.csv")


#### Submittal #35: XGBOOST - ####  New data/new columns KAGGLE  
submissionS35 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS35, "Santander35.csv")


#### Submittal #36: XGBOOST - ####  E^e - my best algorithm
submissionS36 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS36, "Santander36.csv")


#### Submittal #37: XGBOOST - ####  log - Kaggle
submissionS37 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS37, "Santander37.csv")


#### Submittal #38: XGBOOST - #### e^x,log, base up to up to ind_var39_0
submissionS38 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS38, "Santander38.csv")


#### Submittal #39: XGBOOST - #### e^x,log, base  - ALl, no cluster
submissionS39 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS39, "Santander39.csv")

#### Submittal #40: XGBOOST - #### e^x,log, base and cluster - ALl
submissionS40 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS40, "Santander40.csv")

#### Submittal #41: XGBOOST - #### grid search on base
submissionS41 <- data.frame(ID=Stest$ID, TARGET=predT)
cat("saving the submission file\n")
write.csv(submissionS41, "Santander41.csv")

#### Submittal #42: XGBOOST - #### kaggle + cluster data
submissionS42 <- data.frame(ID=Stest$ID, TARGET=predKC)
cat("saving the submission file\n")
write.csv(submissionS42, "Santander42.csv")

#### Submittal #43: XGBOOST - #### kaggle + cluster data
submissionS43 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS43, "Santander43.csv")



#### Submittal #44: XGBOOST - #### best+feature+grid search-NOID
submissionS44 <- data.frame(ID=Stest$ID, TARGET=preds)
cat("saving the submission file\n")
write.csv(submissionS44, "Santander44.csv")


#### Submittal #44: XGBOOST - #### best+feature+grid search-NOID
submissionS45 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS45, "Santander45.csv")


#### Submittal #46: XGBOOST - #### best+feature+grid search-NOID
submissionS46 <- data.frame(ID=Stest$ID, TARGET=preds2)
cat("saving the submission file\n")
write.csv(submissionS46, "Santander46.csv")




# @@@@@@@@@@@@@@@@   glm-auto select by t-test   @@@@@@@@@@@@@@@@


#from homesite




# @@@@@@@@@@@@@@@@   glmnet, subset, step, LASSO, RIDGE   @@@@@@@@@@@@@@@@


# Step-Logistic Regression (by significant features)


SSTest
summary(SSTrain)

#modeling
glm.Sant=glm(SSTrain$TARGET~ID+var3+var15+imp_ent_var16_ult1+imp_op_var39_comer_ult1+imp_op_var39_comer_ult3+imp_op_var40_comer_ult1+imp_op_var40_comer_ult3+imp_op_var40_efect_ult3+imp_op_var40_ult1+imp_op_var41_efect_ult1+imp_op_var41_efect_ult3+imp_op_var41_ult1+imp_sal_var16_ult1+ind_var1_0+ind_var5_0+ind_var5+ind_var8_0+ind_var8+ind_var12_0+ind_var12+ind_var13_0+ind_var13_corto_0+ind_var13_corto+ind_var14_0+ind_var17_0+ind_var19+ind_var20_0+ind_var20+ind_var24_0+ind_var25_cte+ind_var30_0+ind_var30+ind_var31_0+ind_var37_cte+ind_var37_0+ind_var39_0+ind_var41_0+num_var4+num_var5_0+num_var13_0+num_var26_0+num_op_var41_hace2+num_op_var41_hace3+num_op_var41_ult1+num_var35+num_var37_med_ult2+num_var37_0+num_var39_0+num_var41_0+num_var42+saldo_var5+saldo_var12+saldo_var13_corto+saldo_var13_largo+imp_aport_var13_ult1+imp_var7_recib_ult1+imp_var43_emit_ult1+imp_trans_var37_ult1+ind_var10_ult1+ind_var10cte_ult1+ind_var9_cte_ult1+ind_var43_emit_ult1+ind_var43_recib_ult1+var21+num_aport_var13_hace3+num_ent_var16_ult1+num_var22_hace2+num_var22_hace3+num_var22_ult1+num_med_var22_ult3+num_med_var45_ult3+num_meses_var5_ult3+num_meses_var8_ult3+num_meses_var12_ult3+num_meses_var13_corto_ult3+num_meses_var39_vig_ult3+num_op_var39_comer_ult1+num_op_var39_comer_ult3+num_op_var41_efect_ult1+num_op_var41_efect_ult3+num_var43_emit_ult1+num_var43_recib_ult1+num_trasp_var11_ult1+num_var45_hace2+num_var45_hace3+num_var45_ult1+saldo_medio_var5_hace2+saldo_medio_var5_hace3+saldo_medio_var5_ult1+saldo_medio_var5_ult3+saldo_medio_var8_hace2+saldo_medio_var8_hace3+saldo_medio_var8_ult1+saldo_medio_var8_ult3+saldo_medio_var12_hace2+saldo_medio_var12_hace3+saldo_medio_var12_ult1+saldo_medio_var12_ult3+saldo_medio_var13_corto_hace2+saldo_medio_var13_corto_hace3+saldo_medio_var13_corto_ult3+saldo_medio_var13_largo_hace2+var38,data=SSTrain,family=gaussian)
summary(glm.Sant)  #AIC: -328   # 0.94 @0.5
Sant.step=step(glm.Sant)

glm.Sant_Sig=glm(SSTrain$TARGET~var15+imp_op_var40_comer_ult1+ind_var17_0+ind_var31_0+num_var22_ult1+num_var43_emit_ult1,data=SSTrain,family=gaussian)
summary(glm.Sant_Sig) #AIC: -428.26  #0.939 @0.15
Sant.step=step(glm.Sant_Sig)

glm.Sant_Sig3=glm(SSTrain$TARGET~var15+num_var22_ult1+num_var43_emit_ult1,data=SSTrain,family=gaussian)
summary(glm.Sant_Sig3) #AIC: -434
Sant.step=step(glm.Sant)

glm.predTest=predict(glm.Sant_Sig3,newdata=SSTest, type="response")   # predict using model
glm.predTest
summary(glm.predTest)
head(glm.predTest)


#accuracy
table(SSTest$TARGET, glm.predTest > 0.15)
=TN+TP/(TN+TP+FN+FP)
(280+1)/(280+1+3+16)


# Chapter 6 Lab 1: Subset Selection Methods

# Best Subset Selection

install.packages("leaps")
library(ISLR)
library(leaps)
regfit.full=regsubsets(TARGET~ID+var3+var15+imp_ent_var16_ult1+imp_op_var39_comer_ult1+imp_op_var39_comer_ult3+imp_op_var40_comer_ult1+imp_op_var40_comer_ult3+imp_op_var40_efect_ult3+imp_op_var40_ult1+imp_op_var41_efect_ult1+imp_op_var41_efect_ult3+imp_op_var41_ult1+imp_sal_var16_ult1+ind_var1_0+ind_var5_0+ind_var5+ind_var8_0+ind_var8+ind_var12_0+ind_var12+ind_var13_0+ind_var13_corto_0+ind_var13_corto+ind_var14_0+ind_var17_0+ind_var19+ind_var20_0+ind_var20+ind_var24_0+ind_var25_cte+ind_var30_0+ind_var30+ind_var31_0+ind_var37_cte+ind_var37_0+ind_var39_0+ind_var41_0+num_var4+num_var5_0+num_var13_0+num_var26_0+num_op_var41_hace2+num_op_var41_hace3+num_op_var41_ult1+num_var35+num_var37_med_ult2+num_var37_0+num_var39_0+num_var41_0+num_var42+saldo_var5+saldo_var12+saldo_var13_corto+saldo_var13_largo+imp_aport_var13_ult1+imp_var7_recib_ult1+imp_var43_emit_ult1+imp_trans_var37_ult1+ind_var10_ult1+ind_var10cte_ult1+ind_var9_cte_ult1+ind_var43_emit_ult1+ind_var43_recib_ult1+var21+num_aport_var13_hace3+num_ent_var16_ult1+num_var22_hace2+num_var22_hace3+num_var22_ult1+num_med_var22_ult3+num_med_var45_ult3+num_meses_var5_ult3+num_meses_var8_ult3+num_meses_var12_ult3+num_meses_var13_corto_ult3+num_meses_var39_vig_ult3+num_op_var39_comer_ult1+num_op_var39_comer_ult3+num_op_var41_efect_ult1+num_op_var41_efect_ult3+num_var43_emit_ult1+num_var43_recib_ult1+num_trasp_var11_ult1+num_var45_hace2+num_var45_hace3+num_var45_ult1+saldo_medio_var5_hace2+saldo_medio_var5_hace3+saldo_medio_var5_ult1+saldo_medio_var5_ult3+saldo_medio_var8_hace2+saldo_medio_var8_hace3+saldo_medio_var8_ult1+saldo_medio_var8_ult3+saldo_medio_var12_hace2+saldo_medio_var12_hace3+saldo_medio_var12_ult1+saldo_medio_var12_ult3+saldo_medio_var13_corto_hace2+saldo_medio_var13_corto_hace3+saldo_medio_var13_corto_ult3+saldo_medio_var13_largo_hace2+var38,SSTrain,really.big=T)
summary(regfit.full)

#var15+imp_op_var40_comer_ult1+imp_op_var40_comer_ult3+imp_op_var40_efect_ult3+imp_op_var40_ult1+imp_sal_var16_ult1+ind_var30+num_var43_emit_ult1


glm.SantRS=glm(SSTrain$TARGET~var15+imp_op_var40_comer_ult1+imp_op_var40_comer_ult3+imp_op_var40_efect_ult3+imp_op_var40_ult1+imp_sal_var16_ult1+ind_var30+num_var43_emit_ult1,data=SSTrain,family=gaussian)
summary(glm.SantRS)
glm.predTest=predict(glm.SantRS,newdata=SSTest, type="response")   # predict using model
glm.predTest
summary(glm.predTest)
head(glm.predTest)

#accuracy
table(SSTest$TARGET, glm.predTest > 0.13)
=TN+TP/(TN+TP+FN+FP)
(268+6)/(268+6+11+15)   0.9133    Threshold-0.08
(252+8)/(252+8+11+15)   0.9090909 Threshold-0.07
(269+5)/(269+5+14+12)   #0.9133333 Threshold-0.085
(272+4)/(272+4+11+13)   #[1] 0.92 Threshold-0.09
(272+4)/(272+4+11+13)   #[1] 0.92 Threshold-0.09
(281+0)/(281+2+17)      #0.9366667 0.13

#regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6)

#var15+ind_var25_cte+ind_var30+num_var26_0+num_var22_ult1+num_var43_emit_ult1 

glm.SantRF2=glm(SSTrain$TARGET~var15+ind_var25_cte+ind_var30+num_var26_0+num_var22_ult1+num_var43_emit_ult1 ,data=SSTrain,family=gaussian)
glm.predTest=predict(glm.SantRF2,newdata=SSTest, type="response")   # predict using model
glm.predTest
summary(glm.predTest)
head(glm.predTest)

#accuracy
table(SSTest$TARGET, glm.predTest > 0.09)
=TN+TP/(TN+TP+FN+FP)
(271+6)/(271+6+11+12)     #0.9233333   0.09

# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# Choosing Among Models

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)
regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)
test.mat=model.matrix(Salary~.,data=Hitters[test,])
val.errors=rep(NA,19)
for(i in 1:19){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
which.min(val.errors)
coef(regfit.best,10)
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)
coef(reg.best,11)


# Chapter 6 Lab 2: Ridge Regression and the Lasso

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# Ridge Regression

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))
predict(ridge.mod,s=50,type="coefficients")[1:20,]
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])-y.test)^2)
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]

# The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]


# Chapter 6 Lab 3: PCR and PLS Regression

# Principal Components Regression

library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# Partial Least Squares

set.seed(1)
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)

