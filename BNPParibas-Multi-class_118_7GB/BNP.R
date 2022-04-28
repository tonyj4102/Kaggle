#@@@@@@@@@@@@@@@@@  BNP - Evaluation Metric  @@@@@@@@@@@@@@@@@

# Plot xgb.cv (logloss)
plot(log(bst.cv$test.mlogloss.mean),type="l") 

#Log loss
print(-mean(log(preds2)*SStest$TARGET+log(1-preds2)*(1-SStest$TARGET)))



#@@@@@@@@@@@@@@@@@  Read in data  @@@@@@@@@@@@@@@@@

rm(list = ls())

setwd("~/Dropbox/Projects/BNPParibas-Multi-class")

Btrain=read.csv("BNP_train.csv")
Btest=read.csv("BNP_test.csv")

# for submission ID
Ltest=read.csv("BNP_test.csv")

str(Btrain, list.len = 10)
str(Btest, list.len = 10)

#>||||||   nrow(Btrain)  # [1] 114321  114321  nrow(Btest)   # [1] 114393 114393




#@@@@@@@@@@@@@@@@@@@  (Optional) Small feature addition - Count NA percentage


Btrain$NACount_N <- NULL #rowSums(is.na(Btrain)) / nrow(Btrain) 
Btrain$NACount <- NULL #rowSums(is.na(Btrain)) 

Btest$NACount_N <- NULL #rowSums(is.na(Btest)) / nrow(Btest) 
Btest$NACount <- NULL #rowSums(is.na(Btest)) 


# @@@@@@@@@@@@@@@@@  make feature of counts of zeros factor

feature.names <- names(Btrain)
Btrain$ZeroCount <- NULL #rowSums(Btrain[,feature.names]== 0) / nrow(Btrain)
Btrain$ZeroAmount <- NULL #rowSums(Btrain[,feature.names]== 0) 
feature.names <- names(Btest)
Btest$ZeroCount <- NULL #rowSums(Btest[,feature.names]== 0) / nrow(Btest)
Btest$ZeroAmount <- NULL #rowSums(Btest[,feature.names]== 0)


# @@@@@@   Explore:  write out 80% to to fit in Trifactor Wranger <100MB  @@@@@@
set.seed(15071)
spl = sample(nrow(Btrain), 0.8*nrow(Btrain))
BBTrain = Btrain[spl,]
BBTest = Btrain[-spl,]


BNP_0_8Train <- data.frame(BBTrain)
cat("saving the submission file\n")
write.csv(BNP_0_8Train, "BNP_0_8Train.csv")

# Review in Trifactor 

#>||||||   lots of N/A's 

#  @@@@@@ Convert N/A's to zeros

Btrain[is.na(Btrain)] <- 0
Btest[is.na(Btest)] <- 0

#BNP_noNATrain <- data.frame(BBtrain)
#cat("saving the submission file\n")
#write.csv(BNP_noNATrain, "BNP_noNATrain.csv")


# @@@@@@@@  Convert categories to numbers for XGBoost  @@@@@@@@@@@@@@

Btrain$v3<-as.integer(factor(Btrain$v3)) #v3
Btrain$v22<-as.integer(factor(Btrain$v22)) #v22 -285 categories
Btrain$v24<-as.integer(factor(Btrain$v24)) #v24
Btrain$v30<-as.integer(factor(Btrain$v30)) #v30 - 7 categories
Btrain$v31<-as.integer(factor(Btrain$v31)) #v31 - 3 categories
Btrain$v47<-as.integer(factor(Btrain$v47)) #v47 - 7 categories
Btrain$v52<-as.integer(factor(Btrain$v52)) #v52 - 12 categories
Btrain$v56<-as.integer(factor(Btrain$v56)) #v56 - 44 
Btrain$v66<-as.integer(factor(Btrain$v66)) #v66 - 3
Btrain$v71<-as.integer(factor(Btrain$v71)) #v71 - 3
Btrain$v72<-as.integer(factor(Btrain$v72)) #v72 - 9
Btrain$v74<-as.integer(factor(Btrain$v74)) #v74 - 1
Btrain$v75<-as.integer(factor(Btrain$v75)) #v75 - 2
Btrain$v79<-as.integer(factor(Btrain$v79)) #v79 - 14
Btrain$v91<-as.integer(factor(Btrain$v91)) #v91 - 7
Btrain$v107<-as.integer(factor(Btrain$v107)) #v107 - 7
Btrain$v110<-as.integer(factor(Btrain$v110)) #v110 -3
Btrain$v112<-as.integer(factor(Btrain$v112)) #v112 - 22
Btrain$v113<-as.integer(factor(Btrain$v113)) #v113 - 26
Btrain$v125<-as.integer(factor(Btrain$v125)) #v125 - 74



Btest$v3<-as.integer(factor(Btest$v3)) 
Btest$v22<-as.integer(factor(Btest$v22))  
Btest$v24<-as.integer(factor(Btest$v24))  
Btest$v30<-as.integer(factor(Btest$v30))  
Btest$v31<-as.integer(factor(Btest$v31))  
Btest$v47<-as.integer(factor(Btest$v47))  
Btest$v52<-as.integer(factor(Btest$v52))  
Btest$v56<-as.integer(factor(Btest$v56))  
Btest$v66<-as.integer(factor(Btest$v66))  
Btest$v71<-as.integer(factor(Btest$v71))  
Btest$v72<-as.integer(factor(Btest$v72))  
Btest$v74<-as.integer(factor(Btest$v74))  
Btest$v75<-as.integer(factor(Btest$v75))  
Btest$v79<-as.integer(factor(Btest$v79))  
Btest$v91<-as.integer(factor(Btest$v91))  
Btest$v107<-as.integer(factor(Btest$v107))  
Btest$v110<-as.integer(factor(Btest$v110))  
Btest$v112<-as.integer(factor(Btest$v112))  
Btest$v113<-as.integer(factor(Btest$v113))  
Btest$v125<-as.integer(factor(Btest$v125))


## @@@@@@@@@ Delete ID columns  @@@@

Btrain$ID<-NULL
Btest$ID<-NULL
str(BBtrain)
str(Btest)



## @@@@@@@@@@@  Exploratory:  Statistics  @@@@@@@@@@@

install.packages("foreign")
install.packages("xlsx")
install.packages("dplyr")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("GGally)
install.packages("vcd")
library(foreign)
library(xlsx)
library(dplyr)
library(reshape2)
require(ggplot2)
require(GGally)
require(vcd)

# Histogram - Response variable
ggplot(Btrain, aes(x = target)) + geom_histogram()

# Density plots
ggplot(Btrain, aes(x = target)) + geom_density()

# Box plot
ggplot(Btrain, aes(x = 1, y = v50)) + geom_boxplot()

# Conditional Visualization
ggplot(Btrain, aes(x = v50)) + geom_density() + facet_wrap(~ target)

# Box plot - Factor by variable 
ggplot(Btrain, aes(x = factor(target), y = v50)) + geom_boxplot()

# Extented boxplot vizualization

ggplot(melt(Btrain[, 10:15]), aes(x = variable, y = value)) + geom_boxplot() #not incl ID, target

# boxplot by response variable

ggplot(melt(Btrain[, 8:13], id.vars = target),
       aes(x = variable, y = value, fill = factor(target))) +
                 geom_boxplot()

# simple scatter plot /check correlation

xyplot(v10 ~ v50, data = Btrain)


# conditioned scatter plot
xyplot(v6 ~ v50 | target, data = Btrain)

# conditioning on two variables
# xyplot(read ~ write | prog * schtyp, data = d)

# box and whisker plots
bwplot(v50 ~ factor(target), data = Btrain)
                 
# categorical data

xtabs( ~ v3, data = Btrain)

# two-way cross tab

xtabs( ~ v3 + target, data = Btrain)
xtabs( ~ v3 + v91, data = Btrain)
xtabs( ~ v3 + v91 + target, data = Btrain) # 3 way

#Categorical Independence
#We can test whether ses and schtyp are independent with a permutation test from the vcd package.

??
require(vcd)
(tab2 <- xtabs( ~ v3 + v91, data = Btrain))
set.seed(10)
(testtab2 <- coindep_test(tab2, n = 5000))


# Vizualizing categorical data

library(vcd)
install.packages("vcd")
# simple mosaic plot
mosaic(tab2)

# To visually understand whether the variables are independent, let's shade the cells based on pearsonized residuals from what we would observe if the data were independent.

mosaic(tab2, gp = shading_hsv,
  gp_args = list(p.value = testtab2$p.value, interpolate = -1:2))
                 

#viz a three way crosstab

cotabplot(~ ses + prog | schtyp, data = d, panel = cotab_coindep, n = 5000)

#correlations - bivariate

cor(Btrain[,1:50])
cor(Btrain)
cor(d[, 7:11], use = "complete.obs")  #If there are missing data, for listwise deletion, use only complete observations.

cor(Btrain[, 7:11], use = "pairwise.complete.obs") #If there are missing data, for pairwise deletion, 

# Visual Summaries, Continuous Variables
# We can inspect univariate and bivariate relationships using a scatter plot matrix.
# Correlation coefficients whose magnitude are between 0.7 and 0.9 indicate variables which can be considered highly correlated. 

CRtrain<-Btrain

cor(CRtrain[,1:50])

CRtrain$v2  <-NULL
CRtrain$v4  <-NULL
CRtrain$v5  <-NULL
CRtrain$v6  <-NULL
CRtrain$v7  <-NULL
CRtrain$v8  <-NULL
CRtrain$v11  <-NULL
CRtrain$v13  <-NULL
CRtrain$v10  <-NULL
CRtrain$v16  <-NULL
CRtrain$v17  <-NULL
CRtrain$v20  <-NULL
CRtrain$v28  <-NULL
CRtrain$v26  <-NULL
CRtrain$v29  <-NULL
CRtrain$v32  <-NULL
CRtrain$v15  <-NULL
CRtrain$v27  <-NULL
CRtrain$v35  <-NULL
CRtrain$v36  <-NULL
CRtrain$v41  <-NULL
CRtrain$v42  <-NULL
CRtrain$v44  <-NULL
                 CRtrain$v45  <-NULL

                 CRtrain$v48  <-NULL
                 CRtrain$v49  <-NULL
CRtrain$v53  <-NULL
CRtrain$v55  <-NULL
CRtrain$v11  <-NULL
CRtrain$v11  <-NULL
CRtrain$v11  <-NULL
CRtrain$v57  <-NULL
CRtrain$v59  <-NULL
CRtrain$v67  <-NULL
CRtrain$v68  <-NULL
CRtrain$v69  <-NULL
CRtrain$v70  <-NULL
CRtrain$v73  <-NULL
CRtrain$v76  <-NULL
CRtrain$v77  <-NULL
CRtrain$v78  <-NULL
CRtrain$v80  <-NULL
CRtrain$v83  <-NULL
CRtrain$v84  <-NULL
CRtrain$v85  <-NULL
CRtrain$v86  <-NULL
CRtrain$v87  <-NULL
CRtrain$v88  <-NULL
CRtrain$v90  <-NULL
CRtrain$v94  <-NULL
CRtrain$v92  <-NULL
CRtrain$v93  <-NULL
CRtrain$v95  <-NULL
CRtrain$v96  <-NULL
CRtrain$v57  <-NULL
CRtrain$v59  <-NULL
CRtrain$v57  <-NULL
CRtrain$v59  <-NULL


install.packages("GGally")
library(GGally)
ggpairs(Btrain[, 7:11])
                 

# Analyzing relationships between categorical variables
# The events considered must be mutually exclusive and have total probability 1

str(Btrain)

(tab <- xtabs(~ v3 + v91, data = Btrain))
chisq.test(tab)


# ANOVA and Regression # not meaningful for factors
m <- lm(v3 ~ v10, data = Btrain)
anova(m)
                 
#======================

http://www.ats.ucla.edu/stat/r/seminars/intro.htm

#======================


#@@@@@@@@@@@@@@@@ (Optional) impute missing data @@@@@@@@@@@@@

# required libraries
install.packages("Boruta")
library(caret)
library(Boruta)
library(dplyr)

###
# select random sample for analysis using caret createDataPartition() function
                 ###
                 set.seed(123)
                 idx <- createDataPartition(Btrain$target,p=0.01,list=FALSE)
                 sample.df <- Btrain[idx,]


###
# segregate numeric vs character data types
###
# get names of the explanatory variables
explanatory.attributes <- setdiff(names(sample.df),c("ID","target"))
                 
# determine data type for each explanatory variable
data.classes <- sapply(explanatory.attributes,function(x){class(sample.df[,x])})
                 
# segregate explanatory variables by data type, eg. character, numeric, integer
unique.classes <- unique(data.classes)
attr.by.data.types <- lapply(unique.classes,function(x){names(data.classes[data.classes==x])})
names(attr.by.data.types) <- unique.classes
comment(attr.by.data.types) <- "list that categorize training data types"
                 
                 
# for numeric attributes use caret preProcess() and predict() functions to impute missing values
pp <- preProcess(sample.df[c(attr.by.data.types$numeric,attr.by.data.types$integer)],
method=c("medianImpute"))
pp.sample.df <- predict(pp,sample.df[c(attr.by.data.types$numeric,attr.by.data.types$integer)])
                 
# combine numeric data with character data
df <- cbind(pp.sample.df,sample.df[attr.by.data.types$character])


@@@@@@@@@ feature engineering/relevance with Boruta

Analyze sample data with Boruta package to determine relevance of the explanatory variables.

set.seed(13)
bor.results <- Boruta(Btrain,factor(sample.Btrain$target),
maxRuns=101,
doTrace=0)

Btrain$v72 <- NULL
Btrain$v62 <- NULL
Btrain$v112 <- NULL
Btrain$v107 <- NULL
Btrain$v125 <- NULL
Btrain$v75 <- NULL
Btrain$v71 <- NULL
Btrain$v91 <- NULL
Btrain$v74 <- NULL
Btrain$v52 <- NULL
Btrain$v22 <- NULL
Btrain$v3 <- NULL


str(Btrain$v3)
                 




                                                  
                

#(NOT INCL>)@@@@@@@@@@@@@@@@@  K-means for issues/additional features  @@@@@@@@@@@@@@@@@


Btrain$ID<-NULL 

km<-kmeans(Btrain,5,100)
#review output
km
km$size

#create set of number of clusters from 1 to 15
wss<-numeric(10)

#find wss (within-cluster sum of squares) for each kmeans operation for i clusters
for (i in 1:10) wss [i] <-sum(kmeans(Btrain, centers=i)$withinss)

#plot 
plot(1:10, wss, type="b", xlab="No. of clusters", ylab="Within groups sum of squares")


# @@@@@@@@@@@@@@@@@@@  feature scaling /normalization
                 library(caret)

preprocTrain = preProcess(Btrain)           
Dtrain = predict(preprocTrain, Btrain)
                          

preprocTest = preProcess(Btest)
Dtest = predict(preprocTest, Btest)

Dtrain$target<-Btrain$target
Dtest$target<-Btest$target


#@@@@@@@@@@@@@@@@@  Split for local test  @@@@@@@@@@@@@@@@@

#yes

set.seed(15071)
spl = sample(nrow(Dtrain), 0.8*nrow(Dtrain))
Ctrain = Dtrain[spl,]
Ctest = Dtrain[-spl,]


#no


Ctrain = Btrain
Ctest = Btest


Ctrain = CRtrain





#@@@@@@@@@@@@@@@@@  XGBoost  @@@@@@@@@@@@@@@@@


require(xgboost)
## Loading required package: xgboost
require(methods)
## Loading required package: methods
require(data.table)
## Loading required package: data.table
require(magrittr)


 

# @@@@@ Cross-validate XGBoost model 

y1=Ctrain$target
y1

#2
#remove label column from training dataset, otherwise XGBoost would use it to guess the labels!

Ctrain$target<-NULL

#convert both datasets (training and test) in numeric Matrix format.

#trainMatrix <- SS2Train[,lapply(.SD,as.numeric)] %>% as.matrix
#testMatrix <- SSTest[,lapply(.SD,as.numeric)] %>% as.matrix

BtrainMatrix <- as.matrix(Ctrain)#[,lapply(.SD,as.numeric)] %>% 
BtestMatrix <- as.matrix(Ctest) #[,lapply(.SD,as.numeric)] %>% 

#1.	use the cross validation to evaluate the our error rate

numberOfClasses <- max(y1) + 1


print( difftime( Sys.time(), start_time, units = 'sec'))
cat("Training a XGBoost classifier with cross-validation\n")

# Cross validation with grid search parameters
bst.cv = xgb.cv(data = as.matrix(BtrainMatrix), 
                label = y1, 
                booster = "gbtree", 
                max.depth = 8,      
                eta=0.01,            
                nrounds = 2000,
                nthread = 2, 
                objective = "binary:logistic",
                eval_metric = "logloss", #auc",
                nfold = 2,
                #early.stop.round=50,
                gamma=1
)

#xgbLinear

"subsample" = 0.9
"colsample_bytree" = 0.9


str(bst.cv)
            7  0.1,
0.1530      6, 0.25, 10, 2
0.1531      5, 0.25, 10, 2 
0.614808    5, 0.01, 10, 2 
0.273027    5, 0.1, 10, 2 
0.206064    5, 0.15, 10, 2 
0.1531      5, 0.25, 10, 2 
0.1359      5, 0.5, 10, 2
0.138559    5, 0.75, 10, 2
0.142292    5, 1, 10, 2 
18.28       5, 10, 10, 2 


# Plot xgb.cv (logloss)
plot(log(bst.cv$test.logloss.mean),type="l") 
str(bst.cv)

### Cross validate using sparse matrix

# Column Improved is excluded because it will be our output column, the one we want to predict.
sparse_matrix = sparse.model.matrix(data = trainMatrix)   # (Improved~.-1, data = df)


# plot (AUC)
plot((bst.cv$test.auc.mean),type="l")

# plot ("test.logloss.mean", "train.logloss.mean"))
library(ggplot2)
library(reshape)

str(dst)
dst<-bst.cv
dst<dst$ID

for (i in 1:nrow(dst)) {
  dst$ID [i]<-i
}
summary(dst)
str(dst)
logloss.long <- melt(dst, id = "ID", measure = c("test.logloss.mean", "train.logloss.mean"))
ggplot(data=logloss.long, aes(ID,value,color = variable)) + 
  geom_line() + 
  theme_bw()

# ============= Train ===========
y1
# version#1
bst <- xgboost(data = BtrainMatrix, label = y1, booster = "gbtree", max.depth = 8, eta=0.01, 
               nrounds = 1120,
               nthread = 2, objective = "binary:logistic",eval_metric = "logloss", gamma=1)
"subsample" = 0.9,
"colsample_bytree" = 0.9,

11:33-12:06 599 rounds


# ============= Predict on Test set ===============

summary(bst)

#predict
preds=predict(bst,BtestMatrix)

#@@@@@@@ Submittal #8 FINAL: XGBOOST - see notes @@@@@@
submissionBNP8 <- data.frame(ID=Btest$ID, PredictedProb=preds)
cat("saving the submission file\n")
write.csv(submissionBNP8, "submissionBNP8.csv")


summary(preds)

names <- dimnames(Btrain)[[2]] #names <- dimnames(SStrain$data)[[2]]
importance_matrix<-xgb.importance(names,model =bst)
xgb.plot.importance(importance_matrix[51:100])


#  ======================= accuracy, logloss, AUC

#accuracy
table(Ctest$target, preds> 0.5)
=TN+TP/(TN+TP+FN+FP)
(487+15588)/(487+15588+4952+1838)
(2066+10505)/(2066+10505+6921+3373)

#logloss
print(-mean(log(preds)*Ctest$target+log(1-preds)*(1-Ctest$target)))

#AUC
library(cvAUC)
auc <- AUC(preds, Ctest$target)
auc 

str(Ctest$target)
summary(bst)


# ========================== Grid search =================

require(xgboost)

library(caret)
library(pROC)

# set up the cross-validated hyper-parameter search

##  The tuning parameter grid should have columns 
nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight

xgb_grid_1 = expand.grid(
  nrounds =500,
  eta = c(0.13, 0.125,0.1,0.01),   #0.3,0.32, 0.34, 0.35, 0.36
  max_depth = c(5, 6, 7, 8),  #5.75, 5.6, 5.65, 5.5
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight =1
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,    #5
  verboseIter = TRUE,
  #returnData = FALSE,
  #returnResamp = "all",                                                        # save losses across all models
  #classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  #summaryFunction = twoClassSummary,
  #allowParallel = TRUE
)

# Error (below): At least one of the class levels is not a valid R variable name
# refactoring the outcome variable will fix the problem.
# levels <- unique(data$outcome) data$outcome <- factor(data$outcome, labels=make.names(levels))
# occurs when classProbs=TRUE which causes the train function to generate additional statistics related to the outcome class

levels <- unique(BBtrain$target)
BBtrain$target <- factor(BBtrain$target, labels=make.names(levels))


summary(BBtrain$target)
BBtrain$target<-Btrain$target
y2<-BBtrain$target
y2
BBtrain$target<-NULL
str(BBtrain$target)
# train the model for each parameter combination in the grid,
# using CV to evaluate

#BBtrain$target<-y2

str(y2)

xgb_train_1 = train(
  x = as.matrix(BBtrain),
  y = as.factor(y2),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree",
  metric = "AUC"
)

#@@@@@@@@@@@@@@@@   #### test

xgb_grid_1 = expand.grid(
  nrounds = 500,
  eta = c(0.125,0.1,0.01,0.001),   #0.3,0.32, 0.34, 0.35, 0.36
  max_depth = c(5, 6, 7, 8),  #5.75, 5.6, 5.65, 5.5
  gamma = 1,
  colsample_bytree = 1,
  min_child_weight =1
)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter=TRUE
)

testFit <- train(as.matrix(BBtrain), y2,
                 method = "xgbTree",
                 tuneGrid = xgb_grid_1,
                 metric = "AUC",
                 trControl = ctrl)


str(testFit)
summary(testFit)

# scatter plot of the AUC against max_depth and eta
ggplot(testFit$results, aes(x = as.factor(eta), y = as.factor(max_depth), size = Accuracy, color = Accuracy)) +
  geom_point() +
  theme_bw() +
  scale_size_continuous(guide = "none")   #</code>

#@@@@@@@@@@@@@@@@   #### test


summary(BBtrain)

x<-0# = as.matrix(BBtrain %>% select(-target))
y<-0#= as.factor(BBtrain$target)

summary(BBtrain$target)
str(y)
str(xgb_train_1)

# scatter plot of the AUC against max_depth and eta
ggplot(testFit$results, aes(x = as.factor(eta), y = max_depth, size = accuracy, color = accuracy)) +
  geom_point() +
  theme_bw() +
  scale_size_continuous(guide = "none")#</code>


# with selected parameters
xbesttrain <- xgboost(data = as.matrix(x), label = y,
                      max.depth = 6,gamma=1, colsample_bytree = 1, min_child_weight = 1,
                      eta = 0.01, nthread = 2, nround = 1000, objective = "binary:logistic")


#@@@@@@@ Submittal #2: XGBOOST - no NA, Cat to fact/int @@@@@@
submissionBNP2 <- data.frame(ID=Btest$ID, PredictedProb=preds)
cat("saving the submission file\n")
write.csv(submissionBNP2, "submissionBNP2.csv")

#@@@@@@@ Submittal #3: XGBOOST - NA=0, Cat to fact/int @@@@@@
submissionBNP3 <- data.frame(ID=Btest$ID, PredictedProb=preds)
cat("saving the submission file\n")
write.csv(submissionBNP3, "submissionBNP3.csv")

str(Btest)
str(preds)

#@@@@@@@ Submittal #4: XGBOOST - NA=0, Cat to fact/int || corrected @@@@@@
submissionBNP4 <- data.frame(ID=Btest$ID, PredictedProb=preds)
cat("saving the submission file\n")
write.csv(submissionBNP4, "submissionBNP4.csv")


#@@@@@@@ Submittal #5: XGBOOST - NA=0, Cat to fact/int || max.depth = 7, eta=0.1, nrounds = 86 @@@@@@
submissionBNP5 <- data.frame(ID=Btest$ID, PredictedProb=preds)
cat("saving the submission file\n")
write.csv(submissionBNP5, "submissionBNP5.csv")


#@@@@@@@ Submittal #6: XGBOOST - NA=0, Cat to fact/int || max.depth = 8, eta=0.01, nrounds = 600 @@@@@@
submissionBNP6 <- data.frame(ID=Bntest$ID, PredictedProb=preds)
cat("saving the submission file\n")
write.csv(submissionBNP6, "submissionBNP6.csv")


#@@@@@@@ Submittal #7: XGBOOST - see notes @@@@@@
submissionBNP7 <- data.frame(ID=Ltest$ID, PredictedProb=preds)
cat("saving the submission file\n")
write.csv(submissionBNP7, "submissionBNP7.csv")


#install.packages("AUC")
library(cvAUC)
auc <- AUC(pred, SStest$TARGET)
auc


#predict on test data
str(FETest)
FETest$ID<-NULL

testMatrix2 <- as.matrix(FETest)
preds=predict(bst,testMatrix2)

preds2=predict(bst2,testMatrix2)





#accuracy
table(BBTest$target, Bpreds > 0.5)
=TN+TP/(TN+TP+FN+FP)
FALSE TRUE
0  2881    7
1   109    3

(2881+3)/(2881+3+7+109)
[1] 0.9613333

View(trees)

#view models
model <- xgb.dump(bst, with.stats = T)
model[1:10]

#get the feature real names
names <- dimnames(trainMatrix)[[2]]
importance_matrix<-xgb.importance(names,model =bst)
xgb.plot.importance(importance_matrix[1:10])

#plot the tree #n_first_tree = 2, #(plot the first two trees)

xgb.plot.tree(feature_name = names,model =bst, n_first_tree = 2)



#@@@@@@@@@@@@@@@@@  BNP - Logistic Regression + LASSO  @@@@@@@@@@@@@@@@@


SSTest
SSTrain

#modeling
Bglm.Sant=glm(target~.,data=BBTrain,family=gaussian)
summary(Bglm.Sant)
Bglm.predTest=predict(Bglm.Sant,newdata=BBTest, type="response")   # predict using model
Bglm.predTest
summary(Bglm.predTest)
head(Bglm.predTest)

#accuracy
table(BBTest$target, Bglm.predTest > 0.18)
=TN+TP/(TN+TP+FN+FP)
(2852+5)/(2852+5+36+107)
[1] 0.9333333 #1k @ 0.5
[1] 0.9523333 #10k @0.2

##plot AUC curve
###?/ convert to 0's and 1's before submitting after seeting threshold


#@@@@@@@@@@@@@@@@@  CART  @@@@@@@@@@@@@@@@@
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

BTree.Sant = rpart(target~.,data=BBTrain, method="class", minbucket=200)
prp(BTree.Sant)
BPredict.Sant = predict(BTree.Sant, newdata = BBTest, type = "class")  
summary(BPredict.Sant)
table(BPredict.Sant)
table(BBTest$target)

#accuracy
table(BBTest$target, BPredict.Sant )
=TN+TP/(TN+TP+FN+FP)
(192+13)/(192+13+36+59)

[1] 0.6833333



#@@@@@@@@@@@@@@@@@  Random Forest  @@@@@@@@@@@@@@@@@

install.packages("randomForest")
library(randomForest)
BStevensForest = randomForest (target~.,data=BBTrain,ntree=200, nodesize=25)   #9:20
summary(BStevensForest)
str(BStevensForest)
# predict
BPredictForest = predict(BStevensForest, newdata = BBTest)
#accuracy
table(BBTest$target, BPredictForest > 0.5)
=TN+TP/(TN+TP+FN+FP)

#@@@@@@@@@@@@@@@@@  Nueral Network  @@@@@@@@@@@@@@@@@

install.packages("nnet")
library(nnet)

Bir.nn2 <- nnet(target~.,data=BBTrain, size = 1, rang = 0.7,decay = 5e-4, maxit = 500)
#table(ird$species[-samp]
BNN.Sander<-predict(Bir.nn2, newdata=BBTest, type = "raw")
summary(BNN.Sander)
str(BNN.Sander)

#accuracy
table(BBTest$Target, BNN.Sander > 0.10)
=TN+TP/(TN+TP+FN+FP)
(2364+63)/(2364+63+49+524)


#@@@@@@@@@@@@@@@@@  Naive Bayes  @@@@@@@@@@@@@@@@@

install.packages("e1071")
library(e1071)

BmodelNB <- naiveBayes(target~.,data=BBTrain, laplace = 0)
summary(BmodelNB)
BpredNB <- predict(BmodelNB, newdata = BBTest,threshold=0.0001, eps=0, type="class")
summary(BpredNB)
table(BBTest$target)
table(BpredNB)
#accuracy
table(BBTest$target, BpredNB > 0.55)
=TN+TP/(TN+TP+FN+FP)

#@@@@@@@@@@@@@@@@@  Support Vector Machines  @@@@@@@@@@@@@@@@@

Bm <- svm(target~.,data=BBTrain, gamma = 0.5)
BpredSVM=predict (Bm, newdata = BBTest)
table(BBTest$target)
table(BpredSVM >0.5)
#accuracy
table(BBTest$TARGET, BpredSVM > 0.55)
=TN+TP/(TN+TP+FN+FP)

# visualize:
plot(X, col = 1:1000 %in% m$index + 1, xlim = c(-5,5), ylim=c(-5,5))
points(newdata, pch = "+", col = 2, cex = 5)


#@@@@@@@@@@@@@@@@@  Ensemble:H20  @@@@@@@@@@@@@@@@@

from weblink

#@@@@@@@@@@@@@@@@@  Ensemble:mboost  @@@@@@@@@@@@@@@@@

BBTrainFACT<-BBTrain
BBTrainFACT$target<-as.factor(BBTrainFACT$target)

install.packages("mboost")
library(mboost)
Bm.boost <-glmboost(target~.,family=Binomial(),data=BBTrainFACT) # Binomial-needed for classification
coef(Bm.boost)

#v1+v22ADNJ+v22ADPU+v22AEMT+v22AUN+v22AV+v22AWA+v22BDT+v22CTB+v22DHC+v22EYN+v22FLR+v22GOK+v22KFR+v22KHF+v22KZE+v22MDQ+v22NLH+v22PBC+v22QZ+v22ST+v22TIB+v22TPN+v22TYR+v22UHI+v22UNS+v22UVM+v22XEL+v22XFA+v22XHE+v22XJQ+v22XZO+v22YGJ+v22YXN+v22ZCY+v30C+v30E+v47I+v50+v66B+v82+v112C+v113AB+v113AG+v114+v125AR+v125BG+v125BX+v125L 
#v1+v22+v30+v47+v50+v66+v82+v112+v113+v114+v125 


plot(Bm.boost, ylim=range(coef(Bm.boost,which=c("v1","v22","v30","v47","v50","v66","v82","v112","v113","v114","v125"))))

Bmboostpred=predict(Bm.boost,newdata=BBTest, type="response")   # predict using model 12:47
summary(mboostpred)
head(mboostpred)

#accuracy
table(SSTest$TARGET, Bmboostpred > 0.09)
=TN+TP/(TN+TP+FN+FP)
(276+2)/(276+2+7+15) #0.09  [1] 0.9266667
(275+3)/(275+3+8+14) # 0.08     [1] 0.9266667
(270+5)/(270+5+12+13) #0.07     [1] 0.9166667


Bcv.boost <-cvrisk(Bm.boost)
mstop(Bcv.boost)

plot(Bcv.boost, main="Cross-validated estimates of empirical risk")

#@@@@@@@@@@@@@@@@@  Ensemble:Gamboost  @@@@@@@@@@@@@@@@@

Bmb.boost <-gamboost(target~v1+v22+v30+v47+v50+v66+v82+v112+v113+v114+v125,family=Binomial(),data=BBTrainFACT,dfbase = 10,control = boost_control(mstop = 50))
# needed for classification 12:49
Bmb.boost

Bgamboostpredict=predict(Bmb.boost,newdata=BBTest, type="response")   # predict using model
summary(Bgamboostpredict)
head(Bgamboostpredict)

#accuracy
table(BBTest$target, glm.predTest > 0.5)
=TN+TP/(TN+TP+FN+FP)


#@@@@@@@@@@@@@@@@@  Ensemble:Adaboost  @@@@@@@@@@@@@@@@@

install.packages("ada")
library(ada)
Bm.ada <-ada(target~v1+v22+v30+v47+v50+v66+v82+v112+v113+v114+v125,data=BBTrain,iter=50)

Bm.ada.test <-addtest(Bm.ada,test.x=BBTest,BBTest$target)

Bm.ada.test

plot(Bm.ada.test, test=TRUE)
varplot(Bm.ada.test, max.var.show=5)# first 5 variables

(146+524)/(146+524+4+26) #[1] 0.9571429 
[1] 0.9571429

#@@@@@@@@@@@@@@@@@  Ensemble:Superlearner  @@@@@@@@@@@@@@@@@

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
                "SL.randomForest")
method <- "method.NNLS"
family <- "binomial"

BBTrainY<-as.vector(BBTrain$target)   #Target vector Y

#How to train & test
BfitSL <- SuperLearner(BBTrainY, BBTrain,BBTest,
                       family = family,
                       SL.library = SL.library,
                       method = method)
BfitSL$SL.predict [1:10]

#accuracy
table(BBTest$target, BfitSL$SL.predict > 0.5)
=TN+TP/(TN+TP+FN+FP)
summary(BfitSL)


#Result:  Superlearner ("SL.knn","SL.glm", "SL.randomForest")  1k  0.5  Accuracy =1.0 

#@@@@@@@@@@@@@@@@@  Ensemble:Subsemble  @@@@@@@@@@@@@@@@@

library(subsemble)

By<-as.vector(BBTrain$target)   #Target vector Y
Bx<-BBTrain

#Set up the ensemble
learner <- c("SL.knn",
             "SL.glm",
             "SL.randomForest")
metalearner <- "SL.nnls"
family <- "binomial"
subsets <- 2

#How to train & test
Bfit <- subsemble(x = Bx, y = By,BBTest,
                  family = family,
                  learner = learner,
                  metalearner = metalearner,
                  subsets = subsets)

str(fit$pred)
#accuracy
table(SSTest$TARGET, fit$pred > 0.5)
=TN+TP/(TN+TP+FN+FP)
summary(fitSL)

## 21 Subsemble (""SL.knn","SL.glm","SL.randomForest") subsets <- 2  Threshold=0.5  Accuracy =1.0

#pred <- predict(fit, SSTest)


#@@@@@@@@@@@@@@@@@  more ....  @@@@@@@@@@@@@@@@@


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


###################################################

#Submit#1 adaboost with mboost variables on test/Trained on 100k ######################

Bm.ada <-ada(target~v1+v30+v47+v50+v66+v82+v112+v114+v125,data=Btrain,iter=50)

BM.predict<-predict(Bm.ada, newdata=Btest)

table(BM.predict)
str(Btest)

BNPsubmission1 <- data.frame(ID=Btest$ID, PredictedProb=BM.predict)
cat("saving the submission file\n")
write.csv(BNPsubmission1, "BNPsubmission1.csv")


#Submit#1 adaboost with mboost variables on test/trained on ALL ######################

Bm.ada <-ada(target~v1+v22+v30+v47+v50+v66+v82+v112+v113+v114+v125,data=BBTrain,iter=50)

Bm.ada.test <-addtest(Bm.ada,test.x=BBTest,BBTest$target)

Bm.ada.test

plot(Bm.ada.test, test=TRUE)
varplot(Bm.ada.test, max.var.show=5)# first 5 variables

(146+524)/(146+524+4+26) #[1] 0.9571429 
[1] 0.9571429


submission <- data.frame(QuoteNumber=test$QuoteNumber, QuoteConversion_Flag=pred1)
cat("saving the submission file\n")
write.csv(submission, "predGBSplit.csv")


# ############## ############ ############ step ###########  ########## 

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

