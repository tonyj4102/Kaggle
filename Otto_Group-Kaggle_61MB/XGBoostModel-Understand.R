1 Introduction

XGBoost is an implementation of the famous gradient boosting algorithm. This model is often described as a blackbox, meaning it works well but it is not trivial to understand how. Indeed, the model is made of hundreds (thousands?) of decision trees. You may wonder how possible a human would be able to have a general view of the model?

While XGBoost is known for its fast speed and accurate predictive power, it also comes with various functions to help you understand the model. The purpose of this RMarkdown document is to demonstrate how easily we can leverage the functions already implemented in XGBoost R package. Of course, everything showed below can be applied to the dataset you may have to manipulate at work or wherever!
  
  First we will prepare the Otto dataset and train a model, then we will generate two vizualisations to get a clue of what is important to the model, finally, we will see how we can leverage these information.

2 Preparation of the data

This part is based on the R tutorial example by Tong He

First, let’s load the packages and the dataset.

install.packages("xgboost")
require(xgboost)
## Loading required package: xgboost
require(methods)
## Loading required package: methods
install.packages("data.table")
require(data.table)
## Loading required package: data.table
require(magrittr)
## Loading required package: magrittr
train <- fread('train.csv', header = T, stringsAsFactors = F)
test <- fread('test.csv', header=TRUE, stringsAsFactors = F)
magrittr and data.table are here to make the code cleaner and much more rapid.

Let’s explore the dataset.

# Train dataset dimensions
dim(train)
## [1] 61878    95
# Training content
train[1:6,1:5, with =F]
##    id feat_1 feat_2 feat_3 feat_4
## 1:  1      1      0      0      0
## 2:  2      0      0      0      0
## 3:  3      0      0      0      0
## 4:  4      1      0      0      1
## 5:  5      0      0      0      0
## 6:  6      2      1      0      0
# Test dataset dimensions
dim(train)
## [1] 61878    95
# Test content
test[1:6,1:5, with =F]
##    id feat_1 feat_2 feat_3 feat_4
## 1:  1      0      0      0      0
## 2:  2      2      2     14     16
## 3:  3      0      1     12      1
## 4:  4      0      0      0      1
## 5:  5      1      0      0      1
## 6:  6      0      0      0      0
We only display the 6 first rows and 5 first columns for convenience

Each column represents a feature measured by an integer. Each row is an Otto product.

Obviously the first column (ID) doesn’t contain any useful information.

To let the algorithm focus on real stuff, we will delete it.

# Delete ID column in training dataset
train[, id := NULL]

# Delete ID column in testing dataset
test[, id := NULL]
According to its description, the Otto challenge is a multi class classification challenge. We need to extract the labels (here the name of the different classes) from the dataset. We only have two files (test and training), it seems logical that the training file contains the class we are looking for. Usually the labels is in the first or the last column. We already know what is in the first column, let’s check the content of the last one.

# Check the content of the last column
train[1:6, ncol(train), with  = F]
##     target
## 1: Class_1
## 2: Class_1
## 3: Class_1
## 4: Class_1
## 5: Class_1
## 6: Class_1
# Save the name of the last column
nameLastCol <- names(train)[ncol(train)]
nameLastCol
The classes are provided as character string in the 94th column called target. As you may know, XGBoost doesn’t support anything else than numbers. So we will convert classes to integers. Moreover, according to the documentation, it should start at 0.

For that purpose, we will:
  
  extract the target column
remove “Class_” from each class name
convert to integers
remove 1 to the new value
# Convert from classes to numbers
y <- train[, nameLastCol, with = F][[1]] %>% gsub('Class_','',.) %>% {as.integer(.) -1}
# Display the first 5 levels
y[1:5]
## [1] 0 0 0 0 0
We remove label column from training dataset, otherwise XGBoost would use it to guess the labels!
  
  train[, nameLastCol:=NULL, with = F]
data.table is an awesome implementation of data.frame, unfortunately it is not a format supported natively by XGBoost. We need to convert both datasets (training and test) in numeric Matrix format.

trainMatrix <- train[,lapply(.SD,as.numeric)] %>% as.matrix
testMatrix <- test[,lapply(.SD,as.numeric)] %>% as.matrix
3 Model training

Before the learning we will use the cross validation to evaluate the our error rate.

Basically XGBoost will divide the training data in nfold parts, then XGBoost will retain the first part and use it as the test data. Then it will reintegrate the first part to the training dataset and retain the second part, do a training and so on…

Look at the function documentation for more information.

numberOfClasses <- max(y) + 1

param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)

cv.nround <- 5
cv.nfold <- 3

bst.cv = xgb.cv(param=param, data = trainMatrix, label = y, 
                nfold = cv.nfold, nrounds = cv.nround)
## [0]  train-mlogloss:1.540662+0.002251    test-mlogloss:1.555795+0.005016
## [1]  train-mlogloss:1.282451+0.000741    test-mlogloss:1.306336+0.004621
## [2]  train-mlogloss:1.113019+0.003081    test-mlogloss:1.143448+0.002971
## [3]  train-mlogloss:0.991797+0.003887    test-mlogloss:1.028172+0.001182
## [4]  train-mlogloss:0.899973+0.002423    test-mlogloss:0.941580+0.001557
As we can see the error rate is low on the test dataset (for a 5mn trained model).

Finally, we are ready to train the real model!!!
  
plot(log(bst.cv$test.logloss.mean),type="l")
plot(log(bst.cv$test.mlogloss.mean),type="l")  
  
  nround = 50
bst = xgboost(param=param, data = trainMatrix, label = y, nrounds=nround)
## [0]  train-mlogloss:1.539929
## [1]  train-mlogloss:1.284352
## [2]  train-mlogloss:1.116242
## [3]  train-mlogloss:0.997410
## [4]  train-mlogloss:0.908786
## [5]  train-mlogloss:0.837502
## [6]  train-mlogloss:0.780620
## [7]  train-mlogloss:0.735472
## [8]  train-mlogloss:0.696930
## [9]  train-mlogloss:0.666730
## [10] train-mlogloss:0.641023
## [11] train-mlogloss:0.618734
## [12] train-mlogloss:0.599407
## [13] train-mlogloss:0.583202
## [14] train-mlogloss:0.568400
## [15] train-mlogloss:0.555463
## [16] train-mlogloss:0.543348
## [17] train-mlogloss:0.532382
## [18] train-mlogloss:0.522701
## [19] train-mlogloss:0.513794
## [20] train-mlogloss:0.506249
## [21] train-mlogloss:0.497970
## [22] train-mlogloss:0.491400
## [23] train-mlogloss:0.484099
## [24] train-mlogloss:0.477010
## [25] train-mlogloss:0.470935
## [26] train-mlogloss:0.466101
## [27] train-mlogloss:0.461392
## [28] train-mlogloss:0.456607
## [29] train-mlogloss:0.450932
## [30] train-mlogloss:0.446368
## [31] train-mlogloss:0.442488
## [32] train-mlogloss:0.437648
## [33] train-mlogloss:0.433682
## [34] train-mlogloss:0.428969
## [35] train-mlogloss:0.424687
## [36] train-mlogloss:0.421398
## [37] train-mlogloss:0.418917
## [38] train-mlogloss:0.415504
## [39] train-mlogloss:0.411823
## [40] train-mlogloss:0.407470
## [41] train-mlogloss:0.404227
## [42] train-mlogloss:0.401174
## [43] train-mlogloss:0.397705
## [44] train-mlogloss:0.394443
## [45] train-mlogloss:0.392279
## [46] train-mlogloss:0.389940
## [47] train-mlogloss:0.387887
## [48] train-mlogloss:0.385097
## [49] train-mlogloss:0.382814


# # # # # #
#
#   PREDICT
# 
# # # # # # #

preds=predict(bst,trainMatrix)
preds[1:10]

#log loss

print(-mean(log(preds)*test$target+log(1-preds)*(1-test$target))) # need split test with responses

str(test)

Trees = xgb.model.dt.tree(dimnames(train$names)[[2]], model = bst)



names <- dimnames(train)[[2]]
Importance_matrix<-xgb.importance(names,model =bst)
xgb.plot.importance(Importance_matrix[1:10])

#Error: Ckmeans.1d.dp package is required for plotting the importance

install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)

#plot the tree
xgb.plot.tree(feature_name = names,model =bst, n_first_tree = 2)

#Error: DiagrammeR package is required for xgb.plot.tree
install.packages("DiagrammeR")
library("DiagrammeR")

4 Model understanding

4.1 Feature importance

So far, we have built a model made of 50 trees.

To build a tree, the dataset is divided recursively several times. At the end of the process, you get groups of observations (here, these observations are properties regarding Otto products).

Each division operation is called a split.

Each group at each division level is called a branch and the deepest level is called a leaf.

In the final model, these leafs are supposed to be as pure as possible for each tree, meaning in our case that each leaf should be made of one class of Otto product only (of course it is not true, but that’s what we try to achieve in a minimum of splits).

Not all splits are equally important. Basically the first split of a tree will have more impact on the purity that, for instance, the deepest split. Intuitively, we understand that the first split makes most of the work, and the following splits focus on smaller parts of the dataset which have been missclassified by the first tree.

In the same way, in Boosting we try to optimize the missclassification at each round (it is called the loss). So the first tree will do the big work and the following trees will focus on the remaining, on the parts not correctly learned by the previous trees.

The improvement brought by each split can be measured, it is the gain.

Each split is done on one feature only at one value.

Let’s see what the model looks like.

model <- xgb.dump(bst, with.stats = T)
model[1:10]
##  [1] "booster[0]"                                                         
##  [2] "0:[f16<1.5] yes=1,no=2,missing=1,gain=309.719,cover=12222.8"        
##  [3] "1:[f29<26.5] yes=3,no=4,missing=3,gain=161.964,cover=11424"         
##  [4] "3:[f77<2.5] yes=7,no=8,missing=7,gain=106.092,cover=11416.3"        
##  [5] "7:[f52<12.5] yes=13,no=14,missing=13,gain=43.1389,cover=11211.9"    
##  [6] "13:[f76<1.5] yes=25,no=26,missing=25,gain=37.407,cover=11143.5"     
##  [7] "25:[f16<2.00001] yes=49,no=50,missing=50,gain=36.3329,cover=10952.1"
##  [8] "49:leaf=-0.0905567,cover=1090.77"                                   
##  [9] "50:leaf=-0.148413,cover=9861.33"                                    
## [10] "26:[f83<26] yes=51,no=52,missing=52,gain=167.766,cover=191.407"
For convenience, we are displaying the first 10 lines of the model only.

Clearly, it is not easy to understand what it means.

Basically each line represents a branch, there is the tree ID, the feature ID, the point where it splits, and information regarding the next branches (left, right, when the row for this feature is N/A).

Hopefully, XGBoost offers a better representation: feature importance.

Feature importance is about averaging the gain of each feature for all split and all trees.

Then we can use the function xgb.plot.importance.

# Get the feature real names
names <- dimnames(trainMatrix)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Nice graph
xgb.plot.importance(importance_matrix[1:10,])


To make it understandable we first extract the column names from the Matrix.

4.2 Interpretation

In the feature importance above, we can see the first 10 most important features.

This function gives a color to each bar. Basically a K-means clustering is applied to group each feature by importance.

From here you can take several actions. For instance you can remove the less important feature (feature selection process), or go deeper in the interaction between the most important features and labels.

Or you can just reason about why these features are so importat (in Otto challenge we can’t go this way because there is not enough information).

4.3 Tree graph

Feature importance gives you feature weight information but not interaction between features.

XGBoost R package have another useful function for that. Note that you need to scroll the screen to right to see these trees due to layout of the rmarkdown.

xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)
