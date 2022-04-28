Strain=read.csv("train.csv")
Stest=read.csv("santander_test.csv")

# Modules
require(xgboost)
## Loading required package: xgboost
require(methods)
## Loading required package: methods
require(data.table)
## Loading required package: data.table
require(magrittr)
## Loading required package: magrittr

# Check the content of the last column
Strain[1:6, ncol(Strain)]

# Create response variable
#nameLastCol <- names(Strain)[ncol(Strain)]
#nameLastCol  #TARGET

# Convert classes to integers XGBoost doesn’t support anything else than numbers.. 
#Moreover, according to the documentation, it should start at 0.
#y <- Strain[, nameLastCol][[1]] %>% gsub('Class_','',.) %>% {as.integer(.) -1}
y=Strain$TARGET

# Remove label column from training dataset, otherwise XGBoost would use it to guess the labels!
#Strain$nameLastCol<-NULL
#Strain$nameLastCol
Strain$TARGET<-NULL

# Convert both datasets (training and test) in numeric Matrix format.
#data.frame is not a format supported natively by XGBoost.

#trainMatrix <- Strain[,lapply(.SD,as.numeric)] %>% as.matrix
#testMatrix <- Stest[,lapply(.SD,as.numeric)] %>% as.matrix

trainMatrix <- as.matrix(Strain)
testMatrix <- as.matrix(Stest)

# Use the cross validation to evaluate the our error rate

#Nrounds – should run to 400 (i.e. #of trees it grows)
#Log los – minimize
#Eta – learning rate
#Max.depth – tree depth
# Look at test log-loss (look for when log loss in increasing, from decreasing

numberOfClasses <- max(y) + 1
                             
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = numberOfClasses)
                             
cv.nround <- 5
cv.nfold <- 3
                             
bst.cv = xgb.cv(param=param, data = as.matrix(trainMatrix), label = y, 
                            nfold = cv.nfold, nrounds = cv.nround)

# Plot xgb.cv
plot(log(bst.cv$test.mlogloss.mean),type="l") 
                             
                             
# Train model with Optimum # of rounds (i.e. # of trees)
#Thread=no. of cores

nround = 50
bst = xgboost(param=param, data = trainMatrix, label = y, nrounds=nround)
                             
#predict
Preds=predict(bst,trainMatrix)
                             
                             Preds[1:10]
                             
                             
                             Log loss
                             Print(-mean(log(preds)*test$label+log(1-preds)*(1-testlabel)))
                             
                             
                             
                             Trees = xgb.model.dt.tree(dimnames(train$data)[[2]], model = bst)
                             
                             Trees = xgb.model.dt.tree(dimnames(train$names)[[2]], model = bst)
                             
                             View(Trees)
                             
                             
                             
                             Yes->0-1 (ID# - leaf/feature)
                                       N/A’s – missing values/robust to missing values
                                       
                                       
                                       
                                       View model
                                       
                                       
                                       Each line is a branch
                                       
                                       model <- xgb.dump(bst, with.stats = T)
                                       model[1:10]
                                       
                                       
                                       [1] "booster[0]"                                                         
                                       [2] "0:[f16<1.5] yes=1,no=2,missing=1,gain=309.719,cover=12222.8"        
                                       [3] "1:[f29<26.5] yes=3,no=4,missing=3,gain=161.964,cover=11424"         
                                       [4] "3:[f77<2.5] yes=7,no=8,missing=7,gain=106.092,cover=11416.3"        
                                       …..
                                       Get the feature real names:
                                         (Feature selection)
                                       
                                       
                                       
                                       importance_matrix[1:10]<-based on importance matrix
                                       
                                       Names <- dimnames(train$data)[[2]]
                                       importance_matrix<-xgb.importance(names,model =bst)
                                       xgb.plot.importance(importance_matrix[1:10])
                                       
                                       
                                       
                                       
                                       Plot the tree
                                       
                                       n_first_tree = 2
                                       (plot the first two trees)
                                       
                                       
                                       xgb.plot.tree(feature_name = names,model =bst, n_first_tree = 2)
                                       
                                       
                                       
                                       
                                       # plot the AUC for the training and testing samples
                                       library(dplyr)
                                       library(tidyr)

                                                                       
xgb_cv_1$dt %>%
                                         select(-contains("std")) %>%                  #dplyr - select contains
                                         mutate(IterationNum = 1:n()) %>%              #dplyr - add columns that is a function fo existing columns
                                         gather(TestOrTrain, AUC, -IterationNum) %>%   #tidyr -  create key value pair
                                         ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
                                         geom_line() + 
                                         theme_bw()
                                       
                                       
                                       