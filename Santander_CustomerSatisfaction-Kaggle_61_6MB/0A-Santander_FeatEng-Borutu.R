### Analyze sample data with Boruta package to determine relevance of the explanatory variables.

set.seed(13)
bor.results <- Boruta(df,factor(sample.df$target),
                      maxRuns=101,
                      doTrace=0)

###
# select random sample for analysis using caret createDataPartition() function
###
set.seed(123)
idx <- createDataPartition(train$target,p=0.01,list=FALSE)
sample.df <- train[idx,]

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


