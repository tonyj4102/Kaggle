#sci kit learn code

# @@@@@@@@@@@@@@@@   Cleaning  @@@@@@@@@@@@@@@@

### Examine NAs using mice and VIM packages ###

# read file and convert char to int
library(readr)
train <- read_csv("../input/train.csv")
for (f in names(train)) {
  if (class(train[[f]])=="character") {
    levels <- unique(train[[f]])
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
  }
}

# make a table of missing values
# library(mice)
# missers <- md.pattern(train[, -c(1:2)])
# head(missers)
# write_csv(as.data.frame(missers),"NAsTable.csv")

# make plots of missing values
library(VIM)

png(filename="NAsPatternEq.png",
    type="cairo",
    units="in",
    width=12,
    height=6.5,
    pointsize=10,
    res=300)

miceplot1 <- aggr(train[, -c(1:2)], col=c("dodgerblue","dimgray"),
                  numbers=TRUE, combined=TRUE, varheight=FALSE, border="gray50",
                  sortVars=TRUE, sortCombs=FALSE, ylabs=c("Missing Data Pattern"),
                  labels=names(train[-c(1:2)]), cex.axis=.7)
dev.off()

png(filename="NAsPatternAdj.png",
    type="cairo",
    units="in",
    width=12,
    height=6.5,
    pointsize=10,
    res=300)

miceplot2 <- aggr(train[, -c(1:2)], col=c("dodgerblue","dimgray"),
                  numbers=TRUE, combined=TRUE, varheight=TRUE, border=NA,
                  sortVars=TRUE, sortCombs=FALSE, ylabs=c("Missing Data Pattern w/ Height Adjustment"),
                  labels=names(train[-c(1:2)]), cex.axis=.7)
dev.off()





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

####


# label encode the categorical variables
for i in range(train.shape[1]):
  if i in [0,3,5,11,12,13,14,15,16,20,22,24,26,28,30,32,34]:
  print(i,list(train[1:5,i]) + list(test[1:5,i]))
lbl = preprocessing.LabelEncoder()
lbl.fit(list(train[:,i]) + list(test[:,i]))
train[:,i] = lbl.transform(train[:,i])
test[:,i] = lbl.transform(test[:,i])


###

cat("replacing missing values with -1\n")
train[is.na(train)] <- -1
test[is.na(test)]   <- -1


##
The proportion of NA values.

length(train[is.na(train)])/(ncol(train)*nrow(train)) 

##
Check for dupicate rows.

nrow(train) - nrow(unique(train))

##
Lets look at the columns with only one unique value.

col_ct = sapply(train, function(x) length(unique(x)))
cat("Constant feature count:", length(col_ct[col_ct==1]))

## Constant feature count: 5

# we can just remove these columns
train = train[, !names(train) %in% names(col_ct[col_ct==1])]


#number of unique vales per column

Now lets look at the number of unique values per column.

num_ct = sapply(train_numr, function(x) length(unique(x)))
char_ct = sapply(train_char, function(x) length(unique(x)))
date_ct = sapply(train_date, function(x) length(unique(x)))
all_ct = rbind(data.frame(count=num_ct, type="Numerical"), 
               data.frame(count=char_ct, type="Character"), 
               data.frame(count=date_ct, type="Date"))

# lets plot the unique values per feature
g1 = ggplot(all_ct, aes(x = count, fill=type)) + 
  geom_histogram(binwidth = 1, alpha=0.7, position="identity") + 
  xlab("Unique values per feature (0-100)")+ theme(legend.position = "none") + 
  xlim(c(0,100)) +theme(axis.title.x=element_text(size=14, ,face="bold"))
g2 = ggplot(all_ct, aes(x = count, fill=type)) +  
  geom_histogram(binwidth = 100, alpha=0.7, position="identity") + 
  xlab("Unique values per feature(101+)")  + xlim(c(101,nrow(train))) +
  theme(axis.title.x=element_text(size=14, ,face="bold"))
grid.arrange(g1, g2, ncol=2)


#number of N/A's per count

Lets look at the number of NA’s per feature type (Numeric, Character or String).

num_na = sapply(train_numr, function(x) sum(is.na(x)))
char_na = sapply(train_char, function(x) sum(is.na(x)))
date_na = sapply(train_date, function(x) sum(is.na(x)))
all_na = rbind(data.frame(count=num_na, type="Numerical"), 
               data.frame(count=char_na, type="Character"), 
               data.frame(count=date_na, type="Date"))

#table(all_na)
all_na = data.frame(all_na)
all_na = all_na[all_na$count>0,]

breaks <- c(5,10,50,100,500,1000,2000)

ggplot(all_na, aes(x = count, fill=type)) +  
  geom_histogram(alpha=0.7) + 
  #  scale_y_log10(limits=c(1,2000), breaks=breaks) + 
  scale_x_log10(limits=c(1,20000), breaks=c(breaks,5000,10000,20000)) + 
  labs(title="Histogram of feature count per NA count", size=24, face="bold") +
  theme(plot.title = element_text(size = 16, face = "bold")) +
  xlab("NA Count") + ylab("Feature Count")


# Now we drill down on the numerical values. We randomly sample the rows to look at.

set.seed(200)
train_numr_samp = train_numr[,sample(1:ncol(train_numr),100)]
str(lapply(train_numr_samp[,sample(1:100)], unique))


# Lets take a look at 100 sample numerical records. We shall impute -99999999 to the missing values and check for non-unique columns

train_numr_samp[is.na(train_numr_samp)] = -99999999
length(colnames(train_numr[,sapply(train_numr, function(v) var(v, na.rm=TRUE)==0)]))

## [1] 41


## Check the highly correlated numerical values from this sampled set. We get quite different results using different metrics.

#compute the correlation matrix
descrCor_pear <-  cor(scale(train_numr_samp,center=TRUE,scale=TRUE), method="pearson")
descrCor_spea <-  cor(scale(train_numr_samp,center=TRUE,scale=TRUE), method="spearman")
# Kendall takes to long to run
# descrCor_kend <-  cor(scale(train_numr_samp,center=TRUE,scale=TRUE), method="kendall")
#visualize the matrix, clustering features by correlation index.
corrplot(descrCor_pear, order = "hclust", mar=c(0,0,1,0), tl.pos="n", main="Pearson correlation of 100 sampled numerical features")
corrplot(descrCor_spea, order = "hclust", mar=c(0,0,1,0), tl.pos="n", main="Spearman correlation of 100 sampled numerical features")


## Now lets check the cumulative distribution of correlation within these 100 sampled numericalcolumns. The below plot shows the proportion of features containing a max correlation to another feature below the correlation threshold.

corr.df = expand.grid(corr_limit=(0:100)/100, perc_feat_pear=NA, perc_feat_spea=NA)
for(i in 0:100){
  corr.df$perc_feat_pear[i+1]=length(findCorrelation(descrCor_pear, i/100))
  corr.df$perc_feat_spea[i+1]=length(findCorrelation(descrCor_spea, i/100))
}

plot(corr.df$corr_limit, abs(100-corr.df$perc_feat_pear), pch=19, col="blue",
     ylab = "Feature % falling below abs(correlation)", xlab="Absolute Correlation",
     main="Cumulative distribution of correlation\n(Within 100 sampled numerical features)")
abline(h=(0:10)*10, v=(0:20)*.05, col="gray", lty=3)
points(corr.df$corr_limit, abs(100-corr.df$perc_feat_pear), pch=19, col="blue")
points(corr.df$corr_limit, abs(100-corr.df$perc_feat_spea), pch=19, col="red")
legend("topleft", c("Pearson", "Spearman"), col = c("blue", "red"), pch = 19, bg="white")


#Finally lets take a look at our target.

hist(y, main="Binary Target")


# A simple script for visualizing the location of NA values in your data frame.

library(readr)
basic.mat <- read_csv('../input/train.csv', n_max=23208)

na.idx <- is.na(basic.mat)
null.idx <- basic.mat == ''
neg.1 <- basic.mat == -1

large.idx1 <- basic.mat > 99999999
large.idx2 <- basic.mat <= 999999999
large.idx <- large.idx1 & large.idx2


large.neg <- basic.mat <= -99999
nines3 <- basic.mat == 999
nines2 <- basic.mat == 99

any.three <- na.idx | null.idx | large.idx
basic.mat[!any.three] <- 0

basic.mat[large.idx] <- 4
basic.mat[large.neg] <- 5
basic.mat[neg.1] <- 3
basic.mat[null.idx] <- 2
basic.mat[na.idx] <- 1
basic.mat[nines3] <- 6
basic.mat[nines2] <- 7

basic.mat[basic.mat == 0] <- NA
gc()

#basic.mat <- type_convert(basic.mat, col_types=cols)
basic.mat <- type_convert(basic.mat, col_types=paste0(rep('i', 55), collapse=''))

basic.mat <- as.matrix(basic.mat)
gc()

unis <- unique(as.vector(basic.mat))
len <- sum(complete.cases(unis))
print(len)

cols = rainbow(len)

image(t(basic.mat)[, ncol(basic.mat):1], col=cols, xaxt='n', yaxt='n')

legend(x='topright', legend=c('NA values', 'Empty Strings', '-1', 'Very Large Numbers',
                              '-99999 (and less)', '999', '99'), fill=cols)

It could be that you converted everything correctly but you just need to make sure that the matrix you're passing in is not a character matrix. Basically, if you start with a data frame that has character columns and convert every element to be '1', the elements in those character columns will technically be the character '1' and not the number '1'. Then, if you try converting that data frame into a matrix it will end up with all characters since matrices can only be one type and R will, in that case, coerce all numbers to be characters.

One quick side note: If you're going to run this on your own computer make sure that you test it out with only a few hundred rows and also, if you want to plot the whole data frame, try creating your plot with png and not plot it directly to the screen...


## Unabashedly separating the numeric & non-numeric columns

The only problem is that the numeric columns can also be converted to factors
In [6]:
  
train_num <- train[,names(train)[which(sapply(train, is.numeric))], with = FALSE]
train_char <- train[,names(train)[which(sapply(train, is.character))], with = FALSE]
cat("Numerical column count : ", dim(train_num)[2], 
    "; Character column count : ", dim(train_char)[2])

Numerical column count :  269 ; Character column count :  28


## #Converting the characters to factors
train_char <- train_char[,(names(train_char)) := lapply(.SD, as.factor), .SDcols =names(train_char)]
str(train_char)

## Checking for relationship between date variables & quote conversion

temp <- train_char %>% group_by(OQD_Year) %>% summarise(Conversion = sum(QuoteConversion_Flag), countR = n()) %>% arrange(-Conversion)
temp$Conversion = (temp$Conversion/sum(temp$Conversion))
temp$countR = (temp$countR/sum(temp$countR))
par(mar=c(2,2,2,2), mfrow=c(4,2))
barplot(temp$countR, names.arg = temp$OQD_Year, main = "Count-Year")
barplot(temp$Conversion, names.arg = temp$OQD_Year, main = "Conversions-Year")


##  Calculating the Spearman correlation for the numeric variables

In [17]:
  
  #Selecting only complete cases, so as to avoid errors during correlation calculations
  temp = train_num[complete.cases(train_num),]
temp = temp[,2:270, with=FALSE] #excluding the QuoteNumber (key field)

#Converting the columns to numeric ::: just in case
temp <- temp[,(names(temp)) := lapply(.SD, as.numeric), .SDcols =names(temp)]

#Selecting 50 columns randomly (again for aesthetic reasons :) )
selectedCols <- sample(names(temp), 50)
temp2 <- temp[, (selectedCols), with=FALSE]

#Calculate correlation coeeficients
descrCor_spear <- cor(scale(temp2,center=TRUE,scale=TRUE), method="spearman")

#Not sure if this is the best method, but here I am simply converting NAs in the data to 0
sum(is.na(descrCor_spear))
descrCor_spear[is.na(descrCor_spear)] = 0

#library(corrplot)
corrplot(descrCor_spear, order = "hclust", type="lower", diag=FALSE, tl.cex=0.6, 
         mar = c(0.4,0.4,0.4,0.4),
         main="Spearman correlation of 50 random numerical features")


## Cleaning data 

We establish a valid range of ages as (14, 100) and assume that any values between 1919 and 1995 are birth years. Any ages outside the valid range are set to NaN.

##  cleaning questions


Is there any mistakes in the data?
Does the data have peculiar behavior?
Do I need to fix or remove any of the data to be more realistic?


# @@@@  one-hot-encoding features
ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)


# @@@@@  Examining the structure of the training data
str(train)
summary(train)
length(unique(train$id))
# reformat/clean data
train$date_account_created <- as.Date(train$date_account_created)
#coded_gender <- dummy.code(train$gender)  # uses pysch library
# what data is missing
md.pattern(train)  # 125,461 obs are complete
aggr_plot_train <- aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(train), cex.axis=.7, gap=3, ylab=c("Histogram of missing training data","Pattern"))


# filter out bad ages
too_young_train <- subset(train, age < 18)  # 158 obs
summary(too_young_train$age)
hist(too_young_train$age)
summary(too_young_train$country_destination)


##@@@@ heatmap

require(data.table)
require(ggplot2)
require(reshape2)
require(RColorBrewer)
train_users_2 <- fread('../input/train_users_2.csv')
train_users_2$date_account_created <- as.Date(train_users_2$date_account_created)
train_users_2_clean <- train_users_2[age>=18 & age<=100,]
heatmapData <- as.matrix(table(train_users_2_clean$age,train_users_2_clean$date_account_created)^.2)
p <- qplot(x=Var2, y=Var1, data=melt(heatmapData), fill=value, geom="tile", 
           xlab = "Date Account Created", ylab = "User Age", 
           main = "Number of Accounts Created Over Time Across User Ages")
p <- p + scale_x_discrete(breaks=levels(as.factor(train_users_2_clean$date_account_created))[c(TRUE, rep(FALSE, 90))], 
                          labels=levels(as.factor(train_users_2_clean$date_account_created))[c(TRUE, rep(FALSE, 90))])
p <- p + theme(axis.text.x  = element_text(angle=90, hjust=0.5, size=10))
p <- p + scale_fill_gradient(low="white", high="orangered")
print(p)


# Separate categorical, continous and discrete variables

cat.var.names <- c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(2,3,5), sep=""),
                   paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   "Family_Hist_1", paste("Medical_History_", c(2:14, 16:23, 25:31, 33:41), sep=""))

# Are there any duplicate rows?

cat("Train data set - Number of duplicated rows:", nrow(train) - nrow(unique(train)), "\n")

## Train data set - Number of duplicated rows: 0


## Plot histograms of categorical variables

plotHist <- function(data.in, i) {
  data <- data.frame(x=data.in[,i])
  p <- ggplot(data=data, aes(x=factor(x))) + geom_histogram() + xlab(colnames(data.in)[i]) + theme_light() + 
    theme(axis.text.x=element_text(size=8))
  return (p)
}

doPlots <- function(data.in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data.in=data.in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

doPlots(data.in=train.cat, fun=plotHist, ii=1:4, ncol=2) 


### H20

train<-as.data.frame(train)
test<-as.data.frame(test)

train_log<-train
train_log$Hazard<-log(train_log$Hazard)

val=train[30600:40799,c(1,3:34)]
train=train[c(1:30599,40800:50999),]
train_log=train_log[c(1:30599,40800:50999),]




###write exploratory to log 
# Write to the log:
cat(sprintf("Training set has %d rows and %d columns\n", nrow(train), ncol(train)))
cat(sprintf("Test set has %d rows and %d columns\n", nrow(test), ncol(test)))


## extract data

val=train[30600:40799,c(1,3:34)]
train=train[c(1:30599,40800:50999),]
train_log=train_log[c(1:30599,40800:50999),]


# ### Clean NA values
for(i in 1:ncol(train)){
  if(is.numeric(train[,i])){
    train[is.na(train[,i]),i] = -1
  }else{
    train[,i] = as.character(train[,i])
    train[is.na(train[,i]),i] = "NAvalue"
    train[,i] = as.factor(train[,i])
  }
}



### Clean variables with too many categories
for(i in 1:ncol(train)){
  if(!is.numeric(train[,i])){
    freq = data.frame(table(train[,i]))
    freq = freq[order(freq$Freq, decreasing = TRUE),]
    train[,i] = as.character(match(train[,i], freq$Var1[1:30]))
    train[is.na(train[,i]),i] = "rareValue"
    train[,i] = as.factor(train[,i])
  }
}

test = train[which(train$id > 0),]
train = train[which(train$id < 0),]






