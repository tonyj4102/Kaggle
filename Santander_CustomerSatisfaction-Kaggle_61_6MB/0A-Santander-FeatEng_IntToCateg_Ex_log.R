Strain=read.csv("train.csv")
Stest=read.csv("santander_test.csv")


# @@@@@@@@@@  Create and merge new training set  @@@@@@@@@@

# Create new variable
TrainN=Strain

# Change column name for new dataframe [TrainN]           Pseudocode: 
for (i in 1:ncol(TrainN)){                                #for column number i
  newname<-paste((colnames(TrainN)[i]),-2,sep='')             #create new column name
  names(TrainN)[i]<-paste(newname)                            #change column name
}

# Keep ID Variable
TrainNY<-TrainN$ID

# Element multiple e^x to create new variables
TrainN<-exp(TrainN)
str(TrainN)

# Delete Factor response variable, that would be a duplicate in the merge below
TrainN$`TARGET-2`<-NULL

# Add back ID for merge
TrainN$ID=TrainNY

# Rename column name =ID for merge below 
library(plyr)
rename(TrainN, c("ID-2"="ID"))

# Create TrainY before normalizing Train to keep 0,1's
TrainY<-Train$TARGET
Train$TARGET<-NULL

# Combine TrainX and TrainN
TrainB<-merge(Train, TrainN, by.x = "ID", by.y = "ID")


# Add response variable to new data frame
TrainB$FACTOR<-TrainY


# Verify new dataset
str(TrainB)
summary(TrainB)
nrow(TrainB)
ncol(TrainB)
colnames(TrainB)


# Write new file to to TrainC.csv 
write.csv(TrainC, "TrainC.csv")


# @@@@@@@@@@  Create and merge new test set  @@@@@@@@@@ 

#Create working variable
TestN=Stest

# Change column name for new dataframe [TrainN]           Pseudocode: 
for (i in 1:ncol(TestN)){                                #for column number i
  newname<-paste((colnames(TestN)[i]),-2,sep='')             #create new column name
  names(TestN)[i]<-paste(newname)                            #change column name
}

# Keep ID Variable
TestNY<-TestN$ID

# Element multiple e^x to create new variables
TestN<-exp(TestN)
str(TestN)

# Delete Factor response variable, that would be a duplicate in the merge below
TestN$`TARGET-2`<-NULL

# Add back ID for merge
TestN$ID=TestNY

# Rename column name =ID for merge below 
library(plyr)
rename(TestN, c("ID-2"="ID"))

# Create TrainY before normalizing Train to keep 0,1's
TestY<-Test$TARGET
Test$TARGET<-NULL


# Combine TrainX and TrainN
TestB<-merge(Test, TestN, by.x = "ID", by.y = "ID")


# Add response variable to new data frame
TestB$FACTOR<-TestY


# Verify new dataset
str(TrainB)
TestB$`ID-2`<-NULL
summary(TestB$`ID-2`)
nrow(TestC)
ncol(TestC)
colnames(TestC)


# Write new file to to TrainC.csv 
write.csv(TestC, "TestC.csv")


OLD
# @@@@@@@@@@  Create and merge new training set  @@@@@@@@@@

Train=read.csv("train.csv")
TrainN=Train

# Change column name for new dataframe [TrainN]           Pseudocode: 
for (i in 1:ncol(TrainN)){                                #for column number i
  newname<-paste((colnames(TrainN)[i]),-2,sep='')             #create new column name
  names(TrainN)[i]<-paste(newname)                            #change column name
}

# Keep ID Variable

TrainNY<-TrainN$ID

# Element multiple e^x to create new variables
TrainN<-exp(TrainN)
str(TrainN)

# Delete Factor response variable, that would be a duplicate in the merge below

TrainN$`TARGET-2`<-NULL

# Add back ID for merge

TrainN$ID=TrainNY

# Rename column name =ID for merge below 

library(plyr)
rename(TrainN, c("ID-2"="ID"))

# Normalize [TrainN] to improve training performance

install.packages("caret")
library(caret)

preprocA = preProcess(TrainN)
TrainN-N <- predict(preprocA, TrainN)


# Create TrainY before normalizing Train to keep 0,1's

TrainY<-Train$TARGET
Train$TARGET<-NULL


# Create TrainX for normalization

TrainX<-Train

# Normalize Train X

preprocA2 <- preProcess(TrainX)
Train-N <- predict(preprocA2, TrainX)


# Combine TrainX and TrainN

TrainB<-merge(Train, TrainN, by.x = "ID", by.y = "ID")


# Add response variable to new data frame

TrainB$FACTOR<-TrainY


# Verify new dataset

str(TrainB)
summary(TrainB)
nrow(TrainB)
ncol(TrainB)
colnames(TrainB)


# Write new file to to TrainC.csv 

write.csv(TrainC, "TrainC.csv")


# @@@@@@@@@@  Create and merge new test set  @@@@@@@@@@ 

Test=read.csv("santander_test.csv")
TestN=Test

# Change column name for new dataframe [TrainN]           Pseudocode: 
for (i in 1:ncol(TestN)){                                #for column number i
  newname<-paste((colnames(TestN)[i]),-2,sep='')             #create new column name
  names(TestN)[i]<-paste(newname)                            #change column name
}

# Keep ID Variable

TestNY<-TestN$ID

# Element multiple e^x to create new variables
TestN<-exp(TestN)
str(TestN)

# Delete Factor response variable, that would be a duplicate in the merge below

TestN$`TARGET-2`<-NULL

# Add back ID for merge

TestN$ID=TestNY

# Rename column name =ID for merge below 

library(plyr)
rename(TestN, c("ID-2"="ID"))



# Create TrainY before normalizing Train to keep 0,1's

TestY<-Test$TARGET
Test$TARGET<-NULL


# Combine TrainX and TrainN

TestB<-merge(Test, TestN, by.x = "ID", by.y = "ID")


# Add response variable to new data frame

TestB$FACTOR<-TestY


# Verify new dataset

str(TrainB)
TestB$`ID-2`<-NULL
summary(TestB$`ID-2`)
nrow(TestC)
ncol(TestC)
colnames(TestC)


# Write new file to to TrainC.csv 

write.csv(TestC, "TestC.csv")



# @@@    extract numeric variables into a new dataframe - Explore, @@@@@@@ 


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


#  @@@@@@@@@@   numeric to Log @@@@@@@@@@@@

# new data frame
StrainLNY<-Strain$TARGET
StrainLI<-Strain
StrainLI<-data.frame(Strain)

# keep only numeric 
StrainLN  <- StrainLI[,sapply(StrainLI,is.double)]
str(StrainLN)
str(StrainLN [1:4])

# convert numerical # to log except 0's

for (i in 1:ncol(StrainLN)) {
     for (j in 1:nrow(StrainLN)) {
                 if (StrainLN[j,i] !=0)  {  
                     StrainLN[j,i]<-log(StrainLN[j,i])
                 }
     }
}

## 13 minutes : time 5:28pm - 5:41 

# output:
There were 50 or more warnings (use warnings() to see the first 50)
> warnings()
Warning messages:
  1: In log(StrainLN[j, i]) : NaNs produced
2: In log(StrainLN[j, i]) : NaNs produced


# verification (below)
ncol(StrainLN)
str(StrainLN [110:127])
str(StrainLN) 


$ imp_op_var39_comer_ult1: num  0 0 0 195 0 0 0 0 0 0 ...
$ imp_op_var39_comer_ult3: num  0 0 0 195 0 0 0 0 0 0 ...
$ imp_op_var41_comer_ult1: num  0 0 0 195 0 0 0 0 0 0 ...
$ imp_op_var41_comer_ult3: num  0 0 0 195 0 0 0 0 0 0 ...

str(StrainLN [100:127])
$ saldo_medio_var13_corto_hace2: num  0 300 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var13_corto_hace3: num  0 122 0 0 0 ...
$ saldo_medio_var13_corto_ult1 : num  0 300 0 0 0 0 0 0 0 0 ...
$ saldo_medio_var13_corto_ult3 : num  0 241 0 0 0 ...
$ var38                        : num  39205 49278 67334 64008 117311 ...


##delete N/A's introduced 
summary(StrainLN)

#NA's
saldo_var1         saldo_var5      saldo_var8 
saldo_var30
saldo_var40       saldo_var42
delta_imp_aport_var13_1y3 delta_imp_aport_var17_1y3
delta_imp_aport_var33_1y3 delta_imp_compra_var44_1y3
delta_imp_reemb_var17_1y3
delta_imp_trasp_var17_in_1y3
delta_imp_trasp_var33_in_1y3
delta_num_aport_var13_1y3 delta_num_aport_var17_1y3
delta_num_trasp_var17_in_1y3
delta_num_reemb_var17_1y3
delta_num_trasp_var33_in_1y3
saldo_medio_var5_hace2
saldo_medio_var5_hace3 saldo_medio_var5_ult1 saldo_medio_var5_ult3 saldo_medio_var8_hace2
saldo_medio_var8_ult1
saldo_medio_var8_ult3
delta_num_aport_var33_1y3 delta_num_compra_var44_1y3

# Replace N/A's with zeros
StrainLN[is.na(StrainLN)] <- 0

##check to confirm no N/A's
summary(StrainLN)
str(StrainLN)

# convert numerical to categorical features
  
Testtrain=StrainLN                              ## data to test
summary(Testtrain)

Testtrain=Testtrain+5                           ## to get rid of negative values
str(Testtrain)

# convert each row to categorical

Testtrain$saldo_medio_var8_ult1<-cut(Testtrain$saldo_medio_var8_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
summary(Testtrain$saldo_medio_var8_ult1)
Testtrain$imp_ent_var16_ult1<-cut(Testtrain$imp_ent_var16_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
summary(Testtrain$imp_ent_var16_ult1)
Testtrain$imp_op_var39_comer_ult1<-cut(Testtrain$imp_op_var39_comer_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
summary(Testtrain$imp_op_var39_comer_ult1)
Testtrain$imp_op_var39_comer_ult3<-cut(Testtrain$imp_op_var39_comer_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
summary(Testtrain$imp_op_var39_comer_ult3)
Testtrain$imp_op_var40_comer_ult1<-cut(Testtrain$imp_op_var40_comer_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
summary(Testtrain$imp_op_var40_comer_ult1)
Testtrain$imp_op_var40_comer_ult3<-cut(Testtrain$imp_op_var40_comer_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
summary(Testtrain$imp_op_var40_comer_ult3)
Testtrain$imp_op_var40_efect_ult1<-cut(Testtrain$imp_op_var40_efect_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
summary(Testtrain$imp_op_var40_efect_ult1)
Testtrain$imp_op_var40_efect_ult3<-cut(Testtrain$imp_op_var40_efect_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
summary(Testtrain$imp_op_var40_efect_ult3)
Testtrain$imp_op_var40_ult1<-cut(Testtrain$imp_op_var40_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_op_var41_comer_ult1<-cut(Testtrain$imp_op_var41_comer_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_op_var41_comer_ult3<-cut(Testtrain$imp_op_var41_comer_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_op_var41_efect_ult1<-cut(Testtrain$imp_op_var41_efect_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_op_var41_efect_ult3<-cut(Testtrain$imp_op_var41_efect_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_op_var41_ult1<-cut(Testtrain$imp_op_var41_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_op_var39_efect_ult1<-cut(Testtrain$imp_op_var39_efect_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_op_var39_efect_ult3<-cut(Testtrain$imp_op_var39_efect_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_op_var39_ult1<-cut(Testtrain$imp_op_var39_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_sal_var16_ult1<-cut(Testtrain$imp_sal_var16_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var1<-cut(Testtrain$saldo_var1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var5<-cut(Testtrain$saldo_var5, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var6<-cut(Testtrain$saldo_var6, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var8<-cut(Testtrain$saldo_var8, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var12<-cut(Testtrain$saldo_var12, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var13_corto<-cut(Testtrain$saldo_var13_corto, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var13_largo<-cut(Testtrain$saldo_var13_largo, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var13<-cut(Testtrain$saldo_var13, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var14<-cut(Testtrain$saldo_var14, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var17<-cut(Testtrain$saldo_var17, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var20<-cut(Testtrain$saldo_var20, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var24<-cut(Testtrain$saldo_var24, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var26<-cut(Testtrain$saldo_var26, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var25<-cut(Testtrain$saldo_var25, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var29<-cut(Testtrain$saldo_var29, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var30<-cut(Testtrain$saldo_var30, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var31<-cut(Testtrain$saldo_var31, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var32<-cut(Testtrain$saldo_var32, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var33<-cut(Testtrain$saldo_var33, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var37<-cut(Testtrain$saldo_var37, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var40<-cut(Testtrain$saldo_var40, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var42<-cut(Testtrain$saldo_var42, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_var44<-cut(Testtrain$saldo_var44, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_amort_var18_1y3<-cut(Testtrain$delta_imp_amort_var18_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_amort_var34_1y3<-cut(Testtrain$delta_imp_amort_var34_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_aport_var13_1y3<-cut(Testtrain$delta_imp_aport_var13_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_aport_var17_1y3<-cut(Testtrain$delta_imp_aport_var17_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_aport_var33_1y3<-cut(Testtrain$delta_imp_aport_var33_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_compra_var44_1y3<-cut(Testtrain$delta_imp_compra_var44_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_reemb_var13_1y3<-cut(Testtrain$delta_imp_reemb_var13_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_reemb_var17_1y3<-cut(Testtrain$delta_imp_reemb_var17_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_reemb_var33_1y3<-cut(Testtrain$delta_imp_reemb_var33_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_trasp_var17_in_1y3<-cut(Testtrain$delta_imp_trasp_var17_in_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_trasp_var17_out_1y3<-cut(Testtrain$delta_imp_trasp_var17_out_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_trasp_var33_in_1y3<-cut(Testtrain$delta_imp_trasp_var33_in_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_trasp_var33_out_1y3<-cut(Testtrain$delta_imp_trasp_var33_out_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_imp_venta_var44_1y3<-cut(Testtrain$delta_imp_venta_var44_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_aport_var13_1y3<-cut(Testtrain$delta_num_aport_var13_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_aport_var17_1y3<-cut(Testtrain$delta_num_aport_var17_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_aport_var33_1y3<-cut(Testtrain$delta_num_aport_var33_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_compra_var44_1y3<-cut(Testtrain$delta_num_compra_var44_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_reemb_var13_1y3<-cut(Testtrain$delta_num_reemb_var13_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_reemb_var17_1y3<-cut(Testtrain$delta_num_reemb_var17_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_reemb_var33_1y3<-cut(Testtrain$delta_num_reemb_var33_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_trasp_var17_in_1y3<-cut(Testtrain$delta_num_trasp_var17_in_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_trasp_var17_out_1y3<-cut(Testtrain$delta_num_trasp_var17_out_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_trasp_var33_in_1y3<-cut(Testtrain$delta_num_trasp_var33_in_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_trasp_var33_out_1y3<-cut(Testtrain$delta_num_trasp_var33_out_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$delta_num_venta_var44_1y3<-cut(Testtrain$delta_num_venta_var44_1y3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_amort_var18_ult1<-cut(Testtrain$imp_amort_var18_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_amort_var34_ult1<-cut(Testtrain$imp_amort_var34_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_aport_var13_hace3<-cut(Testtrain$imp_aport_var13_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_aport_var13_ult1<-cut(Testtrain$imp_aport_var13_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_aport_var17_hace3<-cut(Testtrain$imp_aport_var17_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_aport_var17_ult1<-cut(Testtrain$imp_aport_var17_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_var7_emit_ult1<-cut(Testtrain$imp_var7_emit_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_var7_recib_ult1<-cut(Testtrain$imp_var7_recib_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_compra_var44_hace3<-cut(Testtrain$imp_compra_var44_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_compra_var44_ult1<-cut(Testtrain$imp_compra_var44_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_reemb_var13_ult1<-cut(Testtrain$imp_reemb_var13_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_reemb_var17_hace3<-cut(Testtrain$imp_reemb_var17_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_reemb_var17_ult1<-cut(Testtrain$imp_reemb_var17_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_var43_emit_ult1<-cut(Testtrain$imp_var43_emit_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_trans_var37_ult1<-cut(Testtrain$imp_trans_var37_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_trasp_var17_in_hace3<-cut(Testtrain$imp_trasp_var17_in_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_trasp_var17_in_ult1<-cut(Testtrain$imp_trasp_var17_in_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_trasp_var17_out_ult1<-cut(Testtrain$imp_trasp_var17_out_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_trasp_var33_in_hace3<-cut(Testtrain$imp_trasp_var33_in_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_trasp_var33_in_ult1<-cut(Testtrain$imp_trasp_var33_in_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_venta_var44_hace3<-cut(Testtrain$imp_venta_var44_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$imp_venta_var44_ult1<-cut(Testtrain$imp_venta_var44_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var5_hace2<-cut(Testtrain$saldo_medio_var5_hace2, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var5_hace3<-cut(Testtrain$saldo_medio_var5_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var5_ult1<-cut(Testtrain$saldo_medio_var5_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var5_ult3<-cut(Testtrain$saldo_medio_var5_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var8_hace2<-cut(Testtrain$saldo_medio_var8_hace2, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var8_hace3<-cut(Testtrain$saldo_medio_var8_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var8_ult1<-cut(Testtrain$saldo_medio_var8_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var8_ult3<-cut(Testtrain$saldo_medio_var8_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var12_hace2<-cut(Testtrain$saldo_medio_var12_hace2, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var12_hace3<-cut(Testtrain$saldo_medio_var12_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var12_ult1<-cut(Testtrain$saldo_medio_var12_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var12_ult3<-cut(Testtrain$saldo_medio_var12_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var13_corto_hace2<-cut(Testtrain$saldo_medio_var13_corto_hace2, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var13_corto_hace3<-cut(Testtrain$saldo_medio_var13_corto_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var13_corto_ult1<-cut(Testtrain$saldo_medio_var13_corto_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var13_corto_ult3<-cut(Testtrain$saldo_medio_var13_corto_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var13_largo_hace2<-cut(Testtrain$saldo_medio_var13_largo_hace2, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var13_largo_hace3<-cut(Testtrain$saldo_medio_var13_largo_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var13_largo_ult1<-cut(Testtrain$saldo_medio_var13_largo_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var13_largo_ult3<-cut(Testtrain$saldo_medio_var13_largo_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var13_medio_hace2<-cut(Testtrain$saldo_medio_var13_medio_hace2, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var13_medio_ult3<-cut(Testtrain$saldo_medio_var13_medio_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var17_hace2<-cut(Testtrain$saldo_medio_var17_hace2, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var17_hace3<-cut(Testtrain$saldo_medio_var17_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var17_ult1<-cut(Testtrain$saldo_medio_var17_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var17_ult3<-cut(Testtrain$saldo_medio_var17_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var29_hace2<-cut(Testtrain$saldo_medio_var29_hace2, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var29_hace3<-cut(Testtrain$saldo_medio_var29_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var29_ult1<-cut(Testtrain$saldo_medio_var29_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var29_ult3<-cut(Testtrain$saldo_medio_var29_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var33_hace2<-cut(Testtrain$saldo_medio_var33_hace2, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var33_hace3<-cut(Testtrain$saldo_medio_var33_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var33_ult1<-cut(Testtrain$saldo_medio_var33_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var33_ult3<-cut(Testtrain$saldo_medio_var33_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var44_hace2<-cut(Testtrain$saldo_medio_var44_hace2, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var44_hace3<-cut(Testtrain$saldo_medio_var44_hace3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var44_ult1<-cut(Testtrain$saldo_medio_var44_ult1, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$saldo_medio_var44_ult3<-cut(Testtrain$saldo_medio_var44_ult3, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))
Testtrain$var38<-cut(Testtrain$var38, breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,100), labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))

str(Testtrain, list.len=ncol(Testtrain))


# Create new Train #2 with categorical version of numerical variables

TrainNEW<-Testtrain

str(TrainNEW, list.len=ncol(TrainNEW))

TrainCON<-TrainNEW

# create integer from categorical

labels=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","AA","BB","CC", "DD", "EE","FF"))


for (i in 1:ncol(TrainCON)) {
  for (j in 1:nrow(TrainCON)) {
    if (TrainCON[j,i] =="A")  {  
      TrainCON[j,i]<-1
    } else {
      if (TrainCON[j,i] =="B")  {  
        TrainCON[j,i]<-2
      } else {
        if (TrainCON[j,i] =="C")  {  
          TrainCON[j,i]<-3
        } else {
          if (TrainCON[j,i] =="D")  {  
            TrainCON[j,i]<-4
          } else {
            if (TrainCON[j,i] =="E")  {  
              TrainCON[j,i]<-5
            } else {
              if (TrainCON[j,i] =="F")  {  
                TrainCON[j,i]<-6
              } else {
                if (TrainCON[j,i] =="G")  {  
                  TrainCON[j,i]<-7
                } else {
                  if (TrainCON[j,i] =="H")  {  
                    TrainCON[j,i]<-8
                  } else {
                    if (TrainCON[j,i] =="I")  {  
                      TrainCON[j,i]<-9
                    } else {
                      if (TrainCON[j,i] =="J")  {  
                        TrainCON[j,i]<-10
                      } else {
                        if (TrainCON[j,i] =="K")  {  
                          TrainCON[j,i]<-11
                        } else {
                          if (TrainCON[j,i] =="L")  {  
                            TrainCON[j,i]<-12
                          } else {
                            if (TrainCON[j,i] =="M")  {  
                              TrainCON[j,i]<-13
                            } else {
                              if (TrainCON[j,i] =="N")  {  
                                TrainCON[j,i]<-14
                              } else {
                                if (TrainCON[j,i] =="O")  {  
                                  TrainCON[j,i]<-15
                                } else {
                                  if (TrainCON[j,i] =="P")  {  
                                    TrainCON[j,i]<-16
                                  } else {
                                    if (TrainCON[j,i] =="Q")  {  
                                      TrainCON[j,i]<-17
                                    } else {
                                      if (TrainCON[j,i] =="R")  {  
                                        TrainCON[j,i]<-18
                                      } else {
                                        if (TrainCON[j,i] =="S")  {  
                                          TrainCON[j,i]<-19
                                        } else {
                                          if (TrainCON[j,i] =="T")  {  
                                            TrainCON[j,i]<-20
                                          } else {
                                            if (TrainCON[j,i] =="U")  {  
                                              TrainCON[j,i]<-21
                                            } else {
                                              if (TrainCON[j,i] =="O")  {  
                                                TrainCON[j,i]<-22
                                              } else {
                                                if (TrainCON[j,i] =="P")  {  
                                                  TrainCON[j,i]<-23
                                                } else {
                                                  if (TrainCON[j,i] =="Q")  {  
                                                    TrainCON[j,i]<-24
                                                  } else {
                                                    if (TrainCON[j,i] =="R")  {  
                                                      TrainCON[j,i]<-25
                                                    } else {
                                                      if (TrainCON[j,i] =="S")  {  
                                                        TrainCON[j,i]<-26
                                                      } else {
                                                        if (TrainCON[j,i] =="T")  {  
                                                          TrainCON[j,i]<-27
                                                        } else {
                                                          if (TrainCON[j,i] =="U")  {  
                                                            TrainCON[j,i]<-28
                                                          } else {
                                                            if (TrainCON[j,i] =="V")  {  
                                                              TrainCON[j,i]<-29
                                                            } else {
                                                              if (TrainCON[j,i] =="W")  {  
                                                                TrainCON[j,i]<-30
                                                              } else {
                                                                if (TrainCON[j,i] =="X")  {  
                                                                  TrainCON[j,i]<-31
                                                                } else {
                                                                  if (TrainCON[j,i] =="Y")  {  
                                                                    TrainCON[j,i]<-32
                                                                  } else {
                                                                    if (TrainCON[j,i] =="Z")  {  
                                                                      TrainCON[j,i]<-33
                                                                    } else {
                                                                      if (TrainCON[j,i] =="AA")  {  
                                                                        TrainCON[j,i]<-34
                                                                      } else {
                                                                        if (TrainCON[j,i] =="BB")  {  
                                                                          TrainCON[j,i]<-35
                                                                        } else {
                                                                          if (TrainCON[j,i] =="CC")  {  
                                                                            TrainCON[j,i]<-36
                                                                          } else {
                                                                            if (TrainCON[j,i] =="DD")  {  
                                                                              TrainCON[j,i]<-37
                                                                            } else {
                                                                              if (TrainCON[j,i] =="EE")  {  
                                                                                TrainCON[j,i]<-38
                                                                              } else {
                                                                                if (TrainCON[j,i] =="FF")  {  
                                                                                  TrainCON[j,i]<-39
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}



#view new variable

str(TrainCON)

# add response varaible back

TrainNEW$TARGET<-StrainLNY


### @@@@@@@@@@@@@@@     FINALIZED SCRIPTS   @@@@@@@@@@@@@@

rm(list = ls())

Strain=read.csv("train.csv")
Stest=read.csv("santander_test.csv")


# @@@@@@@@@@@   e^x data (Numeric) @@@@@@@@@@


# Remove response variables

StrainEX<-Strain
StrainEXY<-Strain$TARGET
StrainEX$TARGET<- NULL

StestEX<-Stest
StestEXY<-Stest$TARGET
StestEX$TARGET<- NULL


# remove integer variable.names

StrainEX  <- StrainEX[,sapply(StrainEX,is.double)]
StestEX  <- StestEX[,sapply(StestEX,is.double)]


# Apply e^x on all none zero values


# Convert numerical # to log except 0's

for (i in 1:ncol(StrainEX)) {
  for (j in 1:nrow(StrainEX)) {
    if (StrainEX[j,i] !=0)  {  
      StrainEX[j,i]<-exp(StrainEX[j,i])
    }
  }
}

for (i in 1:ncol(StestEX)) {
  for (j in 1:nrow(StestEX)) {
    if (StestEX[j,i] !=0)  {  
      StestEX[j,i]<-exp(StestEX[j,i])
    }
  }
}

# create new base

library(dplyr)
ETrain<-select(Strain, 
               ID, TARGET)
ETest<-select(Stest, 
              ID)

# add ID to new ex

StrainEX$ID<-Strain$ID
StestEX$ID<-Stest$ID


# merge data on ID (base +e^x)


MEtrain <- merge(ETrain, StrainEX, by.x = "ID", by.y = "ID")
MEtest <- merge(ETest, StestEX, by.x = "ID", by.y = "ID")



str(MEtrain)

# @@@@@@@@@@@   log data (Numeric)  @@@@@@@@@@


# Remove response variables

StrainLN<-Strain
StrainLNY<-Strain$TARGET
StrainLN$TARGET<- NULL

StestLN<-Stest
StestLNY<-Stest$TARGET
StestLN$TARGET<- NULL


# remove integer variable.names

StrainLN  <- StrainLN[,sapply(StrainLN,is.double)]
StestLN  <- StestLN[,sapply(StestLN,is.double)]

# Apply e^x on all none zero values


# Convert numerical # to log except 0's

for (i in 1:ncol(StrainLN)) {
  for (j in 1:nrow(StrainLN)) {
    if (StrainLN[j,i] !=0)  {  
      StrainLN[j,i]<-log(StrainLN[j,i])
    }
  }
}

for (i in 1:ncol(StestLN)) {
  for (j in 1:nrow(StestLN)) {
    if (StestLN[j,i] !=0)  {  
      StestLN[j,i]<-log(StestLN[j,i])
    }
  }
}

str(StestLN)
summary(StestLN)


# create new base

library(dplyr)
LTrain<-select(Strain, 
               ID, TARGET)
LTest<-select(Stest, 
              ID)


# add ID to new ex


StrainLN$ID<-Strain$ID
StestLN$ID<-Stest$ID


# merge data on ID (base +e^x)


MLtrain <- merge(LTrain, StrainLN, by.x = "ID", by.y = "ID")
MLtest <- merge(LTest, StestLN, by.x = "ID", by.y = "ID")



# @@@@@@@  normalize variables @@@@@@@@


library(caret)

preproc = preProcess(StrainLN)
StrainLNN = predict(preproc, StrainLN)

preproc = preProcess(StestLN)
StestLNN = predict(preproc, StestLN)

preproc = preProcess(StrainEX)
StrainEXN = predict(preproc, StrainEX)

preproc = preProcess(StestEX)
StestEXN = predict(preproc, StestEX)

# @@@@@@@@@@@   Create varaible names for new dataframes  @@@@@@@@@@@@


# Change column name for new dataframe [StrainLNN]           Pseudocode: 
for (i in 1:ncol(StrainLNN)){                                #for column number i
  newname<-paste((colnames(StrainLNN)[i]),-1,sep='')             #create new column name
  names(StrainLNN)[i]<-paste(newname)                            #change column name
}


# Change column name for new dataframe [StestLNN]           Pseudocode: 
for (i in 1:ncol(StestLNN)){                                #for column number i
  newname<-paste((colnames(StestLNN)[i]),-1,sep='')             #create new column name
  names(StestLNN)[i]<-paste(newname)                            #change column name
}

str(StestLNN)

# Change column name for new dataframe [StrainEXN]           
for (i in 1:ncol(StrainEXN)){                                
  newname<-paste((colnames(StrainEXN)[i]),-2,sep='')            
  names(StrainEXN)[i]<-paste(newname)                           
}


# Change column name for new dataframe [StestEXN]           
for (i in 1:ncol(StestEXN)){                                
  newname<-paste((colnames(StestEXN)[i]),-2,sep='')             
  names(StestEXN)[i]<-paste(newname)                            
}

# @@@@@@@@@@@   Combine variables to create new Data  @@@@@@@@@@

# remove response variable from base

SStrainY<- Strain$TARGET
SStrain<-Strain
SStrain$TARGET<-NULL
  
SStestY<- Strain$TARGET
SStest<-Stest
SStest$TARGET<-NULL


# add ID to LL and EXN 

StrainLNN$ID<-Strain$ID
StestLNN$ID<-Stest$ID

StrainEXN$ID<-Strain$ID
StestEXN$ID<-Stest$ID

str(StrainLNN)
str(StrainEXN)

 # merge data on ID 
base + log numeric  + e^x numeric


M1train <- merge(Strain, StrainLNN, by.x = "ID", by.y = "ID")
Mtrain <- merge(M1train, StrainEXN, by.x = "ID", by.y = "ID")

M1test <- merge(Stest, StestLNN, by.x = "ID", by.y = "ID")
Mtest <- merge(M1test, StestEXN, by.x = "ID", by.y = "ID")


# @@@@@@@@@@@   Add back response variable  @@@@@@@@@@

Mtrain$TARGET<-Strain$TARGET




#@@***************************Factor plus log data ***************@@@@# 


# remove integer variable.names

StrainF  <- Strain[,sapply(Strain,is.integer)]
StestF  <- Stest[,sapply(Stest,is.integer)]

str(StrainF)
str(StrainLNN) [1:2]
StrainLNN5<-StrainLNN+5
str(StrainLNN5) [1:2]
StestLNN5<-StestLNN+5
str(StestLNN5) [1:2]

M2train <- merge(StrainF, StrainLNN5, by.x = "ID", by.y = "ID")
M2test <- merge(StestF, StestLNN, by.x = "ID", by.y = "ID")


str(M2train)[200:210]


