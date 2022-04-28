
#K-means Clustering algorithm

kmeans (x, centrs, iter.max=10, nstart = 1, alogorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))

# x - numeric matrix of data
# centers - no. of cluster centers; random choosen otherwise
# iter.max - no. of iterations of the algorithm
# nstart - no. of random sets if a number

#Set path i.e. working directory where file is located 
#"NAME"=your folder name in users

setwd("C:/Users/"NAME"/Documents/")

#read file into a R object
stores=read.csv("Stores_E11_2.csv", header = TRUE, sep=",")

#5-number summary
summary(stores)    
#remove column Store
stores$Store =NULL
#look at the top 5 rows
head(stores)      

km<-kmeans(stores,3,100)
#review output
km

#cluster - a vector of intergers (from 1 to k) cluster no. allocation
#centers - a matrix (cluster centers)
#withinss - within-cluster sum of squares for each cluster
#totss - total within-cluster sum of squares
#tot.withinss - total within-cluster sum of squares i.e. sum(withinss)
#betweenss - btween-clsuteer sum of squares
#size - the number of points in each cluster

#Find the suitable number of clusters

#creeate set of number of clusters from 1 to 15
wss<-numeric(15)

#find wss (within-cluster sum of squares) for each kmeans operation for i clusters
for (i in 1:15) wss [i] <-sum(kmeans(stores, centers=i)$withinss)

#plot 
plot(1:15, wss, type="b", xlab="No. of clusters", ylab="Within groups sum of squares")

