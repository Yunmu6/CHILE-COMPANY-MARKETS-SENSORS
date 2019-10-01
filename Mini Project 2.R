#Q1
storea <- read.csv("a.csv")
str(storea)
storen = storea[-c(1,2),]
str(storen)

#a Hierarchical clustering
#Normalization
library(lattice)
library(ggplot2)
library(caret)
preproc = preProcess(storen)
storeNorm = predict(preproc,storen)

#Hierarchical clustering
distances = dist(storeNorm,method="euclidean")
HC = hclust(distances, method = "ward.D2")
plot(HC,labels = storen$Sum.of.Traffic)
groups = cutree(HC,5)
rect.hclust(HC, k=5, border="red") 
table(groups)
storeNorm$Sum.of.Traffic[groups == 1]
storeNorm$Sum.of.Traffic[groups == 2]
storeNorm$Sum.of.Traffic[groups == 3]
storeNorm$Sum.of.Traffic[groups == 4]
storeNorm$Sum.of.Traffic[groups == 5]

#b)

#c)
#Hierarchical clustering
storec <- read.csv("b.csv")
storen = storec[-c(1,2),] 

#Normalization
preproc = preProcess(storen)
storeNorm = predict(preproc,storen)
#Clustering with 4 cluster  groups
distances = dist(storeNorm,method="euclidean")
HC = hclust(distances, method = "ward.D2")
plot(HC,labels= storen$Average.of.Traffic)
#Assign points to clusters
groups = cutree(HC,4)
rect.hclust(HC, k=4, border="red") 
table(groups)
storeNorm$Average.of.Traffic[groups ==1]
storeNorm$Average.of.Traffic[groups == 2]
storeNorm$Average.of.Traffic[groups == 3]
storeNorm$Average.of.Traffic[groups == 4]

#d #K-means clustering
stored = read.csv("b.csv")
storen = stored[-c(1,2),]

#Assume data is MCAR, too much missing data can be a problem.
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(storen,2,pMiss)
apply(storen,1,pMiss)
#Safe maximum threshold is 5% of the total for large datasets.check for columns
#where more than 5% of the data is missing.
#delete column column.labels/x/x.5/x.6/x.7/x.8/x.9/x.10/x.11/
storen = storen[,-c(2,3,8,9,10,11,12,13,14)]
sum(is.na(storen))
#Then no NA.

#Normalization
preproc = preProcess(storen)
storeNorm = predict(preproc,storen)
rownames(storeNorm) <- storen[,1]
storeNorm[,1] <- NULL
sum(is.na(storeNorm))

#k = 4
set.seed(500)
KC = kmeans(storeNorm,centers=4)
table(KC$cluster)
KC

#Visualization
install.packages("useful")
library(useful)
require(useful)
plot(KC, data= storeNorm)

KC1 = subset(storeNorm,KC$cluster ==1)
row.names(KC1)
KC2 = subset(storeNorm,KC$cluster ==2)
row.names(KC2)
KC3 = subset(storeNorm,KC$cluster ==3)
row.names(KC3)
KC4 = subset(storeNorm,KC$cluster ==4)
row.names(KC4)

#e)
store1 = read.csv("e.1.csv")

#We delete four rows in excel.
#Estado: Wiki:"state".  traffic: low
#Puente: can't find specific number
#Puente Alto(Dijon) & Temuco(Esp. Dijon): Dijon physical store
#add population and avg traffic over the year

store2 = read.csv("e.2.csv")

#Rename columns
colnames(store1) <-c("Average.of.Traffic","Domingo","Jueves","Lunes","Martes","Miercoles","Sabado","Viernes")
rownames(store1) <- store1[,1]
store1 = store1[-1,]

#Combine tables
storee = cbind(store1,store2)
#Delete duplicated column
colnames(storee)
storee = storee[,-9]
colnames(storee)

#f.Hierarchical clustering (4 clusters)
sum(is.na(storee))
str(storee)
#transform factor variables to numeric variables
store <- storee[,1]
storee = storee[,-1]
storee[] <- lapply(storee[,], function(x)as.numeric(as.character(x)))
storee = data.frame(storee,store)
str(storee)

#Normalization
preproc = preProcess(storee)
storeNorm = predict(preproc,storee)

distances = dist(storeNorm,method="euclidean")
HC = hclust(distances, method = "ward.D2")
plot(HC,labels = storeNorm$colnames)
groups = cutree(HC,4)
rect.hclust(HC, k=4, border="red") 
table(groups)

storeNorm$store[groups ==1]
storeNorm$store[groups ==2]
storeNorm$store[groups ==3]
storeNorm$store[groups ==4]


#g. K-means clustering (K=4)
#Normalization
preproc = preProcess(storee)
storeNorm = predict(preproc,storee)
storeNorm = storeNorm[,-10]

set.seed(500)
KC = kmeans(storeNorm,centers=4)
KC
require(useful)
plot(KC, data= storeNorm)
KC1 = subset(storeNorm,KC$cluster ==1)
row.names(KC1)
KC2 = subset(storeNorm,KC$cluster ==2)
row.names(KC2)
KC3 = subset(storeNorm,KC$cluster ==3)
row.names(KC3)
KC4 = subset(storeNorm,KC$cluster ==4)
row.names(KC4)

wss <- (nrow(storeNorm)-1)*sum(apply(storeNorm,2,var))
wss
for (i in 1:10) wss[i] <- sum(kmeans(storeNorm, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#h
#Choose K-means  cluster (g)


#Q2:
#2b
store2b <- read.csv("2b.csv")
str(store2b)
#count 0 in each row
rowSums(store2b !=0)
#delete 1st row, and six stores
store2b <- store2b[-c(1,2,10,16,19,22,24),]

#Normalization
preproc = preProcess(store2b)
storeNorm = predict(preproc,store2b)

#Hierarchical clustering
distances = dist(storeNorm,method="euclidean")
HC = hclust(distances, method = "ward.D2")
plot(HC,labels = storeNorm$Average.of.Salespeople)
groups = cutree(HC,4)
rect.hclust(HC, k=4, border="red") 
table(groups)
storeNorm$Average.of.Salespeople[groups ==1]
storeNorm$Average.of.Salespeople[groups ==2]
storeNorm$Average.of.Salespeople[groups ==3]
storeNorm$Average.of.Salespeople[groups ==4]

#2b except Christmas
store2b <- read.csv("2b.1.csv")
str(store2b)
store2b <- store2b[-c(1,2,10,16,19,22,24),-c(50,51,52,53)]

#Normalization
preproc = preProcess(store2b)
storeNorm = predict(preproc,store2b)

#Hierarchical clustering
distances = dist(storeNorm,method="euclidean")
HC = hclust(distances, method = "ward.D2")
plot(HC,labels = storeNorm$Average.of.Salespeople)
groups = cutree(HC,4)
rect.hclust(HC, k=4, border="red") 
table(groups)
storeNorm$Average.of.Salespeople[groups ==1]
storeNorm$Average.of.Salespeople[groups ==2]
storeNorm$Average.of.Salespeople[groups ==3]
storeNorm$Average.of.Salespeople[groups ==4]

#2c
store2c <- read.csv("2c.csv")
str(store2c)
store2c <- store2c[-c(1,2,10,16,19,22,24),]

#Normalization
preproc = preProcess(store2c)
storeNorm = predict(preproc,store2c)
#Hierarchical clustering
distances = dist(storeNorm,method="euclidean")
HC = hclust(distances, method = "ward.D2")
plot(HC,labels = storeNorm$Average.of.Salespeople.Working.Same.Time)
groups = cutree(HC,4)
rect.hclust(HC, k=4, border="red") 
table(groups)
storeNorm$Average.of.Salespeople.Working.Same.Time[groups ==1]
storeNorm$Average.of.Salespeople.Working.Same.Time[groups ==2]
storeNorm$Average.of.Salespeople.Working.Same.Time[groups ==3]
storeNorm$Average.of.Salespeople.Working.Same.Time[groups ==4]
#2c except Christmas
store2c <- read.csv("2c.1.csv")
str(store2c)
store2c <- store2c[-c(1,2,10,16,19,22,24),-c(50,51,52,53)]

#Normalization
preproc = preProcess(store2c)
storeNorm = predict(preproc,store2c)
#Hierarchical clustering
distances = dist(storeNorm,method="euclidean")
HC = hclust(distances, method = "ward.D2")
plot(HC,labels = storeNorm$Average.of.Salespeople.Working.Same.Time)
groups = cutree(HC,4)
rect.hclust(HC, k=4, border="red") 
table(groups)
storeNorm$Average.of.Salespeople.Working.Same.Time[groups ==1]
storeNorm$Average.of.Salespeople.Working.Same.Time[groups ==2]
storeNorm$Average.of.Salespeople.Working.Same.Time[groups ==3]
storeNorm$Average.of.Salespeople.Working.Same.Time[groups ==4]

