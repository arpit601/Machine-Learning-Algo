#Knn on iris data

#get package class
library(class)
library(caret)
#get the data
iris

#shuffling the data
set.seed(9850)
g<- runif(nrow(iris))
irisr<- iris[order(g),]
irisr

#splitting dataset
trainset<- irisr[1:100,]
trainset
testset<- irisr[101:150,]
testset

# BUilding k-nn model 
m <- knn(trainset[,1:4],testset[,1:4], cl=trainset[,5],k=3)
summary(m)

#creating confusion matrix
confusionMatrix(testset[,5],m)


######################################
# knn on HouseVotes84
library(mlbench)
data("HouseVotes84")
HouseVotes84
HouseVotes85<- (HouseVotes84)
dim(HouseVotes85)

#splitting dataset
trainset1<- HouseVotes84[1:200,]
trainset1
testset1<- HouseVotes84[201:232,]
testset1

head(HouseVotes84)
#grow tree
m <- knn(!is.na(trainset1[,-1]),!is.na(testset1[,-1]), cl=trainset1[,1],k=3)
table(testset[,1],m)


library(ISLR)

#get the data
Smarket
head(Smarket)

#splitting dataset
trainset<- Smarket[1:1000,]
trainset
testset<- Smarket[1001:1250,]
testset

# BUilding k-nn model 
m <- knn(trainset[,2:8],testset[,2:8], cl=trainset[,9],k=150)


#creating confusion matrix
confusionMatrix(testset[,9],m)

##########################################################
## knn on carseats

library(ISLR)
Carseats
head(Carseats)
Carseats$Urban<- as.numeric(as.factor(Carseats$Urban))
Carseats$ShelveLoc<- as.numeric((as.factor(Carseats$ShelveLoc)))
#splitting dataset
trainset<- Carseats[1:320,]
trainset
testset<- Carseats[321:400,]
testset
# BUilding k-nn model 
m <- knn(trainset[,1:10],testset[,1:10], cl=trainset[,11],k=3)


#creating confusion matrix
confusionMatrix(testset[,11],m)


