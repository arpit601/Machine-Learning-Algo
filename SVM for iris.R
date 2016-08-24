library(e1071)

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

#building the SVM model on trainset #
m<- svm(trainset[,1:4], trainset[,5],cost=100, gamma=0.25)
summary(m)

# Using above model on testset #
p<- predict(m,testset[,1:4])
p

# create confusion matrix 
table(testset[,5],p)


# knn on HouseVotes84
library(mlbench)
data("HouseVotes84")
HouseVotes84


#splitting dataset
trainset<- HouseVotes84[1:200,]
trainset
testset<- HouseVotes84[201:232,]
testset

library(mlbench)
write.csv(HouseVotes84,"HouseVotes84.csv")
HouseVotes84<- read.csv(file.choose())
#building the SVM model on trainset #
m<- svm(trainset[,2:17], trainset[,1],cost=100)
summary(m)

# Using above model on testset #
p<- predict(m,testset[,2:17])
p

# create confusion matrix 
table(testset[,1],p)

library(ISLR)
Carseats

Carseats$ShelveLoc<- as.numeric(as.factor(Carseats$ShelveLoc))
Carseats$Urban<- as.numeric(as.factor(Carseats$Urban))
#splitting dataset
trainset<- Carseats[1:300,]
trainset
testset<- Carseats[301:400,]
testset

#building the SVM model on trainset #
m<- svm(trainset[,1:10], trainset[,11],cost=100)
summary(m)

# Using above model on testset #
p<- predict(m,testset[,1:10])
p

# create confusion matrix 
table(testset[,11],p)

#knn for Smarket
#get the data
Smarket
head(Smarket)

#splitting dataset
trainset<- Smarket[1:1000,]
trainset
testset<- Smarket[1001:1250,]
testset

#building the SVM model on trainset #
m<- svm(trainset[,1:8], trainset[,9])
summary(m)

# Using above model on testset #
p<- predict(m,testset[,1:8])
p

# create confusion matrix 
table(testset[,9],p)
