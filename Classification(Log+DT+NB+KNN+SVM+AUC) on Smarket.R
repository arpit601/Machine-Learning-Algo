################################################################################
############1. DECISION TREE WITH AUC and CONFUSION MATRIX#####################
#Classification tree with rpart
library(rpart)# for creating decision tree
library(ISLR)
library(rpart.plot) # for plotting decision tree
library(ROCR) # for finding AUC
library(e1071)
library(class)
#get the data
Smarket

#top few rows of data
head(Smarket)

# to know about the data 
?Smarket

#shuffling the data ( could be done in case my class is not random)
set.seed(9850)
g<- runif(nrow(Smarket))
Smarket1<- Smarket[order(g),]
Smarket1

#attach kyphosis in global environment
attach(Smarket)

# dimensions in data
dim(Smarket)

#splitting the dataset
trainset<- Smarket1[1:1000,]
trainset
testset<- Smarket1[1001:1250,]
testset

#Building the decision tree model on trainset using rpart
m <- rpart(Direction~., data=trainset, method="class")
m
#Plot the decision tree
rpart.plot(m,type=3,extra=101,fallen.leaves=T)

#Using the above model on testset to predict ( here output will be class to create confusion matrix)
p<- predict(m,testset,type="class")
p

#Using the above model on testset to predict ( here output will be probabilities to find AUC)
p1<- predict(m,testset,type="prob")
p1

#Create the confusion matrix
table(testset[,9],p)

# Building ROC curve and finding AUC on Decision Tree 
library(ROCR)

# use prediction to convert into standardised format
pred<- prediction(p1[,2],testset[,9])
perf<- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1,lty=2)
AUClog2<- performance(pred,measure = "auc")@y.values[[1]]
cat("AUC:",AUClog2)
##############################################################################
########2. NAIVE BAYES CLASSIFIER WITH AUC and CONFUSION MATRIX #############
library(e1071)
# Building Naive Bayes classifier on dataset #
m5<- naiveBayes(Direction~.,data=trainset)
m5

#predict using model to create confusion matrix 
p2<- predict(m5,testset,type = "class")
p2

#predict using model to create ROC and find AUC
p3<- predict(m5,testset,type = "raw")
p3

# create confusion matrix 
table(p2,testset[,9])

### Building ROC curve and AUC using ROCR package
pred<- prediction(p3[,2],testset[,9])
perf<- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1,lty=2)
AUClog2<- performance(pred,measure = "auc")@y.values[[1]]
cat("AUC:",AUClog2)
##############################################################################
########3. SVM CLASSIFIER WITH AUC and CONFUSION MATRIX #############
#building the SVM model on trainset 
m6<- svm(trainset[,1:8], trainset[,9], probability = TRUE)
summary(m6)

# Using above model on testset ( in order to create confusion matrix)
p4<- predict(m6,testset[,1:8])
p4

# Using above model on testset ( in order to find AUC)
p6<- predict(m6,testset[,1:8], probability = TRUE)
p6
p7<- attributes(p6)
p7<- p7$probabilities
p7

# create confusion matrix 
table(testset[,9],p4)

### Building ROC curve and AUC
pred<- prediction(p7[,2],testset[,9])
perf<- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1,lty=2)
AUClog2<- performance(pred,measure = "auc")@y.values[[1]]
cat("AUC:",AUClog2)

##############################################################################
########4. KNN CLASSIFIER WITH AUC and CONFUSION MATRIX #############
library(class)
m7 <- knn(trainset[,1:8],testset[,1:8], cl=trainset[,9],k=3,prob = TRUE)
summary(m7)
attributes(m7)
m8<- attributes(m7)
m8<- m8$prob
m9<- 1-m8
#creating confusion matrix
table(testset[,9],m7)

### Building ROC curve and AUC
library(ROCR)
pred<- prediction(m9,testset[,9])
perf<- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1,lty=2)
AUClog2<- performance(pred,measure = "auc")@y.values[[1]]
cat("AUC:",AUClog2)

##############################################################################
########5. LOGISTIC REGRESSION WITH AUC and CONFUSION MATRIX #############
# building logistic on kyphosis
m10<- glm(Direction~. , data = trainset, family = binomial(link = "logit"))

# prediction
p8 <- predict(m10, testset, type = "response")
p8

#predicting values using prediction
kyphosis_pred <- rep("Down",250)
kyphosis_pred[p8>0.5]<- "Up"
kyphosis_pred

#confusion matrix
table(testset[,9],kyphosis_pred)

### Building ROC curve and AUC
pred<- prediction(p8,testset[,9])
perf<- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1,lty=2)
AUClog2<- performance(pred,measure = "auc")@y.values[[1]]
cat("AUC:",AUClog2)

#K- fold cross validation ##

library(caret)
library(rpart)
library(kernlab)


# define training control
train_control<- trainControl(method = "cv", number=10)
##########method##########
train_control

#train the model
model <- train(diabetes~., data = PimaIndiansDiabetes1,trControl=train_control,method="glm")

print(model)