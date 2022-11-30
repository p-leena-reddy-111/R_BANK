#Bank dataset
#install.packages("mice")
#install.packages("VIM")
library(mice)
library(VIM)
data=read.csv(file.choose(), header=TRUE)
head(data)
sum(data$y=="yes")
str(data)
summmary(data)

#Converting unknown values to NA
data[data=="unknown"]<-NA
head(data)

#Converting it into factors
data$marital=as.factor(as.numeric(factor(data$marital)))
data$education=as.factor(as.numeric(factor(data$education)))
data$default=as.factor(as.numeric(factor(data$default)))
data$housing=as.factor(as.numeric(factor(data$housing)))
data$loan=as.factor(as.numeric(factor(data$loan)))
data$job=as.factor(as.numeric(factor(data$job)))
data$contact=as.factor(as.numeric(factor(data$contact)))
data$month=as.factor(as.numeric(factor(data$month)))
data$day_of_week=as.factor(as.numeric(factor(data$day_of_week)))
data$poutcome=as.factor(as.numeric(factor(data$poutcome)))
data$y=factor(data$y)
data$y<-ifelse(data$y=="yes",1,0)
head(data)
summary(data)

#Imputing using mice
library(mice)
impute<-mice(data,m=1,seed=123)
data<-complete(impute,1)
head(data)

#Boruta Feature
newdata=data[,c(-1,-5,-6,-7,-10,-11)]
head(newdata)


#random forest
set.seed(120)  # Setting seed
library("randomForest")
classifier_RF = randomForest(y~.,data=newdata, ntree=50)


#Decision Tree
library("party")
DT<- ctree(y ~ .,newdata)

#SVM
#install.packages('e1071')
library(e1071)
svmmodel = svm(y~.,newdata)


#logistic Regression
lr<-glm(y~.,data=newdata)




#test data
test=read.csv(file.choose(), header=TRUE)
head(test)
nrow(test)

test[test=="unknown"]<-NA
head(test)
length(test)

#Imputing using mice

test$marital=as.factor(as.numeric(factor(test$marital)))
test$education=as.factor(as.numeric(factor(test$education)))
test$default=as.factor(as.numeric(factor(test$default)))
test$housing=as.factor(as.numeric(factor(test$housing)))
test$loan=as.factor(as.numeric(factor(test$loan)))
test$job=as.factor(as.numeric(factor(test$job)))
test$contact=as.factor(as.numeric(factor(test$contact)))
test$month=as.factor(as.numeric(factor(test$month)))
test$day_of_week=as.factor(as.numeric(factor(test$day_of_week)))
test$poutcome=as.factor(as.numeric(factor(test$poutcome)))
test$y=factor(test$y)
test$y<-ifelse(test$y=="yes",1,0)


impute_test<-mice(test,m=1,seed=123)
print(impute_test)
test<-complete(impute_test,1)

newdata_test=test[,c(-1,-5,-6,-7,-10,-11)]
head(newdata_test)
#prediction
nrow(newdata_test)

y_rf = predict(classifier_RF, newdata = newdata_test)
y_dt=predict(DT, newdata = newdata_test)
y_svm=predict(svmmodel,newdata=newdata_test)
y_lr=predict(lr,newdata=newdata_test)


# Confusion Matrix
confusion_rf = table(newdata_test$y, y_rf)
confusion_rf
confusion_dt = table(newdata_test$y, y_dt)
confusion_dt
confusion_svm = table(newdata_test$y, y_svm)
confusion_svm
confusion_lr = table(newdata_test$y, y_lr)
confusion_lr


#Random Forest
rf_predict <- ifelse(y_rf>0.5,"1","0")
table(newdata_test$y,rf_predict)
missing_classerr <- mean(rf_predict != newdata_test$y)
print(missing_classerr)
print(paste('Accuracy Random Forest=', 1 - missing_classerr))

#Decision Tree
dt_predict=ifelse(y_dt>0.5, "1", "0")
table(newdata_test$y,dt_predict)
missing_classerr <- mean(dt_predict!= newdata_test$y)
print(missing_classerr)
print(paste('Accuracy Decision Tree=', 1 - missing_classerr))

#SVM
svm_predict=ifelse(y_svm>0.5,"1","0")
table(newdata_test$y,svm_predict)
missing_classerr <- mean(svm_predict!= newdata_test$y)
print(missing_classerr)
print(paste('Accuracy SVM=', 1 - missing_classerr))

#LR
lr_predict=ifelse(y_lr>0.5, "1", "0")
table(newdata_test$y,lr_predict)
missing_classerr <- mean(lr_predict!= newdata_test$y)
print(missing_classerr)
print(paste('Accuracy Logistic Regression=', 1 - missing_classerr))



