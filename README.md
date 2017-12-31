Practical Machine Learning Course Project
=========================================
by Przemek
----------

Introduction
------------

The goal of the project is to predict the manner in which the subject of the study did the exercise. 
The study is decribed here: http://groupware.les.inf.puc-rio.br/har and the data also comes from the study.

In the project, several classification models was created and the models are comparred in terms of predicting accuracy.
The main meassure for the model performance is prediction accuracy on the testing model.

Data preparation
----------------

Installing the packages needed in the analysis.
```{r}
install.packages("caret")
library(caret)
install.packages('e1071')
library(e1071)
```

Downloading the data
```{r}
url_train='https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
url_test='https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'

train_set<-read.csv(url_train)
test_set<-read.csv(url_test)
```

As many variables in the datasets have NA or empty values, they will be removed. Also First 7 variables are useless for the classification exercise.

Removing variables with almost only NA or empty values
```{r}
nas<-c()
empt<-c()
for(i in names(train_set)){
  nas[i]<-sum(is.na(train_set[,i]))
  empt[i]<-sum(train_set[,i]=="")
} 
training<-train_set[,nas<19000&empt<19000]
testing<-test_set[,nas<19000&empt<19000]
```

Removing first 7 variables that are also useless for the classification
```{r}
training<-training[,8:60]
testing<-testing[,8:60]
```

Spliting the training data set into training and testing subsets.The proportions 60:40.

```{r}
set.seed(20171230)
inTrain = createDataPartition(y=training$classe, p = .60)[[1]]
trainingsubset = training[ inTrain,]
testingsubset = training[-inTrain,]
```

Classification models
---------------------

Three classification models will be created:Classification Tree, Disriminant Classification and Random Forests. For each model, classification matrix for testing data subset will be calculated to obtain the classification accuracy of the models.

1. Classification Tree
```{r}
FitTREE <- train(classe~.,data=trainingsubset,method = "rpart")
print(FitTREE$finalModel)
#plot(FitTREE$finalModel,uniform=TRUE, main="Classification FitTREE")
#text(FitTREE$finalModel,use.n=TRUE,all=TRUE,cex=.8)
TREEpredict<-predict(FitTREE,testingsubset)
confusionMatrix(testingsubset$classe,TREEpredict)
```
The out of sample accuracy is 49,71% which is not a good result.


2. Discriminant Analysis Classification
```{r}
FitDDA <- train(classe~.,data=trainingsubset,method = "dda")
print(FitDDA$finalModel)

DDApredict<-predict(FitDDA,testingsubset)
confusionMatrix(testingsubset$classe,DDApredict)
```
For the DDA model, the out of sample accuracy is even lower, than for the Classification Tree. 47,21% for the DDA.


3. Random Forest Model
```{r}
FitRF5 <- train(classe~.,data=trainingsubset,method = "rf",trControl=trainControl(method = "cv", number = 5))
print(FitRF5$finalModel)

RF5predict<-predict(FitRF5,testingsubset)
confusionMatrix(testingsubset$classe,RF5predict)
```

Random Forests Classification seems to be the most accurate. 99,06% of the observations in the testing subset was classified correctly.

To explore the model, variable importance plot was created

```{r}
plot(varImp(FitRF5),main="Importance of the variables")

```
Predicting the values for the test set.
--------------------------------------
```{r}
test_set$PredictedClasse<-predict(FitRF5,testing)
test_set$PredictedClasse
```


Summary
-------
Random Forrest classification gives a very good results for the classification. In the out of sample classification, the accuracy war 99%. It was much higher comparred to the Tree and DDA Classification. Therefore, the Random Forest Classification was chosen as a final model.
