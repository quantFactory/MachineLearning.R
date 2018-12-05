install.packages("dplyr")
install.packages("caTools")
install.packages("e1071")

library(ElemStatLearn)
library(dplyr)
library(caTools)
library(e1071)




getwd()
setwd("F://wqd7003/")
# Naive Bayes

 #Importing the dataset
dataset = read.csv('F://wqd7003/attr.csv')
#dataset = dataset[3:5]
dataset

# Encoding the target feature as factor
dataset$Attrition = factor(dataset$Attrition, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(111)

split = sample.split(dataset$Attrition, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
str(dataset)
training_set[-35] = scale(training_set[-35])
test_set[-35] = scale(test_set[-35])

#removing Nan columns after feature scaling 
training_set <- training_set %>% select(-c('EmployeeCount','StandardHours','Over18'))
dim(training_set)
test_set<- test_set %>% select(-c('EmployeeCount','StandardHours','Over18'))
str(test_set)




# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
dim(training_set)

#classifying using Naive Bayes classifer
classifier = naiveBayes(x = training_set[-32],
                        y = training_set$Attrition)

summary(classifier)
# Predicting the Test set results

y_pred = predict(classifier, newdata = test_set[-32])
y_pred

# Making the Confusion Matrix
confusion_matrix = table(test_set[, 32], y_pred)
confusion_matrix
install.packages("MLmetrics")
library(MLmetrics)

y_true = test_set[,32]
y_true

#F1
F1 = F1_Score(y_true = test_set[,32],y_pred = y_pred)
F1
#accuracy
acc =Accuracy(y_pred = y_pred,y_true = test_set[,32])
acc

#precision
att_precision = Precision(y_true = y_true, y_pred = y_pred) ; att_precision

#recall
att_recall = Recall(y_pred = y_pred,y_true = y_true) ; att_recall

#AUC
att_auc <-  AUC(y_pred = y_pred, y_true = y_true) ; att_auc

#cofusionMtrix data frame 
att_confusion  <-  ConfusionDF(y_pred = y_pred, y_true = y_true) ; att_confusion

#normal consuon matrxi
att_cm <-  ConfusionMatrix(y_pred = y_pred, y_true = y_true); att_cm

sum_0 <-  sum(test_set$Attrition == 0) ; sum_0
sum_1 <-  sum(test_set$Attrition ==1) ; sum_1

total = 262 + 46 +26+33 ; total
tfpfn = 46+26; tfpfn
Miss_Classification_rate <-  tfpfn/total; Miss_Classification_rate


accuracy = (369+5)/(369+1+5+66)
accuracy
##accuracy = %84
#calcualting recall 
(369)/(369+1)
#recall = 0.99 %$99 
#precsion 
(369)/(369 +66) #precsion = %84

# Visualising the Training set results
library(ElemStatLearn)
str(training_set)
set = training_set
set

##getting to know which features atre most significant

accuracy = (369+5)/(369+1+5+66)
accuracy
##accuracy = %84
#calcualting recall 
(369)/(369+1)
#recall = 0.99 %$99 
#precsion 
(369)/(369 +66) #precsion = %84
