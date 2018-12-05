install.packages("dplyr")
install.packages("caTools")
install.packages("e1071")
install.packages("ElemStatLearn")
install.packages('randomForest')
install.packages("Amelia")
install.packages("labeling")
install.packages("MLmetrics")

library(MLmetrics)
library(randomForest)
library(ElemStatLearn)
library(dplyr)
library(caTools)
library(e1071)
library(Amelia)
library(ggplot2)
library(labeling)
setwd("F://wqd7003/")
# Importing the dataset
dataset = read.csv('F://wqd7003/attr.csv')
  
## exploratory data analysis 
#1- checking for missing values   
missmap(dataset)

plot(dataset$Attrition)
## observing attrition percentage in dataset
ggplot(dataset,aes(Attrition,fill = Attrition)) + geom_bar()


# Encoding the target feature as factor
dataset$Attrition = factor(dataset$Attrition, levels = c(0, 1))

str(dataset)
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Attrition, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-35] = scale(training_set[-35])
test_set[-35] = scale(test_set[-35])

# Fitting Random Forest Classification to the Training set
set.seed(123)

training_set <- training_set %>% select(-c('EmployeeCount','StandardHours','Over18'))

training_set["Attrition"]
str(training_set)
dim(training_set)
str(test_set)

classifier = randomForest(x = training_set[-1],
                          y = training_set$Attrition,
                          ntree = 128)
classifier

# Predicting the Test set results
library(dplyr)
test_set<- test_set %>% select(-c('EmployeeCount','StandardHours','Over18'))
test_set['Attrition']
y_pred = predict(classifier, newdata = test_set[-32])
#y_pred = predict(classifier, newdata = training_set[-32])

y_pred
y_true = test_set[,32]; y_true
# Making the Confusion Matrix
cm = table(test_set[, 32], y_pred)
cm

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

total = 306+48+2+11 ; total
tfpfn = 48+2 ; 
Miss_Classification_rate <-  tfpfn/total; Miss_Classification_rate







# Visualising the Training set results

library(ElemStatLearn)

set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)

colnames(grid_set) = c('Age', 'DailyRate')
training_set
y_grid = predict(classifier, grid_set)
y_grid

plot(set[, 1:2],
     main = 'Random Forest Classification (Training set)',
     xlab = 'Age', ylab = 'DailyRate',
     xlim = range(X1), ylim = range(X2))


contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 32] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)

set = test_set

X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)

colnames(grid_set) = c('Age', 'DailyRate')

y_grid = predict(classifier, grid_set)

plot(set[, 1:2], main = 'Random Forest Classification (Test set)',
     xlab = 'Age', ylab = 'DailyRate',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Choosing the number of trees
plot(classifier)
