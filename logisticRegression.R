install.packages('caTools')
library(caTools)
install.packages("Amelia")
library(Amelia)

attrition_df = read.csv('F://wqd7003/attr.csv')
set.seed(101)

#spliting data to test and training set, ratio == 70/30
sample <-  sample.split(attrition_df$Attrition, SplitRatio = 0.7)
att_train <-  subset(attrition_df,sample==T)
att_train
att_test <-  subset(attrition_df,sample == F)
att_test

?glm()

model <-  glm(Attrition ~ . ,family = binomial(link = 'logit'),data = att_train)

summary(model)
##feature enginner ing using step fucntion 
## information from model summary shown us variables that are nit signifianalty 
##add to the fit , with "step()" funtion we can delte those variables autoamatiically 
## it uses an AIC( Akaike information criterion) it tires a bunch of sifferent combnation in logsitcs 
##regression models and keep the once that are sigificant 
new.step.model  <-  step(model)

summary(new.step.model)
missmap(attrition_df)
#validation
att_test$Predicted.Attriton <- predict(model,newdata = att_test,type = 'response')
table(att_test$Attrition,att_test$Predicted.Attriton > 0.8  )

att_test$Predicted.Attriton

##measure accuracy of the model 
accuracy = (369+5)/(369+1+5+66)
accuracy
##accuracy = %84
#calcualting recall 
(369)/(369+1)
#recall = 0.99 %$99 
#precsion 
(369)/(369 +66) #precsion = %84


  

