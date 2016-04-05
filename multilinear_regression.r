library(ISLR)
library(MASS)
train=sample(nrow(Carseats), nrow(Carseats)*.8)#creating a training dataset
lm_sales=lm(Sales~.,data=Carseats,subset=train)#creating a linear model of Sales against all other predictors in the training dataset
lm_interaction=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats,subset=train)#creating a model of Sales against all the other predictors with interactions
actual_sales=Carseats$Sales[-train]#sales from test data assigned to actual sales
pred1=predict(lm_sales,Carseats[-train,-1])#predicting the test data sales
pred2=predict(lm_interaction,Carseats[-train,-1])#predicting test data sales with interactions
mean((actual_sales-pred1)^2)#mean square error for 1st model, MSE=.8490
mean((actual_sales-pred2)^2)#mean square error for the model with interactions, MSE=.8091
##r^2 value is 0.8722 w/0 interactions and .8741 with interactions hence the effect of interactions are not significant.