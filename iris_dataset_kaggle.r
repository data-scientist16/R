nrow(iris)#number of rows in the iris dataset
names(iris)#column names in the data set
train=sample(nrow(iris),nrow(iris)*.8)#creating a training set which is 80% of the dataset.
lda1=lda(Species~.-Species,data=iris,subset=train)#creating lda model to predict species against all columns but species. Model works on the training dataset.
pred1=predict(lda1,iris[-train,-5])#predicting the species by passing in the testing dataset
class=pred1$class#assigning the prediction class to the class variable
table(Actual=iris$Species[-train],Predicted=class)#creating a confusion matrix of actual vs predicted data
sum(class==iris[-train,5])/nrow(iris[-train,])#calculating the accuracy using the sum function.(correct predictions/total number of values in the dataset)