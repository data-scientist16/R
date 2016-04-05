Auto<- read.csv("C:/Users/joe/Downloads/Auto (1).csv")#reading the file
Auto$horsepower <- as.numeric(levels(Auto$horsepower))[Auto$horsepower]#converting different levels to their actual numeric values
Auto$horsepower[is.na(Auto$horsepower)]=mean(Auto$horsepower, na.rm=TRUE)#replacing the NAs with the mean of the column
library(psych)#used for pairs.panels function
pairs.panels(Auto)#checking for correlations between variables
train=sample(nrow(Auto),nrow(Auto)*.8)#creating a training dataset using 80% of values from the dataset
test=Auto$horsepower[-train]#assigning the rest 20% as the testing dataset
lm1=lm(mpg~horsepower,data=Auto,subset=train)#creating a linear model of mpg vs horsepower using the training dataset
lm2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)#creating a degree 2 model of mpg vs horsepower using the training dataset
lm3=lm(mpg~poly(horsepower,5),data=Auto,subset=train)#creating a degree 5 model of mpg vs horsepower using the training dataset
range=seq(min(Auto$horsepower),max(Auto$horsepower))#assigning a variable called range the range of values in horsepower
horsepower_df=data.frame(horsepower=Auto[train,"horsepower"])#creating a horsepower dataframe using the train data to find training error, 
#test data is used at a later point in pred11..
plot(Auto$horsepower,Auto$mpg,xlab="horsepower",ylab="MPG",cex=.5,col="purple")#plotting hp vs mpg
pred1=predict(lm1,horsepower_df)#predicting the values against the train data, to plot lines change horsepower_df to range 
#so that there would be equal number of entries
lines(range,pred1,lwd=2,col="blue",type="l")#plotting lines
pred2=predict(lm2,horsepower_df)#predicting the values against the train data, to plot lines change horsepower_df to range
#so that there would be equal number of entries
lines(range,pred2,lwd=2,col="green",type="l")#plotting lines
pred3=predict(lm3,horsepower_df)#predicting the values against the train data, to plot lines change horsepower_df to range so that
#there would be equal number of entries
lines(range,pred3,lwd=2,col="red",type="l")#plotting lines
actual_mpg1=Auto$mpg[train]#assigning train mpg as actual mpg to test it against the predicted values
#training error
mean((actual_mpg1-pred1)^2)#finding mean squared error for each model
mean((actual_mpg1-pred2)^2)#finding mean squared error for each model
mean((actual_mpg1-pred3)^2)#finding mean squared error for each model
actual_mpg2=Auto$mpg[-train]#assigning test mpg as actual mpg to test it against the predicted values
#testing error
horsepower_df1=data.frame(horsepower=Auto[test,"horsepower"])#creating a horsepower dataframe using the test data to find testing error
pred11=predict(lm1,horsepower_df1)#predicting the values against the test data
pred22=predict(lm2,horsepower_df1)#predicting the values against the test data
pred33=predict(lm3,horsepower_df1)#predicting the values against the test data
mean((actual_mpg2-pred11)^2)#finding mean squared error for each model
mean((actual_mpg2-pred22)^2)#finding mean squared error for each model
mean((actual_mpg2-pred33)^2)#finding mean squared error for each model