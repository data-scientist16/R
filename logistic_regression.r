library(ISLR)
library(MASS)
lda1=lda(Default$default~Default$balance+Default$student,data=Default)#creating a lda logistic model to predict if a person would default, 
#response is default and student and balance are the predictors 
glm1=glm(Default$default~Default$balance+Default$student,data=Default,family=binomial)#creating a glm logistic model for the same purpose, 
#response is default and student and balance are the predictors
pred1=predict(glm1,Default,type='response')#Predicting the response using the glm model
plot(Default$balance,pred1,xlab='balance',ylab='P(Y=1|x)',type='n')#plotting balance vs predictions, type=n makes sure there are no points plotted on the 
#graph but just the co-ordinates
lines(sort(Default$balance[Default$student=="Yes"]),sort(pred1[Default$student=="Yes"]),col='orange')#drawing lines for the plot, when it's not a student
lines(sort(Default$balance[Default$student=="No"]),sort(pred1[Default$student=="No"]),col='lightblue')#drawing lines for the plot, when it's a student
legend('topleft',legend=c('student','non-student'),col=c('orange','lightblue'),lty=c(1,1))#used to label the plot
#P(X) = exp(β0 +β1 X1+β2 X2+β3 X3) / (1 + exp(β0 +β1 X1+β2 X2+β3 X3))#formula used
attach(Default)
glm2=glm(default~balance+student+income,data=Default,family=binomial)#creating a glm model 
predict(glm2,newdata=data.frame(income=35000,balance=2000,student="Yes"),type='response')#predicting the response when income==35000, balance==2000 
#and given it is a student.
