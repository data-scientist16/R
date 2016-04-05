traindataset=read.csv("C:/Users/joe/Downloads/train (1).csv", header=TRUE)
train=sample(nrow(traindataset), nrow(traindataset)*.8)
library(MASS)
lda1=lda(traindataset$Activity~.,data=traindataset,subset=train)
pred1=predict(lda1,traindataset[-train,-1])
conf_tab=table(Predicted=pred1$class,Actual=traindataset$Activity[-train])
Accuracy=sum(pred1$class==traindataset$Activity[-train])/length(traindataset$Activity[-train])
FP= conf_tab[2,1] / sum(conf_tab[,1])
TP= conf_tab[2,2] / sum(conf_tab[,2])
FP
TP
Accuracy
