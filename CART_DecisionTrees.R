# Setting the working directory
rm(list=ls(all=TRUE))

par(mfrow=c(1,1))


#setwd("")
#install.packages("RCurl")
library(RCurl)
univ=read.table(text = getURL("https://raw.githubusercontent.com/rajsiddarth119/Datasets/master/Bank_dataset.csv"), header=T, sep=',',
                col.names = c('ID', 'age', 'exp', 'inc', 
                              'zip', 'family', 'ccavg', 'edu', 
                              'mortgage', 'loan', 'securities', 
                              'cd', 'online', 'cc'))

# removing the id, Zip and experience. Experience is correlated to age
univ=univ[,-c(1,3,5)]

univ$family=as.factor(univ$family)
univ$edu=as.factor(univ$edu)
univ$mortgage=as.factor(univ$mortgage)
univ$loan=as.factor(univ$loan)
univ$securities=as.factor(univ$securities)
univ$cd=as.factor(univ$cd)
univ$online=as.factor(univ$online)
univ$cc=as.factor(univ$cc)

#convert mortgage as numeric
univ$mortgage=as.numeric(univ$mortgage)

rows=seq(1,5000,1)
set.seed(123)
trainRows=sample(rows,3000)
set.seed(123)
remainingRows=rows[-(trainRows)]
testRows=sample(remainingRows, 1000)
evalRows=rows[-c(trainRows,testRows)]

train = univ[trainRows,] 
test=univ[testRows,] 
eval=univ[evalRows,]

rm(univ,evalRows,remainingRows,rows,testRows,trainRows)

#install.packages("rpart")
library(rpart)
dtCart = rpart(inc ~., data=train, method="anova")    
plot(dtCart, main="Decision Tree for Income", 
     margin=0.15, uniform=TRUE)
text(dtCart, use.n=T)

predCartTrain=predict(dtCart, newdata=train, type="vector")
predCartTest=predict(dtCart, newdata=test, type="vector")
predCartEval=predict(dtCart, newdata=eval, type="vector")
install.packages("DMwR")

#library(DMwR)
regr.eval(train[,"inc"], predCartTrain, train.y = train[,"inc"])
regr.eval(test[,"inc"], predCartTest, train.y = train[,"inc"])
regr.eval(eval[,"inc"], predCartEval, train.y = train[,"inc"])

printcp(dtCart)

dtCart=rpart(inc ~.,data=train,method="anova", cp=0.001)
printcp(dtCart)

dtCart=rpart(inc ~.,data=train,method="anova", cp=0.002)
printcp(dtCart)

predCartTrain=predict(dtCart, newdata=train, type="vector")
predCartTest=predict(dtCart, newdata=test, type="vector")
predCartEval=predict(dtCart, newdata=eval, type="vector")


regr.eval(train[,"inc"], predCartTrain, train.y = train[,"inc"])
regr.eval(test[,"inc"], predCartTest, train.y = train[,"inc"])
regr.eval(eval[,"inc"], predCartEval, train.y = train[,"inc"])

#Decision Trees using CART (For Classification Problem)
dtCart = rpart(loan~., data=train, method="class")    
plot(dtCart, main="Classification Tree for loan Class",
     margin=0.15, uniform=TRUE)
text(dtCart,use.n=T)
summary(dtCart)

a=table(train$loan, predict(dtCart, newdata=train, type="class"))
(a[2,2])/(a[2,1]+a[2,2])*100

