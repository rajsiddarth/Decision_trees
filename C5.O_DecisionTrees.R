# Setting the working directory
rm(list=ls(all=TRUE))

#setwd("")
install.packages("RCurl")
library(RCurl)

univ=read.table(text = getURL("https://raw.githubusercontent.com/rajsiddarth119/Datasets/master/Bank_dataset.csv"), header=T, sep=',',
                col.names = c('ID', 'age', 'exp', 'inc', 
                              'zip', 'family', 'ccavg', 'edu', 
                              'mortgage', 'loan', 'securities', 
                              'cd', 'online', 'cc'))


# Removing the id, Zip and experience. Experience is correlated to age
univ=univ[,-c(1,3,5)]

univ$family=as.factor(univ$family)
univ$edu=as.factor(univ$edu)
univ$mortgage=as.factor(univ$mortgage)
univ$loan=as.factor(univ$loan)
univ$securities=as.factor(univ$securities)
univ$cd=as.factor(univ$cd)
univ$online=as.factor(univ$online)
univ$cc=as.factor(univ$cc)

# Convert mortgage as numeric
univ$mortgage=as.numeric(univ$mortgage)

# Divide the data into train, test and eval
set.seed(123)
rows=seq(1,5000,1)
trainRows=sample(rows,3000)
remainingRows=rows[-(trainRows)]
testRows=sample(remainingRows, 1000)
evalRows=rows[-c(trainRows,testRows)]

train = univ[trainRows,] 
test=univ[testRows,] 
eval=univ[evalRows,]

rm(univ,evalRows,remainingRows,rows,testRows,trainRows)

# Decision Trees using C5.0 (For Classification Problem)
library(C50)

dtC50= C5.0(loan ~ ., data = train, rules=F)
summary(dtC50)
plot(dtC50)


dtC50= C5.0(loan ~ ., data = train, rules=TRUE)
summary(dtC50)
C5imp(dtC50, pct=TRUE)

a = table(train$loan, predict(dtC50, newdata=train, type="class"))
rcTrain=(a[2,2])/(a[2,1]+a[2,2])*100
a=table(test$loan, predict(dtC50, newdata=test, type="class"))
rcTest=(a[2,2])/(a[2,1]+a[2,2])*100

#Experiment for best results
a = table(eval$loan, predict(dtC50, newdata=eval, type="class"))
rcEval=(a[2,2])/(a[2,1]+a[2,2])*100

cat("Recall in Training", rcTrain, '\n',
    "Recall in Testing", rcTest, '\n',
    "Recall in Evaluation", rcEval)

#Test by increasing the number of bins in inc and ccavg to 10
#Test by changing the bin to euqalwidth in inc and ccavg

rm(a,rcEval,rcTest,rcTrain)

