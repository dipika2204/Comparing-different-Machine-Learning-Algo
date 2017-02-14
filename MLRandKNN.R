#prepare Data
mydata = read.csv("wine.csv")
str(mydata)
mydata$TYPE<-factor(mydata$TYPE)
mydata$output<-relevel(mydata$TYPE,ref="1")

#Develop Multinomial Logistic Regression Model
library(nnet)
# dividing the data into test and training
library(caTools)
no=mydata[,1]
boolean_wine = sample.split(no,SplitRatio = 2/3,group = NULL)
Train.x=cbind(MALIC.ACID,MAGNESIUM,TOTAL.PHENOLS,NONFLAVANOID.PHENOLS,PROLINE,HUE)[boolean_wine,]
Test.x=cbind(MALIC.ACID,MAGNESIUM,TOTAL.PHENOLS,NONFLAVANOID.PHENOLS,PROLINE,HUE)[!boolean_wine,]
Train.output=TYPE[boolean_wine]
length(Train.output)
Test.output=TYPE[!boolean_wine]
length(Test.output)
#Multinomial Logistic Regression
mymodel<-multinom(TYPE~MALIC.ACID+MAGNESIUM+TOTAL.PHENOLS+NONFLAVANOID.PHENOLS+PROLINE+HUE,data=mydata)
summary(mymodel)
#prediction
test_predict=predict(mymodel,Test.x,"probs")
test_predict
length(test_predict)
table1<-table(test_predict,Test.output)
print(table1)
#Accuracy
ACCURACY1=sum(diag(table1))/sum(table1)
ACCURACY1
#Recall
recall1=(table1[1,1]/(sum(table1[1,])))
recall2=(table1[2,2]/(sum(table1[2,])))
recall3=(table1[3,3]/(sum(table1[3,])))
recall_logistic=(recall3+recall2+recall1)/3
recall_logistic
#Precision
prec_1=(table1[1,1]/(sum(table1[,1])))
prec_2=(table1[2,2]/(sum(table1[,2])))
prec_3=(table1[3,3]/(sum(table1[,3])))
prec_logistic=(prec_3+prec_2+prec_1)/3
prec_logistic
#Plots
plot(TYPE ~ MALIC.ACID + MAGNESIUM+ TOTAL.PHENOLS+ NONFLAVANOID.PHENOLS+ PROLINE +  HUE,data=mydata, col = rgb(0, 0, 0, 0.3), pch = 19)

#KNN
set.seed(1)
library(class)
knn.pred=knn(Train.x,Test.x,Train.output,k=9)
test.class=TYPE[!boolean_wine]
table=table(knn.pred,test.class)
table
#Accuracy
ACCURACY=(sum(diag(table)))/sum(table)
ACCURACY
#RECALL
recall1=(table[1,1]/(sum(table[1,])))
recall1
recall2=(table[2,2]/(sum(table[2,])))
recall3=(table[3,3]/(sum(table[3,])))
recall=(recall3+recall2+recall1)/3
recall
#PRECISION
prec1=(table[1,1]/(sum(table[,1])))
prec2=(table[2,2]/(sum(table[,2])))
prec3=(table[3,3]/(sum(table[,3])))
prec=(prec1+prec2+prec3)/3
prec
