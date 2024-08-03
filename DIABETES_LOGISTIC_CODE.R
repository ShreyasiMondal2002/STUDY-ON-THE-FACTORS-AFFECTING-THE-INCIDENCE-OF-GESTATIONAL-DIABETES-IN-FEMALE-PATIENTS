rm(list=ls())
library(corrplot)
library(car)
library(dplyr)
library(InformationValue)
library(caret)
library(ISLR)
library(cutpointr)




setwd("C:/Users/USE/Downloads")
getwd()
data=read.csv(file="diabetes.csv")
View(data)
attach(data)
data.500=read.csv(file="diabetes.subset.csv")
data.500
View(data.500)
data.267=read.csv(file="diabetes.data.267.csv")
data.267
View(data.267)
Preg=0
for(i in 1:length(Pregnancies))
{
 if(Pregnancies[i]<=3)
 Preg[i]=0
 else
 Preg[i]=1
}

Preg
plot(data)
hist(Age,xlab = "Age",col = "pink",main = "Histogram of Age",breaks =20,bg=2)




# The max no. of patients suffering from Diabetes belongs to ages between 21-30



boxplot(BMI~as.factor(Age),col=3:8,xlab = "Age(Grouped)",main="Boxplots of 
BMI for Different Age Groups")







boxplot(BMI~Preg,main="Boxplot of BMI for different Pregnancy group",col=2:3,boxwex=0.5)


boxplot(DiabetesPedigreeFunction~Outcome,main="Boxplot of Diabetes Pedigree Function for different Outcome",xlab = "Outcome(0-Non-Diabetic,1-Diabetic)",col=4:5)


## OBTAINING RESULTS USING TEST DATA

#fitting of model, multicollinearity, test of significance ##


## LOGIT MODEL ##


model=glm(Outcome~.,family = binomial(link = "logit"),data=data.500)
summary(model)
vif(model)


## PROBIT MODEL ##


model2=glm(Outcome~.,family = binomial(link = "probit"),data=data.500)
summary(model2)
vif(model2)


#Prediction and evaluation of quality of model (logit model) ##
## LOGIT OPTIMAL ##

mean(predicted)

median(predicted)

predicted=predict(model,data.500,type = "response")

optimal=optimalCutoff(data.500$Outcome, predicted)

p0=optimal             


Y.hat.logit=ifelse(predicted>p0,1,0)

Y.hat.logit
t.logit=table(data.500$Outcome,Y.hat.logit)
t.logit

TPR.logit=t.logit[2,2]/(t.logit[1,2]+t.logit[2,2])
FPR.logit=t.logit[2,1]/(t.logit[2,1]+t.logit[1,1])
TPR.logit
FPR.logit

m.error.logit=1-TPR.logit-FPR.logit
m.error.logit



## TRYING OUT VARIOUS P* VALUES ##


## LOGIT MODEL ##


P_star.logit=c(0.24,0.36,0.5,0.61)
TPR.l=FPR.l=m.logit=array(0)
k=1
for(i in p.logit)
{
 Y.hat.l=ifelse(predicted>i,1,0)
 t.logit=table(Y.hat.l,data.500$Outcome)
 TPR.l[k]=t.logit[2,2]/(t.logit[1,2]+t.logit[2,2])
 FPR.l[k]=t.logit[2,1]/(t.logit[2,1]+t.logit[1,1])
 m.logit[k]=1-TPR.l[k]-FPR.l[k]  
 k=k+1
 }
Y.hat.l
TPR.l
FPR.l
m.logit=c(0.2685,0.1869,0.1103,0.0291)
plot(p_star.logit,m.logit,type="l",ylab="Misclassification Error for Logit model",xlab="pi hat values")



## FOR THE VALUE p0=0.61 WE HAVE LEAST MISCLASSIFICATION ERROR IN CASE OF LOGIT MODEL ##






## APPLIED DATA SET ##

## LOGIT MODEL ##

model.267=glm(Outcome~.,family = binomial(link = "logit"),data=data.267)
summary(model.267)
vif(model.267)

## PROBIT MODEL ##

model2=glm(Outcome~.,family = binomial(link = "probit"),data=data.267)
summary(model2)
vif(model2)

predicted=predict(model.267,data.267,type = "response")
optimal=optimalCutoff(data.267$Outcome, predicted)
p0=optimal             
p.l=0.61


Y.hat.logit.267=ifelse(predicted>0.61,1,0)
Y.hat.logit.267

t.logit.267=table(data.267$Outcome,Y.hat.logit.267)
t.logit.267

TPR.logit.267=t.logit.267[2,2]/(t.logit.267[1,2]+t.logit.267[2,2])
FPR.logit.267=t.logit.267[2,1]/(t.logit.267[2,1]+t.logit.267[1,1])


TPR.logit.267
FPR.logit.267


m.error.logit.267=1-TPR.logit.267-FPR.logit.267
m.error.logit.267

