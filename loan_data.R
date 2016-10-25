library(dplyr)
library(magrittr)
library(ggplot2)
library(caTools)
library(ROCR)
library(pROC)
library(caret)
library(randomForest)

#load data

loan <- read.csv("~/loan.csv")

str(loan)

summary(loan)

######
#Explanatory analysis

#fraction of bad loans per state
loan_state=loan %>%
  group_by(addr_state) %>%
  summarise(fraction_bad = mean(status))
ggplot(data=loan_state,aes(x=addr_state,y=fraction_bad))+geom_bar(stat="identity",aes(fill=addr_state))+ ggtitle("Fraction of bad loans per state") 


#fraction of bad loans per delinq
loan_delinq=loan %>%
  group_by(delinq) %>%
  summarise(fraction_bad = mean(status))
ggplot(data=loan_delinq,aes(x=delinq,y=fraction_bad))+geom_bar(stat="identity",aes(fill=delinq))+ ggtitle("Fraction of bad loans per delinquency") 


#fraction of bad loans per term
loan_term=loan %>%
  group_by(term) %>%
  summarise(fraction_bad = mean(status))
ggplot(data=loan_term,aes(x=term,y=fraction_bad))+geom_bar(stat="identity",aes(fill=term))+ ggtitle("Fraction of bad loans per term") 

#fraction of bad loans per emp_length
loan_emp=loan %>%
  group_by(emp_length) %>%
  summarise(fraction_bad = mean(status))
ggplot(data=loan_emp,aes(x=emp_length,y=fraction_bad))+geom_bar(stat="identity",aes(fill=emp_length)) + ggtitle("Fraction of bad loans per employment length") 

#fraction of bad loans per home_ownership
loan_home=loan %>%
  group_by(home_ownership) %>%
  summarise(fraction_bad = mean(status))
ggplot(data=loan_home,aes(x=home_ownership,y=fraction_bad))+geom_bar(stat="identity",aes(fill=home_ownership))+ ggtitle("Fraction of bad loans per home ownership") 

#fraction of bad loans per purpose
loan_purpose=loan %>%
  group_by(purpose) %>%
  summarise(fraction_bad = mean(status))
ggplot(data=loan_purpose,aes(x=purpose,y=fraction_bad))+geom_bar(stat="identity",aes(fill=purpose))+ ggtitle("Fraction of bad loans per purpose") 

#funded amount

boxplot(funded_amnt~status,data=loan,names=c("good","bad"),main="Boxplot of funded amount")

#interest rate

boxplot(log(int_rate)~status,data=loan,names=c("good","bad"),main="Boxplot of interest rate")

#annual income

boxplot(log(annual_inc)~status,data=loan,names=c("good","bad"),main="Boxplot of log(annual income)")

#dti

boxplot(dti~status,data=loan,names=c("good","bad"),main="Boxplot of debt to income ratio")

#earliest_cr_line

boxplot(earliest_cr_line~status,data=loan,names=c("good","bad"),main="Boxplot of credit line length")

#mths_since_last_delinq

boxplot(mths_since_last_delinq~status,data=loan,names=c("good","bad"),main="Boxplot of months since last delinquency")

#open_acc

boxplot(open_acc~status,data=loan,names=c("good","bad"),main="Boxplot of number of oppen accounts")

#revol_bal

boxplot(log(1+revol_bal)~status,data=loan,names=c("good","bad"),main="Boxplot of revolving balance")

#total_acc

boxplot(total_acc~status,data=loan,names=c("good","bad"),main="Boxplot of number of all accounts")

#total_rec_prncp

boxplot(total_rec_prncp~status,data=loan,names=c("good","bad"),main="Boxplot of total payments so far")

#frac_funded

loan$frac_funded=loan$funded_amnt/loan$loan_amnt
boxplot(log(frac_funded~status,data=loan,names=c("good","bad"),main="Boxplot of funded amount/requested amount")

##################       
#logistic regression
loan$status=as.factor(loan$status)
loan$state=as.factor(loan$addr_state %in% c("AK","DC","UT"))
loan$delinq=as.factor(!(is.na(loan$mths_since_last_delinq)) & loan$mths_since_last_delinq<60)
loan$frac_funded=loan$funded_amnt/loan$loan_amnt
loan$frac_pymnt=loan$total_pymnt/loan$funded_amnt

L=rep(0,20)
for (i in 1:20){
w=i^(as.numeric(loan$status)-1)
logmodel=glm(status~installment+frac_pymnt+delinq+addr_state+frac_funded+funded_amnt+term+int_rate+home_ownership+log(annual_inc)+purpose+dti+delinq_2yrs+log(earliest_cr_line)+open_acc+revol_bal+total_acc+total_rec_prncp+total_rec_int,data=loan,weight=w,family=binomial)
Predicted = predict(logmodel, data=loan,type="response")
ROCpred = prediction(Predicted, loan$status)
ROCperf = performance(ROCpred, "tpr", "fpr")
L[i]=as.numeric(performance(ROCpred,"auc")@y.values)
}

i=which.max(L)
w=i^(as.numeric(loan$status)-1)
logmodel=glm(status~installment+frac_pymnt+delinq+addr_state+frac_funded+funded_amnt+term+int_rate+home_ownership+log(annual_inc)+purpose+dti+delinq_2yrs+log(earliest_cr_line)+open_acc+revol_bal+total_acc+total_rec_prncp+total_rec_int,data=loan,weight=w,family=binomial)
summary(logmodel)
Predicted = predict(logmodel, data=loan,type="response")
ROCpred = prediction(Predicted, loan$status)
ROCperf = performance(ROCpred, "tpr", "fpr")
plot(ROCperf,colorize = TRUE)
as.numeric(performance(ROCpred,"auc")@y.values)

#out of sample analysis

set.seed(123)
spl = sample.split(loan$status, SplitRatio = 0.5)
training = subset(loan, spl == TRUE)
testing = subset(loan, spl == FALSE)
w=i^(as.numeric(training$status)-1)
logmodel=glm(status~installment+frac_pymnt+delinq+addr_state+frac_funded+funded_amnt+term+int_rate+home_ownership+log(annual_inc)+purpose+dti+delinq_2yrs+log(earliest_cr_line)+open_acc+revol_bal+total_acc+total_rec_prncp+total_rec_int,data=training,weight=w,family=binomial)
summary(logmodel)
Predicted = predict(logmodel, newdata=testing,type="response")
ROCpred = prediction(Predicted, testing$status)
ROCperf = performance(ROCpred, "tpr", "fpr")
plot(ROCperf,colorize = TRUE)
as.numeric(performance(ROCpred,"auc")@y.values)

##############
#random forest

#1. unbalanced

Forestmodel=randomForest(status~installment+frac_pymnt+delinq+addr_state+frac_funded+funded_amnt+term+int_rate+home_ownership+annual_inc+purpose+dti+delinq_2yrs+earliest_cr_line+open_acc+revol_bal+total_acc+total_rec_prncp+total_rec_int,data=loan,ntree=1000,nodesize=100)
PredictForest=predict(Forestmodel,type="prob")[,2]
ROCpred = prediction(PredictForest, loan$status)
ROCperf = performance(ROCpred, "tpr", "fpr")
plot(ROCperf,colorize = TRUE)
as.numeric(performance(ROCpred,"auc")@y.values)

#2. downsampled

Forestmodel_down=randomForest(status~installment+frac_pymnt+delinq+addr_state+frac_funded+funded_amnt+term+int_rate+home_ownership+annual_inc+purpose+dti+delinq_2yrs+earliest_cr_line+open_acc+revol_bal+total_acc+total_rec_prncp+total_rec_int,data=loan,ntree=1000,nodesize=200,sampsize=c(450,450),strata=loan$status)
PredictForest_down=predict(Forestmodel_down,type="prob")[,2]

ROCpred_down = prediction(PredictForest_down, loan$status)
ROCperf_down = performance(ROCpred_down, "tpr", "fpr")
plot(ROCperf_down,colorize = TRUE)
as.numeric(performance(ROCpred_down,"auc")@y.values)

#3. upsampled

loan_up=upSample(loan[ ,- which(names(loan) %in% c("status"))], as.factor(loan$status), list = FALSE, yname = "status")
Forestmodel_up=randomForest(status~installment+frac_pymnt+delinq+addr_state+frac_funded+funded_amnt+term+int_rate+home_ownership+annual_inc+purpose+dti+delinq_2yrs+earliest_cr_line+open_acc+revol_bal+total_acc+total_rec_prncp+total_rec_int,data=loan_up,ntree=1500,nodesize=100,strata=loan_up$status)
PredictForest_up=predict(Forestmodel_up,type="prob")[,2]

#no need to remove duplicates as ROC does not change
ROCpred_up = prediction(PredictForest_up, loan_up$status)
ROCperf_up = performance(ROCpred_up, "tpr", "fpr")
plot(ROCperf_up,colorize = TRUE)
as.numeric(performance(ROCpred_up,"auc")@y.values)
plot(ROCperf_up, colorize = TRUE)

#plots

plot(ROCperf, colorize = TRUE)
plot(ROCperf_down, add = TRUE, colorize = TRUE)
plot(ROCperf_up, add = TRUE, colorize = TRUE)

varImpPlot(Forestmodel_up,type=2)

