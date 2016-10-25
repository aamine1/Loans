loan <- read.csv("C:/Users/mitadm/Desktop/Wealthfront/loan.csv")

loan$loan_amnt=as.numeric(loan$loan_amnt)
loan$funded_amnt=as.numeric(loan$funded_amnt)
loan$term=as.numeric(loan$term)
loan$int_rate=as.numeric(loan$int_rate)
loan$installment=as.numeric(loan$installment)
loan$emp_length1=as.numeric(loan$emp_length1)
loan$emp_length2=as.factor(loan$emp_length2)
loan$home_ownership=as.factor(loan$home_ownership)
loan$annual_inc=as.numeric(loan$annual_inc)
loan$purpose=as.factor(loan$purpose)
loan$addr_state=as.factor(loan$addr_state)
loan$dti=as.numeric(loan$dti)
loan$deling_2yrs=as.numeric(loan$deling_2yrs)
loan$earliest_cr_line=as.numeric(loan$earliest_cr_line)
loan$mths_since_last_delinq=as.numeric(loan$mths_since_last_delinq)
loan$open_acc=as.numeric(loan$open_acc)
loan$revol_bal=as.numeric(loan$revol_bal)
loan$total_acc=as.numeric(loan$total_acc)
loan$out_prncp=as.numeric(loan$out_prncp)
loan$total_pymnt=as.numeric(loan$total_pymnt)
loan$total_rec_prncp=as.numeric(loan$total_rec_prncp)
loan$total_rec_int=as.numeric(loan$total_rec_int)
loan$label=as.factor(loan$label)

library('corrplot') #package corrplot
d=data.frame(loan$total_acc,loan$revol_bal,loan$open_acc,loan$earliest_cr_line,loan$deling_2yrs,loan$dti,loan$annual_inc,loan$funded_amnt,loan$term,loan$int_rate,loan$total_rec_prncp)
M <- cor(d) # get correlations
corrplot(M, method = "circle")



log_model=glm(label~loan$purpose+loan$home_ownership+loan$total_acc+loan$revol_bal+loan$open_acc+loan$earliest_cr_line+loan$deling_2yrs+loan$dti+loan$annual_inc+loan$funded_amnt+loan$term+loan$int_rate+loan$total_rec_prncp,data=loan,family=binomial)
summary(log_model)

Prediction = predict(log_model, newdata=loan, type="response")
table_LR=table(loan$label, Prediction>0.5)
table_LR
accuracy=(table_LR[1,1]+table_LR[2,2])/nrow(loan)



library(ROCR)
PredictTrain = predict(log_model, type="response")
ROCpred = prediction(PredictTrain, loan$label)
ROCperf = performance(ROCpred, "tpr", "fpr")
plot(ROCperf,colorize = TRUE)
as.numeric(performance(ROCpred,"auc")@y.values)
