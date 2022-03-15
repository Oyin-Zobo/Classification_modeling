install.packages("randomForest")
library(randomForest)
library(e1071)

hospdata = read.csv("Train host.csv")
view(hospdata)
str(hospdata)

hospdata %>% 
  count(race)

hospdata %>% 
  count(gender)
total_test %>% 
  count(gender)

hospdata %>% 
  count(age)

total_test %>% 
  count(age)

hospdata %>% 
  count(admission_type)

hospdata %>% 
  count(admission_type)

hospdata %>% 
  count(discharge_disposition)

hospdata %>%
  count(patientID)%>%
  arrange(desc(n))

hospdata$payer_code
hospdata$readmitted = as.factor(hospdata$readmitted)
str(hospdata)
hospdata$readmitted
view(hospdata)
int_hosp = hospdata[,c(5,6,7,8,11,12,13,14,15,16,18,45)]
int_hosp
install.packages("Boruta")
library(Boruta)
boruta_output = Boruta(readmitted~., data = na.omit(int_hosp), doTrace=0)

names(boruta_output)

roughfix = TentativeRoughFix(boruta_output)
boruta_sig = getSelectedAttributes(roughfix)

imp = attStats(roughfix)
imps2 = imp[imp$decision != 'Rejected', c('meanImp', 'decision')]
head(imp[order(-imps2$meanImp), ])
imp[order(-imps2$meanImp), ]

sum(is.na(hospdata$readmitted))
sum(is.na(hospdata$race))
sum(is.na(hospdata$gender))
sum(is.na(total_test$gender))
sum(is.na(hospdata$admission_source))
sum(is.na(hospdata$number_outpatient))
sum(is.na(hospdata$number_diagnoses))
sum(is.na(hospdata$num_medications))





#missing value resolution 
library(mice)
md.pattern(hospdata)
install.packages("DMwR")
library(DMwR)
library(Hmisc)

glimpse(hospdata)
hospdata$race = impute(hospdata$race, mode)
library(glmnet)
hospdata = kNN(hospdata[,c(2:4,9,25,26)], k=5)

hosp_data_1 = kNN(hospdata[,c(2:4,9,25,26)], k=5)
hospdata$race= hosp_data_1$race

fit_hosp <- glm(data=hospdata, readmitted~race+gender+age 
                +number_outpatient+number_diagnoses+num_medications
                +discharge_disposition+number_inpatient+number_emergency+
              num_lab_procedures+num_lab_procedures+num_procedures, 
            family="binomial")


hospdata$pred<-as.numeric(fit_hosp$fitted.values>0.5)

hospdata$readmitted = as.numeric(hospdata$readmitted)
fit_hosp$fitted.values
str(hospdata)
LogLoss(fit_hosp$fitted.Values, hospdata$readmitted)
NROW(fit_hosp$fitted.values)
NROW(hospdata$readmitted)

sum(is.na(hospdata$num_medications))

lambdas <- 10^seq(2, -3, by = -.1)
x = hospdata[, c(2,3,4,18,13,6, 14,15,16,11,12,13)]
x_dum = dummy_cols(x)
view(x_dum)
x_dum_1 = x_dum[,-c(1,2,3)]

y = hospdata[,c(45)]
x_train= as.matrix(x_dum_1)
view(hospdata)

fit_hosp1 <- cv.glmnet(x_train, y, family="binomial", alpha = 1, lambda = lambdas,
                      nfolds = 5)


x1 = total_test[, c(2,3,4,18,13,6, 14,15,16,11,12,13)]
x_dum1 = dummy_cols(x1)
view(x_dum1)
x_dum1 = x_dum1[,-c(1,2,3)]

x_test= as.matrix(x_dum1)

predicted1= predict(fit_hosp1, x_test)
prob1 = logit.prob(predicted1)

submission_three =  data.frame(patientID = total_test$patientID, predReadmit = prob1)
write.csv(submission_three, "submission_three.csv", row.names=FALSE)

str(hospdata)

NROW(fit_hosp$fitted.values)
NROW(hospdata$readmitted)


exp(coefficients(fit_hosp))
exp(confint(fit_hosp))
#explain results 


summary(fit_hosp)

fit_hosp2<-rpart(data=hospdata, readmitted~race+gender+age+admission_source 
                 +number_outpatient+number_diagnoses+num_medications
                 +discharge_disposition+number_inpatient+number_emergency
                 +num_lab_procedures+
                   num_lab_procedures+num_procedures,        
              parms=list(split="information"),   
              control=rpart.control(xval=20))


install.packages("misaem")

summary(fit_hosp2)
library(VIM)

hosp_data_1 = kNN(hospdata[,c(2:4,9,25,26)], k=5)
view(hosp_data_1)
hospdata$race= hosp_data_1$race
classifier = svm(formula = readmitted ~race+gender+age+admission_source 
                 +number_outpatient+number_diagnoses+num_medications
                 +discharge_disposition+number_inpatient+number_emergency
                 +num_lab_procedures+
                   num_lab_procedures+num_procedures, 
                 data = hospdata, 
                 type = 'C-classification', 
                 kernel = 'radial') 

str(hospdata)
str(total_test)
summary(classifier)

classifier$decision.value
total_test
nrow(total_test)


predicted = predict(classifier, total_test)

  
str(hospdata)
hospdata$readmitted = as.factor(hospdata$readmitted)
sum(is.na(total_test$race))

hosp_data_1 = kNN(hospdata[,c(2:4,9,25,26)], k=5)
hospdata$race= hosp_data_1$race

total_test %>%
  count(race)

hospdata %>%
  count(race)


ran_class = randomForest(readmitted ~ race+gender+age+admission_source 
                         +number_outpatient+number_diagnoses+num_medications
                         +discharge_disposition+number_inpatient+number_emergency
                         +num_lab_procedures+
                           num_lab_procedures+num_procedures, data = hospdata, 
                         ntree=1000, mtry = 3, importance=TRUE)
plot(ran_class)
ran_class


predicted_2 = predict(ran_class, total_test, type = "prob")

predicted_2 = predicted_2[,2]
ran_class$importance

submission_two =  data.frame(patientID = total_test$patientID, predReadmit = 
                               predicted_2)
write.csv(submission_two, "submission_two.csv", row.names=FALSE)

summary(ran_class)
library(MASS)
lda_class = lda(readmitted ~race+gender+age+admission_source 
                +number_outpatient+number_diagnoses+num_medications
                +discharge_disposition+number_inpatient+number_emergency
                +num_lab_procedures+
                  num_lab_procedures+num_procedures, data = hospdata)


predicted_2 = predict(lda_class, total_test, type = "prob")
predicted_2
predicted_2 = predicted_2[,2]
summary(lda_class)
lda_class



#pick a model and explain it. 
#model performance. use function from old homework. 




roc = function (data)
{
  pred <- prediction(data$fitted.values, data$y)    #ROC curve for training data
  perf <- performance(pred,"tpr","fpr") 
  
  plot(perf,colorize=TRUE, print.cutoffs.at = c(0.25,0.5,0.75)); 
  abline(0, 1, col="red")  
}

precvsrec = function(data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf = performance(pred, measure = "prec", x.measure= "rec")
  
  plot(perf, avg= "vertical",  
       spread.estimate="boxplot", 
       show.spread.at= seq(0.1, 0.9, by=0.1))
}


acc = function(data)
{
  
  pred <- prediction(data$fitted.values, data$y)
  perf = performance(pred, measure = "acc")
  
  plot(perf, avg= "vertical",  
       spread.estimate="boxplot", 
       show.spread.at= seq(0.1, 0.9, by=0.1))  
}


lift = function(data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf = performance(pred, measure = "lift")
  
  plot(perf, avg= "vertical",  
       spread.estimate="boxplot", 
       show.spread.at= seq(0.1, 0.9, by=0.1))
}

auc = function(data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf = performance(pred, "auc")
  y = perf@y.values
  print("auc")
  print(y)
}

aucpr = function(data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf4 = performance(pred, "aucpr")
  y1 = perf4@y.values
  print("aucpr")
  print(y1)
}

prbe = function (data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf5 = performance(pred, "prbe")
  y3 = perf5@y.values
  print("prbe")
  print(y3)
}



honors$pred<-as.numeric(fit$fitted.values>0.5)

#D statistic (2009)

dstatistic = function (data,model)
{
  
  fv<-fitted(model)
  predVals <-  data.frame(trueVal=data$true, predclass=data$pred,predProb=fv)
  data.1<-predVals[predVals$trueVal==1,]
  data.0<-predVals[predVals$trueVal==0,]
  y2 = mean(data.1$predProb) - mean(data.0$predProb) #get separation 
  print("dstatistics")
  print(y2)
}


chart = function (data, model)
{
  
  fv<-fitted(model)
  predVals <-  data.frame(trueVal=data$true, predclass=data$pred,predProb=fv)
  predVals$group<-cut(predVals$predProb,seq(1,0,-.1),include.lowest=T)
  predVals$group
  xtab<-table(predVals$group,predVals$trueVal)
  
  KS<-data.frame(Group=numeric(10),
                 CumPct0=numeric(10),
                 CumPct1=numeric(10),
                 Dif=numeric(10))
  
  #fill data frame with information: Group ID, 
  #Cumulative % of 0's, of 1's and Difference
  for (i in 1:10) {
    KS$Group[i]<-i
    KS$CumPct0[i] <- sum(xtab[1:i,1]) / sum(xtab[,1])
    KS$CumPct1[i] <- sum(xtab[1:i,2]) / sum(xtab[,2])
    KS$Dif[i]<-abs(KS$CumPct0[i]-KS$CumPct1[i])
  }
  print("KS")
  print(KS)
  print(KS[KS$Dif==max(KS$Dif),])
  
  maxGroup<-KS[KS$Dif==max(KS$Dif),][1,1]
  #and the K-S chart
  ggplot(data=KS)+
    geom_line(aes(Group,CumPct0),color="blue")+
    geom_line(aes(Group,CumPct1),color="red")+
    geom_segment(x=maxGroup,xend=maxGroup,
                 y=KS$CumPct0[maxGroup],yend=KS$CumPct1[maxGroup])+
    labs(title = "K-S Chart", x= "Deciles", y = "Cumulative Percent")
}

chart(heartfailure1)



matt = function (data)
{
  pred <- prediction(data$fitted.values, data$y)
  perf <- performance(pred, "mat")
  plot(perf, avg= "vertical",  
       spread.estimate="boxplot", 
       show.spread.at= seq(0.1, 0.9, by=0.1))
}


evaluationfuction = function (data, model)
{
  
    roc(model)
    auc(model)
    acc(model)
    precvsrec(model)
    aucpr(model)
    lift(model)
    prbe(model)
    #print(Concordance(model$y, model$fitted.values))
    matt(model)
    print(F1_Score(data$pred,data$true))
    print(ConfusionDF(data$pred, data$true))
    print(LogLoss(y_pred = model$fitted.Values,y_true=data$pred))
    print(LiftAUC(data$pred, data$true))
    print(GainAUC(data$pred, data$true))
    dstatistic (data,model)
    chart(data,model)
}
matt(fit_hosp)
dstatistic(hospdata,fit_hosp)
chart(hospdata, fit_hosp)

total_test = read.csv("test host.csv")
total_test$race= impute(total_test$race, mode)

#this is the logit 
predicted = predict(fit_hosp, total_test)

install.packages("optiRum")
library(optiRum)
#this is the probability 
prob = logit.prob(predicted)

submission_one =  data.frame(patientID = total_test$patientID, predReadmit = prob)
write.csv(submission_one, "submission_one.csv", row.names=FALSE)


pearsonRes <-residuals(fit_hosp,type="pearson")
devianceRes <-residuals(fit_hosp,type="deviance")
rawRes <-residuals(fit_hosp,type ="response")
studentDevRes<-rstudent(fit_hosp)
fv<-fitted(fit_hosp)
head(hospdata)

#let's go ahead and create a classification based on the probability
fit_hosp$fitted.values
hospdata$readmitted
hospdata$pred<-as.numeric(fit_hosp$fitted.values>0.5)


predVals <-  data.frame(trueVal=hospdata$readmitted, predClass=hospdata$pred, 
                        predProb=fv, 
                        rawRes, pearsonRes, devianceRes, studentDevRes)

evaluationfuction(hospdata, fit_hosp)
