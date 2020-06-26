library(glmnet)
library(purrr)
library(dplyr)    # basic data manipulation procedures
library(generics)
library(ggplot2)
library(cvAUC)
####  Origin ####
idx = round(14999*0.7)
training = hr_trans[1:idx,]
testing = hr_trans[(idx+1):14999,]
logistic = glm(left~.,family = 'binomial', data=training)
summary(logistic)
y_pred = predict(logistic,testing[-1],type='response')
acu = AUC(y_pred,testing[1])
prediction_TF <- ifelse(y_pred>0.5,1,0)
confusion_matrix <- table(testing$left, prediction_TF)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
sensitivity <- confusion_matrix[2,2]/sum(confusion_matrix[2,])
specificity <- confusion_matrix[1,1]/sum(confusion_matrix[1,])
pr <- prediction(y_pred, labels=testing$left)
prf <- performance(prediction.obj = pr, measure="tpr",x.measure="fpr")
auc  <- performance(pr, "auc")
dd   <- data.frame(FP=prf@x.values[[1]],TP=prf@y.values[[1]])
ggplot()+geom_line(data=dd, mapping = aes(x=FP, y=TP, color="Origin"))+geom_segment(mapping = aes(x=0,xend=1,y=0,yend=1))+
  ggtitle(label="ROC curve")+labs(x="FP Rate", y="TP Rate")
as.numeric(auc@y.values[[1]])


##### Lasso ####
lasso=glmnet(x=as.matrix(hr_training[2:182]),y=hr_training$left,alpha = 1,family = "binomial")
plot(lasso, xvar='lambda', main="Lasso")
# find which variable has over big coefficient -> "position_RandD*position_sales"
coef_df =coef(lasso)%>%
  as.matrix()%>%
  as.data.frame()
temp_ls=c()
for(i in c(1:182)){
  r_values = coef_df[i,]
  temp_ls = c(temp_ls,sum(r_values))
}
which.max(temp_ls)
rownames(coef_df)[132]

# del var and rebuild model
hr_training2 = hr_training[-132]  #del  "position_RandD*position_sales"  
hr_training2 = hr_training2[-118]  #del  "position_IT*position_RandD"  

lasso=glmnet(x=as.matrix(hr_training2[2:180]),y=hr_training2$left,alpha = 1,family = "binomial")
plot(lasso, xvar='lambda', main="Lasso")
cv.lasso = cv.glmnet(x=as.matrix(hr_training2[2:180]),y=hr_training2$left,alpha = 1,family = "binomial")
plot(cv.lasso)
best.lambda = cv.lasso$lambda.1se
best.lambda
#under best lambda the coefficient and plot
plot(lasso, xvar='lambda', main="Lasso")
abline(v=log(cv.lasso$lambda.min), col="red", lty=5.5 )
abline(v=log(cv.lasso$lambda.1se), col="blue", lty=5.5 ) #對應到最小Binomial Deviance一個標準差內的最大λ

coef(cv.lasso, s = "lambda.1se")
# feature selection_1
select.ind = which(coef(cv.lasso, s = "lambda.1se") != 0)
select.ind = select.ind[-1]-1
select.varialbes = colnames(hr_training2[2:180])[select.ind]
select.varialbes
training_lasso = hr_training2[,select.varialbes]
training_lasso = cbind(hr_training['left'],training_lasso)

# feature selection_2
logistic = glm(left~.,family = 'binomial', data=training_lasso)
summary(logistic)
p_value = coef(summary(logistic))[,4]
select_idx = which(p_value<0.01)
training_lasso = training_lasso[,select_idx]

# Building Model
hr_testing2 = hr_testing[,colnames(training_lasso)]

logistic_model = glm(left~.,family = 'binomial', data=training_lasso)
summary(logistic_model)
y_pred = predict(logistic_model,hr_testing2[-1],type='response')
auc = AUC(y_pred,hr_testing2[1])

prediction_TF <- ifelse(y_pred>0.5,1,0)
confusion_matrix <- table(hr_testing2$left, prediction_TF)
accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix)
sensitivity <- confusion_matrix[2,2]/sum(confusion_matrix[2,])
specificity <- confusion_matrix[1,1]/sum(confusion_matrix[1,])

pr <- prediction(y_pred, labels=hr_testing2$left)
prf <- performance(prediction.obj = pr, measure="tpr",x.measure="fpr")
auc2  <- performance(pr, "auc")
dd2   <- data.frame(FP=prf@x.values[[1]],TP=prf@y.values[[1]])

ggplot()+geom_line(data=dd2, mapping = aes(x=FP, y=TP, color="Logistic Regression"))+geom_segment(mapping = aes(x=0,xend=1,y=0,yend=1))+
ggtitle(label="ROC curve")+labs(x="FP Rate", y="TP Rate")
as.numeric(auc2@y.values[[1]])
text('0.9810168')

