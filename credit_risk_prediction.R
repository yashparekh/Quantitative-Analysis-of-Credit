#part1---data preparation
credit<-read.csv('Credit_Dataset.csv')
credit$PROFITABLE<-ifelse(credit$PROFIT>0,1,0)
class(credit$CHK_ACCT)
credit$CHK_ACCT<-as.factor(credit$CHK_ACCT)
credit$SAV_ACCT<-as.factor(credit$SAV_ACCT)
credit$HISTORY<-as.factor(credit$HISTORY)
credit$TYPE<-as.factor(credit$TYPE)
credit$JOB<-as.factor(credit$JOB)
set.seed(11217)
test_insts<-sample(nrow(credit),0.3*nrow(credit))
credit_test<-credit[test_insts,]
credit_rem<-credit[-test_insts,]
val_insts<-sample(nrow(credit_rem),0.25*nrow(credit_rem))
credit_val<-credit_rem[val_insts,]
credit_train<-credit_rem[-val_insts,]
#part2---logistic regression model
#install.packages('ROCR')
attach(credit_train)
credit_log<-glm(PROFITABLE~AGE+DURATION+RENT+TELEPHONE+FOREIGN+CHK_ACCT+SAV_ACCT+HISTORY+JOB+TYPE,family='binomial')
summary(credit_log)
log_preds_val<-predict(credit_log,newdata=credit_val,type='response')
log_preds_train<-predict(credit_log,newdata=credit_train,type='response')
library(ROCR)
preds_val<-prediction(log_preds_val,credit_val$PROFITABLE)
preds_train<-prediction(log_preds_train,credit_train$PROFITABLE)
log_tpr<-performance(preds_val,measure = 'tpr')
log_tnr<-performance(preds_val,measure = 'tnr')
?performance()
?plot()
log_acc<-performance(preds_val,measure = 'acc')
plot(log_tpr,ylim=c(0,1),col='GREEN',ylab='TPR,TNR,ACC')
plot(log_tnr, add=T,col='RED')
plot(log_acc,add=T,col='BLUE')
legend(0.6,0.4, legend=c('TPR','TNR','ACC'),col=c('green','red','blue'), lty=1:2,cex=0.8)
best = which.max(slot(log_acc,"y.values")[[1]])
?which.max()
slotNames(log_acc)
?slot()
best
max.acc = slot(log_acc,"y.values")[[1]][best]
max.acc
max.cutoff = slot(log_acc,"x.values")[[1]][best]
max.cutoff
print(c(accuracy= max.acc, cutoff = max.cutoff))
roc_val = performance(preds_val, measure = "tpr", x.measure = "fpr")
roc_train = performance(preds_train, measure = "tpr", x.measure = "fpr")
plot(roc_val,col='RED')
plot(roc_train,col='GREEN',add=T)
abline(a=0,b=1,lty=3)
legend(0.6,0.4, legend=c('Val','Train'),col=c('red','green'), lty=1,cex=0.8)
#part3---classification trees
library(tree)
acc_tab <- NULL

tree_credit_train<-tree(PROFITABLE~AGE+ DURATION+ RENT+ TELEPHONE+ FOREIGN+ CHK_ACCT+ SAV_ACCT+ HISTORY+ JOB+ TYPE)
summary(tree_credit_train)
tree_pred_train <- predict(tree_credit_train,newdata=credit_train)
class_pred <- ifelse(tree_pred_train>.5,1,0)
tab<-table(credit_train$PROFITABLE, class_pred) 
acc_tab[1]<-sum(diag(tab))/sum(tab)

table(PROFITABLE)
acc_tab[2] <- 366/(159+366)

prune_tree_2<-prune.tree(tree_credit_train,best=2)
train_tree_2<- predict(prune_tree_2,newdata=credit_train)
class_pred <- ifelse(train_tree_2>.5,1,0)
tab<-table(credit_train$PROFITABLE, class_pred) 
acc_tab[3]<-sum(diag(tab))/sum(tab)

prune_tree_5<-prune.tree(tree_credit_train,best=5)
train_tree_5<- predict(prune_tree_5,newdata=credit_train)
class_pred <- ifelse(train_tree_5>.5,1,0)
tab<-table(credit_train$PROFITABLE, class_pred) 
acc_tab[4]<-sum(diag(tab))/sum(tab)

prune_tree_10<-prune.tree(tree_credit_train,best=10)
train_tree_10<- predict(prune_tree_10,newdata=credit_train)
class_pred <- ifelse(train_tree_10>.5,1,0)
tab<-table(credit_train$PROFITABLE, class_pred) 
acc_tab[5]<-sum(diag(tab))/sum(tab)

prune_tree_15<-prune.tree(tree_credit_train,best=15)
train_tree_15<- predict(prune_tree_15,newdata=credit_train)
class_pred <- ifelse(train_tree_15>.5,1,0)
tab<-table(credit_train$PROFITABLE, class_pred) 
acc_tab[6]<-sum(diag(tab))/sum(tab)
node_level <- c(0,1,2,5,10,15)
plot(node_level,acc_tab)
node_level
acc_tab

valid_acc_tab <- NULL
tree_credit_train<-tree(PROFITABLE~AGE+ DURATION+ RENT+ TELEPHONE+ FOREIGN+ CHK_ACCT+ SAV_ACCT+ HISTORY+ JOB+ TYPE)
summary(tree_credit_train)
plot(tree_credit_train)
tree_pred_valid<- predict(tree_credit_train,newdata=credit_valid)
class_pred <- ifelse(tree_pred_valid>.5,1,0)
tab<-table(credit_valid$PROFITABLE, class_pred) 
valid_acc_tab[1]<-sum(diag(tab))/sum(tab)

detach(credit_train)
attach(credit_valid)
table(PROFITABLE)
valid_acc_tab[2] <- 117/(117+58)

prune_tree_2<-prune.tree(tree_credit_train,best=2)
valid_tree_2<- predict(prune_tree_2,newdata=credit_valid)
class_pred <- ifelse(valid_tree_2>.5,1,0)
tab<-table(credit_valid$PROFITABLE, class_pred) 
valid_acc_tab[3]<-sum(diag(tab))/sum(tab)

prune_tree_5<-prune.tree(tree_credit_train,best=5)
valid_tree_5<- predict(prune_tree_5,newdata=credit_valid)
class_pred <- ifelse(valid_tree_5>.5,1,0)
tab<-table(credit_valid$PROFITABLE, class_pred) 
valid_acc_tab[4]<-sum(diag(tab))/sum(tab)

prune_tree_10<-prune.tree(tree_credit_train,best=10)
valid_tree_10<- predict(prune_tree_10,newdata=credit_valid)
class_pred <- ifelse(valid_tree_10>.5,1,0)
tab<-table(credit_valid$PROFITABLE, class_pred) 
valid_acc_tab[5]<-sum(diag(tab))/sum(tab)

prune_tree_15<-prune.tree(tree_credit_train,best=15)
valid_tree_15<- predict(prune_tree_15,newdata=credit_valid)
class_pred <- ifelse(valid_tree_15>.5,1,0)
tab<-table(credit_valid$PROFITABLE, class_pred) 
valid_acc_tab[6]<-sum(diag(tab))/sum(tab)

node_level <- c(0,1,2,5,10,15)
plot(node_level,valid_acc_tab)
node_level
valid_acc_tab

#part4---knn model
#install.packages('class')
library(class)
colnames(credit_train)
train.X=credit_train[,c(2,3,4,6,7,10,12,17,18,19)]
val.X=credit_val[,c(2,3,4,6,7,10,12,17,18,19)]
train_profitable=credit_train$PROFITABLE
val_profitable=credit_val$PROFITABLE
knn_pred=knn(train.X,val.X,train_profitable,k=1)
table(val_profitable,knn_pred)
knn_pred_tr=knn(train.X,train.X,train_profitable,k=1)
table(train_profitable,knn_pred_tr)
vals<-matrix(NA,nrow=5,ncol=3)
ind=1
for(kval in c(1,3,5,10,25)){
  knn.pred_val<-knn(train.X,val.X,train_profitable,k=kval)
  knn.pred_train<-knn(train.X,train.X,train_profitable,k=kval)
  correct_val<-sum(ifelse(knn.pred_val==val_profitable,1,0))
  acc_val<-(1.0*correct_val)/nrow(val.X)
  correct_train<-sum(ifelse(knn.pred_train==train_profitable,1,0))
  acc_train<-(1.0*correct_train)/nrow(train.X)
  
  vals[ind,1]<-kval
  vals[ind,2]<-acc_val
  vals[ind,3]<-acc_train
  ind=ind+1
}
plot(vals[,1],vals[,2],type='l',col='red',xlab='k',ylab='Val Acc')
plot(vals[,1],vals[,3],type='l',col='green',xlab='k',ylab='Train Acc')

