library(data.table)
library(xgboost)
X_train<- read.csv("Train.csv")
X_test<- read.csv("Test.csv")
str(X_train)
X_test$TARGETVAR<- NA
X_All<- rbind(X_train,X_test)
X_All$year<- substr(X_All$TIMESTAMP,1,4)
table(X_All$year)
X_All$month<- substr(X_All$TIMESTAMP,5,6)
table(X_All$month)
X_All$date<- substr(X_All$TIMESTAMP,7,8)
table(X_All$date)
X_All$time<- substr(X_All$TIMESTAMP,10,11)
table(X_All$time)
boxplot(X_All$TARGETVAR~X_All$time)
X_All$time<-gsub("[[:punct:]]", "", X_All$time)
X_All$TIMESTAMP<- NULL
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
Fit<- rpart(X_train$TARGETVAR~X_train$month+X_train$year+X_train$date+X_train$time , method = "anova")
fancyRpartPlot(Fit)
X_All$time<- NULL
for(i in 1:168000){
  if(X_All[i,9]== "02" | X_All[i,9]== "03" | X_All[i,9]== "04" | X_All[i,9]== "05" | X_All[i,9]== "06" | X_All[i,9]== "10" | X_All[i,9]== "11" | X_All[i,9]=="12"){
    X_All[i,12]= 1
  }
  else{
    X_All[i,12]= 2
  }
}
table(X_All$time_1)
X_train<- X_All1[1:nrow(X_train),]
X_test<- X_All1[-(1:nrow(X_train)),]
train<- read.csv("Train.csv")
y<- train$TARGETVAR
X_train1$TARGETVAR<- NULL
X_test1$TARGETVAR<- NULL
X_train$ID<- NULL
X_test$ID<- NULL
X_train$ZONEID<- NULL
X_test$ZONEID<- NULL
library(xgboost)
model_xgb_cv <- xgb.cv(data=data.matrix(X_train), label=data.matrix(y), objective="reg:linear", nfold=5, nrounds=500, eta=0.02, max_depth=5, subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="rmse")
model_xgb <- xgboost(data=data.matrix(X_train), label=as.matrix(y), objective="reg:linear", nrounds=500, eta=0.02, max_depth=5, subsample=0.6, colsample_bytree=0.85, min_child_weight=0.8, eval_metric="rmse")
pred1 <- predict(model_xgb, data.matrix(X_test))
c<- as.data.frame(cbind(pred,pred1))
c$pred2<- min(c$pred,c$pred1)
test<- read.csv("Test.csv")
ubmit <- data.frame("ID" = test$ID, "TARGETVAR" = com_preds1)
write.csv(ubmit, "submit10.csv", row.names=F)
norm1<- function(x){
  x<- (x- min(x))/(max(x)-min(x))
} 
norm1(X_All$U10)
norm1(X_All$V10)
norm1(X_All$U100)
norm1(X_All$V100)

binDF<- X_All1
binDF$TARGETVAR<- NULL
#t sne
X_All1$TARGETVAR<- NULL
library(Rtsne)
tsne <- Rtsne(data.matrix(binDF), dims = 2, perplexity = 30, check_duplicates = FALSE, pca = FALSE, theta = 0.5, max_iter = 250, verbose = TRUE)
tsneDF <- as.data.frame(tsne$Y)
plot(tsneDF)
names(tsneDF) <- c("tsne_f1", "tsne_f2")
names(X_All1)
X_All1 <- cbind(X_All1, tsneDF)
X_All1$tsne_f1<- NUL
X_All1$tsne_f2<- NULL
x1<- cbind(X1,tsneDF)
X_All$ID<- NULL
X_All$ZONEID<- NULL
str(X_All)
conv<- function(x){
   as.numeric(x)
}
X_All$year<-conv(X_All$year)
X_All$month<-conv(X_All$month)
X_All$date<-conv(X_All$date)
X_All$time<-conv(X_All$time)
X_All$TARGETVAR<- NULL


X_All$D10<- atan2(-X_All$U10,-X_All$V10)*(180/pi)
X_All$D100<- atan2(-X_All$U100,-X_All$V100)*(180/pi)
str(X_All)
X_All$ID<- NULL
X_All$TARGETVAR<- NULL
X_All$V1<- NULL
X_All$V2<- NULL
train<- read.csv("Train.csv")
y<- train$TARGETVAR
library(xgboost)
X_train$`train$TARGETVAR`<- NULL
preds <- vector("list", length = 10)
for(i in 1:10){
  print(paste('training model:', i))
  model_xgb <- xgboost(data=data.matrix(X_train), label=as.matrix(y), objective="reg:linear", nrounds=500, eta=0.02, max_depth=5, subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="rmse")
  
  print(paste('applying prediction:', i))
  preds[[i]] <- predict(model_xgb, newdata = data.matrix(X_test))
}

com_preds1 <- colMeans(do.call(rbind, preds))
result <- matrix(com_preds,nrow = 26290, ncol = 1, byrow = T)
a<- X_All$U10^2 + X_All$V10^2
X_All$Spd10<- sqrt(a)
b<- X_All$U100^2 + X_All$V100^2
X_All$Spd100<- sqrt(b)
names<- colnames(X_train)
f<- xgb.importance(names,model=model_xgb)
xgb.plot.importance(f)
X_All$U10<- NULL
X_All$U100<- NULL
X_All$V10<- NULL
X_All$V100<- NULL

X_All$ID<- NULL

X_train<- X_All[1:nrow(X_train),]
X_test<- X_All[-(1:nrow())]
x<- model.matrix(~.,data=X_train)
library(glmnet)
fit.ridge=glmnet(x,y,alpha=0)

X_All$abc<- NA
for(i in 1:nrow(X_All)){
  if(X_All[i,8]%in%c(02,03,04,05,06,10,11,12)&& X_All[i,9]%in%c(02,03,07,08,09,15,16,17,18,24,25,26,27,28,29,31)){
    X_All[i,15]=1
  }
  else if(X_All[i,8]%in%c(02,03,04,05,06,10,11,12)&& X_All[i,9]%in% c(01,04,05,06,10,11,12,13,14,19,20,21,22,23,30)){
    X_All[i,15]=2
  }
  else if(X_All[i,8]%in%c(01,07,08,09)&& X_All[i,9]%in% c(01,02,03,09,10,11,12,14,15,16,18,19,20,21,22,23,24,29,30)){
  X_All[i,15]=3
  }
  else {
    X_All[i,15]=4
  }
}
table(X_All$abc)
X_All$month<- NULL
X_All$date<- NULL
table(X_All$ZONEID)
X_All$U10<- NULL
X_All$V10<- NULL
X_All$U100<- NULL
X_All$V100<- NULL
library(dummies)
names(X_All)
X_All2<- dummy.data.frame(X_All, names = c("ZONEID","abc"),sep="_")
library(xgboost)
X_All2$year<- NULL
X_All2$time<- NULL
x<- model.matrix(~.,data=X_train)
library(glmnet)
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)
LogLoss <- function(actual, predicted, eps=1e-15) {
  predicted[predicted < eps] <- eps;
  predicted[predicted > 1 - eps] <- 1 - eps;
  -1/nrow(actual)*(sum(actual*log(predicted)))
}
gcv <- cv.glmnet(x=x ,
                 y = y,
                 family = 'gaussian',
                 type.measure = 'mse',
                 nfolds = 4)
coeffs <- as.matrix(coef(gcv, s = 'lambda.1se'))
chosenVars1 <- rownames(coeffs)[abs(coeffs[,1]) > 0]
chosenVars1
colnames(X_train)
X_All1<- X_All1[,c(1,3,5,6,7,8,9,10,20,21,22,23,24,25,28,29,30,31,32,33,34,35,36,38,39,40,41,42,44,45)]
