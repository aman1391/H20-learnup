#clustering 
X_train<- cbind(X_train,train$TARGETVAR)
t1<- X_train[1:35000,]
t2<- X_train[35001:70000,]
t3<- X_train[70001:105000,]
t4<- X_train[105001:138710,]
param<- list(objective="reg:linear", eta=0.02, max_depth=4, subsample=0.6, colsample_bytree=0.85, min_child_weight=1, eval_metric="rmse")
t1.y<- t1$`train$TARGETVAR`
t2.y<- t2$`train$TARGETVAR`
t3.y<- t3$`train$TARGETVAR`
t4.y<- t4$`train$TARGETVAR`
t1$`train$TARGETVAR`<- NULL
t2$`train$TARGETVAR`<- NULL
t3$`train$TARGETVAR`<- NULL
t4$`train$TARGETVAR`<- NULL
library(xgboost)
bst1 <- xgboost(   params              = param, 
                   data                = data.matrix(t1), 
                   label =  data.matrix(t1.y),
                   nrounds             = 150
                   
)
bst2 <- xgboost(   params              = param, 
                   data                = data.matrix(t2), 
                   label =  data.matrix(t2.y),
                   nrounds             = 150
                   
)
bst3 <- xgboost(   params              = param, 
                   data                = data.matrix(t3), 
                   label =  data.matrix(t3.y),
                   nrounds             = 150
                   
)
bst4 <- xgboost(   params              = param, 
                   data                = data.matrix(t4), 
                   label =  data.matrix(t4.y),
                   nrounds             = 150
                   
)
pred1 <- predict(bst1, data.matrix(X_test))
pred2 <- predict(bst2, data.matrix(X_test))
pred3 <- predict(bst3, data.matrix(X_test))
pred4 <- predict(bst4, data.matrix(X_test))

pred.ensemble<- (pred1+pred2+pred3+pred4)/4
ubmit <- data.frame("ID" = test$ID, "TARGETVAR" = pred.ensemble)
write.csv(ubmit, "submit5.csv", row.names=F)
