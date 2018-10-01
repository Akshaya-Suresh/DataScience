install.packages("xgboost")

library(xgboost)
library(metr)



#Try xgboost

param <- list(objective = "reg:linear",
              max_depth = 7,
              eta = 0.05,
              min_child_weight = 360,
              subsample = 0.85,
              colsample_bytee = 0.3,
              num_classes = 8,
              tree_method = "auto",
              predictor = "cpu_predictor",
              eval_metric = "rmse"
)


table(train_final$Response)
#Training xgboost
clf <- xgboost(data        = data.matrix(train_sec[,feature.names.test]),
               label       = train_sec$Response,
               params = param,
               nrounds = 720)

#Predict
y_pred <- round(predict(clf, data.matrix(test_sec[,feature.names.test])))
#str(y_pred)
min(y_pred)
max(y_pred)

#Some predicted values are less than one and more than 8. Normalizing those values.
y_pred[y_pred<1] <- 1
y_pred[y_pred>8] <- 8

rmse(test_vals,y_pred)

###
imp <- xgb.importance(colnames(train_sec), model = clf)
print(imp)

top_feats = head(imp,10)
xgb.plot.importance(top_feats)





