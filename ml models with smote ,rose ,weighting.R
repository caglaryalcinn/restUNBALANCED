library(ggplot2)
library(corrplot)
numeric_data <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method = "color")
ggplot(data, aes(x = legendary, y = legendary)) +
  geom_col()
ggplot(data, aes(x = legendary, y = total, fill = legendary)) +
  geom_col(position = "dodge")
library(ggplot2)
ggplot(data, aes(x = legendary, y = total)) +
  geom_point()
scatter_plot <- ggplot(data = data, aes(x = total, y = attack , color = legendary )) + 
  geom_point() + 
  scale_color_manual(values = c("red", "blue")) +
  coord_equal()
scatter_plot
#If we look at target imbalance and the plots and data above, we can deal with unbalanced data using tree-based classification algorithms (Desicion Three, ADABoost, XGBoost, Random Forrest), using penalty score and linear svm or using radial kernel. over sampling ) is to use algorithms with under sampling and weighted classes.
#Spit data test(%20) ve train(%80).

set.seed(123)
train_indices <- createDataPartition(data$legendary, p = 0.8, list = FALSE)
train_data <- final_data[train_indices, ]
test_data <- final_data[-train_indices, ]
##desicoon tree
library(rpart)
library(rpart.plot)
set.seed(145)
tree <- rpart(legendary ~ ., data = train_data)
rpart.plot(tree)
### predict now
pred.tree.over <- predict(tree, newdata = test_data,type ="class")
cm<- confusionMatrix(data =pred.tree.over, test_data$legendary)
cm
a <- c(1:399)
a <-as.data.frame(a)
pred.tree.over2 <- predict(tree, newdata = test,type ="class")
RF <- cbind(a,pred.tree.over)
RF$pred.tree.over2<-as.logical(RF$pred.tree.over2,type="class")
file_path <- "C:/Users/user/Desktop/Caglar2291003DCSON.csv"
write.csv(RF, file_path, row.names=FALSE)
###smote desicion tree
set.seed(145)
data_balanced_over <- ovun.sample(legendary ~ ., data = train_data, method = "over",N =250)$data
table(data_balanced_over$legendary)
set.seed(146)
tree_over <- rpart(legendary ~ ., data = data_balanced_over)
rpart.plot(tree)
set.seed(148)
pred.tree.weighted <- predict(tree_over, newdata = test_data,type ="class")
cm_2 <- confusionMatrix(data =pred.tree.weighted, test_data$legendary)
cm_2
##tuned rf
set.seed(132)
ctrl <- trainControl(method = "cv", number = 10)
model3 <- train(legendary~., data = train_data, method = "rf",
                trControl = ctrl)
print(model3$bestTune)

set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train_data[-1],
                             y = train_data$legendary,
                             mtyr = 25,
                             ntree = 500)
classifier_RF
set.seed(145)
y_pred_rf = predict(classifier_RF, newdata = test_data)

confusion_matrix_rf <- confusionMatrix(data = y_pred_rf, reference = test_data$legendary)
confusion_matrix_rf

##scaling rf
set.seed(120)  # Setting seed
classifier_RF2 = randomForest(x = train_data[-1],
                              y = train_data$legendary,
                              preProcess = c("center", "scale"),
                              mtyr = 25,
                              ntree = 500)
classifier_RF2
y_pred_rf2 = predict(classifier_RF2, newdata = test_data)

confusion_matrix_rf2 <- confusionMatrix(data = y_pred_rf2, reference = test_data$legendary)
confusion_matrix_rf2

##smote rf
set.seed(120)  # Setting seed
classifier_RF3 = randomForest(x = data_balanced_over[-1],
                              y = data_balanced_over$legendary,
                              preProcess = c("center", "scale"),
                              mtyr = 25,
                              ntree = 500)

classifier_RF3
predict2 <-predict(classifier_RF3, newdata = test_data)
cm_rf_over <- confusionMatrix(data =predict2, test_data$legendary)
cm_rf_over

## svm tuned
set.seed(412)
ctrl2<-trainControl(method = "repeatedcv", number =10,repeats  =5, savePredictions = T,search = "grid")

modelsvm <- train(legendary ~ ., data =train_data, method = "svmLinear", trControl = ctrl2,search = "random")



svm_model_linear <- svm(legendary ~ ., data = train_data, kernel = 'linear', cost =4)
y_pred_svm = predict(svm_model_linear, newdata = test_data)
attributes(y_pred_svm)$names <- NULL
y_pred_svm <- factor(y_pred_svm, levels = levels(test_data$legendary))
confusion_matrix_svm <- confusionMatrix(data = y_pred_svm, reference = test_data$legendary)
confusion_matrix_svm


##weighted svm

svm_model_weighted <- svm(legendary ~ ., data = train_data, kernel = 'linear', cost =1,class.weights = c("FALSE" = 9, "TRUE" = 1))
y_pred_svm2 = predict(svm_model_weighted, newdata = test_data)
attributes(y_pred_svm2)$names <- NULL
y_pred_svm2 <- factor(y_pred_svm2, levels = levels(test_data$legendary))
confusion_matrix_svm2 <- confusionMatrix(data = y_pred_svm2, reference = test_data$legendary)
confusion_matrix_svm2



##radial svm
modelRad <- train(legendary ~ ., data = train_data, method = "svmRadial", trControl = ctrl,preProcess = c("center", "scale"),class.weights = c("FALSE" = 1, "TRUE" = 9))
modelRad$bestTune
y_pred_svmRad = predict(modelRad, newdata = test_data)
attributes(y_pred_svmRad)$names <- NULL
y_pred_svmRad <- factor(y_pred_svmRad, levels = levels(test_data$legendary))
confusion_matrix_svm_rad <- confusionMatrix(data = y_pred_svmRad, reference = test_data$legendary)
##tuned xgboost
set.seed(123)
modelxg <- train(
  legendary ~., data = train_data, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
modelxg$bestTune

predicted.classes <- modelxg %>% predict(test_data)
head(predicted.classes)

confusion_matrix_xg <- confusionMatrix(data = predicted.classes, reference = test_data$legendary)
##adaboost
model_adaboost <- boosting(legendary~., data=train_data, boos=TRUE, mfinal=50)
pred_ada = predict(model_adaboost, test_data)
pred_ada$class <- as.factor(pred_ada$class)
confusion_matrix_ada <- confusionMatrix(data = pred_ada$class, reference = test_data$legendary)

##compare all f1 abd recall score
recall1 <- confusion_matrix_ada$byClass["Sensitivity"]
f1_score1 <- confusion_matrix_ada$byClass["F1"]

recall2 <- confusion_matrix_xg$byClass["Sensitivity"]
f1_score2 <- confusion_matrix_xg$byClass["F1"]

recall3 <- confusion_matrix_svm2$byClass["Sensitivity"]
f1_score3 <- confusion_matrix_svm2$byClass["F1"]

recall4 <- confusion_matrix_svm$byClass["Sensitivity"]
f1_score4 <- confusion_matrix_svm$byClass["F1"]

recall5 <- confusion_matrix_rf2$byClass["Sensitivity"]
f1_score5 <- confusion_matrix_rf2$byClass["F1"]

recall6 <- cm$byClass["Sensitivity"]
f1_score6 <- cm$byClass["F1"]


recall_scores <- c(ADABoost =recall1, XGBoost =recall2, Weighted_SVM = recall3, SVM_LinearKernel=recall4, Random_Forrest =recall5, Basic_Desicion_Three =recall6)
f1_scores <- c(ADABoost =f1_score1, XGBoost =f1_score2, Weighted_SVM = f1_score3, SVM_LinearKernel=f1_score4, Random_Forrest =f1_score5, Basic_Desicion_Three =f1_score6)

result <- cbind(recall_scores, f1_scores)
print(result)


```


#Since our data is unbalanced, accuracy can be very misleading. The value of "Recall" tells us how well we find "TRUE". These values of the SVM algorithm are fine. But for a general accuracy rate, misclassified TRUE values should also be minimal. Specificity ,Recall and F1 When we look at the values, the ADABoost and Random forrest algorithms will give us the best estimate.

#I used the following codes to save the results:
  
#  "a <- c(1:399)
#a <-as.data.frame(a)
#y_pred_svm1 <- predict(svm_model_linear, newdata = test,type ="class")
#RF <- cbind(a,y_pred_svm1)
#RF$y_pred_svm1<-as.logical(RF$y_pred_svm1)
#file_path <- "C:/Users/user/Desktop/Caglar2291003SVMSon.csv"
#write.csv(RF, file_path, row.names=FALSE)"


