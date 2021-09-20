library(dplyr)
# Read the Data
cust <- read.csv("/media/yomna/New\ Volume/DEBI/uOttawa/DS/Asgn3/customer_churn.csv",header=TRUE);
str(cust)
glimpse(cust)

library(caTools)
set.seed(42)
sample_split <- sample.split(Y = cust$Churn, SplitRatio = 0.67)
train_set <- subset(x = cust, sample_split == TRUE)
test_set <- subset(x = cust, sample_split == FALSE)

# Create the data for the chart
H <- c(nrow(train_set)/nrow(cust), nrow(test_set)/nrow(cust))

# Plot the bar chart 
barplot(H, main="Dataset split distribution",
        names.arg=c("Training Set", "Test Set"))

# -----------------------------

old_yes_train_set_no = table(train_set$Churn)["Yes"]

old_yes_train_set_ratio = old_yes_train_set_no / nrow(train_set)
no_to_yes_resample_ratio = 0.3 - old_yes_train_set_ratio 
no_to_yes_resample_no = no_to_yes_resample_ratio * nrow(train_set) 
new_yes_train_set_no =  nrow(train_set) - table(train_set$Churn)["No"] + no_to_yes_resample_no
new_yes_train_set_ratio = new_yes_train_set_no/ nrow(train_set)

# ---------------------------------
# install.packages("ROSE")
library(ROSE)
# balanced data set with resampling
data.balanced.under <- ovun.sample(Churn~., data=train_set,
                                  method="over",
                                 # N=4000 ,
                                  p=new_yes_train_set_ratio
                                  )$data

table(data.balanced.under$Churn)


new_new_yes_train_set_no = table(data.balanced.under$Churn)["Yes"]

new_new_yes_train_set_ratio = new_new_yes_train_set_no / nrow(data.balanced.under)

# ------------------

library(rpart)
library(rpart.plot)
library(caret)

data.balanced.under = data.balanced.under[,!(names(data.balanced.under) %in% c("customerID"))]

model <- rpart(Churn ~ ., data = data.balanced.under, method = "class") #specify method as class since we are dealing with classification
model

#plot the model
rpart.plot(model)
#Make predictions
preds <- predict(model, newdata = test_set, type = "class") #use the predict() function and pass in the testing subset
preds_prob <- predict(model, newdata = test_set, type = "prob") #use the predict() function and pass in the testing subset

#Print the confusion Matrix
conf = confusionMatrix(as.factor(test_set$Churn), as.factor(preds))

# Function copied from a friend who got it online
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}

# See accuracy before removing the non important features
draw_confusion_matrix(conf)



# ---------------------------

resampled_data = data.balanced.under
final_test <- na.omit(test_set)
# final_data <- na.omit(test_set)
# final_test <- test_set
for(i in 1:ncol(final_test)) {
  
  print(names(final_test[i]))
  print(sum(is.na(final_test[ , i]) ) ) 
}
## Random Forests ----
# random forest with default settings
# install.packages("randomForest")
library(randomForest)
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(300)
rf <- randomForest(as.factor(Churn) ~ ., data = resampled_data)
rf

#Make predictions
# preds_rf <- predict(rf, newdata = final_test, type = "raw") #use the predict() function and pass in the testing subset
preds_rf_prob <- predict(rf, newdata = final_test, type = "prob") #use the predict() function and pass in the testing subset
preds_rf_val <- ifelse(preds_rf_prob[,1]> 0.5,"No","Yes")

#Print the confusion Matrix
conf = confusionMatrix(   as.factor(preds_rf_val), as.factor(final_test$Churn) )
# See accuracy before removing the non important features
draw_confusion_matrix(conf)


# --------------------
#Manually tuning hyperparameters
rf_classifier = randomForest(as.factor(Churn) ~ ., data=resampled_data, ntree=200, mtry=2, importance=TRUE)
rf_classifier


#Make predictions
preds_rf_hyp <- predict(rf_classifier, newdata = final_test, type = "vote") #use the predict() function and pass in the testing subset
preds_rf_hyp_prob <- predict(rf_classifier, newdata = final_test, type = "prob") #use the predict() function and pass in the testing subset

preds_rf_hyp_prob_val <- ifelse(preds_rf_hyp_prob> 0.5,"No","Yes")
#Print the confusion Matrix
conf = confusionMatrix(    as.factor(final_test$Churn), as.factor(preds_rf_hyp_prob_val[,1]) )
# See accuracy before removing the non important features
draw_confusion_matrix(conf)


# -------------------------------------------
# compare their ROC curves
library("pROC")
roc_rf <- roc(final_test$Churn, preds_rf_prob[,1])
plot(roc_rf, col = "red", legacy.axes = TRUE)

roc_c50 <- roc(final_test$Churn, preds_rf_hyp_prob[,1])
plot(roc_c50, col = "blue", add = TRUE)

roc_dt <- roc(test_set$Churn, preds_prob[,1])
plot(roc_dt, col = "green", add = TRUE)

#----------------------

# The next part of the code is a playground for further modifications in the rando forest using cross validation and other methods
# -----------------------------
## Simulate a data mining competition
library(caret)
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 10,
                     # selectionFunction = "best",
                     savePredictions = TRUE,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

# auto-tune a random forest
grid_rf <- expand.grid(mtry = c(2, 4, 8, 16))

# test a random forest with the above settings
# note: this may take a long time to run (~10 minutes)
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(300)
m_rf <- train(Churn ~ ., data = resampled_data, method = "rf",
              metric = "ROC",  trControl = ctrl # ,
              # tuneGrid = grid_rf
          
              )
m_rf

#Make predictions
preds_m_rf <- predict(m_rf, newdata = final_test, type = "raw") #use the predict() function and pass in the testing subset
preds_m_rf_prob <- predict(m_rf, newdata = final_test, type = "prob") #use the predict() function and pass in the testing subset

#preds_m_rf_val <- ifelse(preds_m_rf[,1]> 0.5,"Yes","No")
#Print the confusion Matrix
conf = confusionMatrix(   as.factor(preds_m_rf), as.factor(final_test$Churn) )
# See accuracy before removing the non important features
draw_confusion_matrix(conf)
# ---------------------------
# auto-tune a boosted C5.0 decision tree
grid_c50 <- expand.grid(model = "tree",
                        trials = c(10, 25, 50, 100),
                        winnow = FALSE)

RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(300)
m_c50 <- train(as.factor(Churn) ~ ., data = resampled_data, method = "C5.0",
               metric = "ROC", trControl = ctrl,
               tuneGrid = grid_c50)
m_c50

#Make predictions
preds_m_c50 <- predict(m_c50, newdata = final_test, type = "raw") #use the predict() function and pass in the testing subset
preds_m_c50_prob <- predict(m_c50, newdata = final_test, type = "prob") #use the predict() function and pass in the testing subset


# preds_m_c50_val <- ifelse(preds_m_c50[,1]> 0.5,"Yes","No")
#Print the confusion Matrix
conf = confusionMatrix(   as.factor(preds_m_c50), as.factor(final_test$Churn) )

#Print the confusion Matrix
conf = confusionMatrix(as.factor(final_test$Churn), as.factor(preds_m_c50))
# See accuracy before removing the non important features
draw_confusion_matrix(conf)


# -------------------------------------------
# compare their ROC curves
library("pROC")
roc_rf <- roc(final_test$Churn, preds_m_rf_prob[,1])
plot(roc_rf, col = "red", legacy.axes = TRUE)

roc_c50 <- roc(final_test$Churn, preds_m_c50_prob[,1])
plot(roc_c50, col = "blue", add = TRUE)

roc_dt <- roc(test_set$Churn, preds_prob[,1])
plot(roc_dt, col = "green", legacy.axes = TRUE)

