# Read the csv data file
hypo <-read.csv("/media/yomna/New\ Volume/DEBI/uOttawa/DS/RAsgn2/hypothyroid.csv", header =TRUE, sep =",")
# Display structure
str(hypo)
# Number of data points
n=nrow(hypo)
# Turn it into a data frame
hypo <- data.frame(hypo)
head(hypo)
# Check how many rows in the feature "Age" has the "?" (unknown) Value
# (This is a test line for getting the right number of rows)
sum(hypo$age == "?")

# Check how many rows in each feature has the value "?"
for(i in 1:ncol(hypo)) {
  
  print(names(hypo[i]))
  print(sum(hypo[ , i]=="?") ) 
}

# We note that "TGB" Feature is basically unknown throughout all the rows
# We should drop this feature
# Note also that "TGB" column is related to "TGB_measured" column, so we can drop both, but i will assume that the "TGB_measured" column will be useful 
drop <- c("TBG")
hypo = hypo[,!(names(hypo) %in% drop)]

# Change all the "?" Values to "??"
# (This is a test line for changing the right cells)
hypo[hypo == "?"] <- "??"

# Now replace all the other "??" values with the mode values for categorical columns and mean values for numeric columns

# Categorical Columns: ["sex","on_thyroxine","query_on_thyroxine","on_atithyroid_medication","sick","pregnant",""thyroid_surgery,"I131_treatment","query_hypothyroid","query_hyperthyroid","lithium","goitre","tumor","hypopituitary","psych","TSH_measured","T3_measured","TT4_measured","T4U_measured","FTI_measured","TBG_measured","referral_source","Class"]
# Numerical Columns : ["age","TSH","T3","TT4","T4U","FTI"]

# Fortunately, not all of these columns contain missing values
# Categorical Columns with missing values: ["sex"]
# Numerical Columns with missing values: ["age","TSH","T3", "TT4","T4U", "FTI"]

library(modeest)
sex_mode = mfv(hypo$sex) #mode
hypo$sex[hypo$sex == "??"] <- sex_mode #impute categorical
sum(hypo$sex == "?") #double check that the imputation worked

# Impute numeric
numerical_col_w_missing_values = c ("age","TSH","T3", "TT4","T4U", "FTI")

# First give a unique numerical value to be able to calculate the mean with no errors
hypo$age[hypo$age=="??"] <- "000"
hypo$age[hypo$age=="000"] <- mean(as.numeric(as.character( hypo$age) ) ) #impute
hypo$age <- as.numeric(hypo$age) 

hypo$TSH[hypo$TSH=="??"] <- "000"
hypo$TSH[hypo$TSH=="000"] <- mean(as.numeric(as.character( hypo$TSH) ) )
hypo$TSH <- as.numeric(hypo$TSH) 

hypo$T3[hypo$T3=="??"] <- "000"
hypo$T3[hypo$T3=="000"] <- mean(as.numeric(as.character( hypo$T3) ) )
hypo$T3 <- as.numeric(hypo$T3) 

hypo$TT4[hypo$TT4=="??"] <- "000"
hypo$TT4[hypo$TT4=="000"] <- mean(as.numeric(as.character( hypo$TT4) ) )
hypo$TT4 <- as.numeric(hypo$TT4) 

hypo$T4U[hypo$T4U=="??"] <- "000"
hypo$T4U[hypo$T4U=="000"] <- mean(as.numeric(as.character( hypo$T4U) ) )
hypo$T4U <- as.numeric(hypo$T4U) 

hypo$FTI[hypo$FTI=="??"] <- "000"
hypo$FTI[hypo$FTI=="000"] <- mean(as.numeric(as.character( hypo$FTI) ) )
hypo$FTI <- as.numeric(hypo$FTI) 

sapply(hypo, class)

# Double check the imputation
for(i in 1:ncol(hypo)) {
  print(names(hypo[i]))
  print(sum(hypo[ , i]=="000") ) 
}

library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

# Select features by confusion matrix exploration
# ensure the results are repeatable
set.seed(7)
# install.packages("mlbench")
library(mlbench)
# calculate correlation matrix
numerical_col = c("age","TSH","T3","TT4","T4U","FTI")

correlationMatrix <- cor(hypo[sapply(hypo, is.numeric)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#Using Corrplot
# install.packages("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r") # to use rquery.cormat
#calculate correlation matrix
rquery.cormat(hypo[sapply(hypo, is.numeric)])
cormat<-rquery.cormat(hypo[sapply(hypo, is.numeric)], graphType="heatmap")


##plot correlation matrix using Heatmap
library(corrplot)
library(reshape2) #to meld the data frame
library(ggplot2)

corrplot(cor(hypo[sapply(hypo, is.numeric)]),        # Correlation matrix
         method = "circle",                # Correlation plot method (method = number, circle, pie, or color)
         type = "full",                   # Correlation plot style (also "upper" and "lower")
         diag = TRUE,                     # If TRUE (default), adds the diagonal
         tl.col = "black",                # Labels color
         bg = "white",                    # Background color
         title = "",                      # Main title
         col = NULL,                      # Color palette
         tl.cex =0.7,
         cl.ratio =0.2)                            

corrplot(cor(hypo[sapply(hypo, is.numeric)],method = "pearson"),diag = TRUE, #spearman can e used on qualitative data
         method = "ellipse",
         tl.cex = 0.7, tl.col = "black", cl.ratio = 0.2)

# By looking at the plots and the numbers (0.78) we note that FTI and TT4 have high correlation, we can drop one of them
drop <- c("TT4")
hypo = hypo[,!(names(hypo) %in% drop)]

# Check if any column has only one value

for (i in 1:ncol(hypo)){
  print(names(hypo[i]))
  print(unique(hypo[i]))
}
# Drop TBG_measured 
drop <- c("TBG_measured", "hypopituitary")
hypo = hypo[,!(names(hypo) %in% drop)]

#**********************************************************************************************************
#*BUILDING A SIMPLE DECISION TREE I
#*
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)


set.seed(42)
sample_split <- sample.split(Y = hypo$Class, SplitRatio = 0.75)
train_set <- subset(x = hypo, sample_split == TRUE)
test_set <- subset(x = hypo, sample_split == FALSE)

model <- rpart(Class ~ ., data = train_set, method = "class") #specify method as class since we are dealing with classification
model

#plot the model
rpart.plot(model)

#Select features by checking feature importance
importances <- varImp(model) #use the varImp() function to determine how much predictive power lies in each feature
importances %>% arrange(desc(Overall))


# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#Make predictions
preds <- predict(model, newdata = test_set, type = "class") #use the predict() function and pass in the testing subset
preds

#Print the confusion Matrix
conf = confusionMatrix(as.factor(test_set$Class), as.factor(preds))

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

# Drop Zero importance features
drop <- c("age", "sex", "query_on_thyroxine", "on_antithyroid_medication", "sick", "pregnant", "I131_treatment", "query_hyperthyroid","lithium","goitre","tumor","psych","T3_measured","TT4_measured", "T4U_measured", "T4U", "FTI_measured")
hypo = hypo[,!(names(hypo) %in% drop)]

#*************************************************************************************************************

# Again train the model on the clean data
set.seed(42)
sample_split_clean <- sample.split(Y = hypo$Class, SplitRatio = 0.75)
train_set_clean <- subset(x = hypo, sample_split == TRUE)
test_set_clean <- subset(x = hypo, sample_split == FALSE)

model_clean <- rpart(Class ~ ., data = train_set_clean, method = "class") #specify method as class since we are dealing with classification
model_clean

#plot the model
rpart.plot(model_clean)

#Make predictions
preds_clean <- predict(model_clean, newdata = test_set_clean, type = "class") #use the predict() function and pass in the testing subset

# hypo <- hypo %>% mutate_if(is.character,as.factor)
# hypo <- hypo %>% mutate_if(is.numeric,as.factor)

#Print the confusion Matrix
conf_clean = confusionMatrix(as.factor(test_set_clean$Class), as.factor(preds_clean))
draw_confusion_matrix(conf_clean)
# Note: No big difference, seems like the dropping only speeded up the process

# ********************************************
# K-fold cross validation
# Define training control

set.seed(123) 
sampleSize = 0.8 * nrow(hypo)
ind <- sample(seq_len(nrow(hypo)), size= sampleSize ) # Create the sample
trainData_cv <- hypo[ind,]
trainData_cv
testData_cv <- hypo[ -ind,]

train.control <- trainControl(method = "cv", number = 10)
# Train the model
model_clean_cv <- train(Class ~., data = trainData_cv, method = "rpart",
               trControl = train.control)

print(model_clean_cv)

library(dplyr)
library(tidyr)
# install.packages('rpart.plot')
library(rpart.plot)
#view final model
model_clean_cv$finalModel
prp(model_clean_cv$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#Make predictions
preds_clean_cv <- predict(model_clean_cv, newdata = test_set_clean) #use the predict() function and pass in the testing subset

#Print the confusion Matrix
conf_clean_cv = confusionMatrix(as.factor(test_set_clean$Class), as.factor(preds_clean_cv))
draw_confusion_matrix(conf_clean_cv)

# *****************************************
# Pruning
Ecom_Tree_prune<-prune(model_clean,cp=0.001)
Ecom_Tree_prune

#plot the model
rpart.plot(Ecom_Tree_prune)

#Make predictions
preds_clean_prune <- predict(Ecom_Tree_prune, newdata = test_set_clean, type = "class") #use the predict() function and pass in the testing subset

#Print the confusion Matrix
conf_clean_prune = confusionMatrix(as.factor(test_set_clean$Class), as.factor(preds_clean_prune))
draw_confusion_matrix(conf_clean_prune)
#---------
Sample_tree<-rpart(Class ~ . , method="class", data=train_set_clean, control=rpart.control(minsplit=2, cp=0.001))
Sample_tree
#plot the model
rpart.plot(Sample_tree)

#Make predictions
preds_clean_prune2 <- predict(Sample_tree, newdata = test_set_clean, type = "class") #use the predict() function and pass in the testing subset

#Print the confusion Matrix
conf_clean_prune2 = confusionMatrix(as.factor(test_set_clean$Class), as.factor(preds_clean_prune2))
draw_confusion_matrix(conf_clean_prune2)
# EVEN better accuracy !
# ----------------

# Ways to try to improve tree model 
# Gini and kfolds
model_clean_cv_gini <- train(Class~., data = train_set_clean, method = "rpart", parms = list(split = "gini"), trControl = train.control, tuneLength = 10)
#plot the model
#rpart.plot(model_clean_cv_gini)

library(dplyr)
library(tidyr)
# install.packages('rpart.plot')
library(rpart.plot)
#view final model
model_clean_cv_gini$finalModel
prp(model_clean_cv_gini$finalModel, box.palette = "Reds", tweak = 1.2) #view the tree using prop() function

#Make predictions
preds_clean_cv_gini <- predict(model_clean_cv_gini, newdata = test_set_clean) #use the predict() function and pass in the testing subset

#Print the confusion Matrix
conf_clean_cv_gini = confusionMatrix(as.factor(test_set_clean$Class), as.factor(preds_clean_cv_gini))
draw_confusion_matrix(conf_clean_cv_gini)

