library(dplyr)
# Read the csv data file
dia <-read.csv("/media/yomna/New\ Volume/DEBI/uOttawa/DS/RAsgn2/diabetes.csv", header =TRUE, sep =",")
# Display structure
str(dia)
# Number of data points
n=nrow(dia)
# Turn it into a data frame
dia <- data.frame(dia)
head(dia)

# All columns should be numeric or int (This transformation will replace all "?" with NA values)
dia <- dia %>% mutate_if(is.character,as.numeric)

# Check how many rows in each feature has the value "NA"
for(i in 1:ncol(dia)) {
  
  print(names(dia[i]))
  print(sum(is.na(dia[ , i])) ) 
}


# Impute numeric
hist(dia$Glucose, main = "Glucose", xlab = "Density") #Normal distribution: impute with mean
dia$Glucose[is.na(dia$Glucose)] <- mean( as.numeric(dia$Glucose), na.rm = TRUE) #impute

hist(dia$BloodPressure, main = "BloodPressure", xlab = "Density") #Normal distribution: impute with mean
dia$BloodPressure[is.na(dia$BloodPressure)] <- mean(dia$BloodPressure, na.rm = TRUE ) #impute

hist(dia$SkinThickness, main = "SkinThickness", xlab = "Density") #Normal distribution: impute with mean
dia$SkinThickness[is.na(dia$SkinThickness)] <- mean(dia$SkinThickness, na.rm = TRUE ) #impute

hist(dia$Insulin, main = "Insulin", xlab = "Density") #Positively Skewed: impute with median
dia$Insulin[is.na(dia$Insulin)] <- median(dia$Insulin, na.rm = TRUE ) #impute

hist(dia$BMI, main = "BMI", xlab = "Density") #Normal distribution: impute with mean
dia$BMI[is.na(dia$BMI)] <- mean(dia$BMI, na.rm = TRUE ) #impute

# ***************************
library(neuralnet)
library(caret)
library(ggplot2)
# NN without scaling data
# Random sampling
samplesize = 0.75 * nrow(dia)
set.seed(80) #to generate same random sample every time & maintain consistency
index = sample( seq_len (nrow(dia)), size = samplesize )

# Create training and test set
datatrain_base = dia[ index, ]
datatest_base = dia[ -index, ]

set.seed(2)
NN_base = neuralnet(Outcome ~., datatrain_base, hidden = 2 , linear.output = T)
plot(NN_base)
# Training Error 65.5

## Prediction using neural network
predict_testNN_base = compute(NN_base, datatest_base[,c(1:8)])
predict_testNN_base = predict_testNN_base$net.result
predict_testNN_base <- ifelse(predict_testNN_base > 0.5, 1, 0)


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

#Print the confusion Matrix
conf_base = confusionMatrix(as.factor(predict_testNN_base[,1]), as.factor(datatest_base$Outcome))

# See accuracy before removing the non important features
draw_confusion_matrix(conf_base)

# ***************************
# Scaling 
## Scale data for neural network - important to prevent a variable from having a large impact on the prediction variable 
max = apply(dia , 2 , max)
min = apply(dia, 2 , min)
scaled = as.data.frame(scale(dia, center = min, scale = max - min))

hist(scaled$Glucose, main = "Glucose", xlab = "Density") #Normal distribution: impute with mean

## Fit neural network 

# install library
# install.packages("neuralnet ")

# load library
library(neuralnet)

# creating training and test set
trainNN_scaled = scaled[index , ]
testNN_scaled = scaled[-index , ]

# fit neural network
set.seed(2)
NN_scaled = neuralnet(Outcome ~ ., trainNN_scaled, hidden = 2 , linear.output = T )

# plot neural network
plot(NN_scaled)
#Error 40.09


## Prediction using neural network
predict_testNN_scaled = compute(NN_scaled, testNN_scaled[,c(1:8)])
predict_testNN_scaled = predict_testNN_scaled$net.result
predict_testNN_scaled <- ifelse(predict_testNN_scaled > 0.5, 1, 0)

#Print the confusion Matrix
conf_scaled = confusionMatrix(as.factor(predict_testNN_scaled[,1]), as.factor(testNN_scaled$Outcome))

# See accuracy before removing the non important features
draw_confusion_matrix(conf_scaled)

# ***************************************
# Netwrok 2 layers 5 nodes
# fit neural network
library(neuralnet)
set.seed(2)
NN_2L5N = neuralnet(Outcome ~ ., trainNN_scaled, hidden = c(5,5) , linear.output = F)

# plot neural network
plot(NN_2L5N)
#Error 17.036


## Prediction using neural network
predict_testNN_2L5N = compute(NN_2L5N, testNN_scaled[,c(1:8)])
predict_testNN_2L5N = predict_testNN_2L5N$net.result
predict_testNN_2L5N <- ifelse(predict_testNN_2L5N > 0.5, 1, 0)

#Print the confusion Matrix
conf_2L5N = confusionMatrix(as.factor(predict_testNN_2L5N[,1]), as.factor(testNN_scaled$Outcome))

# See accuracy before removing the non important features
draw_confusion_matrix(conf_2L5N)

# **************************************
# Change activation function tanh

# fit neural network
library(neuralnet)
set.seed(2)
NN_tanh = neuralnet(Outcome ~ ., trainNN_scaled, hidden = 2, linear.output = F, act.fct = tanh)

# plot neural network
plot(NN_tanh)
#Error 41.31


## Prediction using neural network
predict_testNN_tanh = compute(NN_tanh, testNN_scaled[,c(1:8)])
predict_testNN_tanh = predict_testNN_tanh$net.result
predict_testNN_tanh <- ifelse(predict_testNN_tanh > 0.5, 1, 0)

#Print the confusion Matrix
conf_tanh = confusionMatrix(as.factor(predict_testNN_tanh[,1]), as.factor(testNN_scaled$Outcome))

# See accuracy before removing the non important features
draw_confusion_matrix(conf_tanh)

# ********************************
# Change the learning rate 0.01

# fit neural network
library(neuralnet)
set.seed(2)
NN_lr = neuralnet(Outcome ~ ., trainNN_scaled, hidden = 2, linear.output = F, learningrate = 0.01)

# plot neural network
plot(NN_lr)
#Error 39.6


## Prediction using neural network
predict_testNN_lr = compute(NN_lr, testNN_scaled[,c(1:8)])
predict_testNN_lr = predict_testNN_lr$net.result
predict_testNN_lr <- ifelse(predict_testNN_lr > 0.5, 1, 0)

#Print the confusion Matrix
conf_lr = confusionMatrix(as.factor(predict_testNN_lr[,1]), as.factor(testNN_scaled$Outcome))

# See accuracy before removing the non important features
draw_confusion_matrix(conf_lr)

# ********************************
#  Change number of epochs 10
# fit neural network
library(neuralnet)
set.seed(2)
NN_ep = neuralnet(Outcome ~ ., trainNN_scaled, hidden = 2, linear.output = F,  rep = 10)

# plot neural network
plot(NN_ep)
#Error 39.5


## Prediction using neural network
predict_testNN_ep = compute(NN_ep, testNN_scaled[,c(1:8)])
predict_testNN_ep = predict_testNN_ep$net.result
predict_testNN_ep <- ifelse(predict_testNN_ep > 0.5, 1, 0)

#Print the confusion Matrix
conf_ep = confusionMatrix(as.factor(predict_testNN_ep[,1]), as.factor(testNN_scaled$Outcome))

# See accuracy before removing the non important features
draw_confusion_matrix(conf_ep)

