
#Read the data
bnk <-read.csv("C:/Users/admin/Desktop/DSASGN1/Tut3-EDA_Data_Preparation/Tut3-EDA_Data_Preparation/bank-additional-full.csv", header =TRUE, sep =";")
str(bnk)

dim(bnk) # to determine the number of records

n=nrow(bnk)
bnk$index <- c(1:n) # creates an index on the records
head(bnk) # gives the 1st 5 records

#Reduce the dataset to only four predictors
bnk <- data.frame(age= bnk$age,
                  education = bnk$education,
                  previous  = bnk$previous,
                  pdays     = bnk$pdays ,
                  target    = bnk$y
                  )
head(bnk)

#Note that pdays have so many 999 valued recordsm
#plot histogram to investigate
hist(bnk$pdays, main = "Frequency of pdays", xlab = "pdays number")

#replace the 999 value with a more meaningful NA value
bnk$pdays[bnk$pdays==999] <- NA
#plot histogram again Note: now it's much clearer
hist(bnk$pdays, main = "Frequency of pdays", xlab = "pdays number")

#Transform education values from categorical character to numeric
bnk$education[bnk$education=="illiterate"] <- 0
bnk$education[bnk$education=="basic.4y"]   <- 4
bnk$education[bnk$education=="basic.6y"]   <- 6
bnk$education[bnk$education=="basic.9y"]   <- 9
bnk$education[bnk$education=="high.school"]<- 12
bnk$education[bnk$education=="professional.course"] <- 14
bnk$education[bnk$education=="university.degree"]   <- 16
bnk$education[bnk$education=="unknown"]    <- NA
unique(bnk$education)
#note it's still character
typeof(bnk$education)
#transform to double
bnk$education <- as.numeric(bnk$education)
#make sure transformation happened
typeof(bnk$education)
#plot histogram to visually investigate the values
hist(bnk$education, main = "Frequency of education types", xlab = "education type")

#### Calculate mean, median, mode of Age
#install modeest package for mode calculation
#install.packages("modeest")
#library(modeest)
mfv(bnk$age) #mode

mean(bnk$age) #mean
median(bnk$age) #median

#plot boxplot of age
boxplot(bnk$age,data=bnk, main="Bank Data", xlab="Age", ylab="Age")

#calculate quantiles

#First quartile:
quantile(bnk$age, 0.25)

#Second quartile or median:
quantile(bnk$age, 0.5)


#Third quartile:
quantile(bnk$age, 0.75)

max(bnk$age) #max
min(bnk$age) #min

#The interquartile range is the difference between the 75th percentile and 25th percentile 
quantile(bnk$age, 0.75, names = FALSE) - quantile(bnk$age, 0.25, names =FALSE)

#OR
IQR(bnk$age)

#standadize the age
#Use the scale() function to standardize
bnk$age_z <- scale(x = bnk$age, center= TRUE, scale= TRUE)
bnk$age_z

#Identifying Outliers in age
#find outliers by using the query() function, which identifies
#rows that meet a particular condition

hist(bnk$age_z, main = "Frequency of age_z", xlab = "age_z")

bnk_outliers <-  bnk[ which(bnk$age_z < -3 | bnk$age_z > 3), ]
bnk_outliers


#-----------------------------------------------------