#Read all the datasets
ratings=read.csv2("/media/yomna/New\ Volume/DEBI/uOttawa/DS/Asgn4/raitings.csv", header = TRUE, sep = ";", dec = ",")
courses=read.csv2("/media/yomna/New\ Volume/DEBI/uOttawa/DS/Asgn4/courses.csv", header = TRUE, sep = ";", dec = ",")

library(stringi)
library(reshape2)
library(lsa)
#Create ratings matrix with rows as users and columns as courses
ratingmat_original = dcast(ratings, student_id~course_id, value.var = "rating", na.rm=FALSE)
ratingmat_original_without_null = ratingmat_original
#remove na values
ratingmat_original_without_null[is.na(ratingmat_original_without_null)] <- 0 
#We can now remove user ids
ratingmat = as.matrix(ratingmat_original_without_null[,-1])


#compute the cosine similarity between students.
cosine_method<-cosine(t(ratingmat))
cosine_method
#plot compute the cosine similarity between students.
heatmap(lsa::cosine(t(ratingmat)))

cos_df = data.frame(cosine_method)
heatmap(lsa::cos_df)

# -------------------------------------------------------
# install.packages("recommenderlab", dependencies=TRUE)
library(recommenderlab)

#We can now remove user ids
ratingmat = as.matrix(ratingmat_original[,-1])

#Convert ratings matrix to real rating matrx which makes it dense
ratingmat = as(ratingmat, "realRatingMatrix")

#Create Recommender Model. The parameters are UBCF and Cosine similarity. We take 10 nearest neighbours.
#"UBCF" stands for User-Based Collaborative Filtering
rec_mod = Recommender(ratingmat, method = "UBCF", param=list(method="Cosine",nn=3)) 

#Obtain top first recommendations for 6th student (E.N)
Top_1_pred = predict(rec_mod, ratingmat[6], n=1)

#Convert the recommendations to a list
Top_1_List = as(Top_1_pred, "list")
Top_1_List


#take a look at the courses names that correspond to these number
# install.packages("dplyr")
library(dplyr)

#We convert the list to a dataframe and change the column name to course_id
Top_1_df=data.frame(as(Top_1_pred, "list"))
colnames(Top_1_df)="course_id"

#Since course_id is of type integer in courses data, we typecast id in our recommendations as well
Top_1_df$course_id=as.numeric(Top_1_df$course_id)
# courses$course_Id=as.numeric(Top_5_df$course_id)

#Merge the courses ids with names to get titles and genres
names=left_join(Top_1_df, courses, by="course_id")

#Print the titles and genres
names


#--------------------------------------------

#Create Recommender Model. The parameters are IBCF and Cosine similarity. We take  nearest neighbours
#IBCF: item based collaborative filtering
rec_mod_I = Recommender(ratingmat, method = "IBCF", param=list(method="Cosine")) 
rec_mod_I

#Obtain top recommendations for 1st user entry in dataset
Top_1_pred_I = predict(rec_mod_I, ratingmat[6], n=1)


#Convert the recommendations to a list
Top_1_List_I = as(Top_1_pred_I, "list")
Top_1_List_I


#We convert the list to a dataframe and change the column name to course_id
Top_1_df_I <-data.frame(Top_1_List_I)
colnames(Top_1_df_I)="course_id"

#Since courseid is of type integer 
Top_1_df_I$course_id=as.numeric((Top_1_df_I$course_id))

#Merge the course ids with names
names_I=left_join(Top_1_df_I, courses, by="course_id")

#Print the titles of course
names_I
