# Getting-and-Cleaning-Data
Coursera Week 4 Assignment

#Review Criteria
#1. The submitted data set is tidy.
#2. The Github repo contains the required scripts.
#3. GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
#4. The README that explains the analysis files is clear and understandable.
#5. The work submitted for this project is the work of the student who submitted it.

#Create one R script called run_analysis.R that does the following.

#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.




#1. Merges the training and the test sets to create one data set.

#Install the packages that will help acheive these goals:

library(utils)
library(dplyr)
library(readr)
library(XML)
library(tidyr)
library(data.table)


#read in the data sets from UCI HAR Dataset, found here:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

features <- read.table("features.txt", header=F, stringsAsFactors=F)
activity <- read.table("activity_labels.txt", header=F)

testx <- read.table("test/X_test.txt")
testlabels <- read.table("test/Y_test.txt")
testsubject <- read.table("test/subject_test.txt")

trainx <-read.table("train/X_train.txt")
trainlabels <- read.table("train/Y_train.txt")
trainsubject <- read.table("train/subject_train.txt")


#bind, merge, name columns, make factors for subjects, eliminate duplicates
test <-cbind(testsubject, testlabels, testx)
train <-cbind(trainsubject, trainlabels, trainx)
combo <-rbind(test,train) 
colnames(combo)<- c("subject", "activity", features$V2)
combo$subject <-as.factor(combo$subject)


total <- combo[, !duplicated(colnames(combo))] 




#2. Extracts only the measurements on the mean and standard deviation for each measurement.

extract <-select(total, subject, activity, contains("mean"), contains("std"))
  


#3. Uses descriptive activity names to name the activities in the data set
#This assumes that "the data set" refers to the "the data set" downloaded at the onset of this exercise.
#This is not a rename of the activities in the extract in step #2 above.

activities <- mutate(activity, activity=tolower(V2))
total$activity[total$activity==1] <- activities[1,3]
total$activity[total$activity==2] <- activities[2,3]
total$activity[total$activity==3] <- activities[3,3]
total$activity[total$activity==4] <- activities[4,3]
total$activity[total$activity==5] <- activities[5,3]
total$activity[total$activity==6] <- activities[6,3]
total$activity <- as.factor(total$activity)


#4. Appropriately labels the data set with descriptive variable names.
#Again, this assumes that "the data set" refers to the original data set.

names(total) <- gsub("^t", "time", names(total))
names(total) <- gsub("^f", "frequency", names(total))
names(total) <- gsub("Acc", "acceleration", names(total))
names(total) <- gsub("Mag", "magnitude", names(total))
names(total) <- gsub("Gyro", "gyroscope", names(total))

#5. From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.

tidy <- total %>%
    group_by(subject, activity)%>%
    summarise_each(funs(mean(.,na.rm=TRUE)),-subject, -activity) 

