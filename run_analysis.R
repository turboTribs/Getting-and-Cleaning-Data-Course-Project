###########################################################################################################################
### Date: 11/6/2016                                                                                                      ##
### Author: tribs 
###########################################################################################################################
### Description: https://www.coursera.org/learn/data-cleaning/peer/FIZtT/getting-and-cleaning-data-course-project
###    This is a coursera assignment project to create a tidy data set by combining various data sets.
###########################################################################################################################


library(reshape2)
library(dplyr)

## declare all the relevant files used in this project for Train and Test group.

activity_labels_file <- "UCI HAR Dataset/activity_labels.txt";      ## 6 rows of activities performed. contains (activity Id , activity)
features_file <- "UCI HAR Dataset/features.txt";                    ## 561 rows of column names for the files (x.train/test files)

subject_train_file <- "UCI HAR Dataset/train/subject_train.txt";    ## contains SubjectIDs of individuals who performed the activity
subject_test_file <- "UCI HAR Dataset/test/subject_test.txt";

y_train_file <- "UCI HAR Dataset/train/y_train.txt";                ## y = rows containing activityID performed by each subject 
y_test_file  <- "UCI HAR Dataset/test/y_test.txt";

x_test_file <- "UCI HAR Dataset/test/X_test.txt";                   ## x = column, variables related to the activities performed by subjects
x_train_file <- "UCI HAR Dataset/train/X_train.txt";


## Read files as dataframes, apply column names based on the data analyzed

subjects_train <-  read.table(subject_train_file, col.names=c("subject_id"));     ## 7352 rows of subject IDs
subjects_test  <-  read.table(subject_test_file , col.names=c("subject_id"));     

features_labels_df <-  read.table(features_file, col.names=c("feature_id","feature"));
features_labels <- features_labels_df[,2];                          ## extract the second field which is the variable name or (column names)

activity_labels <- read.table(activity_labels_file, col.names=c("activity_id","activity"));  ## 6 rows, contains Activity Name

y_activity_train <-  read.table(y_train_file, col.names=c("activity_id"));     ## a single column list of activities performed
y_activity_test <-  read.table(y_test_file, col.names=c("activity_id"));

x_train_df <-  read.table(x_train_file, col.names=features_labels);  ## read training data and assign column names
x_test_df  <-  read.table(x_test_file , col.names=features_labels);  ## read test data and assign column names

## reassigning names as col.names in the above code as the string "mean()"  was replaced it with mean.. etc

names(x_train_df) <- features_labels;
names(x_test_df) <- features_labels;


### step #1 
### Merges the training and the test sets to create one data set.

## form the row observation by Subject + Activity + (561 Variables)
cbind(subjects_train,y_activity_train,x_train_df) -> train;
cbind(subjects_test,y_activity_test,x_test_df) -> test;

rbind(train,test) ->TrainAndTest_Combined;

### Step#2
### Extracts only the measurements on the mean and standard deviation for each measurement. 

TrainAndTest_Combined[,grep("subject_id|activity|mean\\(\\)|std\\(\\)", names(TrainAndTest_Combined) )] -> TrainAndTest_subset

### Step 3
### Uses descriptive activity names to name the activities in the data set
### &
### Step 4 Appropriately labels the data set with descriptive variable names. 


## get the activity name from the activity dataset (key = activity_labels.activity_id = TrainAndTest_Activity.Activity_id ) 

TrainAndTest_Activity <- merge(activity_labels, TrainAndTest_subset, by = "activity_id")   

## delete the activity_id variable from the dataset. we now have the activity names and don't need the IDs

TrainAndTest_Activity$activity_id <- NULL;


### step #5
### From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

melted <- melt(TrainAndTest_Activity, id=c("subject_id","activity"))

activitiesBySubject <- dcast(melted,subject_id+activity~variable,mean)


### write tidy data for submission

write.table(activitiesBySubject,"activitiesBySubject.txt", row.name=FALSE)


## print the stats on the new dataset

str(activitiesBySubject)

##------------------------



