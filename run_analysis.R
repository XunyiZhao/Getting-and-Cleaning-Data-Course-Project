#This R script does the following: 
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement.
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names.
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#import library
library(dplyr)

#get data
Url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(Url, "data/UCI.zip", mode = "wb")
# unzip zip file
unzip('data/UCI.zip',exdir = './data')

#read data 
x_train <- read.table('data/UCI HAR Dataset/train/X_train.txt')
y_train <- read.table('data/UCI HAR Dataset/train/Y_train.txt')
x_test <- read.table('data/UCI HAR Dataset/test/X_test.txt')
y_test <- read.table('data/UCI HAR Dataset/test/Y_test.txt')
sub_train <- read.table('data/UCI HAR Dataset/train/subject_train.txt')
sub_test <- read.table('data/UCI HAR Dataset/test/subject_test.txt')

feature = read.table('data/UCI HAR Dataset/features.txt')
act = read.table('data/UCI HAR Dataset/activity_labels.txt')

#rename variable names
colnames(x_train) <- as.character(feature$V2)
colnames(y_train) <- 'activity'
colnames(x_test) <- as.character(feature$V2)
colnames(y_test) <- 'activity'
colnames(sub_train) <- 'subject'
colnames(sub_test) <- 'subject'

#select mean and std variables
col_select <- grepl("mean|std", colnames(x_test))
x_train <- subset(x_train, select = which(as.logical(col_select)))
x_test <- subset(x_test, select = which(as.logical(col_select)))

#merge dataset
train <- cbind(x_train, y_train, sub_train)
test <- cbind(x_test, y_test, sub_test)
dt <- rbind(train, test)

#replace the activity labels
dt$activity <- factor(dt$activity, levels = act[, 1], labels = act[, 2])

#substitude the descreptive names
dcr_name = colnames(dt)
dcr_name <- gsub("^f", "Frequency", dcr_name)
dcr_name <- gsub("^t", "Time", dcr_name)
dcr_name <- gsub("Acc", "Accelerometer", dcr_name)
dcr_name <- gsub("Gyro", "Gyroscope", dcr_name)
dcr_name <- gsub("Mag", "Magnitude", dcr_name)
dcr_name <- gsub("mean", "Mean", dcr_name)
dcr_name <- gsub("std", "StandardDeviation", dcr_name)
colnames(dt) <- dcr_name

#group data by subject and activity, then apply mean fucntion
tidy_dt <- dt %>% group_by(subject, activity) %>% summarise_each(funs(mean))

# output to file "tidy_data.csv"
write.table(tidy_dt, "tidy_data.csv", row.names = FALSE, quote = FALSE)

