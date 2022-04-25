

library(dplyr)

#Downloading and extracting data in R in file "UCI HAR Dataset", which has 8 txt data files:
# 1) activity_labels.txt: labels from 1 to 6 the activities which the 30 participants did
# 2) features.txt: labels from 1 to 561 all the features measured from all participants

# 3) subject_test.txt: labels the subject(participants) from the test data (2, 4, 9, 10, 12, 13, 18, 20 and 24)
# 4) X_test.txt: complete test data set for all the features measured
# 5) y_test.txt: indicates the activity

# 6) subject_train.txt: labels the subjects from the train data (1, 3, 5, 6, 7, 8, 11, 
                  #14, 15, 16, 17, 19, 21, 22, 23, 25, 26, 27, 28, 29, 30) 
# 7) X_train.txt: complete train data set for all the features measured
# 8) y_train.txt: indicates the activity


features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","features"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "ID")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$features)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "ID")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$features)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


#1) Merges the training and the test sets to create one data set.

#Merging the features complete data set
Xtotal <- rbind(x_train, x_test)

#Merging the activities complete data set
Ytotal <- rbind(y_train, y_test)


#Joining all the subjects, ID(1:30), for all activities (code 1:6) from train and test datasets:
Subjecttotal <- rbind(subject_train, subject_test)

Datatotal <- cbind(Subjecttotal, Ytotal, Xtotal)

#2) Extracts only the measurements on the mean and standard deviation for each measurement.

Datatotal <- Datatotal %>% select(ID, code, contains("mean"), contains("std"))

#3) Uses descriptive activity names to name the activities in the data set

Datatotal$code <- activities[Datatotal$code, 2]

#4) Appropriately labels the data set with descriptive variable names. 

colnames(Datatotal)

names(Datatotal)[2] = "Activity"
names(Datatotal)<-gsub("Acc", "Accelerometer", names(Datatotal))
names(Datatotal)<-gsub("Gyro", "Gyroscope", names(Datatotal))
names(Datatotal)<-gsub("Mag", "Magnitude", names(Datatotal))
names(Datatotal)<-gsub("^t", "Time", names(Datatotal))
names(Datatotal)<-gsub("^f", "Frequency", names(Datatotal))


#5) From the data set in step 4, creates a second, independent tidy data set with the average
#of each variable for each activity and each subject.


NewDataAverage <- Datatotal %>%
  group_by(ID, Activity) %>%
  summarise_all(funs(mean))

write.table(NewDataAverage, "NewDataAverage.txt", row.name=FALSE)
