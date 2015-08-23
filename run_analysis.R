## run_analysis.R

# Source of data for this project: 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

# This R script does the following:

# 1. Read the training and the test sets and merge them to create one data set.

setwd("E:/Data Science/3_Getting and Cleaning Data/data")
getwd()
tbl1 <- read.table("train/X_train.txt")
tbl2 <- read.table("test/X_test.txt")
Xset <- rbind(tbl1, tbl2)

tbl1 <- read.table("train/subject_train.txt")
tbl2 <- read.table("test/subject_test.txt")
Sset <- rbind(tbl1, tbl2)

tbl1 <- read.table("train/y_train.txt")
tbl2 <- read.table("test/y_test.txt")
Yset <- rbind(tbl1, tbl2)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
Xset <- Xset[, indices_of_good_features]
names(Xset) <- features[indices_of_good_features, 2]
names(Xset) <- gsub("\\(|\\)", "", names(Xset))
names(Xset) <- tolower(names(Xset))

# 3. Uses descriptive activity names to name the activities in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Yset[,1] = activities[Yset[,1], 2]
names(Yset) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(Sset) <- "subject"
cleaned <- cbind(Sset, Yset, Xset)
write.table(cleaned, "merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(Sset)[,1]
numSubjects = length(unique(Sset)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "data_set_with_the_averages.txt")


