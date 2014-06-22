# The script does the following 5 behaviours

# 1. Merges the training and the test sets to create one data set.

train_data <- read.table("UCI HAR Dataset/train/X_train.txt")
test_data <- read.table("UCI HAR Dataset/test/X_test.txt")
data <- rbind(train_data, test_data)

train_data <- read.table("UCI HAR Dataset/train/subject_train.txt")
test_data <- read.table("UCI HAR Dataset/test/subject_test.txt")
S <- rbind(train_data, test_data)

train_data <- read.table("UCI HAR Dataset/train/y_train.txt")
test_data <- read.table("UCI HAR Dataset/test/y_test.txt")
Y <- rbind(train_data, test_data)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("UCI HAR Dataset/features.txt")
good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
data <- data[, good_features]
names(data) <- features[good_features, 2]
names(data) <- gsub("\\(|\\)", "", names(data))
names(data) <- tolower(names(data))  # see last slide of the lecture Editing Text Variables (week 4)

# 3. Uses descriptive activity names to name the activities in the dataset

activities <- read.table("UCI HAR Dataset/activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the dataset with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(S, Y, data)
write.table(cleaned, "merged_data.txt")

# 5. Creates a 2nd, independent tidy dataset with the average of each variable for each activity and each subject.

distinct_subject = unique(S)[,1]
num_of_subject = length(unique(S)[,1])
num_of_activities = length(activities[,1])
num_of_cols = dim(cleaned)[2]
result = cleaned[1:(num_of_subject * num_of_activities), ]

row = 1
for (s in 1:num_of_subject) {
  for (a in 1:num_of_activities) {
    result[row, 1] = distinct_subject[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:num_of_cols] <- colMeans(tmp[, 3:num_of_cols])
    row = row + 1
  }
}
write.table(result, "tidy_dataset_with_means.txt")