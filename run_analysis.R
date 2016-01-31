# Libraries
librar(plyr)
librar(dplyr)

# Read Data
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")

train_data <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_label <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_subject_no <- read.table("./UCI HAR Dataset/train/subject_train.txt")

test_data <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_label <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_subject_no <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Merges the training and the test sets to create one data set.
merge_train <- train_subject_no
merge_train$V2 <- "train"
merge_train$V3 <- as.matrix(train_label)[,1]
merge_train <- cbind(merge_train,train_data)
names(merge_train) <- c("Subject.Number", "Dataset", "Activity", as.character(features[,2]))

merge_test <- test_subject_no
merge_test$V2 <- "test"
merge_test$V3 <- as.matrix(test_label)[,1]
merge_test <- cbind(merge_test,test_data)
names(merge_test) <- c("Subject.Number", "Dataset", "Activity", as.character(features[,2]))

merge_data <- rbind(merge_train, merge_test)
names(merge_data) <- make.names(names(merge_data), unique = TRUE)
  
# Extracts only the measurements on the mean and standard deviation for each measurement.

mean_index <- intersect(grep("mean", names(merge_data)),grep("Freq", names(merge_data),invert=TRUE))
mean_std_index <- union(mean_index, grep("std", names(merge_data)))
mean_std_names <- names(merge_data)[mean_std_index]

mean_std_data <- select(merge_data, one_of(c("Subject.Number", "Activity", "Dataset"), mean_std_names))

# Uses descriptive activity names to name the activities in the data set

mean_std_data$Activity[mean_std_data$Activity == 1] <- "WALKING"
mean_std_data$Activity[mean_std_data$Activity == 2] <- "WALKING_UPSTAIRS"
mean_std_data$Activity[mean_std_data$Activity == 3] <- "WALKING_DOWNSTAIRS"
mean_std_data$Activity[mean_std_data$Activity == 4] <- "SITTING"
mean_std_data$Activity[mean_std_data$Activity == 5] <- "STANDING"
mean_std_data$Activity[mean_std_data$Activity == 6] <- "LAYING"

# Step 4, Appropriately labels the data set with descriptive variable names.

names(mean_std_data) <- c("Subject.Number", "Dataset", "Activity",  
                          "X.Body.Acceleration.mean", "Y.Body.Acceleration.mean", "Z.Body.Acceleration.mean",
                          "X.Gravity.Acceleration.mean","Y.Gravity.Acceleration.mean", "Z.Gravity.Acceleration.mean",
                          "X.Jerk.Body.Acceleration.mean", "Y.Jerk.Body.Acceleration.mean", "Z.Jerk.Body.Acceleration.mean",
                          "X.Gyro.Rotation.mean", "Y.Gyro.Rotation.mean", "Z.Gyro.Rotation.mean",
                          "X.Gyro.Rotation.Jerk.mean", "Y.Gyro.Rotation.Jerk.mean", "Z.Gyro.Rotation.Jerk.mean",
                          "Body.Acceleration.Magnitude.mean", "Gravity.Acceleration.Magnitude.mean", "Body.Acceleration.Jerk.Magnitude.mean",
                          "Gyro.Rotation.Magnitude.mean", "Gyro.Rotation.Jerk.Magnitude.mean", 
                          "Freq.Domain.X.Body.Acceleration.mean", "Freq.Domain.Y.Body.Acceleration.mean", "Freq.Domain.Z.Body.Acceleration.mean",
                          "Freq.Domain.X.Jerk.Body.Acceleration.mean", "Freq.Domain.Y.Jerk.Body.Acceleration.mean", "Freq.Domain.Z.Jerk.Body.Acceleration.mean",
                          "Freq.Domain.X.Gyro.Rotation.mean", "Freq.Domain.Y.Gyro.Rotation.mean", "Freq.Domain.Z.Gyro.Rotation.mean",
                          "Freq.Domain.Body.Acceleration.Magnitude.mean", "Freq.Domain.Body.Acceleration.Jerk.Magnitude.mean",
                          "Freq.Domain.Gyro.Rotation.Magnitude.mean", "Freq.Domain.Gyro.Rotation.Jerk.Magnitude.mean",
                          
                          "X.Body.Acceleration.std", "Y.Body.Acceleration.std", "Z.Body.Acceleration.std",
                          "X.Gravity.Acceleration.std","Y.Gravity.Acceleration.std", "Z.Gravity.Acceleration.std",
                          "X.Jerk.Body.Acceleration.std", "Y.Jerk.Body.Acceleration.std", "Z.Jerk.Body.Acceleration.std",
                          "X.Gyro.Rotation.std", "Y.Gyro.Rotation.std", "Z.Gyro.Rotation.std",
                          "X.Gyro.Rotation.Jerk.std", "Y.Gyro.Rotation.Jerk.std", "Z.Gyro.Rotation.Jerk.std",
                          "Body.Acceleration.Magnitude.std", "Gravity.Acceleration.Magnitude.std", "Body.Acceleration.Jerk.Magnitude.std",
                          "Gyro.Rotation.Magnitude.std", "Gyro.Rotation.Jerk.Magnitude.std", 
                          "Freq.Domain.X.Body.Acceleration.std", "Freq.Domain.Y.Body.Acceleration.std", "Freq.Domain.Z.Body.Acceleration.std",
                          "Freq.Domain.X.Jerk.Body.Acceleration.std", "Freq.Domain.Y.Jerk.Body.Acceleration.std", "Freq.Domain.Z.Jerk.Body.Acceleration.std",
                          "Freq.Domain.X.Gyro.Rotation.std", "Freq.Domain.Y.Gyro.Rotation.std", "Freq.Domain.Z.Gyro.Rotation.std",
                          "Freq.Domain.Body.Acceleration.Magnitude.std", "Freq.Domain.Body.Acceleration.Jerk.Magnitude.std",
                          "Freq.Domain.Gyro.Rotation.Magnitude.std", "Freq.Domain.Gyro.Rotation.Jerk.Magnitude.std")

# From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

mean_std_data$Activity <- as.factor(mean_std_data$Activity)
avg_data <- mean_std_data[1,]
avg_data[1:6,4:69] <- by_output_df

for (i in 1:30){
  sub_data <- mean_std_data[mean_std_data$Subject.Number == i,]
  train_data <- sub_data[sub_data$Dataset == "train",]
  
  by_output <- by(train_data[,4:69], train_data$Activity, colMeans)
  by_output_df <- do.call(rbind,as.list(by_output))
  
  j <- i*6
  avg_data[j:(j+5),4:69] <- by_output_df
  avg_data$Activity[j:(j+5)] <- row.names(by_output_df)
  avg_data$Subject.Number[j:(j+5)] <- i
  avg_data$Dataset[j:(j+5)] <- "train"
}

for (i in 1:30){
  sub_data <- mean_std_data[mean_std_data$Subject.Number == i,]
  test_data <- sub_data[sub_data$Dataset == "test",]
  
  by_output <- by(test_data[,4:69], test_data$Activity, colMeans)
  by_output_df <- do.call(rbind,as.list(by_output))
  
  j <- (i*6) + dim(avg_data)[1]
  avg_data[j:(j+5),4:69] <- by_output_df
  avg_data$Activity[j:(j+5)] <- row.names(by_output_df)
  avg_data$Subject.Number[j:(j+5)] <- i
  avg_data$Dataset[j:(j+5)] <- "test"
}

write.table(names(avg_data), file="features_updated.txt", row.name = FALSE)
write.table(avg_data, file="tidydata.txt", row.name = FALSE)
