library(plyr)
library(dplyr)
library(tibble)
library(rlang)

#read in the training tables
train <- read.table("train/X_train.txt")
train_lbl <- read.table("train/y_train.txt")
train_subject <-read.table("train/subject_train.txt")

#read in the test tables
test <- read.table("test/X_test.txt")
test_lbl <- read.table("test/y_test.txt")
test_subject <-read.table("test/subject_test.txt")

#read in the features and activity labels
activity_lbl <- read.table("activity_labels.txt")
features <- read.table("features.txt")

#1.Merge the training and the test sets to create one data set
merged_ds <- bind_rows(train,test)
merged_lbl <- bind_rows(train_lbl, test_lbl)
merged_subject <- bind_rows(train_subject, test_subject)

#2. Extract only the measurements on the mean and standard deviation for each measurement.
mean_std_features <- grep("mean[^Freq]()|std[^Freq]()", features$V2)
sub_merged_ds <- select(merged_ds, mean_std_features)

#3. Use descriptive activity names to name the activities in the data set
merged_lbl <- rename(merged_lbl, c("V1" = "lbl_id"))
activity_lbl <- rename(activity_lbl, c("V1" = "lbl_id", "V2" = "lbl_desc"))
desc_merged_lbl <- join(activity_lbl, merged_lbl, by = "lbl_id")
sub_merged_ds <- cbind(activity = desc_merged_lbl$lbl_desc, sub_merged_ds)

#4. Appropriately label the data set with descriptive variable names
non_desc_names <- names(sub_merged_ds)
desc_names <- features[mean_std_features,]
desc_names$V2 <- as.character(desc_names$V2)
desc_names <- desc_names$V2
desc_names <- prepend(desc_names, "activity")
names(sub_merged_ds) <- desc_names

#5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
sub_merged_ds <- cbind(subject = merged_subject$V1, sub_merged_ds)
ncol <- ncol(sub_merged_ds)
summary_set <- sub_merged_ds %>% 
                  group_by(activity, subject) %>% 
                  summarise_all(.vars = letters[3:ncol], .funs = c(mean="mean"))

#write the new data set to a txt file
write.table(summary_set, "summarized_data.txt", row.name=FALSE)


