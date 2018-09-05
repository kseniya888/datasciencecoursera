<b>Loading Data</b>

The run_analysis.R script loads all fo the test and train files of the UCI HAR Dataset as well as its activity labels and features files

<b>Merging Data</b>

The training and test data is merged together.  The associated label and and subject training and test data is merged together as well and stored in separate variables.

<b>Extracting a Subset</b>

A subset of the merged data is created by extracting the mean and standard deviation variables for each measurement.  This is done by seraching the features data and extracting only those variable names that have "mean" and "std" in the name, but don't have any other words such as "Freq" included.
The appropriate columns are then extracted from the merged data set.

<b>Describing Activities</b>

Descriptive activity names are added to the merged dataset by matching the activity ID of the main data set with the activity name in the label data set.

<b>Creating a new data set with averages</b>

A new data set was created with the average of each variable for each activity and each subject.  This was done by grouping the previous data set on activity and subject and then using the "summarise_all" function to compute the mean for each of the speicified variables.
