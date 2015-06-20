# The "run_analysis.R" is a R script that does the following:
# 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.


#####################################
## by newja1390 
###############

### load required libraries
library(data.table)


### ==============================================================
### load data
### ==============

### Read training data
# - 'train/X_train.txt': Training set.
# - 'train/y_train.txt': Training labels.
# - 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample

trainingSet <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
trainingLabels <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
trainingSubject <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)

### Read test data
# - 'test/X_test.txt': Test set.
# - 'test/y_test.txt': Test labels.
# - 'train/subject_test.txt': Each row identifies the subject who performed the activity for each window sample

testSet <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
testLabels <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)

### ==============================================================
### 1. Merge the training and the test sets to create one data set
### ==============

data <- rbind(trainingSet, testSet)
labels <- rbind(trainingLabels, testLabels)
subject <- rbind(trainingSubject, testSubject)

### ==============================================================
### 2. Extracts only the measurements on the mean and standard deviation for each measurement.
### ==============

# read list of all features
features <- read.table("UCI HAR Dataset/features.txt")

# find mean and standard deviation features indices
extracted_features_indices <- grep("-mean()|-std()", features[, 2])

# extracts only the appropriate features for each measurement 
data <- data[, extracted_features_indices]

# set the names of extracted columns
names(data) <- features[extracted_features_indices, 2]

### ==============================================================
### 3. Use descriptive activity names to name the activities in the data set.
### ==============

# read activity labels
activities <- read.table("UCI HAR Dataset/activity_labels.txt")

#remove '_' from activities name
activities[, 2] = gsub("_", "", activities[, 2])

# set descriptive activity names
labels[,1] = activities[labels[,1], 2]

### ==============================================================
### 4. Appropriately label the data set with descriptive activity names.
### ==============

names(subject) <- "subject"
names(labels) <- "activity"

# bind  subject, label and extracted features to get complete data set
dataSet <- cbind(subject, labels, data)

### ==============================================================
### 5. From the data set in step 4, create a second, independent tidy data set 
###    with the average of each variable for each activity and each subject.
### ==============

library (reshape2)
idVars <- c("subject", "activity")
measureVariables <- setdiff(colnames(dataSet), idVars)
dataMelt <- melt(dataSet, id = idVars, measure.vars = measureVariables)

# use 'dcast' function in 'reshape2' package to calculate mean of each variable
tidyData <- dcast(dataMelt,subject + activity ~ variable,mean)

# write the tidy data set in a text file
write.table(tidyData, file = "tidyData.txt", row.name=FALSE)

