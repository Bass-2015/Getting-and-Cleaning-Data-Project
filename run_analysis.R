# Merging the Training and the Test datasets to create one dataset:

setwd("./UCI HAR Dataset")
trainSet <- read.table("./train/X_train.txt")
trainLabels <- read.table("./train/y_train.txt")
table(trainLabels)
trainSubjects <- read.table("./train/subject_train.txt")

testSet <- read.table("./test/X_test.txt")
testLabels <- read.table("./test/y_test.txt")
table(testLabels)
testSubjects <- read.table("./test/subject_test.txt")

mergedSets <- rbind(trainSet, testSet)
mergedLabels <- rbind(trainLabels, testLabels)
mergedSubjects <- rbind(trainSubjects, testSubjects)
dim(mergedSubjects)

# Extracts only the Mean and the Standard Deviation of each measurement:
features <- read.table("./features.txt")
meanstd <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) # 66
mergedSets <- mergedSets[, meanstd]
dim(mergedSets) # 10299*66
names(mergedSets) <- gsub("\\(\\)", "", features[meanstd, 2]) 
names(mergedSets) <- gsub("mean", "Mean", names(mergedSets))
names(mergedSets) <- gsub("std", "Std", names(mergedSets))
names(mergedSets) <- gsub("-", "", names(mergedSets))


# Uses descriptive activity names to name the activities in the dataset
activity <- read.table("./activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[mergedLabels[, 1], 2]
mergedLabels[, 1] <- activityLabel
names(mergedLabels) <- "activity"

# Labels the dataset with descriptive variable names:
names(mergedSubjects) <- "subject"
descriptiveSet <- cbind(mergedSubjects, mergedLabels, mergedSets)
write.table(descriptiveSet, "merged-original-set.txt") 

# Creates a second dataset, from the original dataset, that is a tidy 
# dataset with the average of each variable for each Activity and each Subject:
subjectLengt <- length(table(mergedSubjects)) # 30
activityLengt <- dim(activity)[1] # 6
columnLengt <- dim(descriptiveSet)[2]
averagesData <- matrix(NA, nrow=subjectLengt*activityLengt, ncol=columnLengt)
averagesData <- as.data.frame(averagesData)
colnames(averagesData) <- colnames(descriptiveSet)
row <- 1
for(i in 1:subjectLengt) {
  for(j in 1:activityLengt) {
    averagesData[row, 1] <- sort(unique(mergedSubjects)[, 1])[i]
    averagesData[row, 2] <- activity[j, 2]
    loop1 <- i == descriptiveSet$subject
    loop2 <- activity[j, 2] == descriptiveSet$activity
    averagesData[row, 3:columnLengt] <- colMeans(descriptiveSet[loop1&loop2, 3:columnLengt])
    row <- row + 1
  }
}
head(averagesData)
write.table(averagesData, "averages-data.txt")

showdata <- read.table("./averages-data.txt")
showdata[1:12, 1:6]
