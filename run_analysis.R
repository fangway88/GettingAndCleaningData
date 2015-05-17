# run_analysis.R Description:
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive variable names. 
# 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.


# 1. Merge the training and the test sets to create one data set.

# Set working directory to location of the data files
path <- 'C:/Users/wayfang/Documents/Coursera/Getting and Cleaning Data/UCI HAR Dataset'
setwd(path)

# Read in the data from the files
features        = read.table('./features.txt',header=FALSE)
activity_labels = read.table('./activity_labels.txt',header=FALSE)
subject_train   = read.table('./train/subject_train.txt',header=FALSE)
x_train         = read.table('./train/x_train.txt',header=FALSE)
y_train         = read.table('./train/y_train.txt',header=FALSE)

# Assign column names to the data imported above
colnames(activity_labels)  = c('activityId','activity_labels')
colnames(subject_train)  = "subjectId"
colnames(x_train)        = features[,2]
colnames(y_train)        = "activityId"

# Merge x_train, y_train, subject_train
trainingData = cbind(x_train,y_train,subject_train)

# Read in the test data
subject_test = read.table('./test/subject_test.txt',header=FALSE)
x_test       = read.table('./test/x_test.txt',header=FALSE)
y_test       = read.table('./test/y_test.txt',header=FALSE)

# Assign column names to the test data imported above
colnames(subject_test) = "subjectId"
colnames(x_test)       = features[,2] 
colnames(y_test)       = "activityId"

# Create the final test set by merging the x_test, y_test and subject_test data
testData = cbind(x_test,y_test,subject_test)

# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData)

# Create a vector for the column names from the finalData, which will be used to select the desired mean() & stddev() columns
colNames  = colnames(finalData)

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE]

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activity_labels,by='activityId',all.x=TRUE)

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData)

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassign the new descriptive column names to the finalData set
colnames(finalData) = colNames

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table without the activity_labels column
finalDataNoactivity_labels  = finalData[,names(finalData) != 'activity_labels']

# Summarize the finalDataNoactivity_labels table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoactivity_labels[,names(finalDataNoactivity_labels) != c('activityId','subjectId')],by=list(activityId=finalDataNoactivity_labels$activityId,subjectId = finalDataNoactivity_labels$subjectId),mean)

# Merge the tidyData with activity_labels to include descriptive activity names
tidyData    = merge(tidyData,activity_labels,by='activityId',all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')