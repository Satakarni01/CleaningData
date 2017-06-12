


cleanData <- function(){

  #Download the data 
  if(!file.exists("./data")){
    dir.create("./data")
  }
  
  if(file.exists("./data")){
    urlink <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    if(!file.exists("./data/dataset.zip")){
      download.file(url = urlink, destfile = "./data/dataset.zip",method = "curl")
    }
    unzip(zipfile = "./data/dataset.zip", exdir = "./data")
  }
  
  
  #1. Merges the training and the test sets to create one data set.
    #Read Train DataSests 
  xTrain <- read.table("data/UCI HAR Dataset/train/X_train.txt")
  yTrain <- read.table("data/UCI HAR Dataset/train/y_train.txt")
  subjectTrain <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
  
    #Read Test Datasets
  xTest <- read.table("data/UCI HAR Dataset/test/X_test.txt")
  yTest <- read.table("data/UCI HAR Dataset/test/y_test.txt")
  subjectTest <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
  
    #Merge Datasets 
  dataFeatures <- rbind(xTrain, xTest)
  dataActivity <- rbind(yTrain, yTest)
  dataSubject <- rbind(subjectTrain, subjectTest)
  features <- read.table(file = "data/UCI HAR Dataset/features.txt", header = FALSE, colClasses = "character" )
  
  rm(list = grep("Train|Test", ls(), value = TRUE))
  
 colnames(dataActivity) <- c("activity")
 colnames(dataSubject) <- c("subjectID")
 colnames(dataFeatures) <- features$V2
 
 finalData <- cbind(dataFeatures, dataSubject )
 finalData <- data.frame(cbind(finalData, dataActivity))
  
  #2. Extracts only the measurements on the mean and standard deviation for each measurement.
  
  mean.sd.ls <- grep("mean|std|subjectID|activity", colnames(finalData), value = TRUE)
  finalData <- finalData[, mean.sd.ls]
 
  
  #3. Uses descriptive activity names to name the activities in the data set
  activity_labels <- read.table("data/UCI HAR Dataset/activity_labels.txt")
  Activity.Description <- sapply(finalData$activity, function(i) activity_labels[i,]$V2  )
  finalData$activity <- Activity.Description
  
  #4. Appropriately labels the data set with descriptive variable names.
  colnames(finalData) <- gsub(pattern = "^t", replacement = "Time", colnames(finalData) )
  colnames(finalData) <- gsub(pattern = "Acc", replacement = "Accelerometer", colnames(finalData) )
  colnames(finalData) <- gsub(pattern = "Gyro", replacement = "GyroMeter", colnames(finalData) )
  colnames(finalData) <- gsub(pattern = "Mag", replacement = "Magnitude", colnames(finalData) )
  colnames(finalData) <- gsub(pattern = "^f", replacement = "Frequency", colnames(finalData))
  colnames(finalData) <- gsub(pattern = "BodyBody", replacement = "Body", colnames(finalData) )
  #5. From the data set in step 4, creates a second, independent tidy data set with the average of each 
  #variable for each activity and each subject.
  library(plyr)
  #tidyData2 <- aggregate(finalData, by = list(subjectID, activity), mean)
  tidyData2 <- aggregate(.~subjectID + activity, finalData, mean)
  write.table(tidyData2, file = "data/tidydata2.txt", row.names = FALSE)
  
} 