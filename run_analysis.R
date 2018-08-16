#set the working directory
setwd("/home/akshay/Downloads/UCI HAR Dataset");

#load the features
features <- read.table("./features.txt",header = F);

#load the activity labels and assign column names
activitylabels <- read.table("./activity_labels.txt",header = F);
colnames(activitylabels) <- c("activityid","activityname");

#load the training data
trainsubject=read.table("./train/subject_train.txt",header=F);
colnames(trainsubject) <- "subjectid";
xtrain <- read.table("./train/X_train.txt",header=F);
colnames(xtrain) <- features[,2];
ytrain <- read.table("./train/y_train.txt",header=F);
colnames(ytrain) <- "activityid";
mergedTrain <- cbind(trainsubject,xtrain,ytrain);

#load testing data
testsubject=read.table("./test/subject_test.txt",header=F);
colnames(testsubject) <- "subjectid";
xtest <- read.table("./test/X_test.txt",header=F);
colnames(xtest) <- features[,2];
ytest <- read.table("./test/y_test.txt",header=F);
colnames(ytest) <- "activityid";
mergedTest <- cbind(testsubject,xtest,ytest);

mergedData <- rbind(mergedTrain,mergedTest);
columns <- colnames(mergedData);

vector <- (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) &
                        !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | 
                        grepl("-std..",columns) & !grepl("-std()..-",columns));
mergedData <- mergedData[vector==T];
mergedData <- merge(mergedData,activitylabels, by="activityid",all.x=T);
columns <- colnames(mergedData);
for (i in 1:length(columns)) 
{
  columns[i] <- gsub("\\()","",columns[i])
  columns[i] <- gsub("-std$","StdDev",columns[i])
  columns[i] <- gsub("-mean","Mean",columns[i])
  columns[i] <- gsub("^(t)","time",columns[i])
  columns[i] <- gsub("^(f)","freq",columns[i])
  columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
  columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
  columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
  columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
  columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
  columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
  columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
};

colnames(mergedData) <- columns;

mergedData <- mergedData[,names(mergedData)!="activityType"]

tidydata <- aggregate(mergedData[,names(mergedData)!=c("activityid","subjectid")],
                      by=list(mergedData$activityid,mergedData$subjectid),mean);

write.table(tidyData, './FinalTidyData.txt',row.names=FALSE,sep='\t')