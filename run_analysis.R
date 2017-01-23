#TEST DATA
#Reading in the three parts of the test data
xtest<-read.table("C:\\Projects\\Coursera\\datacleaning\\Datafiles\\test\\X_test.txt") #data elements
ytest<-read.table("C:\\Projects\\Coursera\\datacleaning\\Datafiles\\test\\y_test.txt",col.names="activityid") #Activity information         
subjecttest<-read.table("C:\\Projects\\Coursera\\datacleaning\\Datafiles\\test\\subject_test.txt",col.name="subjectid") #Identification information
table(subjecttest$subjectid)

#merging the activity, Subject id and data elements
test<-cbind(subjecttest,ytest,xtest)

#Subsetting the data for measures that contain Mean() & sd() - STEP 2
test.sub<-test[,c(1:8,43:48,83:88,123:128,163:168,203,205,216,
                  217,229,230,242,243,255,256,268:273,347:352,
                  426:431,505,506,518,519,531,532,544,545,557:563)]
#flagging the records as coming from TEST subjects
test.sub$group <- "test"

#TRAIN DATA
#Reading in the three parts of the training data
xtrain<-read.table("C:\\Projects\\Coursera\\datacleaning\\Datafiles\\train\\X_train.txt")
ytrain<-read.table("C:\\Projects\\Coursera\\datacleaning\\Datafiles\\train\\y_train.txt",col.names="activityid")                
subjecttrain<-read.table("C:\\Projects\\Coursera\\datacleaning\\Datafiles\\train\\subject_train.txt",col.name="subjectid")


#merging the activity, Subject id and data elements
train<-cbind(subjecttrain,ytrain,xtrain)
names(train)
#Subsetting the data for measures that contain Mean() & sd() - STEP 2
train.sub<-train[,c(1:8,43:48,83:88,123:128,163:168,203,205,216,
                    217,229,230,242,243,255,256,268:273,347:352,
                    426:431,505,506,518,519,531,532,544,545,557:563)]
#flagging the records as coming from TRAINING subjects
train.sub$group <- "train"

#merging the test and training data together - STEP 1
train.test<-rbind(train.sub,test.sub)
names(train.test)
train.test.sort<-train.test[order(train.test$subjectid,train.test$activityid),]
class(train.test.sort)

#Add descriptive labels of Activities and Variables - STEP 4

names(train.test.sort) [3:8]<-c("tBodyAcc-mean()-X","tBodyAcc-mean()-Y","tBodyAcc-mean()-Z",
                                 "tBodyAcc-std()-X", "tBodyAcc-std()-Y","tBodyAcc-std()-Z")
                                
names(train.test.sort) [9:14]<-c("tGravityAcc-mean()-X","tGravityAcc-mean()-Y","tGravityAcc-mean()-Z",
                                  "tGravityAcc-std()-X", "tGravityAcc-std()-Y","tGravityAcc-std()-Z")

names(train.test.sort) [15:20]<-c("tBodyAccJerk-mean()-X","tBodyAccJerk-mean()-Y","tBodyAccJerk-mean()-Z",
                                  "tBodyAccJerk-std()-X","tBodyAccJerk-std()-Y", "tBodyAccJerk-std()-Z")
                                  
names(train.test.sort) [21:26]<-c("tBodyGyro-mean()-X", "tBodyGyro-mean()-Y", "tBodyGyro-mean()-Z",
                                  "tBodyGyro-std()-X","tBodyGyro-std()-Y","tBodyGyro-std()-Z") 

names(train.test.sort) [27:32]<-c("tBodyGyroJerk-mean()-X","tBodyGyroJerk-mean()-Y", "tBodyGyroJerk-mean()-Z",
                                  "tBodyGyroJerk-std()-X","tBodyGyroJerk-std()-Y", "tBodyGyroJerk-std()-Z")

names(train.test.sort) [33:42]<-c("tBodyAccMag-mean()","tBodyAccMag-std()","tGravityAccMag-mean()",
                                "tGravityAccMag-std()","tBodyAccJerkMag-mean()","tBodyAccJerkMag-std()",
                                "tBodyGyroMag-mean()", "tBodyGyroMag-std()","tBodyGyroJerkMag-mean()",
                                "tBodyGyroJerkMag-std()")

names(train.test.sort) [43:48]<-c("fBodyAcc-mean()-X","fBodyAcc-mean()-Y","fBodyAcc-mean()-Z",
                                  "fBodyAcc-std()-X","fBodyAcc-std()-Y","fBodyAcc-std()-Z")

names(train.test.sort) [49:54]<-c("fBodyAccJerk-mean()-X","fBodyAccJerk-mean()-Y", "fBodyAccJerk-mean()-Z",
                                  "fBodyAccJerk-std()-X","fBodyAccJerk-std()-Y", "fBodyAccJerk-std()-Z")

names(train.test.sort) [55:68]<-c("fBodyGyro-mean()-X", "fBodyGyro-mean()-Y","fBodyGyro-mean()-Z",
                                  "fBodyGyro-std()-X","fBodyGyro-std()-Y","fBodyGyro-std()-Z","fBodyAccMag-mean()",
                                  "fBodyAccMag-std()","fBodyBodyAccJerkMag-mean()", "fBodyBodyAccJerkMag-std()",
                                  "fBodyBodyGyroMag-mean()", "fBodyBodyGyroMag-std()","fBodyBodyGyroJerkMag-mean()",
                                  "fBodyBodyGyroJerkMag-std()")
names(train.test.sort) [69:75]<-c("angle(tBodyAccMean,gravity)","angle(tBodyAccJerkMean),gravityMean)",
                                  "angle(tBodyGyroMean,gravityMean)", "angle(tBodyGyroJerkMean,gravityMean)",
                                  "angle(X,gravityMean)", "angle(Y,gravityMean)", "angle(Z,gravityMean)")

names(train.test.sort)

#ACTIVITIES LABELS STEP 3
#1 WALKING
#2 WALKING_UPSTAIRS
#3 WALKING_DOWNSTAIRS
#4 SITTING
#5 STANDING
#6 LAYING
class(train.test.sort$activityid)
train.test.sort$activ_label[train.test.sort$activityid==1]<-"WALKING"
train.test.sort$activ_label[train.test.sort$activityid==2]<-"WALKING_UPSTAIRS"
train.test.sort$activ_label[train.test.sort$activityid==3]<-"WALKING_DOWNSTAIRS"
train.test.sort$activ_label[train.test.sort$activityid==4]<-"SITTING"
train.test.sort$activ_label[train.test.sort$activityid==5]<-"STANDING"
train.test.sort$activ_label[train.test.sort$activityid==6]<-"LAYING"

names(train.test.sort)

#From the data set in step 4, creates a second,
#independent tidy data set with the average of each variable for each activity and each subject  STEP 5
class(train.test.sort$subjectid)
train.test.summary<-aggregate(train.test.sort, by=list(train.test.sort$subjectid,train.test.sort$activityid),FUN=mean)
names(train.test.summary)
train.test.summary<-train.test.summary[,1:77]
