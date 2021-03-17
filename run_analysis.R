library(dplyr)
library(tidyr)

setwd("C:/Users/Fitzgerald_J/Documents")
#Path="C:/Users/Fitzgerald_J/Documents"

## Use of the from the URL below  in publications must be acknowledged by referencing the following publication [1]: 
## [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
## This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.
## Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.
## file_URL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(file_URL,destfile=Path)

#########################################################
# 0. Initial Steps - READ IN DATA.                      #
#########################################################
  
#Read in test datasets.
x_test<-read.table("X_test.txt")
y_test<-read.table("y_test.txt")
sub_test<-read.table("subject_test.txt")

#Read in trainig datasets
x_train<-read.table("X_train.txt")
y_train<-read.table("y_train.txt")
sub_train<-read.table("subject_train.txt")

#Read in activity labels
activity_labels<-read.table("activity_labels.txt")

#Read in features/columns/variables
features<-read.table("features.txt")

###########################################################################
# Step 1. - Merge the training and test datasets to create one dataset.   #
###########################################################################

Test_DF<-cbind(sub_test,y_test,x_test)
Training_DF<-cbind(sub_train,y_train,x_train)
All_Activity_Data<-rbind(Training_DF,Test_DF)

#Add column names to dataset.
Var_Names=data.frame(Var_Names=c("Subject_ID", "Activity_ID"))
colnames(All_Activity_Data)[1:2]<-as.character(Var_Names[,1])
colnames(All_Activity_Data)[3:563]<-as.character(features[,2])
colnames(activity_labels)[1:2]<-as.character(c("Activity_ID", "Activity_Desc"))

###############################################################################
# Step 2. - Extract only the mean and standard deviation for each measurement.#
###############################################################################

All_Activity_Data<-All_Activity_Data[,grepl("Subject_ID|Activity_ID|mean|std",colnames(All_Activity_Data))]

####################################################################################
# Step 3. - Add descriptive activity names to the activities in the data set.      #
####################################################################################

#All_Activity_Data<-inner_join(activity_labels,All_Activity_Data,by="Activity_ID")
All_Activity_Data$Activity_ID<-factor(All_Activity_Data$Activity_ID,levels=activity_labels$Activity_ID,labels=activity_labels$Activity_Desc)

###################################################################################
# Step 4. Label the dataset with descriptive names.                               #
###################################################################################
colnames(All_Activity_Data)

names(All_Activity_Data)<-gsub("-", " ", names(All_Activity_Data))
names(All_Activity_Data)<-gsub("std", "Standard Deviation", names(All_Activity_Data))
names(All_Activity_Data)<-gsub("mean", "Mean ", names(All_Activity_Data))
names(All_Activity_Data)<-gsub("Acc", " Accelerometer", names(All_Activity_Data))
names(All_Activity_Data)<-gsub("Gyro", " Gyroscope", names(All_Activity_Data))
names(All_Activity_Data)<-gsub("Mag", " Magnitude ", names(All_Activity_Data))
names(All_Activity_Data)<-gsub("AccelerometerJerk", " Accelerometer Jerk", names(All_Activity_Data))
names(All_Activity_Data)<-gsub("GyroscopeJerk", " Gyroscope Jerk", names(All_Activity_Data))
names(All_Activity_Data)<-gsub("BodyBody", "Body", names(All_Activity_Data))
names(All_Activity_Data)<-gsub("^f", "Frequency ", names(All_Activity_Data))
names(All_Activity_Data)<-gsub("^t", "Time ", names(All_Activity_Data))

#####################################################################################
# Step 5. Create tidy data from All_Activity_Data with the average of each variable #
#         for each activity and each subject.                                       #
#####################################################################################
All_Activity_Data_TidyDataset<-
  All_Activity_Data %>%
    group_by(Activity_ID,Subject_ID) %>%
    summarise_each(funs(mean))

#Write summary dataset/tidy dataset to text file.
write.table(All_Activity_Data_TidyDataset,"Run_Analysis_Tidy_Dataset.txt",row.names=FALSE)
####################################### END R CODE ###########################################
