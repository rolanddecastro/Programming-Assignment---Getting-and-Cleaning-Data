#the following codes will create a directory called "ucidata",
#will download the data set and unzip in "ucidata" directory under
#the R default directory
#it will also load required libraries

fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!dir.exists("./ucidata")){dir.create("./ucidata")}
download.file(fileurl, destfile = "./ucidata/ucidataset.zip")
unzip("./ucidata/ucidataset.zip", exdir = "./ucidata")

library(dplyr)
library(tidyr)

#the following codes will read the data and assign those to variables

features <- read.table("./ucidata/UCI HAR Dataset/features.txt")

subject_test <- read.table("./ucidata/UCI HAR Dataset/test/subject_test.txt")
y_test <- read.table("./ucidata/UCI HAR Dataset/test/y_test.txt")
X_test <- read.table("./ucidata/UCI HAR Dataset/test/X_test.txt")

subject_train <- read.table("./ucidata/UCI HAR Dataset/train/subject_train.txt")
y_train <- read.table("./ucidata/UCI HAR Dataset/train/y_train.txt")
X_train <- read.table("./ucidata/UCI HAR Dataset/train/X_train.txt")



#the following codes will merge the training and the test sets 
#to create one data set.

colnames(subject_test) <- "subject"
colnames(y_test) <- "activity"


merged_test_data <- cbind(subject_test, y_test, X_test)
merged_test_data <- merged_test_data %>% mutate(group = "test")

colnames(subject_train) <- "subject"
colnames(y_train) <- "activity"

merged_train_data <- cbind(subject_train, y_train, X_train)
merged_train_data <- merged_train_data %>% mutate(group = "train")


merged_data <- merge(merged_test_data, merged_train_data, all = TRUE) %>% 
    select(subject, group, activity, V1:V561)
columnnames <- c("subject", "group", "activity", as.vector(features$V2))
colnames(merged_data) <- columnnames

#the following codes will extract the measurements on the mean 
#and standard deviation for each measurement. 

meanstd <- merged_data[,grep("subject|group|activity|Mag-mean[^Freq]|Mag-std", names(merged_data))]


#the following codes will use descriptive activity names to name 
#the activities in the data set


meanstd <- mutate(meanstd, activity = factor(meanstd$activity,
                labels = c("walking","walking upstairs","walking downstairs", 
                           "sitting","standing","laying")))

#Appropriately labels the data set with descriptive variable names. 

names(meanstd) <- gsub("Mag","",names(meanstd))
names(meanstd) <- tolower(names(meanstd)) 
names(meanstd) <- gsub("-","",names(meanstd))
names(meanstd) <- gsub("\\()","",names(meanstd))


#From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable 
#for each activity and each subject.


clean_data <- group_by(meanstd, subject, group, activity) %>%
    summarize(
        tbodyaccmean = mean(tbodyaccmean),         
        tbodyaccstd = mean(tbodyaccstd), tgravityaccmean = mean(tgravityaccmean),     
        tgravityaccstd = mean(tgravityaccstd), tbodyaccjerkmean = mean(tbodyaccjerkmean),    
        tbodyaccjerkstd = mean(tbodyaccjerkstd), tbodygyromean = mean(tbodygyromean),      
        tbodygyrostd = mean(tbodygyrostd), tbodygyrojerkmean = mean(tbodygyrojerkmean),   
        tbodygyrojerkstd = mean(tbodygyrojerkstd),fbodyaccmean = mean(fbodyaccmean),        
        fbodyaccstd = mean(fbodyaccstd), fbodybodyaccjerkmean = mean(fbodybodyaccjerkmean),
        fbodybodyaccjerkstd = mean(fbodybodyaccjerkstd), fbodybodygyromean = mean(fbodybodygyromean),   
        fbodybodygyrostd = mean(fbodybodygyrostd), fbodybodygyrojerkmean = mean(fbodybodygyrojerkmean),
        fbodybodygyrojerkstd = mean(fbodybodygyrojerkstd) 
    )

print(clean_data)
write.csv(clean_data, file = "./run_analysis_data.csv")








