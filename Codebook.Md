# Getting and Cleaning Data Course Project Cadebook


The run_analysis.R contains the script for "tidying" the data of Human Activity Recognition Using Smartphones Dataset Version 1.0. 

The detailed information about the original data can be viewed on 
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones. 

The data set can be downloaded using this link: 
 https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip


The following steps where done to tidy the data:

1. Downloaded the data from original source.
2. Merged the training and the test sets to create one data set.
3. Extracted the measurements on the mean and standard deviation for each measurement.
4. Used descriptive activity names to name the activities in the data set.
5. Labeled the data set with descriptive variable names.
6. Created an independent tidy data set (run_analysis_data.txt) with the average of each variable for each activity and each subject.


The tidy data set contains the following:

subject = subject of experiment (30 subjects)
group = the experiment grouped the subjects into two, train and test
activity = the activity done by the subject (walking, walking upstairs, walking downstairs, sitting, standing, laying)

average (mean) and average standard deviation (std)  for each "activity" of the following "time" variables:

tbodyaccmean        
tbodyaccstd          
tgravityaccmean  
tgravityaccstd     
tbodyaccjerkmean
tbodyaccjerkstd    
tbodygyromean        
tbodygyrostd       
tbodygyrojerkmean
tbodygyrojerkstd   


average (mean) and average standard deviation (std) for each "activity"  of the following "frequency domain signals" variables:

fbodyaccmean      
fbodyaccstd        
fbodybodyaccjerkmean
fbodybodyaccjerkstd
fbodybodygyromean    
fbodybodygyrostd   
fbodybodygyrojerkmean
fbodybodygyrojerkstd




The original data were collected and recoreded by:

Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
