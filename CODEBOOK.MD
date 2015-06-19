# CodeBook for Getting and Cleaning Data Course Project
## David Short

## Background
This code book describes the information held in the Smartphones_tidy.txt file.  This file was created by the run_analysis.R script over files contained within the zip file downloaded from
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The features_info.txt file within that zip contains some information about the original data:

> The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
> 
> Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
> 
> Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
> 
> These signals were used to estimate variables of the feature vector for each pattern:  
> '-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
> 
> tBodyAcc-XYZ
> tGravityAcc-XYZ
> tBodyAccJerk-XYZ
> tBodyGyro-XYZ
> tBodyGyroJerk-XYZ
> tBodyAccMag
> tGravityAccMag
> tBodyAccJerkMag
> tBodyGyroMag
> tBodyGyroJerkMag
> fBodyAcc-XYZ
> fBodyAccJerk-XYZ
> fBodyGyro-XYZ
> fBodyAccMag
> fBodyAccJerkMag
> fBodyGyroMag
> fBodyGyroJerkMag

Subsequently they calculated a number of variables.  From these we extracted only those relating to the mean and standard deviation.

We then calculated the average of each if the variables for each combination of subject and activity.  It is this data that is contained within the Smartphones_tidy.txt file.

## Columns

The Smartphones_tidy.txt file contains the following columns

### Subject
This is a numeric ID used to identify the subjects that conducted the test.  It is an integer in the range 1 to 30.

### ActivityName
The name of the activity being conducted when the measurement was taken.  It is a character string containing one of six possible values:
WALKING
WALKING_UPSTAIRS
WALKING_DOWNSTAIRS
SITTING
STANDING
LAYING

### Measurement columns
The remaining columns contain the mean of either the mean or standard deviation of the measurements taken for each feature for the associated subject and activity.  These columns contain floating point numbers that may be either positive or negative.  The columns are in the form:

FeatureName_CalculationType[_Axis] 

The parts of the column name are defined as follows:

#### FeatureName
This is the phone feature that was measured.  It may have one of the following values:
* tBodyAcc
* tGravityAcc
* tBodyAccJer
* tBodyGyro
* tBodyGyroJerk
* tBodyAccMag
* tGravityAccMag
* tBodyAccJerkMag
* tBodyGyroMag
* tBodyGyroJerkMag
* fBodyAcc
* fBodyAccJerk
* fBodyGyro
* fBodyAccMag
* fBodyAccJerkMag
* fBodyGyroMag
* fBodyGyroJerkMag

See the above quote from the original data for the meaning of these Feature Names.

#### CalculationType
This is the type of calculation we are summarising.  It will be either
* mean - Mean Value; or
* std - Standard Deviation

#### Axis
The axis on which the original measurement was taken.  It may be either X, Y or Z.  Axis coding is only application to the following features:
* tBodyAcc
* tGravityAcc
* tBodyAccJer
* tBodyGyro
* tBodyGyroJerk
* fBodyAcc
* fBodyAccJerk
* fBodyGyro
