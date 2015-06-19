# Getting and Cleaning Data Course Project
## David Short

## Background
THe project calls for us to create a tidy data set from the the given zip file.  This README contains information on how this was achieved.

The code to create the tidy set is contained in the file
run_analysis.R

It is expected that this file will be contained in your working directory.

## Extraction of Data
The data is extracted from the zip file that can be downloaded from the link given in the course project instructions:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

Once the zip file has been downloaded it should be extracted to your working directory.  That is, it is expected that the top directory in the zip - "UCI HAR Dataset" - will be a subdirectory of the directory in which run_analysis.R resides.

## R Libraries used
The run_analysis.R script makes use of the following libraries:

### LaF
The LaF library is used when reading the train/X_train.txt and test/X_test.txt files to dramatically speed up the loading of these large fixed width files.  The function used from this library is "laf_open_fwf".

### reshape2
The reshape2 library is used when melting and recasting the data to create the grouped means.  The functions used from this library are "melt" and "dcast".

## Data Transformation
The run_analysis.R script performs the following steps to transform the data. All files are specified as paths below the "UCI HAR Dataset" directory.

1. Read the features.txt file.  This gives the column names to be associated with the train/X_train.txt and test/X_test.txt files.
2. Identify the columns that we are interested in.  As per the project instructions these are identified as those columns containing "mean()" or "std()".  Other columns containing the word "mean" without the brackets immediately following(eg meanFreq()) have been determined to be not part of the mean for each measurement.
3. Reformats the column names to contain only valid R column name characters.
4. Reads the activity_labels.txt file.  This file gives the names associated with the activity numbers contained in the train/y_train.txt and test/y_test.txt files.
5. Read the train/X_train.txt, train/subject_train.txt, and train/y_train.txt files and bind them together in a single data frame. The data from the train/X_train.txt is given the column names reformatted in step 3.  The train/subject_train.txt data is given the column heading of "Subject", and the train/y_train.txt is given the column heading of "ActivityID".  This is done within the "get_data_set" function.
6. Repeat step 5 for the test/X_test.txt, test/subject_test.txt and test/y_test.txt files.
7. Bind the train and test data frames together into a data frame called full_set.
8. Add the ActivityName column to the full_set data frame by merging it with the activity_labels created in step 4.
9. Melt the full_set data frame, making a four column data frame with:
	* Subject, 
	* Activity,
	* Variable - contains the measurement column headings identified in step 2.
	* Value - the values for each of those measurements.
10. Recast the data so that there is only one row for each Subject/ActivityName combination.  Each row contains a column for each of the measurements identified in step 2.  The numbers in these columns are the mean values for each of these measurements for that Subject/ActivityName combination.
11. Write the file to "Smartphones_tidy.txt" in your working directory.