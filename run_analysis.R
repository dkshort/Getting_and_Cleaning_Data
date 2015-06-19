get_data_set <- function(data_type, subdir, width_vector, types_vector, col_names, wanted_cols) {
    ## Get the full data set from either the test or train directories
    
    source_directory <- paste0(subdir, "/", data_type, "/")
    
    x_set <- laf_open_fwf(paste0(source_directory, "X_", data_type, ".txt"), 
                          column_widths=width_vector,
                          column_types=types_vector, 
                          column_names=col_names)
    
    
    subject_set <- read.table(paste0(source_directory, "subject_", data_type, ".txt"),
                              col.names="Subject")
    
    y_set <- read.table(paste0(source_directory, "y_", data_type, ".txt"),
                        col.names="ActivityID")
    
    return(cbind(subject_set,y_set,x_set[,wanted_cols]))
    
}

run_analysis <- function() {
    library(LaF)
    library(reshape2)
    
    subdir <- "UCI HAR Dataset"
    
    ## Load the names of the columns from features.txt
    features <- read.table(paste0(subdir,"/features.txt"), 
                           sep=" ", stringsAsFactors=FALSE)
    col_names <- features[,2]
    
    ## We only want to keep those columns with mean() or std() in their heading
    wanted_cols <- sapply(col_names,function(x) grepl("mean\\(\\)|std\\(\\)",x))

    col_names <- gsub("-","_",col_names)
    col_names <- gsub("\\(\\)","",col_names)
    
    width_vector = rep(16,length(col_names))
    types_vector = rep("double",length(col_names))
    
    activity_labels <- read.table(paste0(subdir,"/activity_labels.txt"), sep=" ", 
                                  col.names=c("ActivityID","ActivityName"))

    ## For each of the training and test data, merge the x, y and subject files into
    ## a single data frame
    train_set <- get_data_set("train", subdir, width_vector, types_vector, col_names, wanted_cols)
    test_set <- get_data_set("test", subdir, width_vector, types_vector, col_names, wanted_cols)
  
    ## Merge the training and test data frames into a single full data frame, and adds the ActivityName column.
    full_set <- rbind(train_set,test_set)
    full_set <- merge(full_set,activity_labels,sort=FALSE)
    
    ## Reshape the data to contain means for each subject/activity combination.
    melt_set <- melt(full_set,id=c("Subject","ActivityName"),measure.vars=col_names[wanted_cols])
    mean_set <- dcast(melt_set, Subject + ActivityName ~ variable, mean)
    
    ## Write the data out to the working directory
    write.table(mean_set,"Smartphones_tidy.txt",row.names=FALSE)
    return("Data written to 'Smartphones_tidy.txt' in your working directory.")
}