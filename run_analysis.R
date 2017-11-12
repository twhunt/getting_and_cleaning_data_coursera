run_analysis <- function() {
  
  dataset_path <- "Dataset.zip"
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                dataset_path,
                method = "curl")
  unzip(dataset_path)
  
  # Read type of activity associated with a row of data.
  ACTIVITY_TYPE_NAME = "activity_type"
  activity_data <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE, colClasses = c("integer"))
  activity_data <- rbind(activity_data, read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE, colClasses = c("integer")))
  activity_data <- data.frame(lapply(activity_data, ind2activitylabel))
  #colnames(activity_data) <- c(ACTIVITY_TYPE_NAME)
  
  # Read identity of subject who performed each action that produced a row of data.
  SUBJECT_ID_NAME = "subject_id"
  subject_data <- read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE, colClasses = c("integer"))
  subject_data <- rbind(subject_data, read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE, colClasses = c("integer")))
  #colnames(subject_data) <- c(SUBJECT_ID_NAME)
  
  # Read training and test data from data source, and then combine into single data frame.
  train_test_data <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE, colClasses = c("numeric"))
  train_test_data <- rbind(train_test_data, read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE, colClasses = c("numeric")))
  
  # Assign names to columns of data frame that holds combined data sets
  all_col_names <- read.delim("UCI HAR Dataset/features.txt", colClasses = c("NULL", "character"), sep = " ", header=FALSE)[,1]
  
  # "Tidy" data set containing the subject, activity type, and activity data.
  #train_test_data <- cbind(subject_data, activity_data, train_test_data)
  #colnames(train_test_data) <- c(SUBJECT_ID_NAME, ACTIVITY_TYPE_NAME, all_col_names)
  
  # "Tidy" data set containing the subject, activity type, and activity data containing the mean and standard deviation
  mean_std_col_indices <- grep("mean|std", all_col_names)
  reduced_data <- cbind(subject_data, activity_data, train_test_data[, mean_std_col_indices])
  colnames(reduced_data) <- c(SUBJECT_ID_NAME, ACTIVITY_TYPE_NAME, all_col_names[mean_std_col_indices])
  
  combined_data <- cbind(subject_data, activity_data, train_test_data)
  colnames(combined_data) <- c(SUBJECT_ID_NAME, ACTIVITY_TYPE_NAME, all_col_names)
  
  NUM_SUBJECTS = 30
  summary_table <- matrix(nrow=length(activity_labels)*NUM_SUBJECTS, ncol=2+length(mean_std_col_indices))
  colnames(summary_table) <- names(reduced_data)
  row_ind = 1
  for (activity in activity_labels) {
    for (subject in seq(1, NUM_SUBJECTS)) {
      row_inds <- reduced_data[, ACTIVITY_TYPE_NAME] == activity & reduced_data[, SUBJECT_ID_NAME] == subject
      set_of_interest = reduced_data[row_inds,]
      means <- colMeans(set_of_interest[, 3:dim(set_of_interest)[2]])
      summary_table[row_ind, ] <- c(subject, activity, means)
      row_ind = row_ind + 1
      #print(summary_table)
    }
  }
  
  return(data.frame(summary_table))
}

ind2activitylabel <-function(index) {
  # Helper for converting the id of an activity to a description of the activity.
  return(activity_labels[index])
}

# Ohnoes, a global variable!
activity_labels <- c("walking", "walking_upstairs", "walking_downstairs", "sitting", "standing", "laying")
