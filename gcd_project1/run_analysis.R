# Coursera & John Hopkins Bloomberg School of Public Health - Getting and Cleaning Data 2015 Project
# This R script gets and clears some human activity data, based on daily activity while using smartphones.
# Full dataset description -> http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

require(plyr)

download_data = function() {
    "Downloading and unpacking datafile"
    # file variables
    file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
	file_dest <- "data/UCI_HAR_data.zip"
	file_dir <- "data/UCI HAR Dataset"
	# creat dir if not exist
    if (!file.exists("data")) {
        message("Creating data directory...")
        dir.create("data")
    }
    # download data if not exist
    if (!file.exists(file_dir)) {
        message("Downloading data...")
        download.file(url=file_url, destfile=file_dest)
        unzip(file_dest, exdir="data")
    }
}

merge_data = function() {
    "Merges the training and the test sets to create one data set."
    # reading
    message("Reading datasets...")
    x_train <- read.table("data/UCI HAR Dataset/train/X_train.txt")
    y_train <- read.table("data/UCI HAR Dataset/train/y_train.txt")
    subject_train <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
    x_test <- read.table("data/UCI HAR Dataset/test/X_test.txt")
    y_test <- read.table("data/UCI HAR Dataset/test/y_test.txt")
    subject_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
    # merging
    message("Merging datasets...")
    x_merged <- rbind(x_train, x_test)
    y_merged <- rbind(y_train, y_test)
    subject_merged <- rbind(subject_train, subject_test)
    # return result
    list(x=x_merged, y=y_merged, subject=subject_merged)
}

mean_and_std = function(df) {
	"Extracts only the measurements on the mean and standard deviation for each measurement."
    features <- read.table("data/UCI HAR Dataset/features.txt")
    # extract mean an standard deviation
    col_mean <- sapply(features[,2], function(x) grepl("mean()", x, fixed=T))
    col_std <- sapply(features[,2], function(x) grepl("std()", x, fixed=T))
    # creating extracted df
    new_df <- df[, (col_mean | col_std)]
    colnames(new_df) <- features[(col_mean | col_std), 2]
    # return extracted df
    new_df
}

activities = function(df) {
    "Appropriately labels the data set with descriptive variable names. "
    colnames(df) <- "activity"
    # process naming
    df$activity[df$activity == 1] = "WALKING"
    df$activity[df$activity == 2] = "WALKING_UPSTAIRS"
    df$activity[df$activity == 3] = "WALKING_DOWNSTAIRS"
    df$activity[df$activity == 4] = "SITTING"
    df$activity[df$activity == 5] = "STANDING"
    df$activity[df$activity == 6] = "LAYING"
    # return df
    df
}

tidy_dataset = function(df) {
    "Creates a second, independent tidy data set with the average of each variable for each activity and each subject."
    tidy <- ddply(df, .(subject, activity), function(x) colMeans(x[,1:60]))
    tidy
}

clean_data = function() {
	"Main clear data function."
    download_data()
    # merging
    merged <- merge_data()
    # clearing
    x_clear <- mean_and_std(merged$x)
    y_clear <- activities(merged$y)
    colnames(merged$subject) <- c("subject")
    combined <- cbind(x_clear, y_clear, merged$subject)
    # tidying
    tidy <- tidy_dataset(combined)
    # writing in file
    write.table(tidy, "tidy.txt", row.names=FALSE)
}