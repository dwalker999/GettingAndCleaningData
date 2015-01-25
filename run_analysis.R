# This function creates a data frame from text files included in
# the zip file downloaded from:
#
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# The resultant data frame summarizes the mean and std G values from the sensors for 
# different types of activity for a selection of subjects.


run_analysis <- function() {
        
        message("You may need to change your max.print option to see the entire data frame...")
        
        options(warn=-1)
        library(dplyr)
        
        # Convert project text files to tables
        
        activity_labels<-read.table("activity_labels.txt", sep=" ")
        activity_labels$V1 = as.character(as.numeric(activity_labels$V1))
        features<-read.table("features.txt", sep=" ")
        # 
        data <- read.table("X_test.txt", col.names=as.vector(features$V2))
        subject <- read.table("subject_test.txt", col.names="subject")
        activity <- read.table("y_test.txt", col.names="activity")
        
        # Convert numeric activity values to character
        activity$activity[activity$activity] <- as.character(activity$activity)
       
        # Create a table for the X_test data
        data <- cbind(activity, data)
        
        # Add the subject to the X_test data
        datatest <- cbind(subject, data)
        
        #Create a table the X_train data
        data <- read.table("X_train.txt", col.names=as.vector(features$V2))
        subject <- read.table("subject_train.txt", col.names="subject")
        activity <- read.table("y_train.txt", col.names="activity")
        activity$activity[activity$activity] <- as.character(activity$activity)
        
        data <- cbind(activity, data)
        datatrain <- cbind(subject, data)
        
        # Merge the two tables.
        X <- rbind(datatrain, datatest)
        # Sort the table by subject, activity
        X <- arrange(X, subject, activity)
        
        # Replace activity values with their labels.
        X$activity[X$activity == "1"] <- "WALKING"
        X$activity[X$activity == "2"] <- "WALKING_UPSTAIRS"
        X$activity[X$activity == "3"] <- "WALKING_DOWNSTAIRS"
        X$activity[X$activity == "4"] <- "SITTING"
        X$activity[X$activity == "5"] <- "STANDING"
        X$activity[X$activity == "6"] <- "LAYING"
        
        
        
        # Get only the fields we need based on field names.
        # We only want column names containing 'std' or 'mean'
        x <- grep(".std.", colnames(X), fixed = TRUE )
        y <- grep(".mean.", colnames(X), fixed = TRUE )
        z <- as.numeric(c(x, y))
        z <- sort(z)
        
        # Create a data frame with Subject, Activity and
        # the column numbers from the grep.
        X1 <- X[1:2] # Subject and Activity
        X1 <- cbind(X1, X[z]) # columns containing std or mean
        
        #subtotal by Subject and Activity...
        X2<-aggregate(X1, list(Subject=X1$subject,Activity=X1$activity), FUN=mean)
        X2<-arrange(X2, Subject,Activity)
        # remove uneeded columns..
        X2[3:4] <- list(NULL)
        # Return the summarized data frame
        X2
        
        
}


        