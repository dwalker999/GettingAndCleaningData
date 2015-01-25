# GettingAndCleaningData
Course project for the Getting and Cleaning Data Course.

This project summarizes the data from the Human Activity Recognition Using Smartphones Dataset project
from Smartlab - Non Linear Complex Systems Laboratory
DITEN - Universit‡ degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws

The summary file was create from the following text files obtained from the project:


- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.


The X_train and X_test sets contain columns for each of many sensors measuring G forces on a smart phone.
These data sets are combined, including only values containing the mean or std measurements. 

The Activity_Labels text file contains a number from 1 - 6 representing the following activitys for which the measurements 
were recorded:

    1 WALKING
    2 WALKING_UPSTAIRS
    3 WALKING_DOWNSTAIRS
    4 SITTING
    5 STANDING
    6 LAYING

The label is appended to the data set instead of the number for easier reading.

The summary file contains the mean of the mean and std columns, totaled by subject and activity.

The file may be generated by running the run_analysis function from the run_analysis R script.


