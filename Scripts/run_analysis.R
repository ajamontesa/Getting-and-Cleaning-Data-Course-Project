## Load the tidyverse package
## This contains dply, lubridate, etc.
library(tidyverse)

## Download the project data
projecturl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

download.file(projecturl,
              destfile = "./Data/projectdata.zip")

unzip("./Data/projectdata.zip", exdir = "./Data")
list.dirs("./Data")
list.files("./Data/UCI HAR Dataset", recursive = TRUE)

## Load list of features
features <- read_table("./Data/UCI HAR Dataset/features.txt", col_names = "raw") %>%
    mutate(features = str_remove(raw, "^[0-9]+ ")) %>%
    select(features)

## Load class labels and activity names
class_labels <- read_table("./Data/UCI HAR Dataset/activity_labels.txt", col_names = c("classlabel", "activityname"))

## Load training set
training_set <- read_table("./Data/UCI HAR Dataset/train/X_train.txt", col_names = FALSE)

## Load traning labels
training_labels <-read_table("./Data/UCI HAR Dataset/train/y_train.txt", col_names = "classlabel")

## Load test set
test_set <- read_table("./Data/UCI HAR Dataset/test/X_test.txt", col_names = FALSE)

## Load test labels
test_labels <- read_table("./Data/UCI HAR Dataset/test/y_test.txt", col_names = "classlabel")

## Load subject data
training_subjects <- read_table("./Data/UCI HAR Dataset/train/subject_train.txt", col_names = "subject")
test_subjects <- read_table("./Data/UCI HAR Dataset/test/subject_test.txt", col_names = "subject")

## features has 561 obs
## class_labels has 6 obs

## training_set has 7352 obs of 561 vars
## training_labels and training_subjects each have 7352 obs of 1 var

## test_set has 2947 obs of 561 vars
## test_labels and test_subjects each have 2947 obs of 1 var

## Each of the _set datasets have the same number of rows as their respective _labels

## These seem to be the key for class_labels, which contains the descriptive activities
## We bind the columns of these together and join with the class_labels
training_data <- bind_cols(training_subjects, training_labels, training_set) %>%
    left_join(class_labels) %>%
    select(subject, classlabel, activityname, everything())

test_data <- bind_cols(test_subjects, test_labels, test_set) %>%
    left_join(class_labels) %>%
    select(subject, classlabel, activityname, everything())

## features seems to be a list of the var names for both training_set and test_set
## We gather the features
training_gathered <- training_data %>%
    gather("feature", "measurement", -(subject:activityname))

test_gathered <- test_data %>%
    gather("feature", "measurement", -(subject:activityname))

## Now we generate a key for our features data
features_key <- training_gathered %>%
    distinct(feature) %>%
    bind_cols(features)

training_final <- left_join(training_gathered, features_key) %>%
    select(subject, classlabel, activityname, features, measurement)

test_final <- left_join(test_gathered, features_key) %>%
    select(subject, classlabel, activityname, features, measurement)

## We can finally merge/append the training and test data
all_data <- bind_rows(training_final, test_final) %>%
    rename(feature = features)
all_data

## We extract the measurements on mean and standard deviation only
interest_data <- all_data %>%
    filter(str_detect(feature, "mean|std"))
interest_data

## We create an independent tidy data set with the average of each variable
## for each activity and each subject
tidy_data <- interest_data %>%
    group_by(subject, activityname, feature) %>%
    summarize(average = mean(measurement, na.rm = TRUE))


## Item 1 is accomplished in line 80
## Item 2 is accomplished in line 84
## Item 3 is accomplished in line 50
## Item 4 is accomplished throughout the process
## Item 5 is accomplished in line 91
