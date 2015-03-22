# Function solve "Getting and Cleaning Data Course Project"
# No input parameters 
# Return data.frame with results
# 1. Download zip file into ./data folder
# 2. Unzip file
# 3. Open test and train files
# 5. Assign headers and merge datasets
# 6. Get columns with mean and std names
# 7. Aggregate data (group by Activity and apply mean function)

run_analysis <- function(){

  print("Start download file ...")
   if(!file.exists("./data")){dir.create("./data")}
   url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
   download.file(url, destfile="./data/Dataset.zip", mode="wb")

  print("File downloaded, going to unzip ...")
   unzip("./data/Dataset.zip", exdir="./data")

  print("File unzipped, opening X_test, Y_test files ...")
   X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
   Y_test <- read.table("./data/UCI HAR Dataset/test/Y_test.txt")
  
  print("Test files opened, opening X_train, Y_train files ...")
   X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
   Y_train <- read.table("./data/UCI HAR Dataset/train/Y_train.txt")
  
  print("Train files opened, opening features, activity_lavels ...")
   features <- read.table("./data/UCI HAR Dataset/features.txt", stringsAsFactors=FALSE)
   activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
   names(activity_labels) <- c("ActivityCode", "ActivityName")
  
  # Extend Test sets with Training labels
   X_test$ActivityCode <- Y_test$V1
   X_train$ActivityCode <- Y_train$V1

  # Add new column 
   features[nrow(features)+1,] <- c(nrow(features)+1, "ActivityCode")
  
  print("Going to merge datasets ...")
   X <- merge(X_test, X_train, all=TRUE)
  
  print("Going to filter mean, std columns ...")
   names(X) <- features$V2   # Assign names 
   Xmeanstd <- X[, grepl("std|mean|Activity", features$V2)]
   Xact <- merge(Xmeanstd, activity_labels) # Substitute activity_labels
  
  print("Going to aggregate data ...")
   Xact.Cols <- subset(Xact, select = - c(ActivityName,ActivityCode))
   Xact.Activity <- subset(Xact, select = ActivityName)
   # print(aggregate(Xact.Activity, Xact.Activity, length)) # For test purpose
   aggregate(Xact.Cols, Xact.Activity, mean)
}
