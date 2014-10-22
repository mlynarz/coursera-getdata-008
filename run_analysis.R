features <- read.table("features.txt")

X_train <- read.table("train/X_train.txt", col.names=features[,2] )
X_test <- read.table("test/X_test.txt", col.names=features[,2] )

subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")

y_train <- read.table("train/y_train.txt")
y_test <- read.table("test/y_test.txt")

# You should create one R script called run_analysis.R that does the following. 

# 1 Merges the training and the test sets to create one data set.
X_both <- rbind(X_train, X_test)
rm(X_train, X_test)

# 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
which_variables <- sort(c(features[ grep('mean()',features$V2, fixed=TRUE) ,1],
                          features[ grep('std()',features$V2, fixed=TRUE) ,1]))
X_both <- X_both[, which_variables]

# 3 Uses descriptive activity names to name the activities in the data set
y_both <- rbind(y_train, y_test)
names(y_both) <- "activity_code"

yx <- cbind(y_both, X_both)

# NOTE FROM THE STUDENT:
# since I noticed that merging data frame may change the row order, let me first add 
# the column with the subject number so that order is not lost in the process 
# (i.e. if I'm not mistaken I should do cbind(subjects) before merge(activities names) )

subjects <- rbind(subject_train, subject_test)
names(subjects) <- "subject"
syx <- cbind(subjects, yx)

activity_labels <- read.table("activity_labels.txt",
                      col.names=c("activity_code", "activity_name"))

asyx <- merge (activity_labels, syx)
asyx$activity_code <- NULL

# 4 Appropriately labels the data set with descriptive variable names. 
nazwy <- names(asyx)[3:68]
nazwy <- sub("mean", "meanofmeans", nazwy) 
nazwy <- sub("std", "meanofdeviations", nazwy) 
nazwy <- sub("tBody", "timebody", nazwy) 
nazwy <- sub("tGravity", "timegravity", nazwy) 
nazwy <- sub("fBody", "fourierbody", nazwy) 
nazwy <- sub("fBody", "fourierbody", nazwy) 
nazwy <- sub("Mag", "magnitude", nazwy) 
nazwy <- sub("Acc", "accelerometer", nazwy) 
nazwy <- sub("Gyro", "gyroscope", nazwy) 
nazwy <- sub("...X", "x", nazwy) 
nazwy <- sub("...Y", "y", nazwy) 
nazwy <- sub("...Z", "z", nazwy) 
nazwy <- gsub("\\.", "", nazwy) 
nazwy <- tolower(nazwy)
names(asyx)[1:2] <- c("activity", "subject")
names(asyx)[3:68] <- nazwy

# 5 From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.

result <- aggregate(asyx[, 3:68], by=list(asyx$activity, asyx$subject), FUN=mean) 
names(result)[1:2] <- c("activity", "subject")

write.table (result, file="moj_wynik.txt", row.name=FALSE)
