coursera-getdata-008
====================
Description of how my code for cleaning data works:

#First I read table features.txt into features vector, which contains all of the features measured by the gear.

features <- read.table("features.txt")

# Then read X_train and X_test tables and apply the feature names from the features vector

X_train <- read.table("train/X_train.txt", col.names=features[,2] )
X_test <- read.table("test/X_test.txt", col.names=features[,2] )

# Read subject_train and subject_test vectors from the tables - these will describe subject number in rows

subject_train <- read.table("train/subject_train.txt")
subject_test <- read.table("test/subject_test.txt")

# Read y_train and y_test vector, which are numeric activity codes - to be later bound with activity names

y_train <- read.table("train/y_train.txt")
y_test <- read.table("test/y_test.txt")

# You should create one R script called run_analysis.R that does the following. 

# 1 Merges the training and the test sets to create one data set.
# I use rbind to append test to train
X_both <- rbind(X_train, X_test)
rm(X_train, X_test)
#this rm is optional, for freeing memory

# 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
# I used 'grep' to find only those names that have mean() or std() in them. I used fixed=TRUE because I noticed that 
# without this option the grep function passes other strings like "meanSOMETHING()"

which_variables <- sort(c(features[ grep('mean()',features$V2, fixed=TRUE) ,1],
                          features[ grep('std()',features$V2, fixed=TRUE) ,1]))
X_both <- X_both[, which_variables] # replace then the frame leaving only variables required for further processing

# 3 Uses descriptive activity names to name the activities in the data set
# append activity codes (1:6) and give a name to the vector 
y_both <- rbind(y_train, y_test)
names(y_both) <- "activity_code"

# then add the vector with activity codes as a column to the X (variables) data frame
yx <- cbind(y_both, X_both)

# since I noticed that merging data frame may change the row order, I first added 
# the column with the subject number so that order is not lost in the process 
# (i.e. if I''m not mistaken I should do cbind(subjects) before merge(activities names) )

subjects <- rbind(subject_train, subject_test) # appends subjects data frames 
names(subjects) <- "subject" # name for that vector 
syx <- cbind(subjects, yx) # add vector with subjects to the main data (variables and activities)

# now read activity labels - activity names that correspond to codes 1:6 
activity_labels <- read.table("activity_labels.txt",
                      col.names=c("activity_code", "activity_name"))

# then merge labels to the data (activity_code is a common variable name and will be used as the key to join
asyx <- merge (activity_labels, syx)

#don ''t need activity code anymore - delete it from the frame 
asyx$activity_code <- NULL

# 4 Appropriately labels the data set with descriptive variable names. 
# now I did a series of text operation to make the names long and informative
# also gave names to the category variables "activity", "subject" so that they can be used in processing

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

# used aggregate with function mean (applied to numerical variables columns 3:68 )
result <- aggregate(asyx[, 3:68], by=list(asyx$activity, asyx$subject), FUN=mean) 

# had to change the names of categories again , R had changed them to Group.1 and Group.2
names(result)[1:2] <- c("activity", "subject")

#this is what I submitted to Coursera ("moj_wynik" = "my_result" in Polish)

write.table (result, file="moj_wynik.txt", row.name=FALSE)
