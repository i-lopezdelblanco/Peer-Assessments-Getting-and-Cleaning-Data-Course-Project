# file names in directory structures
ddir  <- "UCI HAR Dataset"
dtrain <- "train"
dtest  <- "test"
features   <- "features.txt"
activity   <- "activity_labels.txt"
predictor_train      <- "X_train.txt"
predictor_test       <- "X_test.txt"
labl_train    <- "y_train.txt"
labl_test     <- "y_test.txt"
subject_train   <- "subject_train.txt"
subject_test    <- "subject_test.txt"
y_train         <- "y_train.txt"
y_test          <- "y_test.txt"

train_set_path <- paste(ddir, dtrain, predictor_train, sep="/")
test_set_path  <- paste(ddir, dtest, predictor_test,   sep="/")
y_train_path   <- paste(ddir, dtrain, y_train, sep="/")
y_test_path    <- paste(ddir, dtest, y_test,   sep="/")
subject_train_path <- paste(ddir, dtrain, subject_train, sep="/")
subject_test_path  <- paste(ddir, dtest, subject_test,   sep="/")
features_path  <- paste(ddir, features,  sep="/")
activity_path  <- paste(ddir, activity,  sep="/")

# 1.- Building up 1 dataset

# Test header
nrows <- 20
train_first_look <- read.table(train_set_path, nrows=nrows, header = FALSE, dec = ".", fill = TRUE, comment.char = "")
classes_train <- sapply(train_first_look, class)
rm(train_first_look)
# Full dataset
train <- read.table(train_set_path, colClasses = classes_train, header = FALSE, dec = ".", comment.char = "")
test_first_look <- read.table(test_set_path, nrows=nrows, header = FALSE, dec = ".", fill = TRUE, comment.char = "")
classes_test <- sapply(test_first_look, class)
rm(test_first_look)
test <- read.table(test_set_path, colClasses = classes_test, header = FALSE, dec = ".", fill = TRUE, comment.char = "")

# Variable names (561)
features <- read.table(features_path, colClasses = c("numeric","character"), col.names = c("Variable.id","Variable.Name"), header = FALSE, comment.char = "")

subject_train_ids <- read.table(subject_train_path, colClasses = c("numeric"), col.names = c("Subject.id"), header = FALSE, comment.char = "")
activity_train_ids <- read.table(y_train_path, colClasses = c("numeric"), col.names = c("Activity id"), header = FALSE, comment.char = "")

subject_test_ids <- read.table(subject_test_path, colClasses = c("numeric"), col.names = c("Subject.id"), header = FALSE, comment.char = "")
activity_test_ids <- read.table(y_test_path, colClasses = c("numeric"), col.names = c("Activity.id"), header = FALSE, comment.char = "")

# Merges the training and the test sets to create one data set.
# Fixed variables first, followed by measured variables, related variables are contiguous.
# Rows can then be ordered by the first variable, breaking ties with the second and subsequent fixed variables.

# first add the  subject ids, then the activity ids
test <- cbind(subject_test_ids,activity_test_ids,test)
train <- cbind(subject_train_ids,activity_train_ids,train)
#finally the big merge
dataset <- rbind(test,train)

#name the columns
names(dataset) <- c("Subject.id","Activity.id",features[,2])

#ordering the data by subject id then by activity id
dataset <- dataset[order(dataset$Subject.id,dataset$Activity.id),]


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
meantext<- "mean"
stdtext <- "std"

# Removes all columns with name not containing mean or std
for (i in seq_along(features$Variable.Name)) {
  if (!grepl(meantext,features$Variable.Name[i]) & !grepl(stdtext,features$Variable.Name[i])) {
    dataset[,features$Variable.Name[i]] <- NULL
  }
}

# 3.- Activity names in activity_labels.txt
activity <- read.table(activity_path, colClasses = c("numeric","character"), col.names = c("Activity.id","Activity.Name"), header = FALSE, comment.char = "")

library(plyr)
dataset <- join(dataset, activity, by = "Activity.id")
dataset$Activity.id <- NULL

# 4.- Label descriptively
labels <- names(dataset)
labels <- labels[complete.cases(labels)]

#cleaning names of variables
for (i in seq_along(labels)) {
  labels[i] <- gsub("mean","Mean",labels[i])
  labels[i] <- gsub("std","Std",labels[i])
  labels[i] <- gsub("\\()","",labels[i]) 
  labels[i] <- gsub("-","",labels[i])
}
names(dataset) <- labels
# 5.- Last dataset 
tidy <- ddply(dataset, .(Subject.id, Activity.Name), numcolwise(mean))
labels  <- names(tidy)
for (i in 3:length(labels)) { #skips 2
  labels[i] <- paste0("Mean.",labels[i])
}
names(tidy) <- labels

write.table(tidy,"dataset_5.txt",row.name=FALSE)
