
## 0/ DATA IMPORT

## First, create a directory for the downloaded data
if (!file.exists("./data")){dir.create("./data")}

## Second, dowload and unzip
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL,destfile="./data/ds.zip",mod="wb",method="curl")
unzip("./data/ds.zip")

## Third, read the features
features <- read.table("./UCI HAR Dataset/features.txt",header=FALSE)
head(features)
features <- features[,2]
features
length(features)
features <- as.character(features)

## Fourth, read the training data
xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE)
dim(xtrain)
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE)
dim(ytrain)
subjecttrain <- read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)
dim(subjecttrain)

## Fifth, read the test data
xtest <- read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE)
dim(xtrain)
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE)
dim(ytrain)
subjecttest <- read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)
dim(subjecttest)

## 1/ MERGE THE TEST AND TRAINING SETS

x <- rbind(xtrain,xtest)
y <- rbind(ytrain,ytest)
subject <- rbind(subjecttrain,subjecttest)

## the merging occured ok:
nrow(x) - nrow(xtest) - nrow(xtrain) ##=0!!
dim(x) ## same as xtest/xtrain
nrow(y) - nrow(ytest) - nrow(ytrain) ##=0!!
nrow(subject) - nrow(subjecttest) - nrow(subjecttrain) ##=0!!


## 2/ EXTRACT THE MEASUREMENTS ON THE MEAN AND STANDARD DEVIATION FOR EACH MEASUREMENT

## To accomplish that we need to identify the variables in 'features' whose name contains with either -mean() or -std()
## so we create a function that detects that
is_mean_or_std <- function(x){
    y <- substr(x,nchar(x)-8,nchar(x)-2)
    if (y == "-mean()"){return(TRUE)}
    y <- substr(x,nchar(x)-7,nchar(x)-2)
    if (y == "-std()"){return(TRUE)}
    y <- substr(x,nchar(x)-5,nchar(x))
    if (y == "-std()"){return(TRUE)}
    y <- substr(x,nchar(x)-6,nchar(x))
    if (y == "-mean()"){return(TRUE)}
    FALSE
}

extract_measurement <- which(sapply(features,is_mean_or_std))

## then we select only those features in the dataset:
x <- x[,extract_measurement]

## 3/ USE DESCRIPTIVE ACTIVITY NAMES TO NAME THE ACTIVITIES IN THE DATASET

## First we import the table of activities:
activities <- read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE)
activities <- activities[,2]

## Second, we assign a new variable, activity, to y with the label of the activitiy per observation
library(dplyr)
y <- mutate(y,activity = activities[y[,1]])
head(y)

## Third, we merge x and y and subject
x <- cbind(x,y$activity,subject)
dim(x)

## 4/ APPROPRIATELY LABEL THE DATA SET WITH DESCRIPTIVE VARIABLE NAMES

names(x) <- c(features[extract_measurement],"activity","individual")
str(x)

## 5/ CREATE INDEPENDANT, TIDY DS WITH THE AVG OF EACH VARIABLE FOR EACH ACTIVITY AND EACH SUBJECT

final <- x %>% group_by(activity,individual) %>% summarise_each(funs(mean))
dim(final)

## 6/ WRITE RESULTS

write.table(final,"./final.txt",row.name=FALSE)
