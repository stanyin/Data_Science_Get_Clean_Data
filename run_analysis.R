run_analysis <- function() {
        ## Author: Stan Yin
        ## Johns Hopkins Coursera Course - Get and Cleaning Data - Course Project - Human Grading
        
        setwd("~/Desktop/R")
        
        library(dplyr)
        library(reshape2)
        
        ## Data Directory, which stores the data set.
        data_directory <- "./Data/UCI/"
        features_file <- "features_stan.txt"
        activity_fle <- "activity_labels.txt"
        
        ## Train Data Set Files
        train_folder <- "train/"
        subject_train_file <- "subject_train.txt"
        x_train_file <- "X_train.txt"
        y_train_file <- "Y_train.txt"
        
        ## Test Data Set Files
        test_folder <- "test/"
        subject_test_file <- "subject_test.txt"
        x_test_file <- "X_test.txt"
        y_test_file <- "y_test.txt"
        
        ## Output Files
        output_file <- "human_grading.txt"
        
        ## Start
        
        ##features <- read.table("./Data/UCI/features_stan.txt")
        features <- read.table(paste(data_directory, features_file, sep = ""))
        
        ##activites <- read.table("./Data/UCI/activity_labels.txt")
        activites <- read.table(paste(data_directory, activity_fle, sep = ""))

        ## Read Train Folder Data
        
        ##subject_train <- read.table("./Data/UCI/train/subject_train.txt")
        subject_train <- read.table(paste(data_directory, train_folder, subject_train_file, sep = ""))

        colnames(subject_train)[1] <- "subject"
        
        subject_train <- mutate(subject_train, id = seq(along = subject_train$subject))
        
        ##y_train <- read.table("./Data/UCI/train/y_train.txt")
        y_train <- read.table(paste(data_directory, train_folder, y_train_file, sep = ""))

        colnames(y_train)[1] <- "activity"
        
        y_train <- mutate(y_train, id = seq(along = y_train$activity))
        
        ##x_train <- read.table("./Data/UCI/train/x_train.txt")
        x_train <- read.table(paste(data_directory, train_folder, x_train_file, sep = ""))

        ## colnames(x_train) <- features$V2
        
        ## x_train <- mutate(x_train, id = seq(along = x_train$`tBodyAcc-mean()-X`))
        
        x_train <- mutate(x_train, id = seq(along = x_train[,1]))
        
        z_train = merge(x_train, subject_train, by.x="id", by.y = "id", all=TRUE)
        
        z_train = merge(z_train, y_train, by.x="id", by.y = "id", all=TRUE)
        
        ## Read test data set
        ##subject_test <- read.table("./Data/UCI/test/subject_test.txt")
        subject_test <- read.table(paste(data_directory, test_folder, subject_test_file, sep = ""))
        
        colnames(subject_test)[1] <- "subject"
        
        subject_test <- mutate(subject_test, id = seq(along = subject_test$subject))
        
        ##x_test <- read.table("./Data/UCI/test/x_test.txt")
        x_test <- read.table(paste(data_directory, test_folder, x_test_file, sep = ""))
        
        ##colnames(x_test) <- features$V2

        ##x_test <- mutate(x_test, id = seq(along = x_test$`tBodyAcc-mean()-X`))
        
        x_test <- mutate(x_test, id = seq(along = x_test[,1]))
        
        ##y_test <- read.table("./Data/UCI/test/y_test.txt")
        y_test <- read.table(paste(data_directory, test_folder, y_test_file, sep = ""))
        
        colnames(y_test)[1] <- "activity"
        
        y_test <- mutate(y_test, id = seq(along = y_test$activity))
        
        z_test = merge(x_test, subject_test, by.x="id", by.y = "id", all=TRUE)
        
        z_test = merge(z_test, y_test, by.x="id", by.y = "id", all=TRUE)
        
        ## Item 1: Merges the training and test sets to create one data set.
        
        print("Item 1: result is stored in a global variable, human_grading. Please check the global variable.")

        combine_data <- rbind(z_test, z_train)
        
        ## Store result of item 1 into human_grading global variable, which ignore id column.
        human_grading <<- select(combine_data, 2:length(combine_data))
        
        ## Item 2: Extracts only the mean and standard deviation for each measurement.
        mean_flag <- grepl("mean", features$V2)
        
        std_flag <- grepl("std", features$V2)
        
        flags <- mean_flag | std_flag
        
        column_list <- (features$V1[flags])
        
        print("Item 2: Result is stored in a global variable, measurements.")
        
        measurements <<- select(human_grading, column_list)
        
        ## Item 3: Uses descriptive activity names to name the activities in the data set.
        print("Item 3: Descriptvie activity names. table(human_grading$activity)")
        
        human_grading$activity <<- activites[human_grading$activity,2]
        
        ## Item 4
        print("Item 4: Descriptive Variable Name. names(human_grading)")
        
        column_name <- column_name <- c(as.character(features$V2), "subject", "activity")
        
        colnames(human_grading) <<- column_name
        
        ## Item 5: Average of each variable for each activity and eac subject
        col_name <- as.character(features$V2)
        
        hg_melt <- melt(human_grading, id = c("activity", "subject"), measure.vars = col_name)
        
        hg_data <- dcast(hg_melt, activity + subject ~ variable, mean)

        print("Item 5: Result is saved in ./Data/UCI/human_grading.txt.")
        ##write.table(hg_data, file = "./Data/UCI/human_grading.txt", row.names = FALSE)
        write.table(hg_data, file = paste(data_directory, output_file, sep = ""), row.names = FALSE)
}