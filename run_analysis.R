run_analysis <- function(){
    # load dplyr
    library(dplyr)
    
    #set path to input files and for creating output tidy.txt file
    PATH <- paste(getwd(),"\\UCI HAR Dataset\\", sep = "")
    
    # load traininig files for gyro data, subject data and activity data
    trainFull <- read.table(paste(PATH, "train\\X_train.txt", sep = ""))
    trainSubject <- read.table(paste(PATH, "train\\subject_train.txt", sep = ""))
    trainActivity <- read.table(paste(PATH, "train\\y_train.txt", sep = ""))
    
    # load test files for gyro data, subject data and activity data
    testFull <- read.table(paste(PATH, "test\\X_test.txt", sep = ""))
    testSubject <- read.table(paste(PATH, "test\\subject_test.txt", sep = ""))
    testActivity <- read.table(paste(PATH, "test\\y_test.txt", sep = ""))
    
    # load description files of variable names and activity labels
    columns <- read.table(paste(PATH, "features.txt", sep = ""))
    activities <- read.table(paste(PATH, "activity_labels.txt", sep = "")) 
    
    # merge sets
    allFull <- bind_rows(testFull, trainFull)
    allSubject <- bind_rows(testSubject, trainSubject)
    allActivity <- bind_rows(testActivity, trainActivity)
    
    # remove old sets
    rm(testFull)
    rm(testSubject)
    rm(testActivity)
    rm(trainFull)
    rm(trainSubject)
    rm(trainActivity)
    
    # convert to tbl_dt
    allFull <- tbl_dt(allFull)
    # set descritive variable names
    colnames(allFull) <- as.vector(columns[,2])
    # select relevant fields - mean and std variables
    allMeanStd <- allFull %>% select(matches("mean()|std()", ignore.case = FALSE))
    # join activity with descriptions
    allActivity <- left_join(allActivity, activities, by = "V1") %>% rename(actId = V1, activity = V2)
    # bind subject & activity sets with dataset
    allFinal <- bind_cols(allSubject, allActivity, allMeanStd) %>% select (-actId) %>% rename(subject = V1)
    
    # remove
    rm(allActivity)
    rm(allFull)
    rm(allMeanStd)
    rm(allSubject)
    
    # group by subject and activity and summarise
    allFinalGrouped <- allFinal %>% group_by(subject, activity) %>% summarise_each(funs(mean))
    
    # write txt file
    write.table(allFinalGrouped, paste(PATH, "tidy.txt"), row.names = FALSE, sep = ";")
}
