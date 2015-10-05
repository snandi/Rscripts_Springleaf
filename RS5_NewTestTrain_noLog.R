rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script uses the list produced by RS4 to create new set of
## test and train datasets. This is the same as RS5_NewTestTrain, but
## this does not log transform the numeric variables. This uses the
## function fn_fixIntVars_noLog
########################################################################

########################################################################
## Run Path definition file                                           ##
########################################################################
RScriptPath <- '~/Stat/Stat_Competitions/Kaggle_Springleaf_2015Oct/RScripts_Springleaf/'
Filename.Header <- paste(RScriptPath, 'HeaderFile_Springleaf.R', sep='')
source(Filename.Header)
source(paste(RScriptPath, 'fn_Library_Springleaf.R', sep=''))
RPlotPath <- '~/Stat/Stat_Competitions/Kaggle_Springleaf_2015Oct/Plots/'
DataPath <- '~/Stat/Stat_Competitions/Kaggle_Springleaf_2015Oct/Data/'
RDataPath <- '~/Stat/Stat_Competitions/Kaggle_Springleaf_2015Oct/RData/'
########################################################################

## N_TrainIter <- 80
## Percent_Train <- 0.65
## SubmissionNumberStart <- 34
## Nrounds <- 400

Filename_badVar_Int <- paste0(DataPath, 'badVar_Int.txt')
Filename_badVar_Chr <- paste0(DataPath, 'badVar_Chr.txt')
Filename_NA <- paste0(DataPath, 'badVar_NAremoved.txt')
Filename_99 <- paste0(DataPath, 'badVar_99999removed.txt')

cat("reading the train and test data\n")
Filename_train <- paste0(DataPath, 'train.csv')
train <- readr::read_csv(Filename_train)
Filename_test <- paste0(DataPath, 'test.csv')
test  <- readr::read_csv(Filename_test)

feature.names <- names(train)[2:ncol(train)-1]

feature.int <- c()
feature.chr <- c()
for (feature in feature.names[2:length(feature.names)]) {
  if(class(train[[feature]]) == 'integer'){
    feature.int <- c(feature.int, feature)
  } else{
    feature.chr <- c(feature.chr, feature)
  }
}  

### Load bad int ###
badVar_Int <- as.vector(read.table(file = Filename_badVar_Int)$V1)
feature.int <- feature.int %w/o% badVar_Int


####################
## New test train integer values
####################
train_new <- matrix(0, nrow = nrow(train), ncol = (length(feature.int) + length(feature.chr)))
names(train_new) <- feature.int
test_new <- matrix(0, nrow = nrow(test), ncol = (length(feature.int) + length(feature.chr)))
names(test_new) <- feature.int
Colnum <- 1
for(feature in feature.int){
  testtrain_new <- fn_fixIntVars_noLog(train, test, feature)
  train_new[,Colnum] <- testtrain_new[['trainfeature']]
  test_new[,Colnum] <- testtrain_new[['testfeature']]
  Colnum <- Colnum + 1
}

####################
## Removing NA from chr variables
####################
for(feature in feature.chr){
  if(max(is.na(train[,feature])) > 0){
    trainfeature <- na.is.zero(train[,feature])
    cat(feature, file = Filename_NA, sep='\n', append=T)
  } else{
    trainfeature <- train[, feature]
  }
  if(max(is.na(test[,feature])) > 0){
    testfeature <- na.is.zero(test[,feature])
  } else{
    testfeature <- test[, feature]
  }
  levels <- unique(c(trainfeature, testfeature))
  train_new[,Colnum] <- factor(trainfeature, levels = levels)
  test_new[,Colnum] <- factor(testfeature, levels = levels)
  Colnum <- Colnum + 1
}

####################
## Save the new files
####################
Filename_train <- paste0(RDataPath, 'train_noLog.RData')
train_new <- as.data.frame(train_new)
train_new$target <- train$target
save(train_new, file = Filename_train)

Filename_test <- paste0(RDataPath, 'test_noLog.RData')
test_new <- as.data.frame(test_new)
test_new$ID <- test$ID
save(test_new, file = Filename_test)

