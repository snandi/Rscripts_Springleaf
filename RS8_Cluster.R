rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script clusters the training dataset based on the 100 or so
## variables, so I can run xgboost on each cluster
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

########################################################################
## Command line arguments
########################################################################
##First read in the arguments listed at the command line
## Args <- (commandArgs(TRUE))
## for(i in 1:length(Args)){
##   eval(parse(text = Args[[i]]))
## }

## N_TrainIter <- 80
## Percent_Train <- 0.65
## SubmissionNumberStart <- 38
## Nrounds <- 140
## Max.Depth <- 8

########################################################################
## Read in train & test data, without NAs
########################################################################
Filename_train <- paste0(RDataPath, 'train_noLog.RData')
load(Filename_train)
assign(x = 'train', value = train_new)

Filename_test <- paste0(RDataPath, 'test_noLog.RData')
load(Filename_test)
assign(x = 'test', value = test_new)

rm(train_new, test_new)
gc()

########################################################################
## Load variable list produced by RS7_Lasso
########################################################################
Filename <- paste0(RDataPath, 'LassoVariables2_noLog.RData')
load(Filename)

########################################################################
## Create subsets of only lasso approved variables
########################################################################
train_subset <- train[, c(Variables2, 'target')]
test_subset <- test[, Variables2]

train_kmeans <- kmeans(
  x         = train_subset,
  centers   = 6,
  iter.max  = 10,
  nstart    = 6
)

