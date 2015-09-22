rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script uses Lasso to choose important features
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

system(command = paste('rm -f', Filename_badVar_Int))
system(command = paste('rm -f', Filename_badVar_Chr))

cat("reading the train and test data\n")
Filename_train <- paste0(DataPath, 'train.csv')
train <- readr::read_csv(Filename_train)
Filename_test <- paste0(DataPath, 'test.csv')
test  <- readr::read_csv(Filename_test)

feature.names <- names(train)[2:ncol(train)-1]

########################################################################
## Checking if any factor variable has only 1 level
########################################################################
for (feature in feature.names) {
  if (class(train[[feature]])=="character") {
    levels_train <- unique(c(train[[feature]]))
    levels_test <- unique(c(test[[feature]]))
    if(length(levels_train) == 1){
      print(paste('train', feature))
      print(levels_train)
      print(paste('test', feature))
      print(levels_test)
    }
  }
}
########################################################################

########################################################################
## Checking if any factor variable has only 1 level
########################################################################
for (feature in feature.names[2:length(feature.names)]) {
  if(class(train[[feature]]) == 'integer'){
    print(feature)
    train[,feature] <- na.is.zero(train[,feature])
    if(min(train[,feature]) != max(train[,feature])){
      fn_boxplot(feature = feature, train = train, RPlotPath = RPlotPath)
    } else{
      print(paste('Useless variable', feature))
      print(summary(train[,feature]))
      cat(feature, file = Filename_badVar_Int, sep='\n', append=T)
    }
  }
}
########################################################################


cat("assuming text variables are categorical & replacing them with numeric ids\n")



cat("replacing missing values with -1\n")
train[is.na(train)] <- -1
test[is.na(test)]   <- -1


