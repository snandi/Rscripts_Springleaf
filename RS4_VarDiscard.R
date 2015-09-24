rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script removes NA, removes 999999, lists meaningless integer vars
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

system(command = paste('rm -f', Filename_badVar_Int))
system(command = paste('rm -f', Filename_badVar_Chr))
system(command = paste('rm -f', Filename_NA))
system(command = paste('rm -f', Filename_99))

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

feature.int <- c()
feature.chr <- c()
for (feature in feature.names[2:length(feature.names)]) {
  if(class(train[[feature]]) == 'integer'){
    feature.int <- c(feature.int, feature)
  } else{
    feature.chr <- c(feature.chr, feature)
  }
}  

########################################################################
## Checking if any factor variable has only 1 level
########################################################################
## for (feature in feature.int[10:20]) {
##   fn_boxplot(
##     train = train,
##     feature = feature,
##     RPlotPath = RPlotPath,
##     f_badVar_Int = Filename_badVar_Int,
##     f_NA = Filename_NA,
##     f_99 = Filename_99
##   )
## }

########################################################################
## For parallel execution
########################################################################
NCores <- 8
cl <- makeCluster(NCores)
registerDoParallel(cl)
foreach(feature = feature.int, .inorder=FALSE, .packages=Packages_Par) %dopar%   fn_boxplot(
    train = train,
    feature = feature,
    RPlotPath = RPlotPath,
    f_badVar_Int = Filename_badVar_Int,
    f_NA = Filename_NA,
    f_99 = Filename_99
  )
stopCluster(cl)
gc()
