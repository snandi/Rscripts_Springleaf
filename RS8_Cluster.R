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

Variables2 <- Variables2 %w/o% 'V1347'
########################################################################
## Cluster training data subsets of only lasso approved variables
########################################################################
train_kmeans <- kmeans(
  x         = train[, Variables2],
  centers   = 6,
  iter.max  = 10,
  nstart    = 6
)

Centers <- train_kmeans$centers
train$cluster <- train_kmeans$cluster

closest.cluster <- function(ClusterObj, NewData) {
  cluster.dist <- apply(ClusterObj$centers, 1, function(y) sqrt(sum((NewData - y)^2)))
  return(which.min(cluster.dist)[1])
}

test$cluster <- apply(test[, Variables2], 1, closest.cluster, ClusterObj = train_kmeans)

Filename_train <- paste0(RDataPath, 'train_noLog_cluster.RData')
save(train, file = Filename_train)

Filename_test <- paste0(RDataPath, 'test_noLog_cluster.RData')
save(test, file = Filename_test)
