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
SubmissionNumberStart <- 40

########################################################################
## Read in train & test data, with cluster information
########################################################################
Filename_train <- paste0(RDataPath, 'train_noLog_cluster.RData')
load(Filename_train)

Filename_test <- paste0(RDataPath, 'test_noLog_cluster.RData')
load(Filename_test)

########################################################################
## XGBoost train cluster 1
########################################################################
Nrounds <- 200
feature.names <- names(train) %w/o% c('target', 'cluster')

#dim(data.matrix(subset(train, cluster == 1)[,feature.names]))
train1 <- data.matrix(subset(train, cluster == 1))
test1 <- data.matrix(subset(test, cluster == 1))

Model1 <- xgboost(data        = train1[,feature.names],
                  label       = train1[,'target'],
                  nrounds     = Nrounds,
                  objective   = "binary:logistic",
                  eval_metric = "auc",
                  max.depth   = 6)
gc()

Prediction <- data.frame(ID = test$ID, cluster = test$cluster, target = 0)
Prediction$target[Prediction$cluster == 1] <- predict(Model1, test1[,feature.names])
rm(train1, test1, Model1)
gc()
########################################################################
## XGBoost train cluster 2
########################################################################
Nrounds <- 100

#dim(data.matrix(subset(train, cluster == 1)[,feature.names]))
train2 <- data.matrix(subset(train, cluster == 2))
test2 <- data.matrix(subset(test, cluster == 2))

Model2 <- xgboost(data        = train2[,feature.names],
                  label       = train2[,'target'],
                  nrounds     = Nrounds,
                  objective   = "binary:logistic",
                  eval_metric = "auc",
                  max.depth   = 6)
gc()

Prediction$target[Prediction$cluster == 2] <- predict(Model2, test2[,feature.names])
rm(train2, test2, Model2)
gc()

########################################################################
## XGBoost train cluster 3
########################################################################
Nrounds <- 50

#dim(data.matrix(subset(train, cluster == 1)[,feature.names]))
train3 <- data.matrix(subset(train, cluster == 3))
test3 <- data.matrix(subset(test, cluster == 3))

Model3 <- xgboost(data        = train3[,feature.names],
                  label       = train3[,'target'],
                  nrounds     = Nrounds,
                  objective   = "binary:logistic",
                  eval_metric = "auc",
                  max.depth   = 6)
gc()

Prediction$target[Prediction$cluster == 3] <- predict(Model3, test3[,feature.names])
rm(train3, test3, Model3)
gc()

########################################################################
## XGBoost train cluster 4 & 5
########################################################################
Nrounds <- 20

#dim(data.matrix(subset(train, cluster == 1)[,feature.names]))
train4 <- data.matrix(subset(train, cluster %in% c(4, 5) ))
test4 <- data.matrix(subset(test, cluster %in% c(4, 5)))

Model4 <- xgboost(data        = train4[,feature.names],
                  label       = train4[,'target'],
                  nrounds     = Nrounds,
                  objective   = "binary:logistic",
                  eval_metric = "auc",
                  max.depth   = 6)
gc()

Prediction$target[Prediction$cluster == 4 | Prediction$cluster == 5] <- predict(Model4, test4[,feature.names])
rm(train4, test4, Model4)
gc()

########################################################################
## XGBoost train cluster 6
########################################################################
Nrounds <- 300

train6 <- data.matrix(subset(train, cluster == 6))
test6 <- data.matrix(subset(test, cluster == 6))

Model6 <- xgboost(data        = train6[,feature.names],
                  label       = train6[,'target'],
                  nrounds     = Nrounds,
                  objective   = "binary:logistic",
                  eval_metric = "auc",
                  max.depth   = 6)
gc()

Prediction$target[Prediction$cluster == 6] <- predict(Model6, test6[,feature.names])
rm(train6, test6, Model6)
gc()

Filename_submission <- paste0(RDataPath, "submission_", SubmissionNumberStart, "_Cluster_XGBoost.csv")
write_csv(Prediction[,c('ID', 'target')], Filename_submission)
