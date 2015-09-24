rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This script does xgboost on 30% of the train dataset, predicts the
## test dataset, then repeats the procedure 100 times and uses the
## average as the final prediction
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
Args <- (commandArgs(TRUE))
for(i in 1:length(Args)){
  eval(parse(text = Args[[i]]))
}

N_TrainIter <- 80
Percent_Train <- 0.65
SubmissionNumberStart <- 38
Nrounds <- 140
Max.Depth <- 8

########################################################################
## Read in train & test data, without NAs
########################################################################
Filename_train <- paste0(RDataPath, 'train_new.RData')
load(Filename_train)
assign(x = 'train', value = train_new)

Filename_test <- paste0(RDataPath, 'test_new.RData')
load(Filename_test)
assign(x = 'test', value = test_new)

rm(train_new, test_new)
gc()

feature.names <- names(train) %w/o% 'target'

Prediction <- data.frame(ID=test$ID)
for(k in 1:N_TrainIter){
  Prediction$target <- 0
  colnames(Prediction)[colnames(Prediction)=='target'] <- paste0('target_', k)
}

Nrows_subset <- floor(nrow(train) * Percent_Train)
for(i in 1:N_TrainIter){
  set.seed(i+20)
  colname <- paste0('target_', i)
  train_subset <- train[sample(nrow(train), Nrows_subset),]
  cat(paste("training a XGBoost classifier", i, "\n"))
  Model <- xgboost(data        = data.matrix(train_subset[,feature.names]),
                   label       = train_subset$target,
                   nrounds     = Nrounds,
                   objective   = "binary:logistic",
                   eval_metric = "auc",
                   max.depth   = Max.Depth)
  gc()
  Prediction[, colname] <- predict(Model, data.matrix(test[,feature.names]))
  rm(Model, train_subset)
  gc()
}

Prediction$Mean <- rowMeans(Prediction[,2:ncol(Prediction)])
Prediction$Mean_Trim20 <- apply(X = Prediction[,2:ncol(Prediction)], MARGIN = 1, FUN = mean, trim = 0.20)
Prediction$Median <- apply(X = Prediction[,2:ncol(Prediction)], MARGIN = 1, FUN = median)

#lattice::splom(Prediction[,c('Mean', 'Mean_Trim20', 'Median')])

cat("saving the submission file\n")
Pred_Mean <- Prediction[,c('ID', 'Mean')]
colnames(Pred_Mean) <- c('ID', 'target')
Filename_submission <- paste0(RDataPath, "submission_", SubmissionNumberStart, "_", N_TrainIter, "Iter_", (Percent_Train*100), "pctTrain_Mean.csv")
write_csv(Pred_Mean, Filename_submission)

## Pred_Median <- Prediction[,c('ID', 'Median')]
## colnames(Pred_Median) <- c('ID', 'target')
## Filename_submission <- paste0(RDataPath, "submission_", (SubmissionNumberStart+1), "_", N_TrainIter, "Iter_", (Percent_Train*100), "pctTrain_Median.csv")
## #write_csv(Pred_Median, Filename_submission)

## Pred_Mean_Trim20 <- Prediction[,c('ID', 'Mean_Trim20')]
## colnames(Pred_Mean_Trim20) <- c('ID', 'target')
## Filename_submission <- paste0(RDataPath, "submission_", (SubmissionNumberStart+2), "_", N_TrainIter, "Iter_", (Percent_Train*100), "pctTrain_MeanTrim20.csv")
## #write_csv(Pred_Mean_Trim20, Filename_submission)

rm(Pred_Mean) 
gc()

