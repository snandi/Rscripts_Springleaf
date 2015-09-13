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

N_TrainIter <- 20
Percent_Train <- 0.40 

cat("reading the train and test data\n")
Filename_train <- paste0(DataPath, 'train.csv')
train <- readr::read_csv(Filename_train)
Filename_test <- paste0(DataPath, 'test.csv')
test  <- readr::read_csv(Filename_test)

feature.names <- names(train)[2:ncol(train)-1]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

cat("replacing missing values with -1\n")
train[is.na(train)] <- -1
test[is.na(test)]   <- -1

Prediction <- data.frame(ID=test$ID)
for(k in 1:N_TrainIter){
  Prediction$target <- 0
  colnames(Prediction)[colnames(Prediction)=='target'] <- paste0('target_', k)
}

Nrows_subset <- floor(nrow(train) * Percent_Train)
for(i in 1:N_TrainIter){
  set.seed(i)
  colname <- paste0('target_', i)
  train_subset <- train[sample(nrow(train), Nrows_subset),]
  cat(paste("training a XGBoost classifier", i, "\n"))
  Model <- xgboost(data        = data.matrix(train_subset[,feature.names]),
                   label       = train_subset$target,
                   nrounds     = 30,
                   objective   = "binary:logistic",
                   eval_metric = "auc")
  gc()
  Prediction[, colname] <- predict(Model, data.matrix(test[,feature.names]))
  rm(Model, train_subset)
  gc()
}

Prediction$Mean <- rowMeans(Prediction[,2:ncol(Prediction)])
Prediction$Mean_Trim10 <- apply(X = Prediction[,2:ncol(Prediction)], MARGIN = 1, FUN = mean, trim = 0.20)
Prediction$Median <- apply(X = Prediction[,2:ncol(Prediction)], MARGIN = 1, FUN = median)

lattice::splom(Prediction[,c('Mean', 'Mean_Trim10', 'Median')])

cat("saving the submission file\n")
Filename_submission <- paste0(RDataPath, "submission_4_20Iter_40pctTrain_Mean.csv")
write_csv(Prediction[,c('ID', 'Mean')], Filename_submission)

Filename_submission <- paste0(RDataPath, "submission_5_20Iter_40pctTrain_Median.csv")
write_csv(Prediction[,c('ID', 'Median')], Filename_submission)

Filename_submission <- paste0(RDataPath, "submission_6_20Iter_40pctTrain_MeanTrim20.csv")
write_csv(Prediction[,c('ID', 'Mean_Trim10')], Filename_submission)

gc()
