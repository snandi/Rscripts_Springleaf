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
Filename_train <- paste0(RDataPath, 'train_new.RData')
load(Filename_train)
assign(x = 'train', value = train_new)

Filename_test <- paste0(RDataPath, 'test_new.RData')
load(Filename_test)
assign(x = 'test', value = test_new)

rm(train_new, test_new)
gc()

########################################################################
## Produce the CV Lambda plots, for 250 variables at a time
########################################################################
fn_LassoCV(train = train, NCores = 10)
########################################################################

# feature.names <- names(train) %w/o% 'target'
# 
# #Data <- train[1:4000,]
# 
# set.seed(10)
# VarFrom <- 1
# VarTo <- min(VarFrom + 249, length(feature.names))
# 
# for(i in 1:8){
#   Time1 <- Sys.time()
#   NCores <- 5
#   cl <- makeCluster(NCores)
#   registerDoParallel(cl)
# 
#   CV <- cv.glmnet(
#                   x        = as.matrix(train[, VarFrom : VarTo]),
#                   y        = train[, 'target'],
#                   family   = "binomial",
#                   parallel = TRUE
#                   )
#   
#   stopCluster(cl)
#   Time2 <- Sys.time()
#   print(Time2 - Time1)
# 
#   Filename <- paste0(RPlotPath, 'LassoCV_', VarFrom, '-', VarTo, '.pdf')
#   pdf(file = Filename)
#   plot.cv.glmnet(CV)
#   title(paste('From', VarFrom, 'To', VarTo))
#   dev.off()
#   VarFrom <- VarTo + 1
#   VarTo <- min(VarFrom + 249, length(feature.names))
# }

## Model1_100 <- glmnet(
##                      x        = as.matrix(train[, 1:100]),
##                      y        = train[, 'target'],
##                      family   = "binomial",
##                      lambda   = CV$lambda[24],
##                      alpha    = 1
##                 )

## Time3 <- Sys.time()
## print(Time3 - Time2)

## Prediction1_100 <- predict(Model1_100, newx = as.matrix(test[, 1:100]))
        
