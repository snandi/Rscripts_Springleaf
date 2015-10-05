rm(list=ls(all.names=TRUE))
rm(list=objects(all.names=TRUE))
#dev.off()

########################################################################
## This fits the lasso to 250 variables at a time, by cross validating
## to get the best lambda, then, chooses the coefficients with non-zero
## coefficients and re-fits lasso, after re-running CV. Finally, it
## saves the list of non-zero coefficient variables
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

Lambdas <- exp(seq(from = -7, to = -2, length.out = 20))
feature.names <- names(train) %w/o% 'target'

########################################################################
## Step 1: Produce the CV Lambda plots, for 250 variables at a time
########################################################################
## fn_LassoCV(train = train, NCores = 10, Lambdas = Lambdas)
########################################################################

########################################################################
## Step 2: Collect the best lambdas & fit Lasso
########################################################################
Lambda_CV <- as.data.frame(cbind(Varfrom=c(seq(from=1, to=1751, by=250)), 
                                 Lambda = c(0.002008180, 0.002008180, 0.002612707, 
                                            0.001186388, 0.001186388, 0.000911882, 
                                            0.000911882, 0.002612707)))
Variables <- c()
VarFrom <- 1
VarTo <- min(VarFrom + 249, length(feature.names))

for(i in 1:8){
  
  Model1 <- glmnet(
    x        = as.matrix(train[, VarFrom : VarTo]),
    y        = train[, 'target'],
    family   = "binomial",
    lambda   = subset(Lambda_CV, Varfrom == VarFrom)$Lambda,
    alpha    = 1
  )
  Coef1 <- coef(Model1)
  Variables1 <- (feature.names[VarFrom:VarTo])[which(Coef1 > 1e-04)] %w/o% NA
  Variables <- c(Variables, Variables1)
  print(length(Variables1))
  VarFrom <- VarTo + 1
  VarTo <- min(VarFrom + 249, length(feature.names))
}
## This produces a list of 596 variables that have non-zero coefficients, by lasso
print(length(Variables))

########################################################################
## Step 3: Cross validate on this new set of variables, to reduce to
## an even smaller set
########################################################################
Lambdas <- exp(seq(from = -8, to = -4, length.out = 10))
NCores <- 6
cl <- makeCluster(NCores)
registerDoParallel(cl)
CV <- cv.glmnet(
  x        = as.matrix(train[, Variables]),
  y        = train[, 'target'],
  family   = "binomial",
  lambda   = Lambdas,
  parallel = TRUE
)
stopCluster(cl)

if(Filename_train == "~/Stat/Stat_Competitions/Kaggle_Springleaf_2015Oct/RData/train_noLog.RData"){
  Filename <- paste0(RDataPath, 'LassoCV_SecondRound_noLog.pdf')
} else{
  Filename <- paste0(RDataPath, 'LassoCV_SecondRound.pdf')
}
pdf(file = Filename)
plot.cv.glmnet(CV)
title('Second Round')
dev.off()

Lambda2 <- CV[['lambda.min']]

Model2 <- glmnet(
  x        = as.matrix(train[, Variables]),
  y        = train[, 'target'],
  family   = "binomial",
  lambda   = Lambda2,
  alpha    = 1
)
Coef2 <- coef(Model2)
Variables2 <- (feature.names[1:VarTo])[which(Coef2 > 1e-04)] %w/o% NA

if(Filename_train == "~/Stat/Stat_Competitions/Kaggle_Springleaf_2015Oct/RData/train_noLog.RData"){
  Filename <- paste0(RDataPath, 'LassoVariables2_noLog.RData')
} else{
  Filename <- paste0(RDataPath, 'LassoVariables2.RData')
}
save(Variables2, file = Filename)

