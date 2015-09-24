## Choose USA (IA) as the CRAN mirror
Mirrors <- getCRANmirrors(all = FALSE, local.only = FALSE)
chooseCRANmirror(graphics = F, ind = which(Mirrors$Name == 'USA (IA)'))

Packages <- c(
  'boot',
  'car',
  'cluster',
  'clusterSim',
  'clValid',
  'doParallel',
  'foreach',
  'ggplot2',
  'glmnet',
  'gridExtra',
  'lattice',
  'plyr',
  'readr',
  'reshape',
  'reshape2',
  'randomForest', 
  'stats',
  'xgboost',
  'xts'
)

## For loop for requiring packages and installing them if something doesnt exist
for(Package in Packages){
  if(require(package=Package, character.only=T) == F){
    print(paste('Installing', Package))
    try(install.packages(Package, dependencies = TRUE))
  } else{
    print(paste(Package, 'already exists'))
    require(package=Package, character.only=T)
  }
}

## For parallel processing, when passing the list of packages to load
## in all the cores. Could be different from Packages
MyAutoLoads <- Packages
Packages_Par <- Packages

