######################### Convert NAs to Zero ##########################
na.is.zero <- function(X)
{
  X1 <- X
  X1[is.na(X)] <- 0.0
  return(X1)
}

na.is.neg1 <- function(X)
{
  X1 <- X
  X1[is.na(X)] <- -1
  return(X1)
}
########################################################################

########################################################################
"%notin%" <- function(x, y){
  if(x %in% y){
    return(FALSE)
  } else{
    return(TRUE)
  }
}
########################################################################

########################################################################
"%w/o%" <- function(x, y){
  return(x[!x %in% y])
}
########################################################################

########################################################################
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
########################################################################

is.integer0 <- function(x)
{
  is.integer(x) && !length(x)
}


########################################################################
## substr the last n characters of a character vector
########################################################################
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
########################################################################

fn_get_pValue <- function (lmobject) {
  if (class(lmobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(lmobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail = F)
  attributes(p) <- NULL
  return(round(p, 6))
}

########################################################################
## Returns the SE of mean of each row of a dataset              
########################################################################
rowSE <- function(Data){
  SE <- apply(X = Data, MARGIN = 1, FUN = function(Row){sd(Row)/sqrt(length(Row))})
  return(SE)
}
########################################################################

########################################################################
## Boxplot of integer variables, after handling with NA's
########################################################################
fn_boxplot <- function(train, feature, RPlotPath, f_badVar_Int, f_NA, f_99){
  
  ## Check 1
  if(max(is.na(train[,feature])) > 0){
    train[,feature] <- na.is.zero(train[,feature])
    cat(feature, file = f_NA, sep='\n', append=T)
  }
  
  ## Check 2
  if(min(train[,feature]) < -99990){
    train[,feature][train[,feature] <= -99990] <- 0
    cat(feature, file = f_99, sep='\n', append=T)
  }
  
  if(min(train[,feature]) != max(train[,feature])){
    Filename <- paste0(RPlotPath, feature, '.pdf')
    pdf(file = Filename, onefile = T)
    BoxPlot1 <- qplot(y = train[,feature], x = factor(target), data = train) +
      geom_boxplot(aes(fill = target)) + 
      ggtitle(label = feature) +
      ylab(label = feature) + xlab(label = 'target') +
      coord_flip()
    print(BoxPlot1)
    
    if(min(train[,feature]) >= 0){
      BoxPlot2 <- qplot(y = log(train[,feature]), x = factor(target), data = train) +
        geom_boxplot(aes(fill = target)) + 
        ggtitle(label = feature) +
        ylab(label = paste0('log(', feature, ')')) + xlab(label = 'target') +
        coord_flip()
      
      print(BoxPlot2)
    }
    dev.off()
  } else{
    cat(feature, file = f_badVar_Int, sep='\n', append=T)
  }
}
########################################################################

