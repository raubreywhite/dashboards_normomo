TP <- function(var){
  sum(var=="TP",na.rm=T)
}

FP <- function(var){
  sum(var=="FP",na.rm=T)
}

TN <- function(var){
  sum(var=="TN",na.rm=T)
}

FN <- function(var){
  sum(var=="FN",na.rm=T)
}

PPV <- function(var){
  return(TP(var)/(TP(var)+FP(var)))
}

NPV <- function(var){
  return(TN(var)/(TN(var)+FN(var)))
}

SENS <- function(var){
  return(TP(var)/(TP(var)+FN(var)))
}

SPEC <- function(var){
  return(TN(var)/(TN(var)+FP(var)))
}
