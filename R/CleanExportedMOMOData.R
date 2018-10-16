#' CleanExportedMOMOData
#' @param data a
#' @param s a
#' @param folder_results a
#' @importFrom fhi Censor
#' @import data.table
#' @export CleanExportedMOMOData
CleanExportedMOMOData <- function(
  data,
  s,
  folder_results=fhi::DashboardFolder("results")
  ){

  id <- NULL
  GROUP <- NULL
  wk <- NULL
  wk2 <- NULL
  randomNoise <- NULL
  nbc <- NULL
  nb <- NULL
  UPIc <- NULL


  data <- data[,c("GROUP","wk","wk2","YoDi","WoDi","Pnb","nb","nbc","UPIb2","UPIb4","UPIc","LPIc","UCIc","LCIc","zscore"),with=F]
  data[,id:=paste0(GROUP,wk,wk2)]
  data[,wk2:=as.character(wk2)]

  nam <- names(data)
  if(s[["runName"]]=="Norway"){
    if(file.exists(file.path(folder_results,"censoring.RDS"))){
      oldCensoring <- readRDS(file.path(folder_results,"censoring.RDS"))
      data <- merge(data,oldCensoring[,c("id","randomNoise"),with=F],by="id",all.x=TRUE)
    } else {
      data[,randomNoise:=as.numeric(NA)]
    }
    data[is.na(randomNoise),randomNoise:=as.numeric(sample(c(-3:3),size=.N,replace=TRUE))]
    saveRDS(data,file.path(folder_results,"censoring.RDS"))
    data[,nbc:=fhi::Censor(n=nbc,randomNoise=randomNoise,boundaries=list(data$UPIb2,data$UPIb4))]
    data[,nb:=fhi::Censor(n=nb,randomNoise=randomNoise,boundaries=list(data$UPIb2,data$UPIb4))]
    data[,randomNoise:=NULL]
  }
  setcolorder(data,nam)

  minCorrectedWeek <- min(data[nbc!=nb]$wk)

  # prediction interval
  data[is.na(UPIc) | UPIc < nbc,UPIc:=nbc]
  data[is.na(LPIc) | LPIc > nbc,LPIc:=nbc]

  # making them slightly wider to hide the real information
  data[wk >= minCorrectedWeek & UPIc==0,UPIc:=1]
  data[wk >= minCorrectedWeek & !is.na(UPIc),UPIc:=UPIc+3]
  data[wk >= minCorrectedWeek & !is.na(LPIc),LPIc:=LPIc-3]
  data[LPIc<0,LPIc:=0]

  # prediction interval cant be below the real value!
  data[is.na(LPIc) | LPIc < nb,LPIc:=nb]

  # remove prediction intervals before correction
  data[wk < minCorrectedWeek,UPIc:=nbc]
  data[wk < minCorrectedWeek,LPIc:=nbc]


  data[,excess:=nbc-Pnb]

  return(data)
}
