#' test
#' @param data_clean a
#' @param f a
#' @import fhi
#' @export CreateLatestDoneFile
CreateLatestDoneFile <- function(data_clean=fhi::DashboardFolder("data_clean"),f){
  file = file.path(data_clean,sprintf("done_%s.txt",f))
  try(file.create(file),TRUE)
  #try(file.create(paste0("data_clean/done_",LatestRawID(),".txt")),TRUE)
}

#' GetDataInfo
#' @param folder_raw a
#' @param folder_clean a
#' @importFrom fhi DashboardFolder
#' @import data.table
#' @export GetDataInfo
GetDataInfo <- function(
  folder_raw=fhi::DashboardFolder("data_raw"),
  folder_clean=fhi::DashboardFolder("data_clean")
  ){

  f <- list.files(folder_clean,"done_")
  f <- gsub("done_","",f)
  f <- gsub(".txt$","",f)
  if(length(f)>0){
    fDone <- max(f)
  } else fDone <- ""

  f <- list.files(folder_raw,"FHIDOD2")
  f <- gsub("FHIDOD2_","",f)
  f <- gsub(".txt$","",f)
  f <- max(f)
  dateData <- as.Date(f,format="%Y%m%d")
  dateDataMinusOneWeek <- dateData - 7

  return(list(
    fDone=fDone,
    f=f,
    dateData=dateData,
    dateDataMinusOneWeek=dateDataMinusOneWeek
  ))
}


#' GetData
#' @param folder_raw a
#' @param fDone a
#' @param f a
#' @param forceRun a
#' @importFrom fhi DashboardFolder
#' @importFrom RAWmisc IsFileStable WeekN YearN
#' @import data.table
#' @export GetData
GetData <- function(
  folder_raw=fhi::DashboardFolder("data_raw"),
  fDone,
  f,
  forceRun=FALSE){

  if(fDone==f & !forceRun){
    cat(sprintf("%s/%s/R/NORMOMO No new data",Sys.time(),Sys.getenv("COMPUTER")),"\n")
    quit(save="no", status=0)
  } else if(!RAWmisc::IsFileStable(file.path(folder_raw,paste0("FHIDOD2_",f,".txt")))){
    cat(sprintf("%s/%s/R/NORMOMO Unstable data file",Sys.time(),Sys.getenv("COMPUTER")),"\n")
    quit(save="no", status=0)
  } else {
    cat(sprintf("%s/%s/R/NORMOMO Stable data file",Sys.time(),Sys.getenv("COMPUTER")),"\n")

    masterData <- fread(file.path(folder_raw,paste0("FHIDOD2_",f,".txt")))
    masterData[,DoD:=as.Date(as.character(DODS_DATO),format="%Y%m%d")]
    masterData[,DoR:=as.Date(as.character(ENDR_DATO),format="%Y%m%d")]
    masterData[,DoB:=as.Date(as.character(FDATO_YYYYMMDD),format="%Y%m%d")]
    masterData[,age:=floor(as.numeric(difftime(DoD,DoB,units="days"))/365.25)]
    masterData[is.na(DoR),DoR:=DoD+1]
    masterData[DoR>="2015-09-03",DoR:=DoR+1]
    masterData[FYLKE %in% c(16,17),FYLKE:=50] # recoding south and north tronderlag to tronderlag

    masterData[,ageCat:=cut(age,c(0,4,14,64,200),include.lowest = TRUE)]
    masterData[,deathWeek:=RAWmisc::WeekN(masterData$DoD)]
    masterData[,deathYear:=RAWmisc::YearN(masterData$DoD)]

    return(masterData)
  }
}
