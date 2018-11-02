#' SavingRawData
#' @param folder_results a
#' @param dateDataMinusOneWeek a
#' @param masterData a
#' @importFrom RAWmisc YearWeek
#' @importFrom openxlsx write.xlsx
#' @import data.table
#' @export SavingRawData
SavingRawData <- function(
  folder_results=fhi::DashboardFolder("results"),
  dateDataMinusOneWeek,
  masterData
){

  . <- NULL
  ageCat <- NULL
  yd <- NULL
  N <- NULL
  location <- NULL

  temp <- masterData[,.(N=.N),by=.(deathYear,deathWeek,FYLKE,ageCat)]
  skeleton <- data.table(expand.grid(
    deathYear=unique(temp$deathYear),
    deathWeek=unique(temp$deathWeek),
    FYLKE=unique(temp$FYLKE),
    ageCat=unique(temp$ageCat)))

  skeleton[,yd:=paste0(deathYear,"-",deathWeek)]
  temp[,yd:=paste0(deathYear,"-",deathWeek)]
  skeleton <- skeleton[yd %in% unique(temp$yd)]
  skeleton[,yd:=NULL]
  temp[,yd:=NULL]

  temp <- merge(temp,skeleton,all.y=T,by=c("deathYear","deathWeek","FYLKE","ageCat"))
  temp[is.na(N),N:=0]
  temp[,location:=as.character(FYLKE)]
  tempAll <- temp[,.(N=sum(N)),by=.(deathYear,deathWeek,ageCat)]
  tempAll[,location:="Norway"]
  temp <- temp[FYLKE %in% normomo::CONFIG$VALID_FYLKE]
  temp[,FYLKE:=NULL]
  temp <- rbind(temp,tempAll)
  temp[,ageCat:=as.character(ageCat)]
  tempAll <- temp[,.(N=sum(N)),by=.(deathYear,deathWeek,location)]
  tempAll[,ageCat:="Total"]
  temp <- rbind(temp,tempAll)
  temp[,ageCat:=factor(ageCat,levels=c("[0,4]","(4,14]","(14,64]","(64,200]","Total"))]
  temp[,location:=factor(location,levels=c(normomo::CONFIG$VALID_FYLKE,"Norway"))]
  setcolorder(temp,c("deathYear","deathWeek","ageCat","location","N"))
  setorder(temp,deathYear,deathWeek,ageCat,location)

  cat(sprintf("%s/%s/R/NORMOMO Saving data_raw.xlsx",Sys.time(),Sys.getenv("COMPUTER")),"\n")
  openxlsx::write.xlsx(temp,
                       file.path(
                         folder_results,
                         RAWmisc::YearWeek(dateDataMinusOneWeek),
                         "Data",
                         "data_raw.xlsx")
                       )

}
