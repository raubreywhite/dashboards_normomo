con <- file("/tmp/computer","r")
COMPUTER_NAME <- readLines(con,n=1)
close(con)
Sys.setenv(COMPUTER=COMPUTER_NAME)

cat(sprintf("%s/%s/R/NORMOMO STARTING UP!!",Sys.time(),Sys.getenv("COMPUTER")),"\n")

suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

if(Sys.getenv("RSTUDIO") == "1"){
  devtools::load_all("/packages/dashboards_normomo/", export_all=FALSE)
  forceRun <- TRUE
} else {
  library(normomo)
  forceRun <- FALSE
}

DashboardFolder <- fhi::DashboardFolder
fhi::DashboardInitialise(
  STUB="/",
  SRC="src",
  NAME="normomo"
)

# Set up data

hfile <- data.frame(readxl::read_excel(system.file("extdata", "bank_holidays.xlsx", package = "normomo"))[,c("date", "closed")])
hfile$date <- as.Date(hfile$date)
#fwrite(hfile,file=DashboardFolder("data_clean","bank_holidays.txt"))

info <- GetDataInfo()

masterData <- GetData(
  fClean=info[["fClean"]],
  f=info[["f"]],
  forceRun=forceRun
)

# Set up folders
SetupFolders(dateDataMinusOneWeek=info[["dateDataMinusOneWeek"]])

# Plan out analyses
stack <- GenerateStack(
  f=info[["f"]],
  dateDataMinusOneWeek=info[["dateDataMinusOneWeek"]],
  dateData=info[["dateData"]]
)

stackStatistics <- stack[["stackStatistics"]]
stackAnalyses <- stack[["stackAnalyses"]]

#### STATISTICS
pb <- RAWmisc::ProgressBarCreate(min=0,max=nrow(stackStatistics),flush=TRUE)
allPlotData <- vector("list",length=nrow(stackStatistics))
dataAnalysis <- as.data.frame(masterData[!is.na(age),c("DoD","DoR","age"),with=F])

stack <- stackStatistics[dateData==max(dateData),]
res <- vector("list",length=nrow(stack)+1)
for(i in 1:nrow(stack)){
  s <- stack[i,]
  momo::SetOpts(
    DoA = s[["dateData"]],
    DoPR = as.Date("2012-1-1"),
    WStart = 1,
    WEnd = 52,
    country = s[["runName"]],
    source = "FHI",
    MDATA = dataAnalysis,
    HDATA = hfile,
    INPUTDIR = s[["MOMOFolderInput"]],
    WDIR = tempdir(),
    back = 7,
    WWW = 290,
    Ysum = s[["MOMOYsum"]],
    Wsum = 40,
    plotGraphs = FALSE,
    delayVersion = s[["delayVersion"]],
    MOMOgroups = s[["MOMOgroups"]][[1]],
    MOMOmodels = s[["MOMOmodels"]][[1]],
    verbose=FALSE)

  momo::RunMoMo()

  res[[i]] <- momo::dataExport$aggr_fullDelay
  res[[i]]$delayVersion <- s[["delayVersion"]]
}

aggr <- momo::dataExport$aggr

setDT(aggr)
aggr <- aggr[order(aggr$wk),]

#* Drop obs in week of aggregation

modellingWeeks <- momo::momoAttr$PRWEEK:momo::momoAttr$WEEK2
modellingWeeks <- modellingWeeks[modellingWeeks > min(aggr$wk)+momo::momoAttr$delayCorr*2]

aggr$sin52 <- sin(aggr$WoDi*2*pi/52)
aggr$cos52 <- cos(aggr$WoDi*2*pi/52)

aggr$sin26 <- sin(aggr$WoDi*2*pi/26)
aggr$cos26 <- cos(aggr$WoDi*2*pi/26)

for(r in 0:(momo::momoAttr$delayCorr*2)){
  aggr[,(sprintf("WR0_lag%s",r)):=shift(WR0,n=r)]
  aggr[,(sprintf("WR1_lag%s",r)):=shift(WR1,n=r)]
  aggr[,(sprintf("WR2_lag%s",r)):=shift(WR2,n=r)]
}

WR <- c()
for(r in 0:momo::momoAttr$delayCorr){
  WR <- c(WR,sprintf("WR%s",r))
  WR0Lags <- WR1Lags <- WR2Lags <- c()
  for(rx in 1:momo::momoAttr$delayCorr) WR0Lags <- c(WR0Lags,sprintf("WR0_lag%s",rx))
  for(rx in 2:momo::momoAttr$delayCorr) WR1Lags <- c(WR1Lags,sprintf("WR1_lag%s",rx))
  #WR1Lags <- c(WR1Lags,sprintf("WR1_lag%s",r+1))
  #WR2Lags <- c(WR2Lags,sprintf("WR2_lag%s",r))

  xVars <- c("sin52","cos52","sin26","cos26",WR,WR0Lags)

  x <- as.matrix(aggr[,xVars,with=F])
  xTest <- as.matrix(aggr[aggr$wk %in% modellingWeeks,xVars,with=F])
  y <- aggr[aggr$wk %in% modellingWeeks,]$nb

  s <- RAWmisc::ScaleCreate(xTest)

  fit <- glmnet::cv.glmnet(x=RAWmisc::ScaleApply(xTest,s),y=y,family="gaussian")
  print(r)
  print(coef(fit))
  p <- predict(fit,RAWmisc::ScaleApply(x,s))

  aggr[,(sprintf("pred%s",r)):=p]
}
aggr[,delayVersion:="x"]

res[[length(res)]] <- aggr
resx <- rbindlist(res,fill=T)

resx[,resid0:=nb-pred0]
resx[,resid1:=nb-pred1]
resx[,resid2:=nb-pred2]
resx[,resid3:=nb-pred3]
resx[,resid4:=nb-pred4]
resx[,resid5:=nb-pred5]

resx[wk %in% modellingWeeks,.(
  rmse0=sqrt(mean(resid0^2,na.rm=T)),
  rmse1=sqrt(mean(resid1^2,na.rm=T)),
  rmse2=sqrt(mean(resid2^2,na.rm=T)),
  rmse3=sqrt(mean(resid3^2,na.rm=T)),
  rmse4=sqrt(mean(resid4^2,na.rm=T)),
  rmse5=sqrt(mean(resid5^2,na.rm=T))
),by=.(delayVersion)]



for(i in 1:nrow(stackStatistics)){
  RAWmisc::ProgressBarSet(pb,i)

  s <- stackStatistics[i,]

  momo::SetOpts(
    DoA = s[["dateData"]],
    DoPR = as.Date("2012-1-1"),
    WStart = 1,
    WEnd = 52,
    country = s[["runName"]],
    source = "FHI",
    MDATA = dataAnalysis,
    HDATA = hfile,
    INPUTDIR = s[["MOMOFolderInput"]],
    WDIR = tempdir(),
    back = 7,
    WWW = 290,
    Ysum = s[["MOMOYsum"]],
    Wsum = 40,
    plotGraphs = FALSE,
    delayVersion = s[["delayVersion"]],
    MOMOgroups = s[["MOMOgroups"]][[1]],
    MOMOmodels = s[["MOMOmodels"]][[1]],
    verbose=FALSE)

  momo::RunMoMo()

  dataToSave <- rbindlist(momo::dataExport$toSave, fill=TRUE)

  data <- CleanExportedMOMOData(
    data=dataToSave,
    s=s
  )

  allPlotData[[i]] <- data
  allPlotData[[i]][,DoA:=s[["dateData"]]]
  allPlotData[[i]][,delayVersion:=s[["delayVersion"]]]
}

allPlotData <- rbindlist(allPlotData)[!is.na(excess)]


RunGraphsStatistics(
  runName=s[["runName"]],
  data=data,
  allPlotData=allPlotData,
  folder=s[["MOMOFolderResultsGraphsStatistics"]],
  yearWeek=RAWmisc::YearWeek(s[["dateDataMinusOneWeek"]]),
  dateData=max(s[["dateData"]][[1]])
)

### ANALYSES
pb <- RAWmisc::ProgressBarCreate(min=0,max=nrow(stackAnalyses),flush=TRUE)
allResults <- vector("list",100)

for(i in 1:nrow(stack)){
  RAWmisc::ProgressBarSet(pb,i)

  s <- stackAnalyses[i,]

  if(s[["runName"]]=="Norway"){
    dataAnalysis <- as.data.frame(masterData[!is.na(age),
                                             c("DoD","DoR","age"),with=F])
    plotGraphs <- TRUE
  } else {
    dataAnalysis <- as.data.frame(masterData[!is.na(age) & FYLKE==s[["fylke"]],
                                             c("DoD","DoR","age"),with=F])
    plotGraphs <- FALSE
  }
  #saveRDS(dataAnalysis,file=s[["data_clean_name"]])
  #saveRDS(dataAnalysis,file=DashboardFolder("data_clean","data.RDS"))
  #fwrite(dataAnalysis,file=DashboardFolder("data_clean","data.txt"))


  momo::SetOpts(
    DoA = s[["dateData"]],
    DoPR = as.Date("2012-1-1"),
    WStart = 1,
    WEnd = 52,
    country = s[["runName"]],
    source = "FHI",
    MDATA = dataAnalysis,
    HDATA = hfile,
    INPUTDIR = s[["MOMOFolderInput"]],
    WDIR = s[["MOMOFolderResults"]],
    back = 7,
    WWW = 290,
    Ysum = s[["MOMOYsum"]],
    Wsum = 40,
    plotGraphs = s[["plotGraphs"]],
    delayVersion = "2017-12",
    MOMOgroups = s[["MOMOgroups"]][[1]],
    MOMOmodels = s[["MOMOmodels"]][[1]],
    verbose=FALSE)

  momo::RunMoMo()

  dataToSave <- rbindlist(momo::dataExport$toSave, fill=TRUE)

  data <- CleanExportedMOMOData(
    data=dataToSave,
    s=s
  )

  allResults[[i]] <- dataToSave
  allResults[[i]][,name:=s[["runName"]]]

  saveRDS(data,s[["MOMOFolderResultsData"]])
  if(s[["runName"]]=="Norway"){
    saveRDS(data,DashboardFolder("data_app","data.RDS"))
  }

  RunGraphsDeaths(
    runName=s[["runName"]],
    data=data,
    folder=s[["MOMOFolderResultsGraphsWithUnreliable"]],
    yearWeek=RAWmisc::YearWeek(s[["dateDataMinusOneWeek"]]),
    dateData=max(s[["dateData"]][[1]]),
    dateReliable=max(s[["dateData"]][[1]])-7
  )

  RunGraphsDeaths(
    runName=s[["runName"]],
    data=data,
    folder=s[["MOMOFolderResultsGraphsDeleteUnreliable"]],
    yearWeek=RAWmisc::YearWeek(s[["dateDataMinusOneWeek"]]),
    dateData=max(s[["dateData"]][[1]]),
    dateReliable=max(s[["dateData"]][[1]])-CONFIG$WEEKS_UNRELIABLE*7
  )
}

allResults <- rbindlist(allResults)
cat(sprintf("%s/%s/R/NORMOMO Saving data_processed.xlsx",Sys.time(),Sys.getenv("COMPUTER")),"\n")
openxlsx::write.xlsx(allResults,DashboardFolder("results",file.path(RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),"data","data_processed.xlsx")))

## Grid graph
RunStatusTiles(allResults=allResults,
               folder=fhi::DashboardFolder("results",file.path(RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),"graphs_with_unreliable")),
               yearWeek=RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),
               dateData=info[["dateData"]])

RunStatusTiles(allResults=allResults,
               folder=fhi::DashboardFolder("results",file.path(RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),"graphs_delete_unreliable")),
               yearWeek=RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),
               dateData=info[["dateData"]])

SavingRawData(
  dateDataMinusOneWeek=info[["dateDataMinusOneWeek"]],
  masterData=masterData
)

cat(sprintf("%s/%s/R/NORMOMO Zipping results",Sys.time(),Sys.getenv("COMPUTER")),"\n")
ZipResults(
  folderResults=fhi::DashboardFolder("results"),
  folderResultsYearWeek=fhi::DashboardFolder("results",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]])),
  folderResultsZip=fhi::DashboardFolder("results",paste0("archive_",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),".zip")),
  folderDataAppZip=fhi::DashboardFolder("data_app",paste0("archive_",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),".zip"))
)

if(Sys.getenv("COMPUTER")=="smhb"){
  TEST_EMAILS <- FALSE
} else {
  TEST_EMAILS <- TRUE
}
EmailInternal(folderResultsYearWeek=file.path(fhi::DashboardFolder("results",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]))),
              isTest=TEST_EMAILS)
EmailSSI(folderResultsYearWeek=file.path(fhi::DashboardFolder("results",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]))),
         dateReliable=info$dateData-CONFIG$WEEKS_UNRELIABLE*7,
         isTest=TEST_EMAILS)

cat(sprintf("%s/%s/R/NORMOMO Exited successfully",Sys.time(),Sys.getenv("COMPUTER")),"\n")
quit(save="no", status=0)
