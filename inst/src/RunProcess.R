fhi::DashboardInitialiseOpinionated("normomo", PACKAGE_DIR=".")
fd::initialize("normomo")
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))

# Set up data
if (!dir.exists("/data_raw/sykdomspuls/normomo")) dir.create("/data_raw/sykdomspuls/normomo")

hfile <- data.frame(readxl::read_excel(system.file("extdata", "bank_holidays.xlsx", package = "normomo"))[,c("date", "closed")])
hfile$date <- as.Date(hfile$date)
#fwrite(hfile,file=fhi::DashboardFolder("data_clean","bank_holidays.txt"))

info <- GetDataInfo()

masterData <- GetData(
  fDone=info[["fDone"]],
  f=info[["f"]],
  forceRun=fhi::DashboardIsDev()
  )

# Set up folders
SetupFolders(dateDataMinusOneWeek=info[["dateDataMinusOneWeek"]])

model <- standard$new()
results_x <- model$results_x
model$run_all(masterData=masterData, info=info)

EmailInternal(folderResultsYearWeek=file.path(fhi::DashboardFolder("results",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]))))

CreateLatestDoneFile(f=info$f)

fhi::DashboardMsg("Exited successfully")
if(!fhi::DashboardIsDev()) quit(save="no", status=0)


# Plan out analyses
stack <- GenerateStack(
  f=info[["f"]],
  dateDataMinusOneWeek=info[["dateDataMinusOneWeek"]],
  dateData=info[["dateData"]]
  )

stackStatistics <- stack[["stackStatistics"]]
stackAnalyses <- stack[["stackAnalyses"]]

#### STATISTICS

fhi::DashboardMsg("STARTING STATISTICS")

pb <- RAWmisc::ProgressBarCreate(min=0,max=nrow(stackStatistics),flush=fhi::DashboardIsDev())
allPlotData <- vector("list",length=nrow(stackStatistics))
dataAnalysis <- as.data.frame(masterData[!is.na(age),c("DoD","DoR","age"),with=F])

for(i in 1:nrow(stackStatistics)){
  RAWmisc::ProgressBarSet(pb,i)

  s <- stackStatistics[i,]

  MOMO::SetOpts(
    DoA = s[["dateData"]],
    DoPR = as.Date("2012-1-1"),
    WStart = 1,
    WEnd = 52,
    country = s[["runName"]],
    source = "FHI",
    MDATA = dataAnalysis,
    HDATA = hfile,
    INPUTDIR = s[["MOMOFolderInput"]],
    WDIR = fhi::temp_dir(),
    back = 7,
    WWW = 290,
    Ysum = s[["MOMOYsum"]],
    Wsum = 40,
    plotGraphs = FALSE,
    delayVersion = s[["delayVersion"]],
    MOMOgroups = s[["MOMOgroups"]][[1]],
    MOMOmodels = s[["MOMOmodels"]][[1]],
    verbose=FALSE)

  MOMO::RunMoMo()

  dataToSave <- rbindlist(MOMO::dataExport$toSave, fill=TRUE)

  #dataToSave[GROUP=="Total" & nbc!=nb]

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
  allPlotData=allPlotData,
  folder=s[["MOMOFolderResultsGraphsStatistics"]],
  yearWeek=RAWmisc::YearWeek(s[["dateDataMinusOneWeek"]]),
  dateData=max(s[["dateData"]][[1]])
)

### ANALYSES
fhi::DashboardMsg("STARTING ANALYSES")

pb <- RAWmisc::ProgressBarCreate(min=0,max=nrow(stackAnalyses),flush=TRUE)
allResults <- vector("list",100)

for(i in 1:nrow(stackAnalyses)){
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


  MOMO::SetOpts(
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
    delayVersion = "richard",
    delayFunction = custom_delayMOMO_richard,
    MOMOgroups = s[["MOMOgroups"]][[1]],
    MOMOmodels = s[["MOMOmodels"]][[1]],
    verbose=FALSE)

  MOMO::RunMoMo()

  dataToSave <- rbindlist(MOMO::dataExport$toSave, fill=TRUE)

  data <- CleanExportedMOMOData(
    data=dataToSave,
    s=s
  )

  allResults[[i]] <- dataToSave
  allResults[[i]][,name:=s[["runName"]]]

  saveRDS(data,s[["MOMOFolderResultsData"]])
  saveRDS(data,s[["SykdomspulsFolderResultsData"]])
  if(s[["runName"]]=="Norway"){
    saveRDS(data,fhi::DashboardFolder("data_app","data.RDS"))
  }

  RunGraphsDeaths(
    runName=s[["runName"]],
    data=data,
    folder=s[["MOMOFolderResultsGraphsStatus"]],
    yearWeek=RAWmisc::YearWeek(s[["dateDataMinusOneWeek"]]),
    dateData=max(s[["dateData"]][[1]]),
    dateReliable=max(s[["dateData"]][[1]])-7
  )

}

allResults <- rbindlist(allResults)
fhi::DashboardMsg("Saving data_processed.xlsx")
openxlsx::write.xlsx(allResults,fhi::DashboardFolder("results",file.path(RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),"data","data_processed.xlsx")))

## Grid graph
RunStatusTiles(allResults=allResults,
               folder=s[["MOMOFolderResultsGraphsStatus"]],
               yearWeek=RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),
               dateData=info[["dateData"]])

SavingRawData(
  dateDataMinusOneWeek=info[["dateDataMinusOneWeek"]],
  masterData=masterData
)

fhi::DashboardMsg("Zipping results")
ZipResults(
  folderResults=fhi::DashboardFolder("results"),
  folderResultsYearWeek=fhi::DashboardFolder("results",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]])),
  folderResultsZip=fhi::DashboardFolder("results",paste0("archive_",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),".zip")),
  folderDataAppZip=fhi::DashboardFolder("data_app",paste0("archive_",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),".zip"))
)

EmailInternal(folderResultsYearWeek=file.path(fhi::DashboardFolder("results",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]))))
EmailSSI(folderResultsYearWeek=file.path(fhi::DashboardFolder("results",RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]))),
         dateReliable=info$dateData-CONFIG$WEEKS_UNRELIABLE*7)

CreateLatestDoneFile(f=info$f)

fhi::DashboardMsg("Exited successfully")
if(!fhi::DashboardIsDev()) quit(save="no", status=0)
