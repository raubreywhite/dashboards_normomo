
SMAOColourPalette <- c(
  "01totalorange"="#ff7f00",
  "02totalorange"="#fdbf6f",
  "03norwegianred"="#e31a1c",
  "04norwegianred"="#fb9a99",
  "05blue"="#1f78b4",
  "06blue"="#a6cee3",
  "07green"="#33a02c",
  "08green"="#b2df8a",
  "09pruple"="#6a3d9a",
  "10purple"="#cab2d6",
  "11brown"="#b15928",
  "12yellow"="#ffff99"
)

SMAOLabelsTotal <- "Totalt"
SMAOLabelsNorway <- c("Norge","Norsk","Norske","Norskf\u00F8dte")

SMAOFormatGGPlot <- function (q, sizeMultiplier = 3, legendKey = 3, xAngle = 0, stripes = TRUE, legendPos="bottom", ncol=3, legendBorder=FALSE, reverse=FALSE)
{
  q <- q + theme(axis.ticks = element_line(colour = "black"))
  q <- q + theme(panel.background = element_rect(colour = "white",
                                                 fill = "white"))
  q <- q + theme(axis.line = element_line(colour = "black",
                                          size = 0.5 * sizeMultiplier))
  q <- q + theme(axis.line.x = element_line(colour = "black",
                                          size = 0.5 * sizeMultiplier))
  q <- q + theme(axis.line.y = element_line(colour = "black",
                                          size = 0.5 * sizeMultiplier))
  q <- q + theme(strip.background = element_blank())
  q <- q + theme(panel.grid.major = element_blank())
  q <- q + theme(panel.grid.minor = element_blank())
  if (stripes) {
    q <- q + theme(panel.grid.major = element_line(colour = "black",
                                                   size = 0.25 * sizeMultiplier, linetype = 3))
    q <- q + theme(panel.grid.minor = element_line(colour = "black",
                                                   size = 0.25 * sizeMultiplier, linetype = 3))
  }
  q <- q + theme(legend.key.size = unit(legendKey, "lines"))
  q <- q + theme(legend.key = element_blank())
  if(legendBorder) q <- q + theme(legend.key = element_rect(colour = "black"))
  q <- q + theme(axis.title.x = element_text(size = 10 * sizeMultiplier,
                                             vjust = 0, colour = "black"))
  q <- q + theme(axis.title.y = element_text(size = 10 * sizeMultiplier,
                                             angle = 90, vjust = 0.25, colour = "black"))
  q <- q + theme(axis.text.y = element_text(size = 10 * sizeMultiplier,
                                            hjust = 1, vjust = 0.4, colour = "black"))
  q <- q + theme(axis.text.x = element_text(size = 10 * sizeMultiplier,
                                            hjust = 0.5, vjust = 1, colour = "black", angle = xAngle))
  if (xAngle != 0) {
    q <- q + theme(axis.text.x = element_text(size = 10 *
                                                sizeMultiplier, hjust = 0, vjust = 0.5, colour = "black",
                                              angle = xAngle))
  }
  q <- q + theme(strip.text.y = element_text(size = 10 * sizeMultiplier,
                                             hjust = 0.5, colour = "black"))
  q <- q + theme(strip.text.x = element_text(size = 10 * sizeMultiplier,
                                             hjust = 0.5, colour = "black"))
  q <- q + theme(legend.text = element_text(size = 10 * sizeMultiplier,
                                            hjust = 0.5, colour = "black"))
  q <- q + theme(legend.title = element_text(size = 10 * sizeMultiplier,
                                             hjust = 0.5, colour = "black"))
  q <- q + theme(legend.position = "right")
  q <- q + theme(plot.margin = unit(c(0.5, 0.5, 1, 0.5), "lines"))
  q <- q + theme(plot.title = element_text(size = 14 * sizeMultiplier,
                                           hjust = 0.5, vjust = 1))
  q <- q + theme(legend.position=legendPos)
  q <- q + guides(fill = guide_legend(ncol = ncol, byrow=TRUE, title.position="top", reverse=reverse, order=1))
  q <- q + guides(colour = guide_legend(ncol = ncol, byrow=TRUE, title.position="top", reverse=reverse, order=2))
  q <- q + guides(shape = guide_legend(ncol = ncol, byrow=TRUE, title.position="top", reverse=reverse, order=3))

  return(q)
}

SMAOPriorityLevels <- function(useLabels){
  if(!is.factor(useLabels)){
    useLabels <- as.factor(useLabels)
  }
  useLabels <- levels(useLabels)
  useValues <- SMAOColourPalette
  usedLabels <- rep(FALSE,length(useLabels))
  usedValues <- rep(FALSE,length(useValues))

  if(sum(useLabels %in% SMAOLabelsTotal)>0){
    loc <- min(which(useLabels %in% SMAOLabelsTotal))
    names(useValues)[1] <- useLabels[loc]
    usedLabels[loc] <- TRUE
    usedValues[1] <- TRUE
  }
  if(sum(useLabels %in% SMAOLabelsNorway)>0){
    loc <- min(which(useLabels %in% SMAOLabelsNorway))
    names(useValues)[2] <- useLabels[loc]
    usedLabels[loc] <- TRUE
    usedValues[2] <- TRUE
  }
  j <- 5
  for(i in 1:length(usedLabels)){
    if(usedLabels[i]) next
    names(useValues)[j] <- useLabels[i]
    usedLabels[i] <- TRUE
    usedValues[j] <- TRUE
    j <- j + 1
  }
  values=useValues[usedValues]
  labels=names(useValues)[usedValues]
  return(labels)
}

SMAOColourSpecify <- function(useLabels, total=NULL, total2=NULL, norway=NULL, norway2=NULL, type="colour", lab=""){
  if(!is.factor(useLabels)) stop("THIS IS NOT A FACTOR, REORGANISING WILL DISPLAY WRONG COLOURS")

  origData <- unique(useLabels)
  useLabels <- levels(useLabels)
  useValues <- SMAOColourPalette
  usedLabels <- rep(FALSE,length(useLabels))
  usedValues <- rep(FALSE,length(useValues))

  values <- rep("black", length(useLabels))
  if(!is.null(total)){
    values[total] <- useValues[1]
    usedLabels[total] <- TRUE
  }
  if(!is.null(total2)){
    values[total2] <- useValues[2]
    usedLabels[total2] <- TRUE
  }
  if(!is.null(norway)){
    values[norway] <- useValues[3]
    usedLabels[norway] <- TRUE
  }
  if(!is.null(norway2)){
    values[norway2] <- useValues[4]
    usedLabels[norway2] <- TRUE
  }
  j <- 5
  if(sum(!usedLabels)>0){
    for(i in 1:length(useLabels)){
      if(usedLabels[i]) next
      values[i] <- useValues[j]
      j <- j + 1
    }
  }

  labels=useLabels
  values <- values[labels %in% origData]
  labels <- labels[labels %in% origData]

  palette <- list("values"=values,"labels"=labels)

  fn <- scale_colour_manual
  if(type=="fill"){
    fn <- scale_fill_manual
  }
  return(fn(lab,labels=palette$labels,values=palette$values))
}

SMAOColourYears <- function(useLabels, type="colour", lab=""){
  if(!is.factor(useLabels)){
    useLabels <- as.factor(useLabels)
  }
  useLabels <- levels(useLabels)

  fn <- scale_colour_brewer
  if(type=="fill"){
    fn <- scale_fill_brewer
  }

  return(fn(lab,palette="YlOrRd"))
}



ConvertDate.int <- function(date="01jan2008"){
  day <- as.numeric(substr(date,1,2))
  month <- substr(date,3,5)
  year <- as.numeric(substr(date,6,9))

  if(month=="jan"){
    month <- 1
  } else if(month=="feb"){
    month <- 2
  } else if(month=="mar"){
    month <- 3
  } else if(month=="apr"){
    month <- 4
  } else if(month=="may"){
    month <- 5
  } else if(month=="jun"){
    month <- 6
  } else if(month=="jul"){
    month <- 7
  } else if(month=="aug"){
    month <- 8
  } else if(month=="sep"){
    month <- 9
  } else if(month=="oct"){
    month <- 10
  } else if(month=="nov"){
    month <- 11
  } else if(month=="dec"){
    month <- 12
  }
  dateFixed <- as.Date(paste0(year,"-",month,"-",day))
  return(dateFixed)
}

ConvertDate <- Vectorize(ConvertDate.int)

xGraphRecent <- function(data,title="",norwegian=TRUE,includeRealDeaths=FALSE){
  plottingData <- data[wk>=max(wk)-52]

  if(norwegian){
    filllabels1=c("Prediksjonsintervall","Betydelig h\u00F8yere enn forventet","H\u00F8yere enn forventet","Forventet","Lavere enn forventet")
    filllabels2=c("Prediksjonsintervall","H\u00F8yere enn forventet","Forventet","Lavere enn forventet")
    shapelabels=c("Forel\u00F8pige tall")
    colourlabels=c("Korrigert for forsinkelse","Rapporterte d\u00F8dsfall")
    ylabel="Antall d\u00F8de per uke\n"
  } else {
    filllabels1=c("Prediction interval","Significantly higher than expected","Higher than expected","Expected","Lower than expected")
    filllabels2=c("Prediction interval","Higher than expected","Expected","Lower than expected")
    shapelabels=c("Preliminary numbers")
    colourlabels=c("Corrected for delays","Reported deaths")
    ylabel="Deaths per week\n"
  }

  if(sum(names(data)=="UPIb4")==1){
    plottingData[,ymax := max(nbc,UPIb4)]
    plottingData[,ymin := min(nbc,UPIb4)]
    plottingData[,Lower:=Pnb-abs(UPIb2-Pnb)]
    plottingData[Lower<0, Lower := 0]
    plottingData[,unstableEstimates:="Stable"]
    plottingData[wk>=max(wk)-7,unstableEstimates:="Unstable"]

    breaks <- plottingData[,c("WoDi","YoDi","wk"),with=FALSE]
    breaks <- breaks[seq(1,53,4)]
    breaks[, label := paste(gsub(" ","0",format(WoDi,width=2)),"/",YoDi,sep="")]

    q <- ggplot(plottingData, aes(x=wk))
    q <- q + geom_ribbon(aes(ymin=-Inf,ymax=Lower, fill="4lower"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=Lower,ymax=UPIb2, fill="3expected"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=UPIb2,ymax=UPIb4, fill="2high"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=UPIb4,ymax=Inf, fill="1veryhigh"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=LPIc,ymax=UPIc, fill="0predinterval"),alpha=0.35)
    if(includeRealDeaths) q <- q + geom_line(aes(y=nb,colour="Rapporterte"),lwd=1)
    q <- q + geom_line(aes(y=nbc,colour="Korrigert"),lwd=1)
    q <- q + geom_point(data=plottingData[unstableEstimates=="Unstable"],aes(y=nbc,shape="Usikkert"),size=4)
    q <- q + labs(title=title)
    q <- q + scale_x_continuous("",breaks=breaks$wk, labels=breaks$label)
    q <- q + scale_y_continuous(ylabel)
    q <- q + scale_fill_manual("",
                               values=c("0predinterval"="#636363","1veryhigh"="#fc8d59","2high"="#ffffbf","3expected"="#91bfdb","4lower"="white"),
                               labels=filllabels1)
    q <- q + scale_shape_manual("",
                                 values=c("Usikkert"=16),
                                 labels=shapelabels)
    q <- q + scale_colour_manual("",
                                 values=c("Korrigert"="black","Rapporterte"="red"),
                                 labels=colourlabels)
    q
    #q
  } else {
    plottingData[,ymax := max(nbc,UPIb2)]
    plottingData[,ymin := min(nbc,UPIb2)]
    plottingData[,Lower:=LCIb-abs(UPIb2-LCIb)]
    plottingData[Lower<0, Lower := 0]
    plottingData[,unstableEstimates:="Stable"]
    plottingData[wk>=max(wk)-7,unstableEstimates:="Unstable"]

    breaks <- plottingData[,c("WoDi","YoDi","wk"),with=FALSE]
    breaks <- breaks[seq(1,53,4)]
    breaks[, label := paste(gsub(" ","0",format(WoDi,width=2)),"/",YoDi,sep="")]


    q <- ggplot(plottingData, aes(x=wk))
    q <- q + geom_ribbon(aes(ymin=-Inf,ymax=Lower, fill="4lower"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=Lower,ymax=UPIb2, fill="3expected"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=UPIb2,ymax=Inf, fill="2high"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=LPIc,ymax=UPIc, fill="0predinterval"),alpha=0.35)
    if(includeRealDeaths) q <- q + geom_line(aes(y=nb,colour="Rapporterte"),lwd=1)
    q <- q + geom_line(aes(y=nbc,colour="Korrigert"),lwd=1)
    q <- q + geom_point(data=plottingData[unstableEstimates=="Unstable"],aes(y=nbc,shape="Usikkert"),size=4)
    q <- q + labs(title=title)
    q <- q + scale_x_continuous("",breaks=breaks$wk, labels=breaks$label)
    q <- q + scale_y_continuous(ylabel)
    q <- q + scale_fill_manual("",
                               values=c("0predinterval"="#636363","1veryhigh"="#fc8d59","2high"="#ffffbf","3expected"="#91bfdb","4lower"="white"),
                               labels=filllabels2)
    q <- q + scale_shape_manual("",
                                values=c("Usikkert"=16),
                                labels=shapelabels)
    q <- q + scale_colour_manual("",
                                 values=c("Korrigert"="black","Rapporterte"="red"),
                                 labels=colourlabels)
    #q
  }

  q <- SMAOFormatGGPlot(q, legendPos="right", xAngle=90,ncol=1,legendBorder=TRUE)
  #q <- format_plot(q,2,2,stripes=TRUE, xangle=90)
  return(q)
}

#' @importFrom stats na.omit
xGraphHistoric <- function(data,title="",norwegian=TRUE,includeRealDeaths=FALSE){
  plottingData <- data[wk>=max(wk)-52*5+1]

  if(norwegian){
    filllabels1=c("Betydelig h\u00F8yere enn forventet","H\u00F8yere enn forventet","Forventet","Lavere enn forventet")
    filllabels2=c("H\u00F8yere enn forventet","Forventet","Lavere enn forventet")
    shapelabels=c("Forel\u00F8pige tall")
    colourlabels=c("Korrigert for forsinkelse","Rapporterte d\u00F8dsfall")
    ylabel="Antall d\u00F8de per uke\n"
  } else {
    filllabels1=c("Significantly higher than expected","Higher than expected","Expected","Lower than expected")
    filllabels2=c("Higher than expected","Expected","Lower than expected")
    shapelabels=c("Preliminary numbers")
    colourlabels=c("Corrected for delays","Reported deaths")
    ylabel="Deaths per week\n"
  }

  if(sum(names(data)=="UPIb4")==1){
    plottingData[,ymax := max(nbc,UPIb4)]
    plottingData[,ymin := min(nbc,UPIb4)]
    plottingData[,Lower:=Pnb-abs(UPIb2-Pnb)]
    plottingData[Lower<0, Lower := 0]
    plottingData[,unstableEstimates:="Stable"]
    plottingData[wk>=max(wk)-7,unstableEstimates:="Unstable"]

    breaks <- data.frame(plottingData[,c("wk","YoDi"),with=FALSE])
    breaks$YoDi2 <- NA
    breaks$YoDi2[2:(nrow(breaks)-0)] <- breaks$YoDi[1:(nrow(breaks)-1)]
    breaks <- na.omit(breaks[breaks$YoDi!=breaks$YoDi2,])
    breaks$label <- paste("1/1/",breaks$YoDi,sep="")

    q <- ggplot(plottingData, aes(x=wk))
    q <- q + geom_ribbon(aes(ymin=-Inf,ymax=Lower, fill="4lower"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=Lower,ymax=UPIb2, fill="3expected"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=UPIb2,ymax=UPIb4, fill="2high"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=UPIb4,ymax=Inf, fill="1veryhigh"),alpha=0.7)
    if(includeRealDeaths) q <- q + geom_line(aes(y=nb,colour="Rapporterte"),lwd=1)
    q <- q + geom_line(aes(y=nbc,colour="Korrigert"),lwd=1)
    q <- q + geom_point(data=plottingData[unstableEstimates=="Unstable"],aes(y=nbc,shape="Usikkert"),size=4)
    q <- q + labs(title=title)
    q <- q + scale_x_continuous("",breaks=breaks$wk, labels=breaks$label)
    q <- q + scale_y_continuous(ylabel)
    q <- q + scale_fill_manual("",
                               values=c("1veryhigh"="#fc8d59","2high"="#ffffbf","3expected"="#91bfdb","4lower"="white"),
                               labels=filllabels1)
    q <- q + scale_shape_manual("",
                                values=c("Usikkert"=16),
                                labels=shapelabels)
    q <- q + scale_colour_manual("",
                                 values=c("Korrigert"="black","Rapporterte"="red"),
                                 labels=colourlabels)
  } else {
    plottingData[,ymax := max(nbc,UPIb2)]
    plottingData[,ymin := min(nbc,UPIb2)]
    plottingData[,Lower:=LCIb-abs(UPIb2-LCIb)]
    plottingData[Lower<0, Lower := 0]
    plottingData[,unstableEstimates:="Stable"]
    plottingData[wk>=max(wk)-7,unstableEstimates:="Unstable"]

    breaks <- data.frame(plottingData[,c("wk","YoDi"),with=FALSE])
    breaks$YoDi2 <- NA
    breaks$YoDi2[2:(nrow(breaks)-0)] <- breaks$YoDi[1:(nrow(breaks)-1)]
    breaks <- na.omit(breaks[breaks$YoDi!=breaks$YoDi2,])
    breaks$label <- paste("1/1/",breaks$YoDi,sep="")

    q <- ggplot(plottingData, aes(x=wk))
    q <- q + geom_ribbon(aes(ymin=-Inf,ymax=Lower, fill="4lower"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=Lower,ymax=UPIb2, fill="3expected"),alpha=0.7)
    q <- q + geom_ribbon(aes(ymin=UPIb2,ymax=Inf, fill="2high"),alpha=0.7)
    if(includeRealDeaths) q <- q + geom_line(aes(y=nb,colour="Rapporterte"),lwd=1)
    q <- q + geom_line(aes(y=nbc,colour="Korrigert"),lwd=1)
    q <- q + geom_point(data=plottingData[unstableEstimates=="Unstable"],aes(y=nbc,shape="Usikkert"),size=4)
    q <- q + labs(title=title)
    q <- q + scale_x_continuous("",breaks=breaks$wk, labels=breaks$label)
    q <- q + scale_y_continuous(ylabel)
    q <- q + scale_fill_manual("",
                               values=c("2high"="#ffffbf","3expected"="#91bfdb","4lower"="white"),
                               labels=filllabels2)
    q <- q + scale_shape_manual("",
                                values=c("Usikkert"=16),
                                labels=shapelabels)
    q <- q + scale_colour_manual("",
                                 values=c("Korrigert"="black","Rapporterte"="red"),
                                 labels=colourlabels)
  }
  q <- SMAOFormatGGPlot(q, legendPos="right", xAngle=90,ncol=1,legendBorder=TRUE)
  #q <- format_plot(q,2,2,stripes=TRUE, xangle=90)
  return(q)
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

#' @import data.table
#' @import caTools
GraphDelay <- function(data,folder){
  data[,death:=ConvertDate(DoD)]
  data[,reg:=ConvertDate(DoR)]
  data[,ddeath:=as.Date(death,origin="1970-01-01")]
  data[,dreg:=as.Date(reg,origin="1970-01-01")]

  data[,delay:=as.numeric(dreg-ddeath)]
  data[,year:=format.Date(ddeath,"%G")]
  data[,week:=format.Date(ddeath,"%V")]
  data[,isWithin7:=0]
  data[delay<7,isWithin7:=1]
  data[,isWithin14:=0]
  data[delay<14,isWithin14:=1]
  data[,isWithin21:=0]
  data[delay<21,isWithin21:=1]
  data[,week:=as.numeric(week)]

  res <- data[ddeath>="2013-01-01" & week %in% c(1:52),.(isWithin7=sum(isWithin7),isWithin14=sum(isWithin14),isWithin21=sum(isWithin21)),
              by=.(year,week)]
  keep <- 1:(nrow(res)-3)
  res <- res[keep]

  res[,perc7:=isWithin7/isWithin21]
  res[,perc14:=isWithin14/isWithin21]
  res[,smoothed7:=caTools::runmean(perc7,3)]
  res[,smoothed14:=caTools::runmean(perc14,3)]

  res[,smoothedNum7:=caTools::runmean(isWithin7,3)]
  res[,smoothedNum14:=caTools::runmean(isWithin14,3)]
  res[,smoothedNum21:=caTools::runmean(isWithin21,3)]


  q <- ggplot(res,aes(x=week,y=smoothedNum7,colour=year))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + geom_hline(yintercept=0,alpha=0)
  q <- q + labs(title="Number of deaths reported within 7 days (mean of 3 weeks moving window)")
  q <- q + scale_x_continuous("Week")
  q <- q + scale_y_continuous("Number")
  fhi::SMAOpng(paste0(folder,"/reporting_number_7days.png"),w=0.5,h=0.5)
  print(q)
  dev.off()

  q <- ggplot(res,aes(x=week,y=smoothedNum14,colour=year))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + geom_hline(yintercept=0,alpha=0)
  q <- q + labs(title="Number of deaths reported within 14 days (mean of 3 weeks moving window)")
  q <- q + scale_x_continuous("Week")
  q <- q + scale_y_continuous("Number")
  fhi::SMAOpng(paste0(folder,"/reporting_number_14days.png"),w=0.5,h=0.5)
  print(q)
  dev.off()

  q <- ggplot(res,aes(x=week,y=smoothedNum21,colour=year))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + geom_hline(yintercept=0,alpha=0)
  q <- q + labs(title="Number of deaths reported within 21 days (mean of 3 weeks moving window)")
  q <- q + scale_x_continuous("Week")
  q <- q + scale_y_continuous("Number")
  fhi::SMAOpng(paste0(folder,"/reporting_number_21days.png"),w=0.5,h=0.5)
  print(q)
  dev.off()

  q <- ggplot(res,aes(x=week,y=smoothed7*100,colour=year))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + geom_hline(yintercept=0,alpha=0)
  q <- q + labs(title="Percentage of deaths reported within 7 days compared to 21 days (mean of 3 weeks moving window)")
  q <- q + scale_x_continuous("Week")
  q <- q + scale_y_continuous("Percentage")
  fhi::SMAOpng(paste0(folder,"/reporting_percentage_7days.png"),w=0.5,h=0.5)
  print(q)
  dev.off()

  q <- ggplot(res,aes(x=week,y=smoothed14*100,colour=year))
  q <- q + geom_line()
  q <- q + geom_point()
  q <- q + geom_hline(yintercept=0,alpha=0)
  q <- q + labs(title="Percentage of deaths reported within 14 days compared to 21 days (mean of 3 weeks moving window)")
  q <- q + scale_x_continuous("Week")
  q <- q + scale_y_continuous("Percentage")
  fhi::SMAOpng(paste0(folder,"/reporting_percentage_14days.png"),w=0.5,h=0.5)
  print(q)
  dev.off()


}

#' Run Temporary Graphs
#'
#' Running graphs
#' @param runName Name
#' @param masterData masterData
#' @param folder a
#' @param yearWeek a
#' @param dateData a
#' @import stringr
#' @importFrom RAWmisc MakeFootnote
#' @importFrom fhi SMAOpng
#' @import data.table
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices dev.off
#' @export RunTemporaryGraphs
RunTemporaryGraphs <- function(runName="Norway",
                               masterData,
                               folder=fhi::DashboardFolder("results",file.path(RAWmisc::YearWeek(),"Graphs")),
                               yearWeek=RAWmisc::YearWeek(),
                               dateData=Sys.time()){

  #### NORWEGIAN

  storedData <- list()
  if(runName=="Norway"){
    runList <- c("Total","0to4","5to14","15to64","65P")
  } else runList <- "Total"
  for(i in runList){
    if(i=="Total"){
      title1 <- "Totalt antall d\u00F8de per uke siste \u00E5r"
      title2 <- "Totalt antall d\u00F8de per uke siste 5 \u00E5r"
    } else if(i=="0to4"){
      title1 <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r"
      title2 <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste 5 \u00E5r"
    } else if(i=="5to14"){
      title1 <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r"
      title2 <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
    } else if(i=="15to64"){
      title1 <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r"
      title2 <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
    } else if(i=="65P"){
      title1 <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r"
      title2 <- "Antall (65+ \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
    }

    data <- masterData[GROUP==i]
    data <- data[!is.na(excess)]
    storedData[[i]] <- data

    fhi::SMAOpng(paste0(folder,"/excl_reported_",runName,"-",i,"-", yearWeek,".png"))
    gridExtra::grid.arrange(
      xGraphRecent(data,title=title1,includeRealDeaths=FALSE),
      xGraphHistoric(data,title=title2,includeRealDeaths=FALSE), ncol=1)
    RAWmisc::MakeFootnote(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""), size=1.5)
    dev.off()

    fhi::SMAOpng(paste0(folder,"/incl_reported_",runName,"-",i,"-", yearWeek,".png"))
    gridExtra::grid.arrange(
      xGraphRecent(data,title=title1,includeRealDeaths=TRUE),
      xGraphHistoric(data,title=title2,includeRealDeaths=FALSE), ncol=1)
    RAWmisc::MakeFootnote(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""), size=1.5)
    dev.off()
  }


}
