GraphTogether <- function(
  data,
  norwegian=TRUE,
  title1=NULL,
  title1a=NULL,
  title1b=NULL,
  title2,
  includeRealDeaths=FALSE,
  dateData,
  dateReliable
  ){
  if(!is.null(title1)){
    plottingData1 <- data[wk>=max(wk)-52]
    plottingData2 <- data[wk>=max(wk)-52*5+1]

    plottingData1[,titlex:=title1]
    plottingData2[,titlex:=title2]

    plottingData1[,type:="top"]
    plottingData2[,type:="bottom"]

    plottingData <- rbind(plottingData1,plottingData2)
    plottingData[,titlex:=factor(titlex,levels=c(title1,title2))]
  } else {
    plottingData1a <- data[wk>=max(wk)-52]
    plottingData1b <- data[wk>=max(wk)-52]
    plottingData2 <- data[wk>=max(wk)-52*5+1]

    plottingData1a[,titlex:=title1a]
    plottingData1b[,titlex:=title1b]
    plottingData2[,titlex:=title2]

    plottingData1a[,type:="top"]
    plottingData1b[,type:="top"]
    plottingData2[,type:="bottom"]

    plottingData <- rbind(plottingData1a,plottingData1b,plottingData2)
    plottingData[,titlex:=factor(titlex,levels=c(title1a,title1b,title2))]
  }

  plottingData[,ymax := max(nbc,UPIb4)]
  plottingData[,ymin := min(nbc,UPIb4)]
  plottingData[,Lower:=Pnb-abs(UPIb2-Pnb)]
  plottingData[Lower<0, Lower := 0]
  plottingData[,unstableEstimates:="Stable"]
  plottingData[wk>=max(wk)-7,unstableEstimates:="Unstable"]

  plottingData[,wkSplit:=wk]
  plottingData[type=="bottom",wkSplit:=wk*10]

  breaks <- unique(plottingData[,c("WoDi","YoDi","wk"),with=FALSE])
  breaksTop <- breaks[seq(1,53,4)]
  breaksTop[, label := paste(gsub(" ","0",format(WoDi,width=2)),"/",YoDi,sep="")]

  breaks <- unique(plottingData[,c("wk","YoDi"),with=FALSE])
  setorder(breaks,wk)
  breaks[,YoDi2:=shift(YoDi)]
  breaksBottom <- na.omit(breaks[breaks$YoDi!=breaks$YoDi2,])
  breaksBottom$label <- paste("1/",breaksBottom$YoDi,sep="")
  breaksBottom[,wk:=wk*10]

  breaks <- rbind(breaksTop[,c("wk","label")],breaksBottom[,c("wk","label")])

  if(norwegian){
    filllabels1=c("Prediksjonsintervall","Betydelig h\u00F8yere enn forventet","H\u00F8yere enn forventet","Forventet","Lavere enn forventet")
    shapelabels=c("Forel\u00F8pig")
    colourlabels=c("Korrigert for forsinkelse","Rapporterte d\u00F8dsfall")
    ylabel="Antall d\u00F8de per uke"
  } else {
    filllabels1=c("Prediction interval","Significantly higher than expected","Higher than expected","Expected","Lower than expected")
    filllabels2=c("Prediction interval","Higher than expected","Expected","Lower than expected")
    shapelabels=c("Preliminary numbers")
    colourlabels=c("Corrected for delays","Reported deaths")
    ylabel="Deaths per week"
  }

  q <- ggplot(plottingData[wk2<=RAWmisc::YearWeek(dateReliable)], aes(x=wkSplit))
  q <- q + geom_ribbon(aes(ymin=-Inf,ymax=Lower, fill="5lower"),alpha=0.7)
  q <- q + geom_ribbon(aes(ymin=Lower,ymax=UPIb2, fill="4expected"),alpha=0.7)
  q <- q + geom_ribbon(aes(ymin=UPIb2,ymax=UPIb4, fill="3high"),alpha=0.7)
  q <- q + geom_ribbon(aes(ymin=UPIb4,ymax=Inf, fill="2veryhigh"),alpha=0.7)
  q <- q + geom_ribbon(data=plottingData[wk2<=RAWmisc::YearWeek(dateReliable)  & unstableEstimates=="Unstable" & type=="top"],mapping=aes(ymin=LPIc,ymax=UPIc, fill="1predinterval"),alpha=0.3)
  if(includeRealDeaths) q <- q + geom_line(data=plottingData[wk2<=RAWmisc::YearWeek(dateReliable) & titlex %in% c(title1,title1a)],mapping=aes(y=nb,colour="Rapporterte"),lwd=0.5)
  q <- q + geom_line(aes(y=nbc,colour="Korrigert"),lwd=0.5)
  q <- q + geom_point(data=plottingData[wk2<=RAWmisc::YearWeek(dateReliable) & unstableEstimates=="Unstable"],aes(y=nbc,shape="Usikkert"),size=2)
  q <- q + facet_wrap(~titlex,scales="free",ncol=1)
  #q <- q + labs(title=title)
  q <- q + scale_x_continuous("",breaks=breaks$wk, labels=breaks$label)
  q <- q + scale_y_continuous(ylabel)
  q <- q + scale_fill_manual("",
                             values=c("1predinterval"="#636363","2veryhigh"="#fc8d59","3high"="#ffffbf","4expected"="#91bfdb","5lower"="white"),
                             labels=filllabels1)
  q <- q + scale_shape_manual("",
                              values=c("Usikkert"=16),
                              labels=shapelabels)
  q <- q + scale_colour_manual("",
                               values=c("Korrigert"="black","Rapporterte"="red"),
                               labels=colourlabels)
  q <- q + theme_gray(base_size=18)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  #q <- q + theme(panel.grid.major = element_line(colour = "white"),
  #               panel.grid.minor = element_line(colour = "white", size = 0.25))
  q <- q + guides(fill = guide_legend(title.position="top", reverse=F, order=1, ncol=1))
  q <- q + guides(colour = guide_legend(title.position="top", reverse=F, order=2, ncol=1))
  q <- q + guides(shape = guide_legend(title.position="top", reverse=F, order=3, ncol=1))

  if(!is.null(title1)){
    q <- q + theme(legend.position = "right")
  } else {
    q <- q + theme(legend.position = "bottom")
  }
  #q <- SMAOFormatGGPlot(q, legendPos="right", xAngle=90,ncol=1,legendBorder=TRUE)
  #q <- format_plot(q,2,2,stripes=TRUE, xangle=90)
  return(q)
}

GraphPreviousWeeks <- function(plotData,title){
  plottingData <- plotData[wk>=max(wk)-52]

  vlines <- max(plottingData$wk)
  vlines <- vlines:(vlines-8)

  breaks <- unique(plottingData[,c("WoDi","YoDi","wk"),with=FALSE])
  breaks <- breaks[seq(1,53,2)]
  breaks[, label := paste(gsub(" ","0",format(WoDi,width=2)),"/",YoDi,sep="")]

  plottingData[,ymax := max(nbc,UPIb4)]
  plottingData[,ymin := min(nbc,UPIb4)]
  plottingData[,Lower:=Pnb-abs(UPIb2-Pnb)]
  plottingData[Lower<0, Lower := 0]

  filllabels1=c("Prediksjonsintervall","Betydelig h\u00F8yere enn forventet","H\u00F8yere enn forventet","Forventet","Lavere enn forventet")
  filllabels2=c("Prediksjonsintervall","H\u00F8yere enn forventet","Forventet","Lavere enn forventet")
  shapelabels=c("Forel\u00F8pig")
  colourlabels=c("Korrigert for forsinkelse","Rapporterte d\u00F8dsfall")
  ylabel="Antall d\u00F8de per uke"

  lev <- rev(sort(unique(plottingData$DoA)))[1:5]
  plottingData <- plottingData[DoA %in% lev]

  plottingData[,DoAFactor:=factor(DoA,levels=as.character(lev))]
  plottingData[,DoALevel:=as.numeric(DoAFactor)-1]

  setorder(plottingData,-DoAFactor)

  q <- ggplot(data=plottingData,mapping=aes(x=wk,y=nbc))
  for(i in vlines) q <- q + geom_vline(lty=3,lwd=0.5,xintercept=i)
  q <- q + geom_ribbon(data=plottingData[DoA==max(DoA)],mapping=aes(ymin=-Inf,ymax=Lower, fill="4lower"),alpha=0.5)
  q <- q + geom_ribbon(data=plottingData[DoA==max(DoA)],mapping=aes(ymin=Lower,ymax=UPIb2, fill="3expected"),alpha=0.5)
  q <- q + geom_ribbon(data=plottingData[DoA==max(DoA)],mapping=aes(ymin=UPIb2,ymax=UPIb4, fill="2high"),alpha=0.5)
  q <- q + geom_ribbon(data=plottingData[DoA==max(DoA)],mapping=aes(ymin=UPIb4,ymax=Inf, fill="1veryhigh"),alpha=0.5)
  q <- q + geom_ribbon(data=plottingData[DoA==max(DoA)],mapping=aes(ymin=LPIc,ymax=UPIc, fill="0predinterval"),alpha=0.2)
  q <- q + geom_line(mapping=aes(group=DoAFactor),color="black",lwd=1)
  q <- q + geom_line(mapping=aes(color=DoAFactor),lwd=0.5)
  #q <- q + geom_point(data=plottingData,mapping=aes(color=DoAFactor),size=2)
  q <- q + labs(title=title)
  q <- q + scale_x_continuous("",breaks=breaks$wk, labels=breaks$label)
  q <- q + scale_y_continuous(ylabel)
  q <- q + scale_fill_manual("",
                             values=c("0predinterval"="#636363","1veryhigh"="#fc8d59","2high"="#ffffbf","3expected"="#91bfdb","4lower"="white"),
                             labels=filllabels1)
  q <- q + scale_color_brewer("Rapporteringsdato",palette="Set1")
  q <- q + theme_gray(base_size=18)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  q <- q + guides(fill = guide_legend(title.position="top", reverse=F, order=1, ncol=1))
  q <- q + guides(colour = guide_legend(title.position="top", reverse=F, order=2, ncol=1))
  q <- q + guides(shape = guide_legend(title.position="top", reverse=F, order=3, ncol=1))
  q <- q + theme(legend.position = "bottom")
  return(q)
}


GraphBias1 <- function(plotData,titleBias){
  plottingData <- plotData[wk>=max(wk)-52]

  breaks <- unique(plottingData[,c("WoDi","YoDi","wk"),with=FALSE])
  breaks <- breaks[seq(1,53,2)]
  breaks[, label := paste(gsub(" ","0",format(WoDi,width=2)),"/",YoDi,sep="")]

  q <- ggplot(plottingData[lag<=7],aes(x=factor(lag),y=nbc_bias))
  q <- q + geom_boxplot()
  q <- q + scale_x_discrete("Lag")
  q <- q + scale_y_continuous("Bias")
  q <- q + labs(title=titleBias)
  q <- q + theme_gray(base_size=18)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  return(q)
}

GraphBias2 <- function(plotData,titleBias){
  plottingData <- plotData[wk>=max(wk)-52*4]

  breaks <- unique(plottingData[,c("WoDi","YoDi","wk"),with=FALSE])
  breaks <- breaks[seq(1,nrow(breaks),4)]
  breaks[, label := paste(gsub(" ","0",format(WoDi,width=2)),"/",YoDi,sep="")]

  q <- ggplot(plottingData[lag<=7],aes(x=factor(lag),y=nbc_bias))
  q <- q + geom_boxplot()
  q <- q + labs(title=titleBias)

  q <- ggplot(plottingData[lag<=4],aes(x=wk,y=nbc_bias,color=factor(lag)))
  q <- q + geom_vline(xintercept=max(plottingData$wk),colour="red")
  q <- q + geom_line(lwd=0.5)
  q <- q + scale_x_continuous("",breaks=breaks$wk, labels=breaks$label)
  q <- q + scale_y_continuous("Bias")
  q <- q + labs(title=titleBias)
  q <- q + scale_color_brewer("Lag",palette="Set1")
  q <- q + theme_gray(base_size=18)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  return(q)
}

GraphBiasVsZScore <- function(plotData,titleBias){
  plottingData <- plotData[wk>=max(wk)-52*4]

  breaks <- unique(plottingData[,c("WoDi","YoDi","wk"),with=FALSE])
  breaks <- breaks[seq(1,nrow(breaks),4)]
  breaks[, label := paste(gsub(" ","0",format(WoDi,width=2)),"/",YoDi,sep="")]

  plottingData[,lagLabel:=sprintf("Lag %s weeks",lag)]

  q <- ggplot(plottingData[lag<=5],aes(x=true_zscore,y=nbc_bias))
  #q <- q + geom_vline(xintercept=max(plottingData$wk),colour="red")
  q <- q + geom_point()
  q <- q + stat_smooth(method="lm",se=F,colour="red")
  q <- q + facet_wrap(~lagLabel)
  q <- q + scale_x_continuous("Z Score")
  q <- q + scale_y_continuous("Bias in adjusted number of deaths")
  q <- q + labs(title=titleBias)
  q <- q + theme_gray(base_size=18)
  return(q)
}

GraphBiasVsZScore <- function(plotData,titleBias){
  plottingData <- copy(plotData)

  breaks <- unique(plottingData[,c("WoDi","YoDi","wk"),with=FALSE])
  breaks <- breaks[seq(1,nrow(breaks),4)]
  breaks[, label := paste(gsub(" ","0",format(WoDi,width=2)),"/",YoDi,sep="")]

  plottingData[,lagLabel:=sprintf("Lag %s weeks",lag)]

  q <- ggplot(plottingData[lag<=5],aes(x=true_zscore,y=nbc_bias))
  #q <- q + geom_vline(xintercept=max(plottingData$wk),colour="red")
  q <- q + geom_point()
  q <- q + stat_smooth(method="lm",se=F,colour="red")
  q <- q + facet_grid(delayVersion~lagLabel)
  q <- q + scale_x_continuous("Z Score")
  q <- q + scale_y_continuous("Bias in adjusted number of deaths")
  q <- q + labs(title="Bias")
  q <- q + theme_gray(base_size=18)
  return(q)
}

GraphsSeasons <- function(plotData){
  plotData[,seasonType:="Winter"]
  plotData[WoDi %in% c(21:39),seasonType:="Summer"]

  plotData[,season:=sprintf("%s",YoDi)]
  plotData[seasonType=="Winter" & WoDi>=40,season:=sprintf("%s/%s",YoDi,YoDi+1)]
  plotData[seasonType=="Winter" & WoDi<=20,season:=sprintf("%s/%s",YoDi-1,YoDi)]

  plotData <- plotData[WoDi %in% 1:52]
  minWinter <- min(plotData[seasonType=="Winter"]$season)
  minSummer <- min(plotData[seasonType=="Summer"]$season)
  plotData <- plotData[!season %in% c(minWinter,minSummer)]

  plotData[,WoDiInsideSeason:=1:.N,by=season]

  plotData[,cumExcess:=cumsum(excess),by=season]
  plotData[,cumDeaths:=cumsum(nbc),by=season]

  plotData[,maxWoDiInsideSeason:=max(WoDiInsideSeason),by=season]

  lastYear <- plotData[.N]$YoDi
  lastWeek <- plotData[.N]$WoDi
  lastWeekInsideSeason <- plotData[.N]$WoDiInsideSeason
  plotData[,categoryOfWeekStatus:=ifelse(WoDiInsideSeason<=lastWeekInsideSeason,
                                         sprintf("Up to week %s",lastWeek),
                                         sprintf("After week %s",lastWeek))]

  breaks <- unique(plotData[seasonType=="Winter",c("WoDi","WoDiInsideSeason")])
  setorder(breaks,WoDiInsideSeason)
  q <- ggplot(plotData[seasonType=="Winter"],aes(x=WoDiInsideSeason,y=cumExcess,colour=season))
  q <- q + geom_line()
  q <- q + geom_line(data=plotData[seasonType=="Winter" & season==max(season)],lwd=2)
  q <- q + geom_point(data=plotData[seasonType=="Winter" & season==max(season)],size=4)
  q <- q + scale_color_brewer("Season",palette="Set1",direction = -1)
  q <- q + scale_x_continuous("Weeks",breaks=breaks$WoDiInsideSeason,labels=breaks$WoDi)
  q1 <- q
  q1

  q <- ggplot(plotData[seasonType=="Summer"],aes(x=WoDi,y=cumExcess,colour=season))
  q <- q + geom_line()
  q <- q + geom_line(data=plotData[seasonType=="Summer" & season==max(season)],lwd=2)
  q <- q + geom_point(data=plotData[seasonType=="Summer" & season==max(season)],size=4)
  q <- q + scale_color_brewer("Season",palette="Set1",direction = -1)
  q1 <- q
  q1

  q <- ggplot(plotData[seasonType=="Summer"],aes(
    x=season,
    y=nbc,
    fill=categoryOfWeekStatus
    ))
  q2 <- q + geom_bar(stat="identity")
  q2
}

#' Running graphs
#' @param runName Name
#' @param data data
#' @param folder a
#' @param yearWeek a
#' @param dateData a
#' @param dateReliable a
#' @import stringr
#' @importFrom RAWmisc FootnoteGridArrange
#' @importFrom fhi SMAOpng
#' @import data.table
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices dev.off
#' @export RunGraphsDeaths
RunGraphsDeaths <- function(runName="Norway",
                            data,
                            folder=fhi::DashboardFolder("results",file.path(RAWmisc::YearWeek(),"Graphs")),
                            yearWeek=RAWmisc::YearWeek(),
                            dateData=Sys.time(),
                            dateReliable=RAWmisc::YearWeek()){

  #### NORWEGIAN

  storedData <- list()
  if(runName=="Norway"){
    runList <- c("Total","0to4","5to14","15to64","65P")
  } else runList <- "Total"
  for(i in runList){
    if(i=="Total"){
      title1 <- "Totalt antall d\u00F8de per uke siste \u00E5r"
      title1a <- "Totalt antall d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Totalt antall d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Totalt antall d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av totalt antall d\u00F8de per uke siste"
    } else if(i=="0to4"){
      title1 <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r"
      title1a <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall d\u00F8de (0-4 \u00E5r) per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (0-4 \u00E5r) per uke"
    } else if(i=="5to14"){
      title1 <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (5-14 \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (5-14 \u00E5r) per uke"
    } else if(i=="15to64"){
      title1 <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (15-64 \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (15-64 \u00E5r) per uke"
    } else if(i=="65P"){
      title1 <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r"
      title1a <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r (med rapporterte d\u00F8dsfall)"
      title1b <- "Antall (65+ \u00E5r) d\u00F8de per uke siste \u00E5r (uten rapporterte d\u00F8dsfall)"
      title2 <- "Antall (65+ \u00E5r) d\u00F8de per uke siste 5 \u00E5r"
      titleBias <- "Bias i korrigering av antall d\u00F8de (65+ \u00E5r) per uke"
    }

    q <- gridExtra::grid.arrange(
      GraphTogether(data=data[GROUP==i],
                    title1=title1,
                    title2=title2,
                    includeRealDeaths=FALSE,
                    dateData=dateData,
                    dateReliable=dateReliable
      ),
      ncol=1,
      newpage=F,
      bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""))
    )
    RAWmisc::saveA4(q,filename=paste0(folder,"/excl_reported_",runName,"-",i,"-", yearWeek,".png"))

    q <- gridExtra::grid.arrange(
      GraphTogether(data=data[GROUP==i],
                    title1=title1,
                    title2=title2,
                    includeRealDeaths=TRUE,
                    dateData=dateData,
                    dateReliable=dateReliable
      ),
      ncol=1,
      newpage=F,
      bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""))
    )
    RAWmisc::saveA4(q,filename=paste0(folder,"/incl_reported_",runName,"-",i,"-", yearWeek,".png"))


    # q <- gridExtra::grid.arrange(
    #   GraphTogether(data=data[GROUP==i],
    #                 title1a=title1a,
    #                 title1b=title1b,
    #                 title2=title2,
    #                 includeRealDeaths=TRUE,
    #                 dateData=dateData,
    #                 dateReliable=dateReliable
    #   ),
    #   ncol=1,
    #   newpage=F,
    #   bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""))
    # )
    # RAWmisc::saveA4(q,filename=paste0(folder,"/both_reported_",runName,"-",i,"-", yearWeek,".png"),landscape=F)

  }

}

GraphPercentRecorded <- function(plotData){
  dv <- plotData$delayVersion[1]
  plottingData <- plotData[delayVersion==dv]

  plottingData <- plottingData[,.(perc=nb/true_nbc*100),by=.(lag,DoA)]
  plottingData[,lagLabel:=sprintf("Lag %s weeks",lag)]

  q <- ggplot(plottingData[lag<=9],aes(x=lagLabel,y=perc))
  #q <- q + geom_vline(xintercept=max(plottingData$wk),colour="red")
  q <- q + geom_boxplot()
  q <- q + scale_x_discrete("Weeks lag")
  q <- q + scale_y_continuous("Percent of deaths recorded")
  #q <- q + labs(title="Bias")
  q <- q + theme_gray(base_size=18)
  return(q)
}

GraphNPVPPV <- function(plotData,titleBias){
  plottingData <- copy(plotData)

  plottingData <- plottingData[lag <= 9,.(
    ppv2=PPV(test_results2)*100,
    npv2=NPV(test_results2)*100,
    ppv4=PPV(test_results4)*100,
    npv4=NPV(test_results4)*100
  ),keyby=.(lag,delayVersion)]

  plottingData <- melt.data.table(plottingData,id=c("lag","delayVersion"))
  plottingData[,lagLabel:=sprintf("Lag %s weeks",lag)]

  q <- ggplot(plottingData[is.finite(value)],aes(x=lagLabel,y=value,fill=delayVersion))
  q <- q + geom_bar(stat="identity",pos="dodge",width=0.5)
  q <- q + facet_wrap(~variable,ncol=1)
  q <- q + theme_gray(base_size=18)
  return(q)
}



#' Running graphs
#' @param runName Name
#' @param allPlotData allPlotData
#' @param folder a
#' @param yearWeek a
#' @param dateData a
#' @import stringr
#' @importFrom RAWmisc FootnoteGridArrange
#' @importFrom fhi SMAOpng
#' @import data.table
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices dev.off
#' @export RunGraphsStatistics
RunGraphsStatistics <- function(runName="Norway",
                            allPlotData,
                            folder=fhi::DashboardFolder("results",file.path(RAWmisc::YearWeek(),"Graphs")),
                            yearWeek=RAWmisc::YearWeek(),
                            dateData=Sys.time()){

  #### NORWEGIAN


  plotData <- copy(allPlotData)
  plotData[,max_wk:=max(wk,na.rm=T),by=.(DoA,GROUP)]
  plotData[,lag:=max_wk-wk]

  plotData[DoA==max(DoA),true_nbc:=nbc]
  plotData[,true_nbc:=mean(true_nbc,na.rm=T),by=.(wk,GROUP)]
  plotData[wk>=max(wk)-10,true_nbc:=NA]

  plotData[,nbc_bias:=nbc-true_nbc]
  plotData[nbc==0 & true_nbc>10,nbc_bias:=NA]

  plotData[DoA==max(DoA),true_zscore:=zscore]
  plotData[,true_zscore:=mean(true_zscore,na.rm=T),by=.(wk,GROUP)]
  plotData[wk>=max(wk)-10,true_zscore:=NA]

  plotData[,truePos2:=true_zscore>2]
  plotData[,testPos2:=zscore>2]

  plotData[,test_results2:=as.character(NA)]
  plotData[truePos2==TRUE & testPos2==TRUE, test_results2:="TP"]
  plotData[truePos2==FALSE & testPos2==FALSE, test_results2:="TN"]
  plotData[truePos2==FALSE & testPos2==TRUE, test_results2:="FP"]
  plotData[truePos2==TRUE & testPos2==FALSE, test_results2:="FN"]

  plotData[,truePos4:=true_zscore>4]
  plotData[,testPos4:=zscore>4]

  plotData[,test_results4:=as.character(NA)]
  plotData[truePos4==TRUE & testPos4==TRUE, test_results4:="TP"]
  plotData[truePos4==FALSE & testPos4==FALSE, test_results4:="TN"]
  plotData[truePos4==FALSE & testPos4==TRUE, test_results4:="FP"]
  plotData[truePos4==TRUE & testPos4==FALSE, test_results4:="FN"]

  print(with(plotData[lag==0],xtabs(~test_results2+delayVersion)))
  print(with(plotData[lag==1],xtabs(~test_results2+delayVersion)))
  print(with(plotData[lag==2],xtabs(~test_results2+delayVersion)))

  for(i in unique(plotData$delayVersion)){
    q <- gridExtra::grid.arrange(
      GraphPreviousWeeks(plotData=plotData[delayVersion==i],
                         title="Totalt antall d\u00F8de per uke siste \u00E5r"
      ),
      ncol=1,
      newpage=F,
      bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""))
    )
    RAWmisc::saveA4(q,filename=paste0(folder,"/previous_",i,"-",runName,"-",i,"-", yearWeek,".png"))

    q <- gridExtra::grid.arrange(
      GraphBias1(plotData=plotData[delayVersion==i],
                 titleBias="Bias i korrigering av totalt antall d\u00F8de per uke siste"
      ),
      GraphBias2(plotData=plotData[delayVersion==i],
                 titleBias="Bias i korrigering av totalt antall d\u00F8de per uke siste"
      ),
      ncol=1,
      newpage=F,
      bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""))
    )
    RAWmisc::saveA4(q,filename=paste0(folder,"/bias_",i,"-",runName,"-",i,"-", yearWeek,".png"))
  }

  q <- gridExtra::grid.arrange(
    GraphBiasVsZScore(plotData=plotData,
                      title="Totalt antall d\u00F8de per uke siste \u00E5r"
    ),
    ncol=1,
    newpage=F,
    bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""))
  )
  RAWmisc::saveA4(q,filename=paste0(folder,"/zscore_",runName,"-", yearWeek,".png"))

  q <- gridExtra::grid.arrange(
    GraphPercentRecorded(plotData=plotData
    ),
    ncol=1,
    newpage=F,
    bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""))
  )
  RAWmisc::saveA4(q,filename=paste0(folder,"/perc_recorded_",runName,"-", yearWeek,".png"))

  q <- gridExtra::grid.arrange(
    GraphNPVPPV(plotData=plotData
    ),
    ncol=1,
    newpage=F,
    bottom = RAWmisc::FootnoteGridArrange(paste("Sist oppdatert: ",strftime(dateData,format="%d/%m/%Y"),sep=""))
  )
  RAWmisc::saveA4(q,filename=paste0(folder,"/npvppv_",runName,"-", yearWeek,".png"))

}
