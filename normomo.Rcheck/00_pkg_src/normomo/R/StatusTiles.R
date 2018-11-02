#' blah
#' @param allResults a
#' @param folder a
#' @param yearWeek a
#' @param dateData a
#' @import ggplot2
#' @import data.table
#' @importFrom RAWmisc RecodeDT saveA4
#' @export RunStatusTiles
RunStatusTiles <- function(
  allResults,
  folder,
  yearWeek,
  dateData){

    yrwk <- rev(sort(as.character(unique(allResults$wk2))))[2:53]
    plotData <- allResults[wk2 %in% yrwk & GROUP=="Total"]
    plotData[,status:="1veryhigh"]
    plotData[nbc<UPIb4,status:="2high"]
    plotData[nbc<UPIb2,status:="3expected"]
    #plotData[nbc<LCIb-abs(UPIb2-LCIb),status:="4lower"]

    unique(plotData$name)
    fylkeNames <- c(
      "Norway"="Norge",
      "Fylke_01"="\u00D8stfold",
      "Fylke_02"="Akershus",
      "Fylke_03"="Oslo",
      "Fylke_04"="Hedmark",
      "Fylke_05"="Oppland",
      "Fylke_06"="Buskerud",
      "Fylke_07"="Vestfold",
      "Fylke_08"="Telemark",
      "Fylke_09"="Aust-Agder",
      "Fylke_10"="Vest-Agder",
      "Fylke_11"="Rogaland",
      "Fylke_12"="Hordaland",
      "Fylke_14"="Sogn og Fjordane",
      "Fylke_15"="M\u00F8re og Romsdal",
      "Fylke_50"="Tr\u00F8ndelag",
      "Fylke_18"="Nordland",
      "Fylke_19"="Troms",
      "Fylke_20"="Finnmark"
    )

    RAWmisc::RecodeDT(plotData,switch=fylkeNames,"name")
    unique(plotData$name)

    plotData[,name:=factor(name,levels=rev(fylkeNames))]

    plotColours <- plotData[1:4]
    #plotColours[1,status:="4lower"]
    plotColours[2,status:="3expected"]
    plotColours[3,status:="2high"]
    plotColours[4,status:="1veryhigh"]

    q <- ggplot(plotData, aes(x=wk2,y=name,fill=status))
    q <- q + geom_tile(colour="black")
    q <- q + geom_tile(data=plotColours,alpha=0)
    q <- q + scale_fill_manual("",
                                        values=c("1veryhigh"="#fc8d59","2high"="#ffffbf","3expected"="#91bfdb"),
                                        labels=c(
                                          "Betydelig h\u00F8yere enn forventet",
                                          "H\u00F8yere enn forventet",
                                          "Forventet/lavere enn forventet"
                                        ))
    q <- q + labs(title="Totalt antall d\u00F8de per uke siste \u00E5r")
    q <- q + scale_x_discrete("\u00C5r-uke")
    q <- q + scale_y_discrete("")
    q <- q + labs(caption=sprintf("Sist oppdatert: %s",strftime(dateData,format="%d/%m/%Y")))
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
    RAWmisc::saveA4(q,filename=file.path(folder,sprintf("Status_tiles-%s.png",yearWeek)))
}
