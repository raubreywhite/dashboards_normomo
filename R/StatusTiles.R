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
                           dateData) {
  yrwk <- rev(sort(as.character(unique(allResults$yrwk))))[2:53]
  plotData <- allResults[yrwk %in% yrwk & age == "Total"]
  plotData[, status := "1veryhigh"]
  plotData[nbc < UPIb4, status := "2high"]
  plotData[nbc < UPIb2, status := "3expected"]
  # plotData[nbc<LCIb-abs(UPIb2-LCIb),status:="4lower"]

  plotData[fhidata::norway_locations_long_current,on="location_code",location_name:=location_name]
  plotData <- plotData[!is.na(location_name)]
  unique(plotData$location_code)
  unique(plotData$location_name)


  plotData[, location_name := factor(location_name, levels = rev(fhidata::norway_locations_long_current[location_code %in% plotData$location_code]$location_name))]

  plotColours <- plotData[1:4]
  # plotColours[1,status:="4lower"]
  plotColours[2, status := "3expected"]
  plotColours[3, status := "2high"]
  plotColours[4, status := "1veryhigh"]

  q <- ggplot(plotData, aes(x = yrwk, y = location_name, fill = status))
  q <- q + geom_tile(colour = "black")
  q <- q + geom_tile(data = plotColours, alpha = 0)
  q <- q + scale_fill_manual("",
    values = c("1veryhigh" = "#fc8d59", "2high" = "#ffffbf", "3expected" = "#91bfdb"),
    labels = c(
      "Betydelig h\u00F8yere enn forventet",
      "H\u00F8yere enn forventet",
      "Forventet/lavere enn forventet"
    )
  )
  q <- q + labs(title = "Totalt antall d\u00F8de per uke siste \u00E5r")
  q <- q + scale_x_discrete("\u00C5r-uke")
  q <- q + scale_y_discrete("")
  q <- q + labs(caption = sprintf("Sist oppdatert: %s", strftime(dateData, format = "%d/%m/%Y")))
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  RAWmisc::saveA4(q, filename = file.path(folder, sprintf("Status_tiles-%s.png", yearWeek)))
}
