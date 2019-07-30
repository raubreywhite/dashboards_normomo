#' CleanExportedMOMOData
#' @param data a
#' @param s a
#' @param folder_results a
#' @import data.table
#' @export
clean_exported_momo_data <- function(
                                  data,
                                  s,
                                  folder_results = fhi::DashboardFolder("results")) {
  id <- NULL
  GROUP <- NULL
  wk <- NULL
  wk2 <- NULL
  randomNoise <- NULL
  nbc <- NULL
  nb <- NULL
  UPIc <- NULL


  data <- data[!is.na(Pnb), c("GROUP", "wk", "wk2", "YoDi", "WoDi", "Pnb", "nb", "nbc", "UPIb2", "UPIb4", "UPIc", "LPIc", "UCIc", "LCIc", "zscore"), with = F]

  minCorrectedWeek <- min(data[nbc != nb]$wk)

  # prediction interval
  data[is.na(UPIc) | UPIc < nbc, UPIc := nbc]
  data[is.na(LPIc) | LPIc > nbc, LPIc := nbc]

  # making them slightly wider to hide the real information
  #data[wk >= minCorrectedWeek & UPIc == 0, UPIc := 1]
  #data[wk >= minCorrectedWeek & !is.na(UPIc), UPIc := UPIc + 3]
  #data[wk >= minCorrectedWeek & !is.na(LPIc), LPIc := LPIc - 3]
  data[LPIc < 0, LPIc := 0]

  # prediction interval cant be below the real value!
  data[is.na(LPIc) | LPIc < nb, LPIc := nb]

  # remove prediction intervals before correction
  data[wk < minCorrectedWeek, UPIc := nbc]
  data[wk < minCorrectedWeek, LPIc := nbc]

  data[, excess := nbc - Pnb]

  setnames(data,"wk2","yrwk")
  setnames(data,"GROUP","age")
  data[,location_code:=s[["runName"]]]

  return(data)
}
