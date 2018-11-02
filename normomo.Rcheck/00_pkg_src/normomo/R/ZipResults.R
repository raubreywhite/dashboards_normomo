#' blah
#' @param folderResults a
#' @param folderResultsYearWeek a
#' @param folderResultsZip a
#' @param folderDataAppZip a
#' @importFrom zip zip
#' @export ZipResults
ZipResults <- function(
	folderResults=fhi::DashboardFolder("results"),
	folderResultsYearWeek=fhi::DashboardFolder("results",RAWmisc::YearWeek()),
	folderResultsZip=fhi::DashboardFolder("results",paste0("archive_",RAWmisc::YearWeek(),".zip")),
	folderDataAppZip=fhi::DashboardFolder("data_app",paste0("archive_",RAWmisc::YearWeek(),".zip"))
	){
  orig <- getwd()
  setwd(folderResults)
  zip::zip(folderResultsZip, folderResultsYearWeek)
  setwd(orig)
  file.copy(folderResultsZip, folderDataAppZip)
}
