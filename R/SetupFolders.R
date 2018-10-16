#' SetupFolders
#' @param folder_results a
#' @param dateDataMinusOneWeek a
#' @importFrom fhi DashboardFolder
#' @importFrom RAWmisc YearWeek
#' @import data.table
#' @export SetupFolders
SetupFolders <- function(
  folder_results=DashboardFolder("results"),
  dateDataMinusOneWeek
  ){

  unlink(file.path(folder_results,RAWmisc::YearWeek(dateDataMinusOneWeek)),recursive=TRUE,force=TRUE)

  dir.create(file.path(folder_results,RAWmisc::YearWeek(dateDataMinusOneWeek)))

  dir.create(file.path(
    folder_results,
    RAWmisc::YearWeek(dateDataMinusOneWeek),
    "graphs_status"))

  dir.create(file.path(
    folder_results,
    RAWmisc::YearWeek(dateDataMinusOneWeek),
    "graphs_statistics"))

  dir.create(file.path(
    folder_results,
    RAWmisc::YearWeek(dateDataMinusOneWeek),
    "MOMO"))

  dir.create(file.path(
    folder_results,
    RAWmisc::YearWeek(dateDataMinusOneWeek),
    "data"))
}
