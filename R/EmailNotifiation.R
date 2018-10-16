# DashboardsEmail <- function(emailBCC,emailSubject,emailText){
#   emails <- readxl::read_excel("/etc/gmailr/emails.xlsx")
#   emails <- na.omit(emails[[emailBCC]])
#
#   mime() %>%
#     to("dashboards@fhi.no") %>%
#     from("Dashboards FHI <dashboardsfhi@gmail.com>") %>%
#     bcc(paste0(emails,collapse=",")) %>%
#     subject(emailSubject) %>%
#     text_body(emailText) -> text_msg
#
#   currentWD <- getwd()
#   tmp <- tempdir()
#   file.copy("/etc/gmailr/.httr-oauth",paste0(tmp,"/.httr-oauth"))
#   setwd(tmp)
#   gmail_auth()
#   send_message(text_msg)
#   setwd(currentWD)
# }

#' blah
#' @param folderResultsYearWeek a
#' @param isTest a
#' @importFrom fhi DashboardEmail
#' @export EmailInternal
EmailInternal <- function(folderResultsYearWeek, isTest = TRUE){
  emailText <- "New NorMOMO results available to download from:
<br><br>
<a href='file:///F:/Prosjekter/Dashboards/results/normomo/'>F:/Prosjekter/Dashboards/results/normomo/</a>
"

  if (isTest) {
    fhi::DashboardEmail(
      "normomo_test",
      emailSubject = "TESTING EmailInternal",
      emailText
    )
  } else {
    fhi::DashboardEmail(
      "normomo_results",
      "New NorMOMO results available",
      emailText
    )
  }
}


#' blah
#' @param folderResultsYearWeek a
#' @param dateReliable a
#' @param isTest a
#' @importFrom fhi DashboardEmail
#' @export EmailSSI
EmailSSI <- function(folderResultsYearWeek, dateReliable, isTest = TRUE){

  currentYearWeek <- stringr::str_extract(folderResultsYearWeek,"[0-9]*-[0-9]*")

  files <- list.files(file.path(folderResultsYearWeek,"MOMO"))

  folderNorway1 <- files[stringr::str_detect(files,"Norway")]
  files <- list.files(file.path(folderResultsYearWeek,"MOMO",folderNorway1))

  folderNorway2 <- files[stringr::str_detect(files,"COMPLETE")]
  files <- list.files(file.path(folderResultsYearWeek,"MOMO",folderNorway1,folderNorway2))

  attachFiles <- file.path(folderResultsYearWeek,"MOMO",folderNorway1,folderNorway2,files)

  reliableData <- file.path(tempdir(),files)
  x <- fread(attachFiles)
  x <- x[wk2<=RAWmisc::YearWeek(dateReliable)]
  fwrite(x,reliableData)

  emailText <- sprintf("
<html><body>
Dear EuroMOMO hub,<br><br>
Please find attached the current week's results.<br><br>
Please note that only data up to and including week %s is included, as data beyond this is not reliable.<br><br>
Sincerely,<br>
Norway<br><br><br>
------------------------
<br>
DO NOT REPLY TO THIS EMAIL! This email address is not checked by anyone!
<br>
To add or remove people to/from this notification list, send their details to richardaubrey.white@fhi.no
</body></html>",RAWmisc::YearWeek(dateReliable))

  if (isTest) {
    fhi::DashboardEmail(
      "normomo_test",
      emailSubject = sprintf("TESTING EmailSSI [euromomo input] [Norway] [%s]",stringr::str_replace(currentYearWeek,"-"," ")),
      emailText,
      emailAttachFiles=reliableData,
      emailFooter=FALSE,
      BCC=FALSE
    )
  } else {
    fhi::DashboardEmail(
      "normomo_ssi",
      emailSubject = sprintf("[euromomo input] [Norway] [%s]",stringr::str_replace(currentYearWeek,"-"," ")),
      emailText,
      emailAttachFiles=reliableData,
      emailFooter=FALSE,
      BCC=FALSE
    )
  }
}
