#' blah
#' @param folderResultsYearWeek a
#' @export
email_internal <- function(folderResultsYearWeek) {
  email <-
    blastula::compose_email(
      body =
        "
New NorMOMO results available to download from:

<a href='file:///F:/Prosjekter/Dashboards/results/normomo/'>F:/Prosjekter/Dashboards/results/normomo/</a>
",
      footer = fd::e_footer()
    )

  email %>%
    blastula::smtp_send(
      from = "dashboardsfhi@gmail.com",
      to = "dashboardsfhi@gmail.com",
      bcc = fd::e_emails("normomo_results"),
      subject = fd::e_subject("New NorMOMO results available"),
      credentials = blastula::creds_file("/etc/gmailr/blastula.txt")
    )
}


#' blah
#' @param folderResultsYearWeek a
#' @param dateReliable a
#' @export
email_ssi <- function(folderResultsYearWeek, dateReliable) {
  currentYearWeek <- stringr::str_extract(folderResultsYearWeek, "[0-9]*-[0-9]*")

  files <- list.files(file.path(folderResultsYearWeek, "MOMO"))

  folderNorway1 <- files[stringr::str_detect(files, "norway")]
  files <- list.files(file.path(folderResultsYearWeek, "MOMO", folderNorway1))

  folderNorway2 <- files[stringr::str_detect(files, "COMPLETE")]
  files <- list.files(file.path(folderResultsYearWeek, "MOMO", folderNorway1, folderNorway2))

  attachFiles <- file.path(folderResultsYearWeek, "MOMO", folderNorway1, folderNorway2, files)

  reliableData <- file.path(tempdir(), files)
  x <- fread(attachFiles)
  x <- x[wk2 <= RAWmisc::YearWeek(dateReliable)]
  fwrite(x, reliableData)

  unstable <- ""
  if (CONFIG$WEEKS_UNRELIABLE > 1) {
    unstable <- sprintf("Please note that only data up to and including week %s is included, as data beyond this is not reliable.<br><br>", RAWmisc::YearWeek(dateReliable))
  }

  email <-
    blastula::compose_email(
      body =
        "
Dear EuroMOMO hub,

Please find attached the current week's results. {unstable}

Sincerely,

Norway
",
      footer = fd::e_footer()
    ) %>%
    blastula::add_attachment(reliableData, error_on_missing = TRUE)

  email %>%
    blastula::smtp_send(
      from = "dashboardsfhi@gmail.com",
      to = fd::e_emails("normomo_ssi"),
      subject = fd::e_subject(glue::glue("[euromomo input] [Norway] [{stringr::str_replace(currentYearWeek, '-', ' ')}]")),
      credentials = blastula::creds_file("/etc/gmailr/blastula.txt")
    )
}
