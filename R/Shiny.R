#' Run the shiny dashboard
#' @export RunShiny
RunShiny <- function() {
  options(shiny.port = 4989)
  options(shiny.host = "0.0.0.0")
  file <- system.file("shiny",
    "normomo",
    "flexdashboard.Rmd",
    package = "normomo"
  )
  rmarkdown::run(file, shiny_args = list(port = 4989))
}
