#' quasipoission
#' @import R6
#' @export standard
standard <- R6::R6Class(
  "standard",
  portable = FALSE,
  cloneable = FALSE,
  list(
    results_x = NULL,
    initialize = function() {
      results_x <<- fd::schema$new(
        db_config = CONFIG$db_config,
        db_table = glue::glue("normomo_standard_results"),
        db_field_types = std_results_field_types,
        db_load_folder = "/xtmp/",
        keys = std_results_keys,
        check_fields_match = TRUE
      )

      results_x$db_connect()
    },
    run_all = function(masterData, info) {
      stack <- GenerateStack(
        f = info[["f"]],
        dateDataMinusOneWeek = info[["dateDataMinusOneWeek"]],
        dateData = info[["dateData"]]
      )

      stack <- stack[["stackAnalyses"]]

      std_run_analysis(masterData = masterData, stack = stack)

      std_run_graphs(stack = stack, results_x = results_x)

      email_ssi(
        folderResultsYearWeek = file.path(fhi::DashboardFolder("results", RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]))),
        dateReliable = info$dateData - CONFIG$WEEKS_UNRELIABLE * 7
      )

      SavingRawData(
        dateDataMinusOneWeek = info[["dateDataMinusOneWeek"]],
        masterData = masterData
      )

      data_to_save <- results_x$dplyr_tbl() %>%
        dplyr::filter(age == "Total") %>%
        dplyr::collect() %>%
        fd::latin1_to_utf8()

      std_save_results_as_excel(
        dateDataMinusOneWeek = info[["dateDataMinusOneWeek"]],
        data_to_save = data_to_save
      )
    }
  )
)

std_results_field_types <- c(
  "location_code" = "TEXT",
  "age" = "TEXT",
  "wk" = "INTEGER",
  "yrwk" = "TEXT",
  "YoDi" = "INTEGER",
  "WoDi" = "INTEGER",
  "Pnb" = "DOUBLE",
  "nb" = "DOUBLE",
  "nbc" = "DOUBLE",
  "UPIb2" = "DOUBLE",
  "UPIb4" = "DOUBLE",
  "UPIc" = "DOUBLE",
  "LPIc" = "DOUBLE",
  "UCIc" = "DOUBLE",
  "LCIc" = "DOUBLE",
  "zscore" = "DOUBLE",
  "excess" = "DOUBLE"
)

std_results_keys <- c(
  "location_code",
  "age",
  "yrwk"
)

std_run_analysis <- function(masterData, stack) {
  fd::msg("Running analysis")

  pb <- RAWmisc::ProgressBarCreate(min = 0, max = nrow(stack), flush = TRUE)
  for (i in 1:nrow(stack)) {
    RAWmisc::ProgressBarSet(pb, i)

    s <- stack[i, ]

    if (s[["runName"]] == "norway") {
      dataAnalysis <- as.data.frame(masterData[!is.na(age),
        c("DoD", "DoR", "age"),
        with = F
      ])
      plotGraphs <- TRUE
    } else {
      dataAnalysis <- as.data.frame(masterData[!is.na(age) & FYLKE == s[["fylke"]],
        c("DoD", "DoR", "age"),
        with = F
      ])
      plotGraphs <- FALSE
    }

    MOMO::SetOpts(
      DoA = s[["dateData"]],
      DoPR = as.Date("2012-1-1"),
      WStart = 1,
      WEnd = 52,
      country = s[["runName"]],
      source = "FHI",
      MDATA = dataAnalysis,
      HDATA = hfile,
      INPUTDIR = s[["MOMOFolderInput"]],
      WDIR = s[["MOMOFolderResults"]],
      back = 7,
      WWW = 290,
      Ysum = s[["MOMOYsum"]],
      Wsum = 40,
      plotGraphs = s[["plotGraphs"]],
      delayVersion = "richard",
      delayFunction = NULL,
      MOMOgroups = s[["MOMOgroups"]][[1]],
      MOMOmodels = s[["MOMOmodels"]][[1]],
      verbose = FALSE
    )

    MOMO::RunMoMo()

    dataToSave <- rbindlist(MOMO::dataExport$toSave, fill = TRUE)

    res <- clean_exported_momo_data(
      data = dataToSave,
      s = s
    )

    results_x$db_upsert_load_data_infile(res[, names(results_x$db_field_types), with = F])
  }
}

std_run_graphs <- function(stack, results_x) {
  fd::msg("Running graphs")
  pb <- RAWmisc::ProgressBarCreate(min = 0, max = nrow(stack), flush = TRUE)

  for (i in 1:nrow(stack)) {
    fd::msg(glue::glue("{i}a"))
    RAWmisc::ProgressBarSet(pb, i)
    fd::msg(glue::glue("{i}b"))
    s <- stack[i, ]
    loc_code <- s[["runName"]]
    data <- results_x$dplyr_tbl() %>%
      dplyr::filter(location_code == loc_code) %>%
      dplyr::collect() %>%
      fd::latin1_to_utf8()
    fd::msg(glue::glue("{i}c"))
    RunGraphsDeaths(
      runName = s[["runName"]],
      data = data,
      folder = s[["MOMOFolderResultsGraphsStatus"]],
      yearWeek = RAWmisc::YearWeek(s[["dateDataMinusOneWeek"]]),
      dateData = max(s[["dateData"]][[1]]),
      dateReliable = max(s[["dateData"]][[1]]) - 7
    )
    fd::msg(glue::glue("{i}d"))
  }

  allResults <- results_x$dplyr_tbl() %>%
    dplyr::filter(age == "Total") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()

  RunStatusTiles(
    allResults = allResults,
    folder = s[["MOMOFolderResultsGraphsStatus"]],
    yearWeek = RAWmisc::YearWeek(info[["dateDataMinusOneWeek"]]),
    dateData = info[["dateData"]]
  )
}

#' std_save_results_as_excel
#' @param folder_results a
#' @param dateDataMinusOneWeek a
#' @param data_to_save a
#' @export
std_save_results_as_excel <- function(
                                      folder_results = fhi::DashboardFolder("results"),
                                      dateDataMinusOneWeek,
                                      data_to_save) {
  fd::msg("Saving results")

  openxlsx::write.xlsx(
    data_to_save,
    file.path(
      folder_results,
      RAWmisc::YearWeek(dateDataMinusOneWeek),
      "Data",
      "data_results.xlsx"
    )
  )
}
