#' Fetches the list of drivers available
#'
#' Fetches and return a table with the list of driver available through the
#' `eDrivers` R package.
#'
#' @return An data.frame with details on drivers available containing driver
#' groups (Groups), driver names (Drivers), driver keys used for queries
#' (Key), name of files (FileName), which can also be used for
#' queries, and data source (Source).
#'
#' @export
#'
#' @examples
#' \donttest{
#' res1 <- fetchList()
#' res1
#' }

fetchList <- function() {
  # Load drivers list
  data(driversList,
       package = 'eDrivers',
       envir = environment())

  # Data.frame of data available
  drList <- driversList[, c('Groups','Drivers','Key','FileName','Source')]
  rownames(drList) <- NULL

  # Print kable
  print(knitr::kable(drList))

  # Assign object with data table
  assign('drList', drList, envir = .GlobalEnv)
}
