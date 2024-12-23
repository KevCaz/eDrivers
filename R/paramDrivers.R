#' Sets required parameters for internal functions and checks for errors in
#' queried drivers
#'
#' @param drivers a vector of characters indicating the queried drivers as keys
#' or file names. Consult list of drivers using the `fetchList()` function to
#' identify desired drivers and use `FileName` or `Key` column to identify proper
#' spelling for queries.

#' @return drNames object containing driver names to use internally in `eDrivers` functions
#'
#' @export
#'
#' @examples
#' \donttest{
#' paramDrivers(c("SST+", "SST-"))
#' }
paramDrivers <- function(drivers) {
     # List of drivers
     data(driversList,
          package = "eDrivers",
          envir = environment()
     )

     # eDrivers referece
     data(eDriversBib,
          package = "eDrivers",
          envir = environment()
     )

     # Number of drivers queried
     nDr <- length(drivers)

     # Check if queries are in drivers list
     # Stop if queries are not in drivers list
     id <- drivers %in% driversList$FileName | drivers %in% driversList$Key
     if (any(!id)) {
          stop(paste(
               "Queried drivers",
               paste(drivers[!id], collapse = ", "),
               "are not available or are mispelled. Consult list of",
               "drivers using the `fetchList()` function to identify desired",
               "drivers and use `FileName` or `Key` column to identify proper",
               "spelling for queries"
          ))
     }

     # ID of drivers queried
     id <- driversList$Key %in% drivers | driversList$FileName %in% drivers

     # Names of drivers queried
     drNames <- driversList$FileName[id]

     return(drNames)
}
