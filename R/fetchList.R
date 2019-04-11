#' - `fetchList()`:
#'   - fetches the list of data available through SLGO's API
#'   - arguments:
#'
#' - returns list containing
#'   - data.frame containing list of drivers with sources
#'   - data.frame with bibliography
#'
#' @examples
#' drivers <- fetchList()
#'

fetchList <- function() {
  # Load drivers list and bibtex file
  data(driversList)
  data(eDriversBib)
  library(RefManageR)

  # List
  eD <- vector('list', 2)
  names(eD) <- c('drivers', 'bibliography')

  # Data.frame of data available
  eD$drivers <- driversList[, c('Groups','Drivers','Key','Source')]

  # Citations
  for(i in eD$drivers$Source) Cite(bib[[i]])
    
  # Citations
  for(i in 1:nrow(eD$drivers)) {
    eD$drivers$Source[i] <- TextCite(bib[[eD$drivers$Source[i]]], .opts = list(max.names = 1))
  }

  # Bibliography
  capture.output(eD$bibliography <- PrintBibliography(bib))

  # Return object
  return(eD)
}
