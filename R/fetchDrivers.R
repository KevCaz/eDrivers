#' - `fetchDrivers()`:
#'   - fetches the data through SLGO's API
#'   - arguments:
#'     - driver: character vector containing the names of the drivers to fetch
#'     - output: character, file output location, default is current working directory
#'     - import: logical, \code{TRUE} whether to import the data in R
#'
#' - returns:
#'  - list containing:
#'     1. data queried (rasterstack)
#'     2. citation (bibtex entries)
#'     3. metadata (list of matrices?)
#'  - imports individual rasters, citations and metadata for queried drivers data
#'
#' @examples
#' # Example 1
#' drivers <- fetchDrivers(drivers = c('DD', 'DNH', 'SHP'))
#'
#' output <- '/users/davidbeauchesne/desktop/test/'
fetchDrivers <- function(drivers,
                         output = NULL,
                         import = T) {

# Import data
data(driversList)
data(eDriversBib)
# data(metadata)
nDr <- length(drivers) # Number of drivers
id <- driversList$Accronym %in% drivers
drNames <- driversList$FileName[id]
data(list = drNames, envir=environment())


# Export to output folder
  # -------------------------------#
  # ------      Rasters      ------#
  # -------------------------------#
  # Check for output specification
  if(is.null(output)) output <- getwd()

  # Check whether the last character is a "/"
  x <- substr(output, nchar(output), nchar(output))

  # If not, add it
  if(x != '/') output <- paste0(output, '/')

  # Export rasterbrick
  for(i in 1:nDr) {
    raster::writeRaster(x = get(drNames[i]),
                        filename = paste0(output, drivers[i], '.tif'),
                        format = 'GTiff',
                        overwrite = TRUE)
  }

  # ---------------------------------#
  # ------      Citations      ------#
  # ---------------------------------#
  cite <- c('beauchesne2019', driversList$Source[id])
  citeNames <- c('eDrivers', drivers)
  for(i in 1:length(cite)) {
    bibtex::write.bib(bib[[cite[i]]],
                      file = paste0(output, citeNames[i], '.bib'),
                      verbose = F)
  }



  # ------------------------------#
  # ------      Import      ------#
  # ------------------------------#
  if(import) {
    # Create list to store eDrivers object including rasterbrick, citations and metadata
    # Empty list
    eD <- vector('list', 3)
    names(eD) <- c('Drivers','Citations','Metadata')

    # Rasters
      # Load fetched rasters
      rDrivers <- vector('list', nDr)
      names(rDrivers) <- drivers
      for(i in 1:nDr) rDrivers[[i]] <- raster::raster(paste0(output, drivers[i], '.tif'))

      # Create rasterbrick with raster data
      eD$Drivers <- raster::brick(rDrivers)

    # Citations
      eD$Citations <- driversList[id, c('Groups','Drivers','Accronym','Source')]
      eD$Citations$Source <- format(bib[[eD$Citations$Source]], type = 'text')
      eD$Citations$Source <- gsub("[\n]", " ", eD$Citations$Source)

    # Metadata
      # TO BE DONE

    # Create class object
    class(eD) <- 'eDrivers'
  }


  # ------------------------------#
  # ------      Return      ------#
  # ------------------------------#
  return(eD)
}
