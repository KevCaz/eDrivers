#' - `fetchDrivers()`:
#'   - fetches the data through SLGO's API
#'   - arguments:
#'     - driver: character vector containing the names of the drivers to fetch
#'     - output: character, file output location, default is current working directory
#'     - import: logical, \code{TRUE} whether to import the data in R
#'     - brick: create raster brick with imported data
#'
#' - returns:
#'  - list containing:
#'     1. data queried (rasterbrick)
#'     2. citation (bibtex entries)
#'     3. metadata (list of matrices?)
#'  - imports individual rasters, citations and metadata for queried drivers data
#'
#' @examples
#' # Example 1
#' drivers <- fetchDrivers(drivers = c('DD', 'DNH', 'SHP'))
#' drivers <- fetchDrivers(drivers = c('DD', 'DNH', 'SHP', 'X'))
#' drivers <- fetchDrivers(drivers = c('FisheriesDD', 'FisheriesDNH', 'Shipping'))
#'

fetchDrivers <- function(drivers,
                         output = NULL,
                         import = T,
                         brick = F) {

  # -------------------------------#
  # ------     PARAMETERS    ------#
  # -------------------------------#
  drNames <- paramDrivers(drivers)

  # -------------------------------#
  # ------      OUTPUT       ------#
  # -------------------------------#
  # Check for output specification
  if(is.null(output)) output <- getwd()

  # Check whether the last character is a "/"
  x <- substr(output, nchar(output), nchar(output))

  # If not, add it
  if(x != '/') output <- paste0(output, '/')


  # -------------------------------#
  # ------       LOAD        ------#
  # -------------------------------#
  # Load drivers queried
  # These will eventually be API calls to the SLGO web portal
  data(list = drNames,
       package = 'eDrivers',
       envir = environment())


  # -------------------------------#
  # ------      EXPORT       ------#
  # -------------------------------#
  # Export data
  for(i in drNames) {
    # Raster
    raster::writeRaster(x = get(i)$Data,
                        filename = paste0(output, i, '.tif'),
                        format = 'GTiff',
                        overwrite = TRUE)

    # Metadata
    yaml::write_yaml(x = get(i)$Metadata,
                     file = paste0(output, i, '.yaml'))

    # Source
    bibtex::write.bib(entry = get(i)$Source,
                      file = paste0(output, i, '.bib'),
                      verbose = F)
  }

  # Export platform citation
  bibtex::write.bib(entry = eDriversBib,
                    file = paste0(output, 'eDriversBib.bib'),
                    verbose = F)


  # ------------------------------#
  # ------      IMPORT      ------#
  # ------------------------------#
  if(import) {
    importDrivers(drivers,
                  input = output,
                  brick = brick)
  }
}
