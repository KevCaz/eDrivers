#' Import driver layers from memory
#'
#' Imports driver layers with accompanying attributes (metadata and source) for
#' a list of drivers
#'
#' @param drivers a vector of characters indicating the queried drivers as keys
#' or file names. Consult list of drivers using the `fetchList()` function to
#' identify desired drivers and use `FileName` or `Key` column to identify proper
#' spelling for queries.
#' @param input path to disk location of loaded drivers. Default is `null`, which
#' uses the current working directory
#' @param brick a logical. Should a `eDriversBrick` object be returned, which
#' a single `RasterBrick` object containing all drivers queried. See
#' `brickDrivers()` for further details.
#'
#' @return if brick is `TRUE`
#'
#' @export
#'
#' @examples
#' \donttest{
#' res1 <- importDrivers
#' }



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

importDrivers <- function(drivers,
                          input = NULL,
                          brick = F) {
  #
  # drivers = c('DD', 'DNH', 'SHP')
  # input = NULL
  # brick = F

  # -------------------------------#
  # ------     PARAMETERS    ------#
  # -------------------------------#
  drNames <- paramDrivers(drivers)


  # -------------------------------#
  # ------    INPUT SPEC     ------#
  # -------------------------------#
  # Check for input specification
  if(is.null(input)) input <- getwd()

  # Check whether the last character is a "/"
  x <- substr(input, nchar(input), nchar(input))

  # If not, add it
  if(x != '/') input <- paste0(input, '/')


  # ------------------------------#
  # ------      IMPORT      ------#
  # ------------------------------#
  # Create objects and import data
  for(i in drNames) {
    # Raster data
    r <- raster::raster(paste0(input, i, '.tif'))

    # Metadata
    meta <- yaml::read_yaml(paste0(input, i, '.yaml'))

    # Source
    bib <- RefManageR::ReadBib(paste0(input, i, '.bib'))

    # Assign to driver name
    assign(i,
      list(Data = r,
           Metadata = meta,
           Source = bib))
  }

  # -----------------------------------#
  # ------   INDIVIDUAL LAYERS   ------#
  # -----------------------------------#
  if(!brick) {
    for(i in drNames) {
      # eDrivers class object
      assign(i,
          structure(get(i), class = 'eDrivers'),
          envir = .GlobalEnv)
    }
  }

  # ------------------------------#
  # ------      BRICK       ------#
  # ------------------------------#
  if(brick) {
    assign('eDriversBrick',
           brickDrivers(drivers),
           envir = .GlobalEnv)

    # eDriversBrick <- brickDrivers(drivers)
    # return(eDriversBrick)
  }
}
