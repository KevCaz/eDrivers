#' Import driver layers from memory
#'
#' Imports driver layers with accompanying attributes (metadata and source) for
#' a selection of drivers
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
#' @return if brick is `FALSE`, `eDrivers` objects are returned, one for each
#' driver queried, containing a list with:
#'  [[1]]: Spatial data as a `RasterLayer`
#'  [[2]]: Metadata as a `yaml`
#'  [[3]]: Sources as a `BibEntry`
#' if brick is `TRUE`, an `eDriversBrick` object containing a list with:
#'  [[1]]: RasterBrick` with drivers queried
#'  [[2]]: list containing metadata for each driver queried
#'  [[3]]: list containing sources for each driver queried
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example 1
#' fetchDrivers(c("SST+", "SST-"))
#' res1 <- importDrivers(drivers = c("SST+", "SST-"))
#'
#' # Example 2
#' res2 <- importDrivers(drivers = c("SST+", "SST-"), brick = TRUE)
#'
#' }
importDrivers <- function(drivers,
                          input = NULL,
                          brick = FALSE) {
  # -------------------------------#
  # ------     PARAMETERS    ------#
  # -------------------------------#
  drNames <- paramDrivers(drivers)


  # -------------------------------#
  # ------    INPUT SPEC     ------#
  # -------------------------------#
  # Check for input specification
  if (is.null(input)) input <- getwd()

  # Check whether the last character is a "/"
  x <- substr(input, nchar(input), nchar(input))

  # If not, add it
  if (x != "/") input <- paste0(input, "/")


  # ------------------------------#
  # ------      IMPORT      ------#
  # ------------------------------#
  # Create objects and import data
  for (i in drNames) {
    # Raster data
    r <- raster::raster(paste0(input, i, ".tif"))

    # Metadata
    meta <- yaml::read_yaml(paste0(input, i, ".yaml"))

    # Source
    bib <- RefManageR::ReadBib(paste0(input, i, ".bib"))

    # Assign to driver name
    assign(
      i,
      list(
        Data = r,
        Metadata = meta,
        Source = bib
      )
    )
  }

  # -----------------------------------#
  # ------   INDIVIDUAL LAYERS   ------#
  # -----------------------------------#
  if (!brick) {
    for (i in drNames) {
      # eDrivers class object
      assign(i,
        structure(get(i), class = "eDrivers"),
        envir = .GlobalEnv
      )
    }
  }

  # ------------------------------#
  # ------      BRICK       ------#
  # ------------------------------#
  if (brick) {
    assign("eDriversBrick",
      brickDrivers(drivers),
      envir = .GlobalEnv
    )
  }
}
