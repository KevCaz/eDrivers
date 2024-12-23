#' Downloads and imports driver layers
#'
#' Downloads and imports queried driver layers with accompanying attributes
#' (metadata and source) for a selection of drivers
#'
#' @param drivers a vector of characters indicating the queried drivers as keys
#' or file names. Consult list of drivers using the `fetchList()` function to
#' identify desired drivers and use `FileName` or `Key` column to identify proper
#' spelling for queries.
#' @param output path to disk location of downloaded drivers. Default is `null`,
#' which uses the current working directory.
#' @param import a logical. Should queried drivers be imported in R session as
#' `eDrivers` objects or `eDriversBrick` objects (see brick parameters).
#' @param brick a logical. Should a `eDriversBrick` object be returned, which
#' a single `RasterBrick` object containing all drivers queried. See
#' `brickDrivers()` for further details.
#'
#' @return Saves queried drivers and accompanying attributes (metadata and
#' sources) to disk and imports data in R session if import is `TRUE`.
#' if brick is `FALSE`, `eDrivers` objects are imported, one for each
#' driver queried, containing a list with:
#'  [[1]]: Spatial data as a `RasterLayer`
#'  [[2]]: Metadata as a `yaml`
#'  [[3]]: Sources as a `BibEntry`
#' if brick is `TRUE`, an `eDriversBrick` object is imported containing a list with:
#'  [[1]]: RasterBrick` with drivers queried
#'  [[2]]: list containing metadata for each driver queried
#'  [[3]]: list containing sources for each driver queried
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1
#' res1 <- fetchDrivers(drivers = c("SST+", "SST-"), import = FALSE)
#'
#' # Example 2
#' res2 <- fetchDrivers(drivers = c("SST+", "SST-"), import = TRUE, brick = FALSE)
#'
#' # Example 3
#' res3 <- fetchDrivers(drivers = c("SST+", "SST-"), import = TRUE, brick = TRUE)
#' }

fetchDrivers <- function(drivers,
                         output = NULL,
                         import = TRUE,
                         brick = FALSE) {
  # -------------------------------#
  # ------     PARAMETERS    ------#
  # -------------------------------#
  drNames <- paramDrivers(drivers)

  # -------------------------------#
  # ------      OUTPUT       ------#
  # -------------------------------#
  # Check for output specification
  if (is.null(output)) output <- getwd()

  # Check whether the last character is a "/"
  x <- substr(output, nchar(output), nchar(output))

  # If not, add it
  if (x != "/") output <- paste0(output, "/")


  # -------------------------------#
  # ------       LOAD        ------#
  # -------------------------------#
  # Load drivers queried
  # These will eventually be API calls to the SLGO web portal
  data(
    list = drNames,
    package = "eDrivers",
    envir = environment()
  )


  # -------------------------------#
  # ------      EXPORT       ------#
  # -------------------------------#
  # Export data
  for (i in drNames) {
    # Raster
    dat <- get(i)$Data %>%
      raster::projectRaster(crs = 32198)
    raster::writeRaster(
      x = dat,
      filename = paste0(output, i, ".tif"),
      format = "GTiff",
      overwrite = TRUE,
      NAflag = -1
    )

    # Metadata
    yaml::write_yaml(
      x = get(i)$Metadata,
      file = paste0(output, i, ".yaml")
    )

    # Source
    bibtex::write.bib(
      entry = get(i)$Source,
      file = paste0(output, i, ".bib"),
      verbose = FALSE
    )
  }

  # Export platform citation
  bibtex::write.bib(
    entry = eDriversBib,
    file = paste0(output, "eDriversBib.bib"),
    verbose = FALSE
  )


  # ------------------------------#
  # ------      IMPORT      ------#
  # ------------------------------#
  if (import) {
    importDrivers(drivers,
      input = output,
      brick = brick
    )
  }
}
