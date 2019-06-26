#' Stack `eDrivers` objects as `eDriversBrick`
#'
#' Uses a list of `eDrivers` objects to create a `eDriversBrick`, which is a `rasterbrick`
#' with additionnal attributes and data associated with `eDrivers` objects
#'
#' @param drivers a vector of characters indicating the queried drivers as keys
#' or file names. Consult list of drivers using the `fetchList()` function to
#' identify desired drivers and use `FileName` or `Key` column to identify proper
#' spelling for queries.
#'
#' @return an `eDriversBrick` object containing a list with:
#'  [[1]]: rasterbrick with drivers queried
#'  [[2]]: list containing metadata for each driver queried
#'  [[3]]: list containing sources for each driver queried
#'
#' @export
#'
#' @examples
#' \donttest{
#' res1 <- brickDrivers(c('CoastalDevelopment','PositiveSST'))
#' res1
#' summary(res1)
#' plot(res1)
#' }

brickDrivers <- function(drivers) {
  if(length(drivers) < 2) stop('More than one driver must be included to use this functionality')
  # -------------------------------#
  # ------     PARAMETERS    ------#
  # -------------------------------#
  drNames <- paramDrivers(drivers)


  # -------------------------------#
  # ------       BRICK       ------#
  # -------------------------------#

  # ----------- Lists ---------- #
  # List of rasters (r)
  # List of metadata (m)
  # List of sources (s)
  r <- m <- s <-  list()
  for(i in 1:length(drNames)) {
    # Data in lists
    r[[i]] <- get(drNames[i])$Data
    m[[i]] <- get(drNames[i])$Metadata
    s[[i]] <- get(drNames[i])$Source
  }

  # Lists names
  names(r) <- names(m) <- names(s) <- drNames


  # ----------- Brick ---------- #
  r <- raster::brick(r)


  # ----------- `eDriversBrick` ---------- #
  eDriversBrick <- list(Data = r,
                        Metadata = m,
                        Source = s)


  # ------------------------------#
  # ------    ATTRIBUTES    ------#
  # ------------------------------#
  class(eDriversBrick) <- 'eDriversBrick'

  return(eDriversBrick)
}
