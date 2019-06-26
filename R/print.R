#' Print methods for objects of class `eDrivers*`
#'
#' Print the outputs of an `eDrivers*` object call
#'
#' @param x object of class `eDrivers*`.
#' @param ... ignored.
#'
#' @return The JSON result.
#'
#' @name print.eDrivers
NULL

#' @rdname print.eDrivers
#' @method print eDrivers
#' @export
print.eDrivers <- function(x, ...) {
  cat('eDrivers    : Beauchesne et al. 2019\n')
  cat('TITLE       :', x$Metadata$title, '\n')
  if(x$Metadata$subtitle != "") cat('SUBTITLE    :', x$Metadata$subtitle, '\n')
  cat('DESCRIPTION :', x$Metadata$description, '\n')
  cat('SOURCES     :', RefManageR::TextCite(x$Source, .opts = list(max.names = 1, longnamesfirst = F)), '\n')
  cat('CONTENT     : [[1]]: Spatial data `RasterLayer`\n')
  cat('            : [[2]]: Metadata `yaml`\n')
  cat('            : [[3]]: Sources `BibEntry`\n')
}

#' @rdname print.eDrivers
#' @method print eDriversBrick
#' @export
print.eDriversBrick <- function(x, ...) {
  cat('eDrivers          : Beauchesne et al. 2019\n')
  cat('TITLES & SOURCES  : Driver (Source)\n')
  for(i in 1:dim(x[[1]])[3]) {
    cat('                  :',
        x$Metadata[[i]]$title,
        RefManageR::Citep(x$Source[[i]], .opts = list(max.names = 1, longnamesfirst = F)),
        '\n')
    }
  cat('CONTENT           : [[1]]: Spatial data `RasterBrick`\n')
  cat('                  : [[2]]: List of metadata `yaml`\n')
  cat('                  : [[3]]: List of sources `BibEntry`\n')
}
