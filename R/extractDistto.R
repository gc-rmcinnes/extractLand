#' @title extract distance to terra version
#' @export
#' @author T. Micheletti (modified from Julie W. Turner's extract_distto, adapted for terra)
#'
extractDistto <- function(DT, feature, name, where = 'end', crs, intYr = NULL) {
  objectName <- name
  coordsStart <- c('x1_', 'y1_')
  coordsEnd <- c('x2_', 'y2_')
  
  if (!is.null(intYr)) {
    DT <- DT[int.year == intYr]
  }
  
  # Convert feature to SpatVector if it's sf
  if (inherits(feature, 'sf')) {
    featureSv <- vect(feature)
  } else {
    featureSv <- feature
  }
  
  if (where %in% c('start', 'both')) {
    startPts <- vect(DT[, .SD, .SDcols = coordsStart], 
                     geom = coordsStart, 
                     crs = crs)
    
    # Calculate minimum distance to any feature
    distMatrix <- distance(startPts, featureSv)
    distStart <- apply(distMatrix, 1, min)
    
    DT[, paste0('dist', objectName, '_start') := distStart]
  }
  
  if (where %in% c('end', 'both')) {
    validRows <- DT[!is.na(x2_) & !is.na(y2_), which = TRUE]
    
    if (length(validRows) > 0) {
      endPts <- vect(DT[validRows, .SD, .SDcols = coordsEnd], 
                     geom = coordsEnd, 
                     crs = crs)
      
      distMatrix <- distance(endPts, featureSv)
      distEnd <- apply(distMatrix, 1, min)
      
      DT[validRows, paste0('dist', objectName, '_end') := distEnd]
    }
  }
  
  return(DT)
}
