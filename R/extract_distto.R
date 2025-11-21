#' @title extract distance to
#' @export
#' @author Julie W. Turner
#'
extract_distto <- function(DT, feature, name, where = 'end', crs, int.yr= NULL) {
  #object_name <- deparse(substitute(feature))
  object_name <- name
  coords_start  <-  c('x1_', 'y1_')
  coords_end  <-  c('x2_', 'y2_')

  if(!is.null(int.yr)){
    DT <- DT[int.year == int.yr]
    DT
  }

  if(where == 'start'){
    DT[, paste0('dist', object_name, '_start') := distance_to(st_as_sf(.SD, coords = coords_start,
                                                                       crs = crs), feature)]
  }

  if(where == 'end'){
    DT[!is.na(x2_)&!is.na(y2_), paste0('dist', object_name, '_end') := distance_to(st_as_sf(.SD, coords = coords_end,
                                                                                            crs = crs), feature)]
  }

  if(where == 'both'){
    DT[, paste0('dist', object_name, '_start') := distance_to(st_as_sf(.SD, coords = coords_start,
                                                                       crs = crs), feature)]
    DT[!is.na(x2_)&!is.na(y2_), paste0('dist', object_name, '_end') := distance_to(st_as_sf(.SD, coords = coords_end,
                                                                                            crs = crs), feature)]
  }

  return(DT)
}
