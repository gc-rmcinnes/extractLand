extractLandFeaturesYearly <- function(tracks, landscapeYearly, landscape5Yearly, histLandYears) {
  message("Starting extraction...")
  
  # valid years only
  years <- sort(unique(tracks$year))
  validYears1 <- intersect(as.character(years), histLandYears)
  landYears <- substr(x = names(landscapeYearly$landscapeYearly[[1]]), 
                      start = nchar(names(landscapeYearly$landscapeYearly)) - 4, 
                      nchar(names(landscapeYearly$landscapeYearly)))
  validYears2 <- intersect(landYears, validYears1)
  
  # TODO for now we use '5 closest years' anthro layer. Later, if we get an 
  # annual product, we replace it prepLandscape / prep_anthroDisturbance / landscape5Yearly object
  # and re-work the code below to use the specific year
  
  # 5-Year vector layers
  layerYears <- as.numeric(sub("intYear", "", names(landscape5Yearly)))
  interval <- min(diff(sort(layerYears))) / 2
  
  # Create a lookup data.table
  layerLookup <- data.table(
    layerYear = layerYears,
    layerName = names(landscape5Yearly)
  )
  
  # Use rolling join for efficient matching
  setkey(layerLookup, layerYear)
  tracks[, int.year := layerLookup[tracks, on = .(layerYear = year), 
                                   roll = "nearest", 
                                   layerName]]
  
  extracted_list <- lapply(sort(validYears2), function(yr){
    message("Extracting for year ", yr)
    
    pts_yr <- tracks[year == as.integer(yr), ]
    
    # Annual raster layers
    annual_rasts <- landscapeYearly$landscapeYearly[[1]][[paste0("year", yr)]] #TODO If we need to make another resolution window, we need to put this in loop or lapply. 
    # The landscapeYearly$landscapeYearly[[1]] here is a shortcut!!! 

    fiveYearInt <- unique(pts_yr$int.year)
    fiveYearObj <- landscape5Yearly[[fiveYearInt]]

    # if (is.null(fiveYearObj))
    #   fiveYearObj <- landscape5Yearly[[1]] 
    #TODO This was likely here from before, as things were returning only a list. 
    # Now all seems to work fine 
    
    crs_year <- terra::crs(annual_rasts[[1]])
    
    # Annual raster value extraction
    # Extract Start raster values
    pts_yr_start <- terra::vect(pts_yr[, .(x1_, y1_)], geom = c("x1_", "y1_"),
                                crs = terra::crs(annual_rasts[[1]]))
    vals_start <- terra::extract(annual_rasts, pts_yr_start)#[, -1, drop = FALSE]
    
    setnames(vals_start, paste0(names(vals_start), "_start"))
    
    # Extract End raster values
    pts_yr_end <- terra::vect(pts_yr[, .(x2_, y2_)], geom = c("x2_", "y2_"),
                              crs = terra::crs(annual_rasts[[1]]))
    
    vals_end <- terra::extract(annual_rasts, pts_yr_end)#[, -1, drop = FALSE]
    
    setnames(vals_end, paste0(names(vals_end), "_end"))
    
    # # Convert 5-year SpatVectors to sf
    # 
    # # PAVED (SpatVectorCollection)
    # # May need to add a second paved conversion for the other layer
    # paved_sf <- if (!is.null(fiveYearObj$paved))
    #   st_as_sf(fiveYearObj$paved[[2]])
    # else NULL
    # paved_sf <- st_cast(paved_sf, "MULTILINESTRING")
    # 
    # # UNPAVED (SpatVector)
    # unpaved_sf <- if (!is.null(fiveYearObj$unpaved))
    #   st_as_sf(fiveYearObj$unpaved)
    # else NULL
    # unpaved_sf <- st_cast(unpaved_sf, "MULTILINESTRING")
    # 
    # # POLYS (SpatVector)
    # polys_sf <- if (!is.null(fiveYearObj$polys))
    #   st_as_sf(fiveYearObj$polys)
    # else NULL
    # polys_sf <- st_cast(polys_sf, "MULTIPOLYGON")
    
    # Calculate the distance to the vector layers
    
    # Unpaved distance calculation
    if (!is.null(fiveYearObj[["unpaved"]])) {
      pts_yr <- extractDistto(
        DT = pts_yr,
        feature = fiveYearObj[["unpaved"]],
        name = "unpaved",
        where = "both",
        crs = crs_year,
        intYr = unique(pts_yr$int.year)
      )
    } else {
      pts_yr[, dist_unpaved_end := NA_real_]
    }
    
    # Paved distance calculation
    if (!is.null(fiveYearObj[["paved"]])) {
      pts_yr <- extractDistto(
        DT = pts_yr,
        feature = fiveYearObj[["paved"]],
        name = "paved",
        where = "both",
        crs = crs_year,
        intYr = unique(pts_yr$int.year)
      )
    } else {
      pts_yr[, dist_paved_end := NA_real_]
    }
    
    # Polygon distance calculation
    if (!is.null(fiveYearObj[["polys"]])) {
      pts_yr <- extractDistto(
        DT = pts_yr,
        feature = fiveYearObj[["polys"]],
        name = "polys",
        where = "both",
        crs = crs_year,
        intYr = unique(pts_yr$int.year)
      )
    } else {
      pts_yr[, dist_polys_end := NA_real_]
    }
    
    # Combine the annual extractions and 5 year distance calculations
    dt <- data.table(
      cbind(
        as.data.frame(pts_yr),
        vals_start,
        vals_end
      )
    )
    dt$year <- as.integer(yr)
    
    return(dt)
  })
  
  # Combine all years
  extractedLand <- rbindlist(extracted_list, fill = TRUE)
  message("Extraction complete: ", nrow(extractedLand), " records.")
  
  return(extractedLand)
}
