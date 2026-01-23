extractLandFeatures <- function(tracks, 
                                landscapeYearly, 
                                landscape5Yearly, 
                                histLandYears,
                                saveYearlyExtracted,
                                outputDir,
                                hashExtracted) {
  
  message("Starting extraction...")
  
  # valid years only
  years <- sort(unique(tracks$year))
  validYears1 <- intersect(as.character(years), histLandYears)
  landYears <- substr(x = names(landscapeYearly$landscapeYearly[[1]]), 
                      start = nchar(names(landscapeYearly$landscapeYearly)) - 4, 
                      nchar(names(landscapeYearly$landscapeYearly)))
  validYears2 <- intersect(landYears, validYears1)
  
  if (saveYearlyExtracted){
    dig1 <- digest::digest(landscapeYearly) 
    dig2 <- digest::digest(landscape5Yearly)
    dig <- digest::digest(c(dig1, dig2, hashExtracted))
    generalNaming <- paste0("extractedFeatures_",dig)
    yearsNaming <- paste0(validYears2[1], "_", validYears2[length(validYears2)])
    extractedLandName <- file.path(outputDir, paste0(generalNaming, "_", yearsNaming, ".csv"))
  }

  if (all(saveYearlyExtracted,
          file.exists(extractedLandName))){
    
    message(paste0("Loading final table for years ", yearsNaming))
    
    extractedLand <- fread(extractedLandName)

  } else {
    
  extracted_list <- lapply(sort(intersect(as.character(years), validYears2)), function(yr){
    
    tableName <- file.path(outputDir, paste0(generalNaming, "_", yr, ".csv"))
    
    if (all(saveYearlyExtracted, file.exists(tableName))){
      
      message(paste0("Loading table for year ", yr))
      dt <- data.table::fread(tableName)
      
    } else {
     
      message(paste0("Extracting features for year ", yr))
      
      pts_yr <- tracks[year == as.integer(yr), ]
      
      # Annual raster layers
      annual_rasts <- landscapeYearly$landscapeYearly[[1]][[paste0("year", yr)]] #TODO: This needs to go into a lapply. I messed it up when trying to fix the code...

      # 5-Year vector layers
      fiveYearInt <- paste0("intYear", unique(pts_yr$int.year))

      fiveYearObj <- landscape5Yearly[[fiveYearInt]]
      
      # TODO If fiveYearObj is NULL (i.e., a needed 2005)
      # we for now use the 2010 layer. Later, if we get a 
      # 2005, we replace in prepLandscape / prep_anthroDisturbance / landscape5Yearly object
      if (is.null(fiveYearObj))
        fiveYearObj <- landscape5Yearly[[1]]
      
      crs_year <- terra::crs(annual_rasts[[1]])
      
      # Annual raster value extraction
      # Extract Start raster values
      pts_yr_start <- terra::vect(pts_yr[, .(x1_, y1_)], geom = c("x1_", "y1_"),
                                  crs = terra::crs(annual_rasts[[1]]))
      if (is(annual_rasts, "list")){
        stop("annual_rasts is a list. This shouldn't happen. Please debug.")
      }
      vals_start <- terra::extract(annual_rasts, pts_yr_start)#[, -1, drop = FALSE]
      
      setnames(vals_start, paste0(names(vals_start), "_start"))
      
      # Extract End raster values
      pts_yr_end <- terra::vect(pts_yr[, .(x2_, y2_)], geom = c("x2_", "y2_"),
                                crs = terra::crs(annual_rasts[[1]]))
      
      vals_end <- terra::extract(annual_rasts, pts_yr_end)#[, -1, drop = FALSE]
      
      setnames(vals_end, paste0(names(vals_end), "_end"))
      
      # Convert 5-year SpatVectors to sf
      
      # PAVED (SpatVectorCollection)
      # May need to add a second paved conversion for the other layer
      paved_sf <- if (!is.null(fiveYearObj$paved))
        st_as_sf(fiveYearObj$paved[[1]]) # This was wrongly pointing to seismic lines as a [[2]]
      else NULL
      paved_sf <- st_cast(paved_sf, "MULTILINESTRING")
      
      # UNPAVED (SpatVector)
      unpaved_sf <- if (!is.null(fiveYearObj$unpaved))
        st_as_sf(fiveYearObj$unpaved)
      else NULL
      unpaved_sf <- st_cast(unpaved_sf, "MULTILINESTRING")
      
      # POLYS (SpatVector)
      polys_sf <- if (!is.null(fiveYearObj$polys))
        st_as_sf(fiveYearObj$polys)
      else NULL
      polys_sf <- st_cast(polys_sf, "MULTIPOLYGON")
      
      # Calculate the distance to the vector layers
      
      # Unpaved distance calculation
      if (!is.null(unpaved_sf)) {
        pts_yr <- extract_distto(
          DT = pts_yr,
          feature = unpaved_sf,
          name = "unpaved",
          where = "both",
          crs = crs_year,
          int.yr = unique(pts_yr$int.year) # more robust to errors than pts_yr$int.year[1]
        )
      } else {
        pts_yr[, dist_unpaved_end := NA_real_]
      }
      
      # Paved distance calculation
      if (!is.null(paved_sf)) {
        pts_yr <- extract_distto(
          DT = pts_yr,
          feature = paved_sf,
          name = "paved",
          where = "both",
          crs = crs_year,
          int.yr = unique(pts_yr$int.year)
        )
      } else {
        pts_yr[, dist_paved_end := NA_real_]
      }
      
      # Polygon distance calculation
      if (!is.null(polys_sf)) {
        pts_yr <- extract_distto(
          DT = pts_yr,
          feature = polys_sf,
          name = "polys",
          where = "both",
          crs = crs_year,
          int.yr = unique(pts_yr$int.year)
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
      
      if (saveYearlyExtracted){
        fwrite(x = dt, file = tableName)
        message(paste0("Saving the table for year ", yr, ": ", tableName))
      }
    }
    return(dt)
  })
  
  # Combine all years
  extractedLand <- rbindlist(extracted_list, fill = TRUE, use.names = TRUE)
  
  message("Extraction complete: ", nrow(extractedLand), " records.")
  
  if (saveYearlyExtracted){
    message(paste0("Saving final table: ", extractedLandName))
    fwrite(x = extractedLand, file = extractedLandName)
  }
  }
    return(extractedLand)
}