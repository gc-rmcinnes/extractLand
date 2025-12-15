extractLandscape <- function(sim){
  message("Starting extraction...")
  # valid years only
  tracks <- sim$tracks
  years <- sort(unique(tracks$year))
  #need to add a stop if theres no valid years
  validYears <- intersect(as.character(years), Par$histLandYears)

  extracted_list <- lapply(sort(intersect(as.character(years), validYears)), function(yr){
    message("Extracting for year ", yr)
    pts_yr <- tracks[year == as.integer(yr), ]

    # Annual raster layers
    annual_rasts <- sim$landscapeYearly[[paste0("year", yr)]]

    # 5-Year vector layers
    fiveYearInt <- paste0("intYear", pts_yr$int.year[1])
    fiveYearObj <- sim$landscape5Yearly[[fiveYearInt]]

    crs_year <- terra::crs(annual_rasts)

    # Annual raster value extraction
    # Extract Start raster values
    vals_start <- terra::extract(
      annual_rasts,
      terra::vect(pts_yr[, .(x1_, y1_)], geom = c("x1_", "y1_"),
                  crs = crs_year)
    )[, -1, drop = FALSE]

    setnames(vals_start, paste0(names(vals_start), "_start"))

    # Extract End raster values
    vals_end <- terra::extract(
      annual_rasts,
      terra::vect(pts_yr[, .(x2_, y2_)], geom = c("x2_", "y2_"),
                  crs = crs_year)
    )[, -1, drop = FALSE]

    setnames(vals_end, paste0(names(vals_end), "_end"))

    # Convert 5-year SpatVectors to sf

    # PAVED (SpatVectorCollection)
    # May need to add a second paved conversion for the other layer
    paved_sf <- if (!is.null(fiveYearObj$paved))
      st_as_sf(fiveYearObj$paved[[2]])
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
        int.yr = pts_yr$int.year[1]
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
        int.yr = pts_yr$int.year[1]
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
        int.yr = pts_yr$int.year[1]
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
  extractVar <- rbindlist(extracted_list, fill = TRUE)
  message("Extraction complete: ", nrow(extractVar), " records.")
  return(extractVar)
}
