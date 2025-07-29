
# function to get a grid with the LME of interest 
# based on LME polygon and grid_size_km

get_grid_ME <- function(ME_name, grid_size_km = 10) {
  # Load ME shapefile
  meow <- read_sf("../Data/MEOW_shapefiles/meow_ecos.shp")
  meow <- st_make_valid(meow)
  
  # Select the ME of interest
  me_select <- subset(meow, meow$ECOREGION == ME_name) 
  
  if (nrow(me_select) == 0) {
    stop("ME not found. Check the name.")
  }
  
  # Determine UTM zone based on ME centroid
  centroid <- st_centroid(me_select)
  lon <- st_coordinates(centroid)[1]
  utm_zone <- floor((lon + 180) / 6) + 1
  crs_utm <- paste0("+proj=utm +zone=", utm_zone, " +datum=WGS84 +units=m +no_defs")
  
  # Transform LME to UTM projection
  me_utm <- st_transform(me_select, crs = crs_utm)
  
  # Get bounding box and add buffer
  bbox <- st_bbox(me_utm)
  buffer_m <- 10000  # 10 km buffer
  xmin <- bbox["xmin"] - buffer_m
  xmax <- bbox["xmax"] + buffer_m
  ymin <- bbox["ymin"] - buffer_m
  ymax <- bbox["ymax"] + buffer_m
  
  # Define grid step in meters
  grid_size_m <- grid_size_km * 1000
  
  # Generate sequences for grid coordinates
  x_seq <- seq(xmin, xmax, by = grid_size_m)
  y_seq <- seq(ymin, ymax, by = grid_size_m)
  
  # Create grid cells as polygons
  grid <- expand.grid(x = x_seq, y = y_seq)
  grid_polygons <- lapply(1:nrow(grid), function(i) {
    x <- grid$x[i]
    y <- grid$y[i]
    st_polygon(list(matrix(c(
      x, y,
      x + grid_size_m, y,
      x + grid_size_m, y + grid_size_m,
      x, y + grid_size_m,
      x, y  # Closing point
    ), ncol = 2, byrow = TRUE)))
  })
  
  # Create sf object
  grid_sf <- st_sf(geometry = st_sfc(grid_polygons, crs = crs_utm))
  
  # Clip grid to ME boundaries
  grid_sf <- grid_sf[st_intersects(grid_sf, me_utm, sparse = FALSE), ]
  
  # Transform back to WGS84 (lat/lon)
  grid_sf <- st_transform(grid_sf, crs = 4326)
  
  # Compute centroids
  centroids <- st_centroid(grid_sf)
  coords <- st_coordinates(centroids)
  
  # Add lat/lon columns
  grid_sf$lon <- coords[,1]
  grid_sf$lat <- coords[,2]
  
  # Load depth data
  depth_file <- paste0("../Data/Depths_ME_ETOPO_extracted/", ME_name, "_depth.RData")
  if (file.exists(depth_file)) {
    load(depth_file)  # Loads 'Depth' object
    Depth <- subset(Depth, Depth$z < 0)
    pt <- as.data.frame(st_intersects(Depth, grid_sf))
    
    # Compute average depth per grid cell
    avg_depth <- pt %>%
      group_by(col.id) %>%
      summarize(depth = mean(Depth$z[row.id]))
    
    # Ensure col.id is compatible
    avg_depth$col.id <- as.integer(avg_depth$col.id)
    
    # Assign depths to grid
    grid_sf$Depth <- NA
    grid_sf$Depth[avg_depth$col.id] <- avg_depth$depth
  } else {
    warning("Depth data not found for this ME.")
    grid_sf$Depth <- NA
  }
  
  return(grid_sf)
}
