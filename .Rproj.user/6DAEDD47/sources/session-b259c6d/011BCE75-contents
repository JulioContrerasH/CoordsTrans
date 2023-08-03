library(rgee)
library(sf)



ee_Initialize(user = "julio.contreras1@unmsm.edu.pe", drive = T)



# Previous functions ------------------------------------------------------

# Function 1: Extracts coordinates (latitude, longitude) from an Earth Engine point object.
point_coordinates <- function(point, proj) {
  Coords <- ee$Feature(point)$transform(proj$crs())$geometry()$coordinates()
}



# Function 2: Extracts initial coordinate value (x or y) from the projection.
init_coord_image <- function(axis, proj) {
  name <- NULL
  if (axis == "x") {
    name <- "elt_0_2"
  } else if (axis == "y") {
    name <- "elt_1_2"
  } else {
    stop("Axis must be 'x' or 'y'")
  }
  ee$Number$parse(
    ee$String(proj$transform()$match(sprintf('"%s", (-?\\d+\\.?\\d*)', name))$get(1))
  )
}



# Function 3: Transfers a coordinate by scaling and rounding it with a given scale.
coord_transfer <- function(coord, coord_ini, scale) {
  coord_ini$add(coord$add(coord_ini$multiply(-1))$multiply(1 / scale)$round()$multiply(scale))$add(scale / 2)
}



# Method with image metadata extraction -----------------------------------

get_trans_coordsI <- function(point) {
  
  # Convert the point into an Earth Engine geometry object.
  ee_point <- sf_as_ee(point$geometry)
  
  # Get a Sentinel-2 image collection containing the point.
  ee_ic_ref <- ee$ImageCollection("COPERNICUS/S2_HARMONIZED") %>% 
    ee$ImageCollection$filterBounds(ee_point) %>% 
    ee$ImageCollection$filter(ee$Filter$eq('MGRS_TILE', point$s2tile)) %>% 
    ee$ImageCollection$first()
  
  # Get the projection of band B2 from the first image.
  proj <- ee_ic_ref$select("B2")$projection()
  
  # Obtain latitude and longitude coordinates of the point in the defined projection.
  Coords <- point_coordinates(ee_point, proj)
  x <- ee$Number(Coords$get(0))
  y <- ee$Number(Coords$get(1))
  
  # Obtain initial coordinates for the transformation.
  x_ini <- init_coord_image("x", proj)
  y_ini <- init_coord_image("y", proj)
  
  # Perform coordinate transformation to obtain new coordinates.
  new_x <- coord_transfer(x, x_ini, 10)
  new_y <- coord_transfer(y, y_ini, 10)
  
  # Create a new point with the transformed coordinates and the defined projection.
  new_point <- ee$Geometry$Point(c(new_x, new_y), proj$crs())$transform()
  
  # Update the image in the collection with the new coordinates and the EPSG code of the projection.
  ee_ic_ref$set(list("new_x" = new_x))$
    set(list("new_y" = new_y))$
    set(list("new_crs" = ee$Number$parse(proj$crs()$match("EPSG:(\\d+)")$get(1))))
}


# Read spatial data from a GeoJSON file into 'points' object.
points <- read_sf("Data/s2landsatpairs.geojson")

# Iterate through each row (point) in the 'points' object.
container <- list()
for (i in 1:nrow(points)) {
  print(i)  
  container[[i]] <- get_trans_coordsI(points[i, ])
}

# Create an Earth Engine Image Collection 'imgs_container' from the list of transformed coordinates in 'container'.
imgs_container <- ee$ImageCollection$fromImages(container)

# Extract the transformed coordinates from the 'imgs_container' and store them in a data frame 'df'.
df <- data.frame(
  ee$Dictionary$fromLists(
    c("new_x", "new_y", "new_crs"), 
    c(
      imgs_container$aggregate_array("new_x"), 
      imgs_container$aggregate_array("new_y"),
      imgs_container$aggregate_array("new_crs")
    )
  )$getInfo()
)



# Method with feature metadata extraction ---------------------------------

get_trans_coordsF <- function(point) {
  
  # Convert the point into an Earth Engine geometry object.
  ee_point <- sf_as_ee(point$geometry)
  
  # Get a Sentinel-2 image collection containing the point.
  ee_ic_ref <- ee$ImageCollection("COPERNICUS/S2_HARMONIZED") %>% 
    ee$ImageCollection$filterBounds(ee_point) %>% 
    ee$ImageCollection$filter(ee$Filter$eq('MGRS_TILE', point$s2tile)) %>% 
    ee$ImageCollection$first()
  
  # Get the projection of band B2 from the first image.
  proj <- ee_ic_ref$select("B2")$projection()
  
  # Obtain latitude and longitude coordinates of the point in the defined projection.
  Coords <- point_coordinates(ee_point, proj)
  x <- ee$Number(Coords$get(0))
  y <- ee$Number(Coords$get(1))
  
  # Obtain initial coordinates for the transformation.
  x_ini <- init_coord_image("x", proj)
  y_ini <- init_coord_image("y", proj)
  
  # Perform coordinate transformation to obtain new coordinates.
  new_x <- coord_transfer(x, x_ini, 10)
  new_y <- coord_transfer(y, y_ini, 10)
  
  # Create a new point with the transformed coordinates and the defined projection.
  new_point <- ee$Geometry$Point(c(new_x, new_y), proj$crs())$transform()
  
  # Create an Earth Engine feature with the new_point geometry and set the transformed coordinates as properties.
  ee$Feature(new_point)$set(list("new_x" = new_x))$
    set(list("new_y" = new_y))$
    set(list("new_crs" = ee$Number$parse(proj$crs()$match("EPSG:(\\d+)")$get(1))))
}


# Read spatial data from a GeoJSON file into the 'points' object.
points <- read_sf("Data/s2landsatpairs.geojson")

# Iterate through each row (point) in the 'points' object.
container <- list()
for (i in 1:nrow(points)) {
  print(i) 
  container[[i]] <- get_trans_coordsF(points[i, ])
}

# Create an Earth Engine FeatureCollection 'Features' from the list of Earth Engine features in 'container'.
Features <- ee$FeatureCollection(container)

# Convert the Earth Engine FeatureCollection to an sf object 'points_desk' (Simple Features) via the 'drive' method.
points_desk <- ee_as_sf(x = Features, via = "drive")



# Method work on the desktop -----------------------------------------------------
get_trans_coords <- function(point) {
  ee_point <- sf_as_ee(point$geometry)
  ee_ic_ref <- ee$ImageCollection("COPERNICUS/S2_HARMONIZED") %>% 
    ee$ImageCollection$filterBounds(ee_point) %>% 
    ee$ImageCollection$filter(ee$Filter$eq('MGRS_TILE', point$s2tile)) %>% 
    ee$ImageCollection$first()
  proj_metadata <- ee_ic_ref$select("B2")$projection()$getInfo()
  proj_transform <- proj_metadata$transform
  proj_crs <- proj_metadata$crs
  geom <- point$geometry
  geom_utm <- st_transform(geom, proj_crs)
  x <- geom_utm[[1]][1]
  y <- geom_utm[[1]][2]
  new_x <- proj_transform[3] + round((x - proj_transform[3]) / 10) * 10 + 10 / 2
  new_y <- proj_transform[6] + round((y - proj_transform[6]) / 10) * 10 + 10 / 2
  new_geom_utm <- st_sfc(st_point(c(new_x, new_y)), crs = proj_crs)
  
}

