library(leaflet)
library(leaflet.extras)  # For additional map controls
library(terra)
library(rayshader)
library(sf)

wd <- "/Users/ronja/Documents/EAGLE/Svalbard"
setwd(wd)

# Section to create a map of Svalbard in leaflet--------------
# Define WMTS URLs
svalbard_basemap_wmts <- "https://geodata.npolar.no/arcgis/rest/services/Basisdata/NP_Basiskart_Svalbard_WMTS_3857/MapServer/WMTS/tile/1.0.0/Basisdata_NP_Basiskart_Svalbard_WMTS_3857/default/default028mm/{z}/{y}/{x}.jpgpng"

svalbard_satellite_wmts <- "https://geodata.npolar.no/arcgis/rest/services/Basisdata/NP_Satellitt_Svalbard_WMTS_3857/MapServer/WMTS/tile/1.0.0/Basisdata_NP_Satellitt_Svalbard_WMTS_3857/default/default028mm/{z}/{y}/{x}.jpgpng"

# Create Leaflet map with multiple basemaps and overlays
leaflet() %>%
  setView(lng = 15, lat = 78, zoom = 5) %>%  # Center on Svalbard
  
  # Add basemap options
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri Satellite") %>%
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Light") %>%
  
  # Add the Svalbard Basemap WMTS layer
  addWMSTiles(
    baseUrl = svalbard_basemap_wmts,
    layers = "Basisdata_NP_Basiskart_Svalbard_WMTS_3857",
    options = WMSTileOptions(format = "image/jpgpng", transparent = TRUE),
    group = "Svalbard Basemap"
  ) %>%
  
  # Add the Svalbard Satellite WMTS layer
  addWMSTiles(
    baseUrl = svalbard_satellite_wmts,
    layers = "Basisdata_NP_Satellitt_Svalbard_WMTS_3857",
    options = WMSTileOptions(format = "image/jpgpng", transparent = TRUE),
    group = "Svalbard Satellite"
  ) %>%
  
  # Add layer control to switch between base layers and overlays
  addLayersControl(
    baseGroups = c("Esri Satellite", "OpenStreetMap", "CartoDB Light"),
    overlayGroups = c("Svalbard Basemap", "Svalbard Satellite"),
    options = layersControlOptions(collapsed = FALSE)
  )

#----------- Section 2
# Step 1: Read the CSV file into R
data <- vect("larissa_sos/BjorndalenReindeer_Points.gpkg")

# Remove rows with missing values in utm_easting or utm_northing
data_clean <- data %>%
  filter(!is.na(utm_easting) & !is.na(utm_northing))

# Step 2: Create an sf object with UTM coordinates (EPSG:25833)
data_sf <- st_as_sf(data_clean, coords = c("utm_easting", "utm_northing"), crs = 25833)
data_sp <- as(data_sf, "Spatial")

data_sf_cropped <- crop(data, dem)

# Step 3: Reproject the coordinates to EPSG:3857 (Web Mercator)
data_projected <- st_transform(data_sf, crs = 4326)
z_values <- extract(dem, data_sf_cropped)
data_sf_cropped$elevation <- z_values[, 2]

# Step 4: Extract the longitude and latitude (in EPSG:3857) as numeric vectors
coords_x <- geom(data_sf_cropped)[, c("x")]
coords_y <- geom(data_sf_cropped)[, c("y")]

lng <- st_coordinates(data_sf_cropped)[, 1]
lat <- st_coordinates(data_sf_cropped)[, 2]
ele <- data_sf_cropped$elevation

coords_x <- as.numeric(unlist(coords_x))
coords_y <- as.numeric(unlist(coords_y))
z_values <- unlist(ele)

length(coords_x)
length(coords_y)
length(z_values)



# Load the DEM
dem <- rast("larissa_sos/DEM_Bjorndalen.tif")
dem

# Convert the cropped DEM to a matrix
bjorndalen <- raster_to_matrix(dem)

bjorndalen_shade <- sphere_shade(bjorndalen, texture = "desert")
add_overlay(generate_contour_overlay(bjorndalen))
plot_map(bjorndalen_shade)

plot_3d(bjorndalen_shade, bjorndalen, zscale = 20, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))



# Step 3: Add 3D points without the 'extend' argument
render_points(lat = coords_y, long = coords_x, 
              altitude = z_values, zscale = 50, color = "pink",
              extent = ext(dem),
              offset = 50,
              clear_previous = T,
              size = 10)

plot(data_sf_cropped, add = T)
plot(dem)
