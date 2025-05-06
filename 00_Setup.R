#To update the project versions and dependancies, run the following command 
#renv::snapshot()

options(timeout = 1200)
inla.setOption(scale.model.default=TRUE)

#Create data folder and output folders in working directory
if(!dir.exists("Data")) dir.create("Data")
if(!dir.exists("Data/Spatial")) dir.create("Data/Spatial")
if(!dir.exists("Output")) dir.create("Output")
if(!dir.exists("Output/Plots")) dir.create("Output/Plots")


#Assign directories
out.dir <- "Output/"
data.dir <- "Data/"
spatial.dir <- "Data/Spatial/"
plot.dir <- "Output/Plots/"

#Load required libraries
# 
# install.packages("librarian")
# install.packages("devtools")
# install.packages("remotes")
# remotes::install_github("BirdsCanada/naturecounts")
# install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE) 
# devtools::install_github("ropensci/rnaturalearthhires")

librarian::shelf("BirdsCanada/naturecounts", tidyverse, sf, mapview, sdmpredictors,
                 svMisc, terra, geojsonsf, leaflet, HelpersMG, gdalUtilities, ggplot2,
                 exactextractr, readxl, reshape, ggmap, gridExtra, ggspatial, prettymapr, 
                 rnaturalearth, mapview, rnaturalearthhires, INLA, mgcv, sn, fmesher, inlabru, splines, 
                 maps, splancs, spdep, igraph, ggspatial, terra, tidyterra, stringr, reshape2, measurements, ggOceanMaps)


BMDE<-meta_bmde_fields("core")

sp.c<-meta_species_codes()
sp.c<-sp.c %>% filter(authority=="BSCDATA") %>% distinct() %>% select(species_id, species_code)

sp.tax<-meta_species_taxonomy()

sp<-left_join(sp.c, sp.tax, by="species_id")
sp<-sp %>% distinct(english_name, .keep_all = TRUE)

sp<-sp %>% dplyr::select(species_code, scientific_name, english_name) %>% distinct()

# Function to calculate duration in hours
calculate_duration <- function(start, end) {
  # Convert start and end times to total minutes
  start_minutes <- floor(start) * 60 + round((start - floor(start)) * 60)
  end_minutes <- floor(end) * 60 + round((end - floor(end)) * 60)
  
  # Calculate the duration in minutes
  duration_minutes <- end_minutes - start_minutes
  
  # Convert duration back to hours
  duration_hours <- duration_minutes / 60
  
  return(duration_hours)
}

utm_crs <- paste0("EPSG:326", sprintf("%02d", 10))


 epsg6703km <- paste(
   "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5",
   "+lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83",
   "+units=km +no_defs"
 )

##LOESS function

loess_func <- function(i,y){
  tmp <- loess(i~y, 
               span=0.55, na.action = na.exclude)
  preds <- predict(tmp)
  return(preds)
}

#create plot function
make_plot_field <- function(data_stk, scale_label) {
  ggplot(map) +
    geom_sf(fill = NA) +
    coord_sf(datum = NA) +
    geom_spatraster(data = data_stk) +
    labs(x = "", y = "") +
    scale_fill_distiller(scale_label,
                         palette = "Spectral",
                         na.value = "transparent") +
    theme_bw() +
    geom_sf(fill = NA)
}
make_plot_site <- function(data, scale_label) {
  ggplot(map) +
    geom_sf() +
    coord_sf(datum = NA) +
    geom_sf(data = data, size = 1, mapping = aes(colour = value)) +
    scale_colour_distiller(scale_label, palette = "Spectral") +
    labs(x = "", y = "") +
    theme_bw() +
    geom_sf(fill = NA)
}


#Guild default is "No" unless otherwise changed in the setup script
guild<-"No"

##This is the Polygon of the Salish Sea
##Need to download and save the Bathymetry TIFF file from here:https://salish-sea-atlas-data-wwu.hub.arcgis.com/

# bathy <- rast("Data/Spatial/SS_Bathymetry.tif")
# plot(bathy)
# 
# water_mask <- classify(bathy, cbind(-Inf, 0, 1), others = NA)
# 
# # Convert to polygon and clean
# water_poly <- as.polygons(water_mask, dissolve = TRUE) |> 
#   st_as_sf() |> 
#   st_simplify(dTolerance = 100)  # Simplify with 100m tolerance
# 
# plot(water_mask)
# 
# # Suppose your polygon is called 'poly'
# # Ensure your CRS uses meters (projected, not geographic/longlat)
# st_crs(water_poly) # Check CRS
# 
# # If necessary, transform to a projected CRS (example: UTM zone 10N)
# poly_proj <- st_transform(water_poly, 32610) # EPSG:32610 is UTM 10N
# 
# # Create a 5km buffer
# poly_buffer <- st_buffer(poly_proj, dist = 5000)
# 
# # Plot to check
# plot(st_geometry(poly_proj), col = 'lightblue')
# plot(st_geometry(poly_buffer), border = 'red', add = TRUE)
# 
# st_write(water_buffer, "Data/Spatial/Salish_Sea_Water_Polygon.shp")
# 
# canada <- ne_states(country = "canada", returnclass = "sf") 
# BC<- canada[canada$name=="British Columbia",]
# 
# us<- ne_states(country = "united states of america", returnclass = "sf") 
# WA<- us[us$name=="Washington",]
# 
# # Ensure both layers use the same CRS
# if (st_crs(map) != st_crs(BC)) {
#   map <- st_transform(map, st_crs(BC))
# }
# 
# # Clip the water polygon to BC
# map_BC <- st_intersection(map, BC)
# st_write(map_BC, "Data/Spatial/BC_Water_Polygon.shp", layer_options = "ENCODING=UTF-8")
# 
# # Ensure both layers use the same CRS
# if (st_crs(map) != st_crs(WA)) {
#   map <- st_transform(map, st_crs(WA))
# }
# 
# # Clip the water polygon to BC
# map_WA <- st_intersection(map, WA)
# st_write(map_WA, "Data/Spatial/WA_Water_Polygon.shp", layer_options = "ENCODING=UTF-8")


