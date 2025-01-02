#To update the project versions and dependancies, run the following command 
#renv::snapshot()

options(timeout = 1200)

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
#install.packages("remotes")
#remotes::install_github("BirdsCanada/naturecounts")

#install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE) 

#devtools::install_github("ropensci/rnaturalearthhires")

librarian::shelf("BirdsCanada/naturecounts", tidyverse, sf, mapview, sdmpredictors,
                 svMisc, terra, geojsonsf, leaflet, HelpersMG, gdalUtilities, ggplot2,
                 exactextractr, readxl, reshape, ggmap, gridExtra, ggspatial, prettymapr, 
                 rnaturalearth, mapview, rnaturalearthhires, INLA, mgcv, sn, fmesher, inlabru, maps, splancs)

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

# 
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
