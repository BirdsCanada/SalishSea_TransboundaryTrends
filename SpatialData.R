#Spatail data preprocessing

#Specify the name of your shapefile


#############################################################
#Coastal Watersheds US Only

CW_watershed <- st_read("Data/Spatial/2024_CW_Watersheds.shp")
#determine the CRS of the shapefile
shp_crs<-st_crs(CW_watershed)

CW_watershed <- CW_watershed %>%
  filter(!is.na(NAME_12))
#filter point for PSSS
points.s<-points %>% filter(ProjectCode=="PSSS")

#Make points into spatial data frame
points.ss <- st_as_sf(points.s, coords = c("DecimalLongitude", "DecimalLatitude"), crs = shp_crs)

#assign each point.s to the nearest watershed using the st_nearst_feature function
nearest <- st_nearest_feature(points.ss, CW_watershed)
points.s$watershed<-CW_watershed$NAME_12[nearest] 
CW_watershed<-CW_watershed %>% filter(NAME_12 %in% unique(points.s$watershed))

ggplot(data = CW_watershed) +
  geom_sf(aes(fill = NAME_12)) +  # Map fill color to NAME_12
  geom_point(data = points.s, aes(x = DecimalLongitude, y = DecimalLatitude), color = "black", size = 1) +
  labs(title = "Coastal Watersheds",
       fill = "Watershed Name")  
theme_minimal() +  # Optional: Use a minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis text
        legend.position = "right"  # Optional: Position the legend
  )

###############################################################
#Open the Midwinter Aerial Survey geodatabase

#filter point for PSSS
points.s<-points %>% filter(ProjectCode=="PSSS")
points_sf <- st_as_sf(points.s, coords = c("DecimalLongitude", "DecimalLatitude"), crs = 4326)  

#specify path
path <- "Data/Spatial/MidwinterAerialSeabirdSurveys.gdb"
layers <- "PSEMP_Analysis_Strata"
psemp_data <- st_read(path, layer = layers)
shp_crs<-st_crs(psemp_data)
psemp_data$StrataID <- as.factor(psemp_data$StrataID)


# Cast the geometry to MULTIPOLYGON
psemp_data$Shape <- st_cast(psemp_data$Shape, "MULTIPOLYGON")
psemp_data <- st_make_valid(psemp_data)

# Transform sample locations to NAD83 / UTM zone 10N
points_sf <- st_transform(points_sf, crs = st_crs(psemp_data))

# Basic plot using ggplot2
ggplot(data = psemp_data) +
  geom_sf(aes(fill = Basin)) + 
  geom_sf(data = points_sf, color="black", size = 1)+ 
  labs(title = "PSEMP Basins",
       fill = "Basin Name") +
  #theme_minimal() +  # Optional: Use a minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis text
        legend.position = "right"  # Optional: Position the legend
  )

#assign each point.s to the nearest watershed using the st_nearst_feature function
nearest <- st_nearest_feature(points_sf, psemp_data)
points.s$Basin<-psemp_data$Basin[nearest] 
psemp_data<-psemp_data %>% filter(Basin %in% unique(points.s$Basin))

###################################################
#Canada Coastal Marine Emergency Planning

#read in the shapefile
canada <- st_read("Data/Spatial/UpdatesAreaPlanLayers.shp")
canada <- st_make_valid(canada)

points.n<-points %>% filter(ProjectCode=="BCCWS")
points_sfn <- st_as_sf(points.n, coords = c("DecimalLongitude", "DecimalLatitude"), crs = 4326)  

ggplot(data = canada) +
  geom_sf(aes(fill = Area)) + 
  geom_sf(data = points_sfn, color="black", size = 1)+ 
  labs(title = "Canada Coastal Marine Emergancy Planning",
       fill = "Planning Unit") +
  #theme_minimal() +  # Optional: Use a minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),  # Rotate x-axis text
        legend.position = "right"  # Optional: Position the legend
  )