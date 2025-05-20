all.trends<-read.csv(paste(out.dir, name, "_TrendsEndPoint_SPDE.csv", sep=""))
all.trends<-all.trends %>% drop_na(results_code)

map<- st_read("Data/Spatial/Salish_Sea_Water_Polygon.shp")
events_sf <- st_transform(map, st_crs(Grid))


# Create individual maps for each species and save them
species_list3 <- unique(all.trends$species_code)
for(current_sp in species_list3) {
  # Filter data for current species
  species_data <- all.trends %>% 
    filter(species_code == current_sp)
  
  # Join with spatial grid
  Grid_sp <- Grid %>%
    left_join(species_data, by = c("Name" = "area_code"))
  
  # Calculate species-specific limits
  species_min <- min(species_data$trnd, na.rm = TRUE)
  species_max <- max(species_data$trnd, na.rm = TRUE)
  
  # Create map with dynamic scale
  p <- ggplot() +
    geom_sf(data = Grid_sp, aes(fill = trnd), color = "white", size = 0.1) +
    scale_fill_viridis_c(
      name = "Annual Trends", 
      limits = c(species_min, species_max),  # Now species-specific
      na.value = "grey90"  # Handle NA values
    ) +
    geom_sf(data = map, size = 0.2) +
    labs(title = paste("Trends for", current_sp)) +
    theme_minimal()
  
  # Save map
  ggsave(paste0(plot.dir, "iCAR_Map_", gsub(" ", "_", current_sp), ".jpeg"), 
         plot = p, width = 8, height = 6, dpi = 300)
}