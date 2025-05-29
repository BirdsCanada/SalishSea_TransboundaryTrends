sp.tax<-meta_species_taxonomy()
sp.tax<-sp.tax %>% dplyr::select(species_id, sort_order, english_name)

#read indices and trends for a specific site

indices<-read.csv(paste(out.dir, name, "_AnnualIndices_iCAR.csv", sep=""))
all.trends<-read.csv(paste(out.dir, name, "_TrendsEndPoint_iCAR.csv", sep=""))

##Prepare trends for plotting
#Remove any row with all NA values across all columns
all.trends <- all.trends[rowSums(is.na(all.trends)) < ncol(all.trends), ]
all.trends<-all.trends %>% dplyr::select(area_code, period, species_code, species_id, years, trnd, lower_ci, upper_ci, index_type, percent_change) %>% filter(period=="all years")

if(guild == "No"){
all.trends<-left_join(all.trends, sp.tax, by=c("species_id"="species_id"))
} 

ed.trnd <- all.trends %>% 
  mutate(sp.trnd = paste(round( trnd, digits = 2),  " (", round( lower_ci, digits = 2), ", ", round( upper_ci, digits = 2), ")", sep = "")) %>% dplyr::select(-trnd, -upper_ci, -lower_ci)
ed.trnd <- ed.trnd %>% dplyr::select(english_name, species_code, sp.trnd, area_code)

#Prepare Indices for plotting
indices <- indices[rowSums(is.na(indices)) < ncol(indices), ]
index <- indices %>%
  dplyr::select(index, species_code, upper_ci, lower_ci, LOESS_index, year, season, area_code) 

plot.dat <- NULL
plot.dat <- full_join(index, ed.trnd, by = c("area_code", "species_code"), relationship = "many-to-many")

if(name == "SalishSea_Species"){
  plot.dat$area_code[plot.dat$area_code == "Full Study Area"] <- "Salish Sea"
}

title <- NULL
title <- plot.dat %>%
  select(english_name, area_code, sp.trnd) %>%
  distinct() %>%  # Remove duplicates
  group_by(english_name) %>%  # Process each species separately
  mutate(
    # Combine area code with trend for each line
    area_trend = paste0(area_code, " ", sp.trnd),
    # Create full title: species name + all area trends (line breaks)
    full_title = paste(
      english_name, 
      paste(area_trend, collapse = "\n"), 
      sep = "\n"
    )
  ) %>% 
  select(english_name, full_title) %>% 
  distinct() 

plot.dat <- left_join(plot.dat, title, by="english_name")

plot.dat$species <- plot.dat$full_title
sp.list2 <- unique(plot.dat$full_title) 

# Calculate number of batches needed
num_batches <- ceiling(length(sp.list2)/6)

# Create PDF outside of loop
pdf(paste(plot.dir, name, "_IndexPlot_", model, ".pdf", sep=""),
    height = 10, width = 8, paper = "letter")

for(k in 1:num_batches) {
  start_idx <- (k-1)*6 + 1
  end_idx <- min(k*6, length(sp.list2))
  
  if(start_idx <= length(sp.list2)) {
    current_sp_trnd <- sp.list2[start_idx:end_idx]
    dat_sub <- subset(plot.dat, species %in% current_sp_trnd)
    
    if(nrow(dat_sub) > 0) {
      
      dat_sub <- dat_sub %>%
        filter(index > 0, lower_ci > 0, upper_ci > 0, LOESS_index > 0)
      
      current_plot <- ggplot(dat_sub, 
                             aes(x = as.numeric(year), y = index, colour = area_code)) +
        facet_wrap(~ full_title, ncol = 2, scales = "free_y", as.table = TRUE) +
        geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.7) +
        geom_line(aes(y = LOESS_index), linewidth = 0.8) +
        # Black-and-white friendly color palette
        scale_color_grey(start = 0, end = 0.8, name = "Region") +
        scale_y_continuous(trans = "log10", expand = expansion(mult = c(0.1, 0.1))) +
        #scale_y_continuous() +
        xlab("Year") +
        ylab("Annual Index (log scale)") +
        #ylab("Annual Index") +
        theme_classic() +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1, color = "black"), # Vertical, black
          strip.background = element_blank(),
          strip.text = element_text(face = "bold"),
          legend.position = "bottom",             # Put legend at bottom
          legend.direction = "horizontal"         # Make legend horizontal
  
        )
      
      print(current_plot)
    }
  }
}

while (!is.null(dev.list())) dev.off()

