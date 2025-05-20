sp.tax<-meta_species_taxonomy()
sp.tax<-sp.tax %>% dplyr::select(species_id, sort_order, english_name)

#read indices and trends for a specific site

indices<-read.csv(paste(out.dir, name, "_AnnualIndices_SPDE.csv", sep=""))
all.trends<-read.csv(paste(out.dir, name, "_TrendsEndPoint_SPDE.csv", sep=""))

##Prepare trends for plotting
#Remove any row with all NA values across all columns
all.trends <- all.trends[rowSums(is.na(all.trends)) < ncol(all.trends), ]
all.trends<-all.trends %>% dplyr::select(area_code, period, species_code, species_id, years, trnd, lower_ci, upper_ci, index_type, percent_change) %>% filter(period=="all years")

if(guild == "NO"){
all.trends<-left_join(all.trends, sp.tax, by=c("species_id"="species_id"))
} 

ed.trnd <- all.trends %>%
  mutate(sp.trnd = paste(species_code, " Trend: ", round( trnd, digits = 2),  " (", round( lower_ci, digits = 2), ", ", round( upper_ci, digits = 2), ")", sep = "")) %>% dplyr::select(-trnd, -upper_ci, -lower_ci)
ed.trnd <- ed.trnd %>% dplyr::select(species_code, sp.trnd, area_code)

#Prepare Indices for plotting
indices <- indices[rowSums(is.na(indices)) < ncol(indices), ]
index <- indices %>%
  dplyr::select(index, upper_ci, lower_ci, LOESS_index, species_code, year, season, area_code) 

plot.dat <- NULL
plot.dat <- full_join(index, ed.trnd, by = c("species_code", "area_code"), relationship = "many-to-many")
plot.dat <- plot.dat %>% filter(area_code %in% c("SalishSea", "PSSS", "BCCWS"))

sp.list2 <- as.character(unique(plot.dat$sp.trnd))

# Calculate number of batches needed
num_batches <- ceiling(length(sp.list2)/6)

# Create PDF outside of loop
pdf(paste(plot.dir, name, "_IndexPlot.pdf", sep=""),
    height = 10, width = 8, paper = "letter")

# Process each batch
for(k in 1:num_batches) {
  # Calculate indices for this batch
  start_idx <- (k-1)*6 + 1
  end_idx <- min(k*6, length(sp.list2))
  
  # Only process if we have valid indices
  if(start_idx <= length(sp.list2)) {
    # Create plot for current batch
    current_plot <- ggplot(data = subset(plot.dat, sp.trnd %in% sp.list2[start_idx:end_idx]), 
                           aes(x = as.numeric(year), y = index)) +
      facet_wrap(~ sp.trnd, ncol = 2, scales = "free", as.table = TRUE) +
      geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci)) +
      geom_smooth(aes(ymin = lower_ci, ymax = upper_ci), method = "loess") + 
      xlab("Year") +
      ylab("Annual Index") +
      theme_bw() +
      scale_y_continuous(trans="log10") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      theme(legend.position = "none") +
      theme_classic()
    
    # Print directly to PDF
    print(current_plot)
  }
}

# Close PDF device once after all plots are done
dev.off()

