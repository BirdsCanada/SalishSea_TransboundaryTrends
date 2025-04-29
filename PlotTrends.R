sp.tax<-meta_species_taxonomy()
sp.tax<-sp.tax %>% dplyr::select(species_id, sort_order, english_name)

#read indices and trends for a specific site

indices<-read.csv(paste(out.dir, name, "_AnnualIndices.csv", sep=""))
trends.slope<-read.csv(paste(out.dir, name, "_TrendsSlope.csv", sep=""))
trends.endpt<-read.csv(paste(out.dir, name, "_TrendsEndPoint.csv", sep=""))

##Prepare trends for plotting
all.trends<-rbind(trends.slope, trends.endpt)
#Remove any row with all NA values across all columns
all.trends <- all.trends[rowSums(is.na(all.trends)) < ncol(all.trends), ]
all.trends<-all.trends %>% dplyr::select(area_code, period, species_code, species_id, years, trnd, lower_ci, upper_ci, index_type, percent_change) %>% filter(period=="all years")

if(guild == "NO"){
all.trends<-left_join(all.trends, sp.tax, by=c("species_id"="species_id"))
} 

slope.trnd<-all.trends %>% filter(index_type=="Slope trend")

sl.trnd <- slope.trnd %>%
  mutate(sp.trnd = paste(species_code, " \n", "Slope: ", round( trnd, digits = 2),  " (", round( lower_ci, digits = 2), ", ", round( upper_ci, digits = 2), ")", sep = "")) %>% dplyr::select(-trnd, -upper_ci, -lower_ci)

end.trnd<-all.trends %>% filter(index_type=="Endpoint trend")

ed.trnd <- end.trnd %>%
  mutate(sp.trnd = paste("Endpoint: ", round( trnd, digits = 2),  " (", round( lower_ci, digits = 2), ", ", round( upper_ci, digits = 2), ")", sep = "")) %>% dplyr::select(-trnd, -upper_ci, -lower_ci)
ed.trnd <- ed.trnd %>% dplyr::select(species_code, sp.trnd)

sl.trnd<-left_join(sl.trnd, ed.trnd, by="species_code")

#paste together the two sp.trnd columns
sl.trnd$sp.trnd<-paste(sl.trnd$sp.trnd.x, sl.trnd$sp.trnd.y, sep="\n")
sl.trnd<-sl.trnd %>% dplyr::select(species_code, sp.trnd)

#Prepare Indices for plotting
indices <- indices[rowSums(is.na(indices)) < ncol(indices), ]
index <- indices %>%
  dplyr::select(index, upper_ci, lower_ci, LOESS_index, species_code, year, season, area_code) 

plot.dat <- NULL
plot.dat <- full_join(index, sl.trnd, by = c("species_code"), relationship = "many-to-many")

sp.list2 <- as.character(unique(plot.dat$sp.trnd))

out.plot <- NULL
i <- 1
j <- 6

for(k in 1:(ceiling(length(sp.list2)/6))) {
  
  out.plot[[k]] <- ggplot(data = subset(plot.dat, sp.trnd %in% sp.list2[i:j]), aes(x = as.numeric(year), y = index)) +
    facet_wrap(~ sp.trnd, ncol = 2, scales = "free", as.table = TRUE) +
    geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci)) +
    geom_smooth(aes(ymin = lower_ci, ymax = upper_ci), method = "loess") + 
    xlab("Year") +
    ylab("Annual Index") +
    theme_bw() +
    scale_y_continuous(trans="log10") +
    # scale_x_continuous(breaks = seq(from = min.yr.filt, to = max.yr.filt, by = 4)) +
    # scale_shape_manual(values = c(1,2)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    theme(legend.position = "none")+
    theme_classic()
  
  
  i <- i + 6
  j <- j + 6
  
  
  length(out.plot)
  # Plot to PDF file
  pdf(paste(plot.dir, name, "_IndexPlot.pdf", sep=""),
      height = 10, width = 8, paper = "letter")
  try(print(out.plot[[1]], silent=T))
  try(print(out.plot[[2]], silent=T))
  try(print(out.plot[[3]], silent=T))
  try(print(out.plot[[4]], silent=T))
  try(print(out.plot[[5]], silent=T))
  try(print(out.plot[[6]], silent=T))
  try(print(out.plot[[7]], silent=T))
  try(print(out.plot[[8]], silent=T))
  try(print(out.plot[[9]], silent=T))
  try(print(out.plot[[10]], silent=T))
  try(print(out.plot[[11]], silent=T))
  try(print(out.plot[[12]], silent=T))
  try(print(out.plot[[13]], silent=T))
  try(print(out.plot[[14]], silent=T))
  try(print(out.plot[[15]], silent=T))
  try(print(out.plot[[16]], silent=T))
  try(print(out.plot[[17]], silent=T))
  try(print(out.plot[[18]], silent=T))
  try(print(out.plot[[19]], silent=T))
  try(print(out.plot[[20]], silent=T))
  try(print(out.plot[[21]], silent=T))
  try(print(out.plot[[22]], silent=T))
  try(print(out.plot[[23]], silent=T))
  try(print(out.plot[[24]], silent=T))
  try(print(out.plot[[25]], silent=T))
  try(print(out.plot[[26]], silent=T))
  try(print(out.plot[[27]], silent=T))
  try(print(out.plot[[28]], silent=T))
  try(print(out.plot[[29]], silent=T))
  
  
  while(!is.null(dev.list())) dev.off()
}
