#This code no longer works. 

#Combining the International and National Data Outputs for inspection and comparison

sp.tax<-sp.tax %>% dplyr::select(species_id, sort_order, english_name)

#read indices and trends for a specific site

# site.list<-c("SalishSea", "BCCWS", "PSSS")
# 
# #create site loop
# for(m in 1:length(site.list)){ 

indices<-read.csv(paste(out.dir, name, "_AnnualIndices_iCAR.csv", sep=""))
trends.slope<-read.csv(paste(out.dir, name, "_TrendsSlope_iCAR.csv", sep=""))
trends.endpt<-read.csv(paste(out.dir, name, "_TrendsEndPoint_iCAR.csv", sep=""))

##Prepare trends for plotting
all.trends<-rbind(trends.slope, trends.endpt)
#Remove any row with all NA values across all columns
all.trends <- all.trends[rowSums(is.na(all.trends)) < ncol(all.trends), ]
all.trends<-all.trends %>% dplyr::select(area_code, period, species_code, species_id, years, trnd, lower_ci, upper_ci, index_type, percent_change) %>% filter(period=="all years")
all.trends<-left_join(all.trends, sp.tax, by=c("species_id"="species_id"))

end.trnd<-all.trends %>% filter(index_type=="Endpoint Trend")

ed.trnd <- end.trnd %>%
  mutate(sp.trnd = paste(area_code, " ", round( trnd, digits = 2),  " (", round( lower_ci, digits = 2), ", ", round( upper_ci, digits = 2), ")", sep = "")) %>% dplyr::select(-trnd, -upper_ci, -lower_ci)
ed.trnd <- ed.trnd %>% dplyr::select(species_id, sp.trnd)

#Prepare Indices for plotting
indices <- indices[rowSums(is.na(indices)) < ncol(indices), ]
index <- indices %>%
  dplyr::select(index, upper_ci, lower_ci, LOESS_index, species_id, year, season, area_code) 

plot_name<-paste(area_code, ".plot", sep="")
assign(plot_name, full_join(index, ed.trnd, by = c("species_id"), relationship = "many-to-many"))


#Combine the plot_name sites into one dataframe
plot.dat<-rbind(SalishSea.plot, BCCWS.plot, PSSS.plot)

#Identify species that have output for the SalishSea but not for BCCWS or PSSS
test <- plot.dat %>% dplyr::select(species_id, area_code) %>% distinct() %>% 
  group_by(species_id) %>% summarise(n = n_distinct(area_code)) 
plot.dat<-left_join(plot.dat, test, by=c("species_id"="species_id"))
#if n=2 in plot.dat, filter the area_code to remove the Salish Sea
plot.dat<-plot.dat %>% filter(!(area_code == "SalishSea" & n == 2))

write.trn<-plot.dat %>% dplyr::select(species_id, sp.trnd) %>% distinct()

write.trn <- write.trn %>%
  group_by(species_id) %>%
  summarise(combine = paste(sp.trnd, collapse = "\n")) %>%
  ungroup()
write.trn<-left_join(write.trn, sp.tax, by=c("species_id"="species_id"))
write.trn<-write.trn %>% mutate(name = paste(english_name, "\n", combine, sep="")) %>% dplyr::select(species_id, name, sort_order)

plot.dat<-plot.dat %>% dplyr::select(-sp.trnd) %>% left_join(write.trn, by=c("species_id"="species_id"))

plot.dat <- plot.dat[order(plot.dat$sort_order), ]

sp.list <- as.character(unique(plot.dat$name))

out.plot <- NULL
i <- 1
j <- 6

for(k in 1:(ceiling(length(sp.list)/6))) {
  
  out.plot[[k]] <- ggplot(data = subset(plot.dat, name%in% sp.list[i:j]), aes(x = as.numeric(year), y = index, colour = area_code, shape = area_code)) +
    facet_wrap(~ name, ncol = 2, scales = "free", as.table = TRUE) +
    geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci, group = area_code, shape = area_code)) +
    geom_smooth(aes(ymin = lower_ci, ymax = upper_ci, group = area_code, colour = area_code, fill = area_code, linetype = area_code), method = "loess") + 
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
  pdf(paste(plot.dir, "Full_IndexPlot.pdf", sep=""),
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


for(m in 1:length(site.list)){ 
  
  indices<-read.csv(paste(out.dir, site.list[m], "_AnnualIndices.csv", sep=""))
  trends.slope<-read.csv(paste(out.dir, site.list[m], "_TrendsSlope.csv", sep=""))
  trends.endpt<-read.csv(paste(out.dir, site.list[m], "_TrendsEndPoint.csv", sep=""))
  
  ##Prepare trends for plotting
  all.trends<-rbind(trends.slope, trends.endpt)
  #Remove any row with all NA values across all columns
  all.trends <- all.trends[rowSums(is.na(all.trends)) < ncol(all.trends), ]
  all.trends<-all.trends %>% dplyr::select(area_code, period, species_code, species_id, years, trnd, lower_ci, upper_ci, index_type, percent_change) %>% filter(period=="all years")
  all.trends<-left_join(all.trends, sp.tax, by=c("species_id"="species_id"))
  
  #create output table of all trend pivot wide on area_code and index_type
  all.trnd<-all.trends %>% pivot_wider(names_from = c(area_code, index_type), values_from = c(trnd, lower_ci, upper_ci, percent_change))
  all.trnd <- all.trnd[order(all.trnd$sort_order),] 
  
  #create output table of all trend pivot wide on area_code and index_type
  all.trnd<-all.trends %>% pivot_wider(names_from = c(area_code, index_type), values_from = c(trnd, lower_ci, upper_ci, percent_change))
  all.trnd <- all.trnd[order(all.trnd$sort_order),] 
  
  table_name<-paste(site.list[m], ".table", sep="")
  assign(table_name, all.trnd)
  
} #end site loop

#Combine the plot_table into one using merge by period, species_code, species_id, years, sort_order, english_name
##keep all missing data 
table.dat<-full_join(SalishSea.table, BCCWS.table, by=c("period", "species_code", "species_id", "years", "sort_order", "english_name"))
table.dat<-full_join(table.dat, PSSS.table, by=c("period", "species_code", "species_id", "years", "sort_order", "english_name"))  

#order columns so that PSSS, BCCWS and SalishSea are together 
all.trnd<-table.dat %>% dplyr::rename(
  `Common Name` = english_name,
  Years = years,
  `Salish Sea Slope` = 'trnd_SalishSea_Slope Trend',
  `Salish Sea Slope LCI` = 'lower_ci_SalishSea_Slope Trend',
  `Salish Sea Slope UCI` = 'upper_ci_SalishSea_Slope Trend',
  `Salish Sea Slope % Change` = 'percent_change_SalishSea_Slope Trend',
  `BCCWS Slope` = 'trnd_BCCWS_Slope Trend',
  `BCCWS Slope LCI` = 'lower_ci_BCCWS_Slope Trend',
  `BCCWS Slope UCI` = 'upper_ci_BCCWS_Slope Trend',
  `BCCWS Slope % Change` = 'percent_change_BCCWS_Slope Trend',
  `PSSS Slope` = 'trnd_PSSS_Slope Trend',
  `PSSS Slope LCI` = 'lower_ci_PSSS_Slope Trend',
  `PSSS Slope UCI` = 'upper_ci_PSSS_Slope Trend',
  `PSSS Slope % Change` = 'percent_change_PSSS_Slope Trend',
  `Salish Sea Endpoint` = 'trnd_SalishSea_Endpoint trend',
  `Salish Sea Endpoint LCI` = 'lower_ci_SalishSea_Endpoint trend',
  `Salish Sea Endpoint UCI` = 'upper_ci_SalishSea_Endpoint trend',
  `Salish Sea Endpoint % Change` = 'percent_change_SalishSea_Endpoint trend',
  `BCCWS Endpoint` = 'trnd_BCCWS_Endpoint trend',
  `BCCWS Endpoint LCI` = 'lower_ci_BCCWS_Endpoint trend',
  `BCCWS Endpoint UCI` = 'upper_ci_BCCWS_Endpoint trend',
  `BCCWS Endpoint % Change` = 'percent_change_BCCWS_Endpoint trend',
  `PSSS Endpoint` = 'trnd_PSSS_Endpoint trend',
  `PSSS Endpoint LCI` = 'lower_ci_PSSS_Endpoint trend',
  `PSSS Endpoint UCI` = 'upper_ci_PSSS_Endpoint trend',
  `PSSS Endpoint % Change` = 'percent_change_PSSS_Endpoint trend') %>% 
  arrange(desc(sort_order)) %>% 
  dplyr::select(-sort_order, -species_code, -period) %>% 
  dplyr::select(`Common Name`, species_id, Years, `Salish Sea Slope`, `Salish Sea Slope LCI`, `Salish Sea Slope UCI`, `Salish Sea Slope % Change`, `Salish Sea Endpoint`, `Salish Sea Endpoint LCI`, `Salish Sea Endpoint UCI`, `Salish Sea Endpoint % Change`, `BCCWS Slope`, `BCCWS Slope LCI`, `BCCWS Slope UCI`, `BCCWS Slope % Change`, `BCCWS Endpoint`, `BCCWS Endpoint LCI`, `BCCWS Endpoint UCI`, `BCCWS Endpoint % Change`, `PSSS Slope`, `PSSS Slope LCI`, `PSSS Slope UCI`, `PSSS Slope % Change`, `PSSS Endpoint`, `PSSS Endpoint LCI`, `PSSS Endpoint UCI`, `PSSS Endpoint % Change`)

#format the dataframe so that there are only 2 significant digits
all.trnd<-all.trnd %>% mutate(across(where(is.numeric), ~round(., 2)))

#write the table to a .csv in the output folder
write.csv(all.trnd, paste(out.dir, "Full_TrendTable.csv", sep=""), row.names = FALSE)
