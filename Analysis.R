#Analysis scripts

# #Load your saved species data 
# sp.data<-read.csv("Data/sp.data.csv")
# #Load your saved events data which is needed for zero-filling
# events<-read.csv("Data/events.csv")


if(length(species.list) == 1){
  sp.list<-unique(sp.data$CommonName)
}else{
  sp.list<-species.list
}

if(area=="BCCWS"){

  sp.dat<-sp.data %>% filter(ProjectCode=="BCCWS")
  event<-events %>% filter(ProjectCode=="BCCWS")
  map<- st_read("Data/Spatial/BC_Water_Polygon.shp")
  
}  

if(area=="PSSS"){
  
  sp.dat<-sp.data %>% filter(ProjectCode=="PSSS")
  event<-events %>% filter(ProjectCode=="PSSS")
  map<- st_read("Data/Spatial/WA_Water_Polygon.shp")
  
  #This is fixed in the data cleaning script and can be removed during next round. 
  event<-event %>% filter(DurationInHours<=3)
  
} 

#Specify the spatial extent of the analysis
if(area=="SalishSea"){
  
  sp.dat<-sp.data 
  event<-events
  map<- st_read("Data/Spatial/Salish_Sea_Water_Polygon.shp")

}

##remove COVID data 
sp.dat<-sp.dat %>% filter(wyear != 2020)
event<-event %>% filter(wyear != 2020)

#If guild is set to "Yes" this will override the species list above.   
if(guild=="Yes"){
  if(type == "migration"){
  sp.list<-unique(sp.data$Migration)
  colnames(sp.dat)[colnames(sp.dat) == "Migration"] <- "Guild"
    }
  if(type=="diet"){
  sp.list<-unique(sp.data$Diet)
  colnames(sp.dat)[colnames(sp.dat) == "Diet"] <- "Guild"
  }
  if(type=="family"){
  sp.list<-unique(sp.data$family_name)  
  colnames(sp.dat)[colnames(sp.dat) == "family_name"] <- "Guild"
  }
}  

#Create a loop for the species list
  for(i in 1:length(sp.list)){
   dat<-NULL 
    #i<-1 #for testing
    
    if(guild =="Yes"){
    dat <- sp.dat %>% filter(Guild==sp.list[i])
    dat<-dat %>% distinct(ProjectCode, SurveyAreaIdentifier, wyear, YearCollected, wmonth, MonthCollected, DayCollected, .keep_all = TRUE)
    sp.code<-sp.list[i]
    dat$SpeciesCode<-sp.list[i]
    species_name<- type
    species_sci_name<- " "
    sp.id<- " "
 
    }else{
    
    #Subset the data for the species
    dat <- sp.dat %>% filter(CommonName==sp.list[i])
    dat<-dat %>% distinct(ProjectCode, SpeciesCode, CommonName, species_id, SurveyAreaIdentifier, wyear, YearCollected, wmonth, MonthCollected, DayCollected, .keep_all = TRUE)
   
    sp.code <- unique(dat$SpeciesCode)
    if (length(sp.code) == 0) {
      sp.code <- unique(dat$CommonName) #for group species
    }
    
    if(length(sp.code)>1){
      sp.code<-unique(dat$CommonName)
    }
    
    sp.id<- unique(dat$species_id)
    if (length(sp.id) == 0) {
      sp.id <- unique(dat$CommonName) #for group species
    }
    
    species_name<-unique(dat$CommonName)
    
    species_sci_name <- unique(dat$scientific_name) #for group species
    if (length(species_sci_name) == 0) {
      species_sci_name <- unique(dat$CommonName) #for group species
    }
    
    }
    
##zero-fill the dat using the events dataframe##
    dat<-left_join(event, dat, by= c("ProjectCode", "SurveyAreaIdentifier", "wyear", "wmonth", "YearCollected", "MonthCollected", "DayCollected"))
#Observation Counts will be backfilled with a 0 whenever it is NA
    dat$ObservationCount[is.na(dat$ObservationCount)]<-0
    dat$CommonName[is.na(dat$CommonName)]<-species_name
    dat$SpeciesCode[is.na(dat$SpeciesCode)]<-sp.code
  
#remove extreme outliers 
    outlier<-(quantile(dat$ObservationCount, probs = c(0.99)))*3
    dat<-dat %>% filter(ObservationCount<outlier)

if(nrow(dat)>0){        
    
#remove routes on which a species was never detected. 
    site.summ <- melt(dat, id.var = "SurveyAreaIdentifier",	measure.var = "ObservationCount")
    site.summ <- cast(site.summ, SurveyAreaIdentifier ~ variable,	fun.aggregate="sum")
    site.sp.list <- unique(subset(site.summ, select = c("SurveyAreaIdentifier"), ObservationCount >= 1))
    
    # Limit raw data to these species, i.e., those that were observed at least once on a route 
    dat <- merge(dat, site.sp.list, by = c("SurveyAreaIdentifier"))
    
        
#Remove SurveyAreaIdentifier from the data on where the sum of ObersevationCount is 0 across all years
#If a species was never detected on a route, we will not include that route in the species specific analysis
#This is considered out of range or in unsuitable habitat
    dat<-dat %>% group_by(SurveyAreaIdentifier) %>% filter(sum(ObservationCount)>0) %>% ungroup()
    routes<-n_distinct(dat$SurveyAreaIdentifier)
    
    # # create date and day of year columns
    # dat$date <- as.Date(paste(dat$YearCollected, dat$MonthCollected, 
    #                           dat$DayCollected, sep = "-"))
    # dat$doy <- as.numeric(format(dat$date, "%j"))
    # 
#Minimum Data Requirements##
    
    #Now we will check that the minimum data requirements are met. 
    #We will want to ensure that the species was detected a minimum of X times in a year
    #That the species was detected in at least 1/2 of the survey years
    #And that greater than a certain % of sites have non-zero counts
    
    SpeciesMean<- dat %>% group_by(wyear) %>% summarize(YearMean = sum(ObservationCount)) %>% ungroup() %>% summarize(MeanMean = mean(YearMean))
    SpeciesMean$NumYears <- n_distinct(dat$wyear)
    
    #Now cheek the SpeciesMean object to see if the species meets the minimum data requirements 
    #all the variable must be large than the values min.abundance, min.years, zero.count, if TRUE continue with the analysis
    
    if(SpeciesMean$MeanMean>=min.abundance & SpeciesMean$NumYears>=min.years & routes>nsites){
  min.data <- TRUE 
      }else{
  min.data <- FALSE
    }

    print(paste(sp.list[i], min.data))
    
    #only continue if the species meets the minimum data requirements      
if(min.data==TRUE){

###Model without spatial effect on abundance 
  wyears <- unique(dat$wyear)
  mean_wyear <- max(wyears)

#Create index variables
    dat <- dat %>% mutate( 
      std_yr = wyear - Y2,
      protocol = factor(ProjectCode), 
      kappa = as.integer(factor(dat$SurveyAreaIdentifier)),
      year_idx = as.integer(wyear - mean_wyear), #intercept is the expected count during the most recent year of data collection. 
      wmonth_idx = as.factor(wmonth), 
      sp_idx = as.integer(factor(CommonName)))%>%
      st_as_sf(coords = c("DecimalLongitude", "DecimalLatitude"), crs = 4326, remove = FALSE) %>% 
      st_transform(epsg6703km) %>%
        mutate(
          easting = st_coordinates(.)[, 1],
          northing = st_coordinates(.)[, 2]) %>%
        arrange(SurveyAreaIdentifier, wyear)
    
    # Aggregate data to annual max count
    # dat <- dat %>%
    #   group_by(SurveyAreaIdentifier, wyear) %>%
    #   slice_max(ObservationCount, n = 1, with_ties = FALSE) %>%
    #   ungroup()


   if(guild=="Yes"){
     
     dat$sp_idx[is.na(dat$sp_idx)] <- 999 #replace NA which are zero counts with generic sp_idx
     
     formula<- ObservationCount ~ -1 + 
       f(year_idx, model = "rw1") + 
       f(kappa, model="iid", hyper=hyper.iid) +  #in the spatial model we remove site as the variation will be captured in the spatial component. 
       f(sp_idx, model="iid", hyper=hyper.iid) 
       #f(protocol, model = "iid", hyper = prec.prior)
       #f(wmonth_idx, model = "ar1", hyper=prec.prior) 
       #f(wmonth_idx, model = "seasonal", season.length = 7) #Represent within-sampled-period seasonality
     
   }else{
    
     formula<- ObservationCount ~ -1 + 
      f(year_idx, model = "rw1") + 
      f(kappa, model="iid", hyper=hyper.iid)
      #f(protocol, model = "iid", hyper = prec.prior)
      #f(wmonth_idx, model = "ar1", hyper=prec.prior) 
      #f(wmonth_idx, model = "seasonal", season.length = 7) #Represent within-sampled-period seasonality

   }
     
     M0<-try(inla(formula, family = fam, data = dat, offset = log(dat$DurationInHours),
                 control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), verbose =TRUE), silent = T)
    

    #Dispersion Statistic to determiner is nbinomial is value
    mu1<-M0$summary.fitted.values[,"mean"]
    E1<-(dat$ObservationCount-mu1)/ sqrt(mu1 + mu1^2) #Pearson residuals
    N<-nrow(dat)
    p<-nrow(M0$summary.fixed + 2) # +1 for each the idd random effect
    Dispersion1<-sum(E1^2)/(N-p)
    print(paste("Dispersions Statistic out1 = ", Dispersion1, sep = ""))

    #write the dispersion statistic to the output file
    dispersion.csv$area_code<-area
    dispersion.csv$SpeciesCode<-sp.list[i]
    dispersion.csv$dispersion<-Dispersion1
    
    write.table(dispersion.csv, file = paste(out.dir,  name, "_DispersionStat.csv", sep = ""),
                col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE, sep = ",")
    
    dat$mu1<-mu1
    #plot ObservationCount and mu1 using ggplot, with 1:1 line
     q <- ggplot(dat, aes(x = ObservationCount, y = mu1)) + 
      geom_point() + 
      geom_abline(intercept = 0, slope = 1)
    
    # Save the plot with specified width, height, and dpi
    ggsave(filename = paste(plot.dir, area, sp.list[i], "_FitPlot.jpeg", sep = ""), 
           plot = q, 
           width = 8,    # Adjust width as needed
           height = 6,   # Adjust height as needed
           dpi = 150)    # Lower dpi for faster saving
    
     #Plot the data for visual inspection

     #Convert the data to a spatial object
     dat_sf <- st_as_sf(dat, coords = c("DecimalLongitude", "DecimalLatitude"), crs = 4326)
     #remove zero ObservationCounts from the output plot
     dat_sf<-dat_sf %>% group_by(SurveyAreaIdentifier, geometry) %>% summarise(ObservationCount = mean(ObservationCount))
     map<-st_transform(map, crs = 4326)

     #write plot to Plots folder in Output
       p<- ggplot()+
       geom_sf(data = map, fill = "white", color = "black") +
       # Plot the points, size by the sum of ObservationCount within a year
       geom_sf(data=dat_sf, aes(size=ObservationCount)) +
       # Facet by survey_year to create the multi-paneled map
       #facet_wrap(~ wyear) +
       # Add a theme with a minimal design and change the font styles, to your preference
       theme_minimal() +
       #theme(legend.position = "bottom") +
       # To make the points in the legend larger without affecting map points
       guides(color = guide_legend(override.aes = list(size = 3))) +
       #make the text on the x-axis vertical
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
       # Define the title and axis names
       labs(title = sp.list[i], x = "Longitude", y = "Latitude")+
       # Change legend lable
       scale_size_continuous(name = "Mean Observation Count")

     ggsave(paste(plot.dir, area, sp.list[i], "_SumCountPlot.jpeg", sep = ""), plot = p, width = 10, height = 6, units = "in")

#create the datframe of covariates
N<-nrow(dat)

    if(guild=="Yes"){
      
      dat$sp_idx[is.na(dat$sp_idx)] <- 999 #replace NA which are zero counts with generic sp_idx
      
      Covariates<- data.frame(
        Intercept=rep(1, N),
        kappa = dat$kappa, 
        sp_idx = dat$sp_idx,
        #protocol = dat$protocol,
        year_idx=dat$year_idx
        #wmonth_idx = dat$wmonth_idx
      )
      
    }else{

    Covariates<- data.frame(
      Intercept=rep(1, N),
       kappa = dat$kappa, 
       #protocol = dat$protocol, 
       year_idx=dat$year_idx 
       #wmonth_idx = dat$wmonth_idx
    )
    }

    #Create the Mesh
    #Make a set of distinct study sites for mapping kappa
    site_map <- dat %>%
      dplyr::select(SurveyAreaIdentifier, easting, northing) %>% distinct()
    
    #make unique locations
    Loc_unique<-dat %>% dplyr::select(SurveyAreaIdentifier, easting, northing) %>%
      distinct() %>%
      dplyr::select(easting, northing) %>% distinct() %>%
      st_drop_geometry() %>%
      as.matrix()

    sample_size<-nrow(Loc_unique)
    
    #make the mesh this way so that the point fall on the vertices of the lattice
    Loc_all<-dat %>% dplyr::select(easting, northing) %>% st_drop_geometry() %>% as.matrix()
    
     #change the crs of map to epsg6703km
    map <- st_transform(map, crs = epsg6703km)
    
    # Convert sf polygon to SpatialPolygons (sp format)
    map_sp <- as(map, "Spatial")
    
    # Convert to inla.mesh.segment
    boundary_segment <- inla.sp2segment(map_sp)
    
    mesh2<- inla.mesh.2d(Loc_unique,
      boundary = boundary_segment,  # Your polygon boundary
      max.edge = c(100, 200),       # Inner/outer resolution (in CRS units)
      cutoff = 50,                  # Minimum distance between vertices
      crs = fm_crs(dat)             # Use the same CRS as your data
    )
    
    spde <- inla.spde2.pcmatern(
      mesh = mesh2,
      prior.range = prior.range,
      prior.sigma = prior.sigma
    )
    
    #Spatial Fields
    # make index sets for the spatial model
    alpha_idx <- inla.spde.make.index(name = "alpha", n.spde = spde$n.spde) #n.repl is used to account for repeated measure at the same location over time.
    #tau_idx <- inla.spde.make.index(name = "tau", n.spde = spde$n.spde) #n.repl is used to account for repeated measure at the same location over time.
    
    #Projection matrix A using all locations
    A_alph <- inla.spde.make.A(mesh=mesh2, loc=Loc_all)  # pg 218 this formula should include an alpha, default is 2 if the model does not include times.
    #A_tau <- inla.spde.make.A(mesh = mesh2, loc = Loc_all, weights = dat$std_yr) # note weights argument
    
    #Create Stack Object for INLA
    Stack <- inla.stack (
      tag="FitGAM",
      data=list(count=as.vector(dat$ObservationCount)), #response from the dataframe
      effects = list(Covariates=Covariates, alpha = alpha_idx),  #covariate list #removed tau=tau_idx
      A = list(
        1, #value of 1 to non-spatial terms
        A_alph
        #A_tau
      )
    )
    
    if(guild=="Yes"){
      
      formula.sp<- count ~ -1 + Intercept + 
      f(year_idx, model = "rw1") +  
      #f(protocol, model = "iid", hyper = prec.prior) +
      #wmonth_idx +
      #f(year_idx, model="iid", hyper=hyper.iid) + #so that alpha can be calculated per year
      f(kappa, model="iid", hyper=prec.prior) + #site effect 
      f(sp_idx, model="iid", hyper=hyper.iid) + 
      #f(wmonth_idx, model = "rw1", cyclic=TRUE) +
      #f(wmonth_idx, model = "seasonal", season.length = 7) +
      #f(wmonth_idx, model="iid", hyper=prec.prior)+
      f(alpha, model =spde)
      #f(tau, model =spde)
    
      }else{
    
     formula.sp<- count ~ -1+ Intercept + 
      f(year_idx, model = "rw1") + 
      #f(protocol, model = "iid", hyper = prec.prior)+
      #wmonth_idx +
      #f(year_idx, model="iid", hyper=hyper.iid) +
      f(kappa, model="iid", hyper=prec.prior) + #site effect
      #f(wmonth_idx, model = "rw1", cyclic = TRUE) +
      #f(wmonth_idx, model = "seasonal", season.length = 7) + #Represent within-sampled-period seasonality
      f(alpha, model =spde) 
      #f(tau, model =spde)
    }   


    #fit the spatial model using INLA
    M1<-inla(formula.sp, family = fam, data = inla.stack.data(Stack), offset = log(dat$DurationInHours),
                    control.predictor = list(A=inla.stack.A(Stack)),
                    control.compute = list(dic=TRUE, waic=TRUE, config = TRUE),
                    control.fixed = list(mean = 0, prec = 0.001), 
             control.family = list(
               hyper = list(theta = list(prior = "loggamma", param = c(3, 0.1)))
             ), verbose =TRUE)

  #Compare the DIC and WIC values
   z.out<-NULL
   dic<-c(M0$dic$dic, M1$dic$dic)
   wic<-c(M0$waic$waic, M1$waic$waic)
   ModelType<-c("GAM", "GAM + SPATIAL")
   z.out<-cbind(ModelType, dic, wic)
   z.out<-as.data.frame(z.out)
   z.out$SpeciesCode<-sp.code

   write.table(z.out, file = paste(out.dir,  name, "_ModelComparison.csv", sep = ""),
               col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE, sep = ",")
   
#Calculate Posterior estimate of abundance
nsamples<- 100
post.sample1 <-NULL #clear previous
post.sample1<-inla.posterior.sample(nsamples, M1)

tmp1<-NULL
tmp1 <- dat %>% dplyr::select(wyear) %>% st_drop_geometry() 

#for each sample in the posterior we want to join the predicted to tmp so that the predictions line up with wmonth/year and we can get the mean count by year
for (h in 1:nsamples){
  pred<-exp(post.sample1[[h]]$latent[1:nrow(dat)])
  tmp1[ncol(tmp1)+1]<-pred
}

# Rename the columns from V2 to V101
colnames(tmp1)[2:(nsamples + 1)] <- paste0("V", 2:(nsamples + 1))

#will want to adjust V to match the posterior sample size   
tmp1<-tmp1 %>% group_by(wyear) %>% summarise_all(mean, na.rm=TRUE)
tmp1<-tmp1 %>% rowwise() %>% mutate(index = median(c_across(V2:V101)), lower_ci=quantile(c_across(V2:V101), 0.025), upper_ci=quantile(c_across(V2:V101), 0.975), stdev=sd(c_across(V2:V101))) 

#Assign data to output table 
indices.csv<-tmp1 %>% dplyr::select(wyear, index, lower_ci, upper_ci, stdev) %>% mutate(
species_code = sp.code,
years = paste(min(dat$wyear), "-", max(dat$wyear), sep = ""),
year = wyear,
period ="all years",
season = "winter",
area_code = area,
model_type = "GLM MONTH AR1 ALPHA+TAU SPATIAL",
species_id=sp.id,
species_name=species_name,
species_sci_name=species_sci_name,
error="",
#Assing missing data fields 
upload_id="",
stderr="",
trend_id="",
smooth_upper_ci="",
smooth_lower_ci="",
upload_dt="",
family=fam,
results_code = "BCCWS+PSSS",
version = "2025",
season="Winter",
area_code=area,
trend_index="") #look at CMMN code to generate in next round of analysis

# Run LOESS function
indices.csv$LOESS_index = loess_func(indices.csv$index, indices.csv$wyear)

# Order output before printing to table
indices.csv<-indices.csv %>% dplyr::select(results_code, version, area_code, season, period, species_code, species_id, year, index, stderr, stdev, upper_ci, lower_ci, LOESS_index, trend_index)

# Write data to table
write.table(indices.csv, 
            file = paste(out.dir,	name, "_AnnualIndices.csv", sep = ""),
            row.names = FALSE, 
            append = TRUE, 
            quote = FALSE, 
            sep = ",", 
            col.names = FALSE)


##TRENDS 
nyears=length(unique(dat$wyear))
list.years<-unique(dat$wyear)
rev.years<-rev(list.years)
endyr <- max(dat$wyear)
startyr <- min(dat$wyear)
totyr<- endyr-startyr
period = "all years"
Y1 <- startyr
Y2 <- endyr
y1 <- 1
y2 <- nyears
  
 
##END POINT TRENDS
  pred.ch<-tmp1 %>% filter(wyear %in% c(Y1, Y2)) %>% dplyr::select(-wyear)
  pred.ch<-t(pred.ch)
  pred.ch<-as.data.frame(pred.ch)

  pred.ch<-pred.ch %>% mutate(ch=(V2/V1), max_year=Y2, min_year=Y1, tr=(100*((ch^(1/(max_year-min_year)))-1)))
  pred.ch<-pred.ch %>% reframe(trnd=median(tr), percent_change=100*(median(ch)-1), lower_ci=quantile(tr, probs=0.025), upper_ci=quantile(tr, probs=0.95), sd=sd(tr), Width_of_Credible_Interval=upper_ci-lower_ci) %>% distinct()

  #write output to table   
  trend.out<-NULL
  trend.out <- pred.ch %>%
    mutate(model_type="GLM MONTH AR1 ALPHA+TAU SPATIAL", 
           model_family = fam,
           years = paste(Y1, "-", Y2, sep = ""),
           year_start=Y1, 
           year_end=Y2,
           period ="all years",
           season = "winter",
           results_code = "BCCWS+PSSS",
           area_code = area,
           version=2025, 
           species_code = sp.code,
           species_id=sp.id, 
           index_type="endpoint", 
           species_name=species_name,
           species_sci_name=species_sci_name,
           stderr = "",
           index_type= "Endpoint trend", 
           model_fit = "", 	
           percent_change_low ="", 
           percent_change_high = "",
           prob_decrease_0 = "",
           prob_decrease_25 = "",
           prob_decrease_30 = "",
           prob_decrease_50 = "",
           prob_increase_0 = "",
           prob_increase_33 = "",	
           prob_increase_100 = "",
           confidence = "",
           precision_num = "",
           suitability="",
           precision_cat = ifelse(pred.ch$Width_of_Credible_Interval<3.5, "High", ifelse(pred.ch$Width_of_Credible_Interval>=3.5 & pred.ch$Width_of_Credible_Interval<=6.7, "Medium", "Low")),
           coverage_num = "",
           coverage_cat = "",
           goal = "",
           goal_lower = "",
           sample_size = sample_size,
           sample_size_units="Number of Routes",
           sample_total = "",
           subtitle = "",
           pval = "",
           pval_str = "",
           post_prob = "",
           trnd_order = "",
           dq = "",
           prob_LD = "",
           prob_MD = "",
           prob_LC = "",
           prob_MI = "",
           prob_LI = "",
           quantile_050 = "",
           quantile_165 = "",
           quantile_835 = "",
           quantile_950 = "",
           trend_id = "",
           upload_dt = "")
 
  write.trend<-trend.out %>% dplyr::select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, index_type, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
  
  write.table(write.trend, 
              file = paste(out.dir, name, "_TrendsEndpoint.csv", sep = ""), 
              row.names = FALSE, 
              append = TRUE, 
              quote = FALSE, 
              sep = ",", 
              col.names = FALSE)  
  
# #SLOPE TRENDS
# #Summary of the GAM smooth on year
# wy=c(y1:y2)
# pred.yr<-tmp1 %>% dplyr::select(-wyear)
# pred.yr<-t(pred.yr)
# ne = log(pred.yr[,wy])
# 
# #This is the slope function.
# #It calculates the coefficient of the lm slope for each row in the smoothed output.
# 
# #slope function 1
# slope  <-  function(x){
#   return(coef(lm(x~I(y1:y2)))[2])
# }
# 
# m =  apply(ne,1,slope)
# m = as.vector((exp(m)-1)*100)
#   
# 
#   #include slope output in new table
#   trend.out$index_type="Slope trend"
#   trend.out$trnd<-median(m, na.rm=TRUE)
#   trend.out$lower_ci<-quantile(m, prob=0.025)
#   trend.out$upper_ci<-quantile(m, prob=0.950)
#   trend.out$sd<-sd(m, na.rm=TRUE)
# 
#   per_trend=trend.out$trnd/100
#   period_num=Y2-Y1
#   trend.out$percent_change<-((1+per_trend)^period_num-1)*100
#   trend.out$Width_of_Credible_Interval_slope<-trend.out$upper_ci-trend.out$lower_ci
#   trend.out$precision_cat = ifelse(pred.ch$Width_of_Credible_Interval<3.5, "High", ifelse(pred.ch$Width_of_Credible_Interval>=3.5 & pred.ch$Width_of_Credible_Interval<=6.7, "Medium", "Low"))
#   
#   write.trend<-trend.out %>% dplyr::select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, index_type, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
#   
#   
#   write.table(write.trend, 
#               file = paste(out.dir, name, "_TrendsSlope.csv", sep = ""), 
#               row.names = FALSE, 
#               append = TRUE, 
#               quote = FALSE, 
#               sep = ",", 
#               col.names = FALSE)  

############################################################################################  
##Annual site level index
  Loc_unique2 <- dat %>%
    dplyr::select(SurveyAreaIdentifier, easting, northing) %>%
    distinct(easting, northing, .keep_all = TRUE) %>%  # Keep unique coordinates with IDs
    st_drop_geometry() %>%
    as.matrix()
  
  # Prepare year mapping with standardized values
  year_table <- dat %>%
    distinct(wyear, std_yr) %>%
    arrange(wyear)
  
  # Create unique location-year grid
  locations <- data.frame(
    area_code = Loc_unique2[, "SurveyAreaIdentifier"],
    easting = Loc_unique2[, "easting"],
    northing = Loc_unique2[, "northing"]
  )
  annual_grid <- expand_grid(
    locations,
    year_table
  )
  
  # Create projection matrices for unique locations
  A_alpha_unique <- inla.spde.make.A(mesh = mesh2, loc = Loc_unique)
  #A_tau_unique <- inla.spde.make.A(mesh = mesh2, loc = Loc_unique)
  
  #create output matrix
  post_samples <- matrix(nrow = nrow(annual_grid), ncol = nsamples)
  
  for (h in 1:nsamples) {
    # Extract spatial field samples
    alpha_sample <- post.sample1[[h]]$latent[grep("alpha", rownames(post.sample1[[h]]$latent))]
    #tau_sample <- post.sample1[[h]]$latent[grep("tau", rownames(post.sample1[[h]]$latent))]
    
    # Project to unique locations
    alpha_loc <- as.vector(A_alpha_unique %*% alpha_sample)
    #tau_loc <- as.vector(A_tau_unique %*% tau_sample)
    
    # Match locations and compute annual index
    #index <- exp(alpha_loc + tau_loc * annual_grid$std_yr)
    index <- exp(alpha_loc)
    
    post_samples[, h] <- index
  }
  
  # Summarize posterior samples
  annual_grid <- annual_grid %>%
    mutate(
      index = apply(post_samples, 1, mean),
      stdev = apply(post_samples, 1, sd),
      lower_ci = apply(post_samples, 1, quantile, probs = 0.025),
      upper_ci = apply(post_samples, 1, quantile, probs = 0.975)
    )
  
  #Assign data to output table 
  indices.csv<-annual_grid %>% dplyr::select(area_code, wyear, index, lower_ci, upper_ci, stdev) %>% mutate(
    species_code = sp.code,
    years = paste(min(dat$wyear), "-", max(dat$wyear), sep = ""),
    year = wyear,
    period ="all years",
    season = "winter",
    model_type = "GLM MONTH AR1 ALPHA+TAU SPATIAL",
    species_id=sp.id,
    species_name=species_name,
    species_sci_name=species_sci_name,
    error="",
    #Assing missing data fields 
    upload_id="",
    stderr="",
    trend_id="",
    smooth_upper_ci="",
    smooth_lower_ci="",
    upload_dt="",
    family=fam,
    results_code = "BCCWS+PSSS",
    version = "2025",
    season="Winter",
    trend_index="") #look at CMMN code to generate in next round of analysis
  
  # Run LOESS function
  indices.csv$LOESS_index = loess_func(indices.csv$index, indices.csv$wyear)
  
  # Order output before printing to table
  indices.csv<-indices.csv %>% dplyr::select(results_code, version, area_code, season, period, species_code, species_id, year, index, stderr, stdev, upper_ci, lower_ci, LOESS_index, trend_index)
  
  # Write data to table
  write.table(indices.csv, 
              file = paste(out.dir,	name, "_AnnualIndices.csv", sep = ""),
              row.names = FALSE, 
              append = TRUE, 
              quote = FALSE, 
              sep = ",", 
              col.names = FALSE)
  
  ##############################################################################
  ##END POINT TRENDS for each site
  
  # Include SurveyAreaIdentifier in tmp1
  tmp1_site <- dat %>% 
    dplyr::select(SurveyAreaIdentifier, wyear) %>% 
    st_drop_geometry()
  
  # Add posterior samples
  for (h in 1:nsamples){
    pred <- exp(post.sample1[[h]]$latent[1:nrow(dat)])
    tmp1_site[[paste0("V", h+1)]] <- pred
  }
  
  # Calculate annual indices per site
  trends_by_site <- tmp1_site %>%
    group_by(SurveyAreaIdentifier, wyear) %>%
    summarise(across(starts_with("V"), mean), .groups = "drop") %>%
    rowwise() %>%
    mutate(
      index = median(c_across(starts_with("V"))),
      lower_ci = quantile(c_across(starts_with("V")), 0.025),
      upper_ci = quantile(c_across(starts_with("V")), 0.975),
      stdev = sd(c_across(starts_with("V")))
    ) %>%
    select(-starts_with("V"))  # Remove sample columns
  
  period_num=Y2-Y1
  
  # Calculate endpoint trends (first vs last year)
  endpoint_trends <- trends_by_site %>%
    group_by(SurveyAreaIdentifier) %>%
    filter(wyear == min(wyear) | wyear == max(wyear)) %>%
    summarise(
      start_year = first(wyear),
      end_year = last(wyear),
      trend = last(index)/first(index) - 1,
      lci = last(lower_ci)/first(upper_ci) - 1,
      uci = last(upper_ci)/first(lower_ci) - 1, 
      Width_of_Credible_Interval = uci-lci, 
      per_trend = trend/100, 
      percent_change = ((1+per_trend)^period_num-1)*100
    )
  
  #write output to table   
  trend.out<-NULL
  trend.out <- endpoint_trends %>%
    mutate(trnd = trend, 
           lower_ci = lci, 
           upper_ci = uci,
           model_type="GLM MONTH AR1 ALPHA+TAU SPATIAL", 
           model_family = fam,
           years = paste(Y1, "-", Y2, sep = ""),
           year_start=start_year, 
           year_end=end_year,
           period ="all years",
           season = "winter",
           results_code = "BCCWS+PSSS",
           area_code = SurveyAreaIdentifier,
           version=2025, 
           species_code = sp.code,
           species_id=sp.id, 
           species_name=species_name,
           species_sci_name=species_sci_name,
           stderr = "",
           index_type= "Endpoint trend", 
           model_fit = "", 	
           percent_change_low ="", 
           percent_change_high = "",
           prob_decrease_0 = "",
           prob_decrease_25 = "",
           prob_decrease_30 = "",
           prob_decrease_50 = "",
           prob_increase_0 = "",
           prob_increase_33 = "",	
           prob_increase_100 = "",
           confidence = "",
           precision_num = "",
           suitability="",
           precision_cat = ifelse(endpoint_trends$Width_of_Credible_Interval<3.5, "High", ifelse(endpoint_trends$Width_of_Credible_Interval>=3.5 & endpoint_trends$Width_of_Credible_Interval<=6.7, "Medium", "Low")),
           coverage_num = "",
           coverage_cat = "",
           goal = "",
           goal_lower = "",
           sample_size = sample_size,
           sample_size_units="Number of Routes",
           sample_total = "",
           subtitle = "",
           pval = "",
           pval_str = "",
           post_prob = "",
           trnd_order = "",
           dq = "",
           prob_LD = "",
           prob_MD = "",
           prob_LC = "",
           prob_MI = "",
           prob_LI = "",
           quantile_050 = "",
           quantile_165 = "",
           quantile_835 = "",
           quantile_950 = "",
           trend_id = "",
           upload_dt = "")
  
  write.trend<-trend.out %>% dplyr::select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, index_type, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
  
  write.table(write.trend, 
              file = paste(out.dir, name, "_TrendsEndpoint.csv", sep = ""), 
              row.names = FALSE, 
              append = TRUE, 
              quote = FALSE, 
              sep = ",", 
              col.names = FALSE)  

  #######################################################################################################
  # #SLOPE TRENDS for each site
  # #Summary of the GAM smooth on year
  # 
  # # Get unique sites and posterior sample columns
  # sites <- unique(tmp1_site$SurveyAreaIdentifier)
  # sample_cols <- grep("^V", names(tmp1_site), value = TRUE)
  # 
  # # Initialize results dataframe
  # slope_results <- data.frame(
  #   SurveyAreaIdentifier = character(),
  #   mean_slope = numeric(),
  #   sd_slope = numeric(),
  #   lower_ci = numeric(),
  #   upper_ci = numeric(),
  #   stringsAsFactors = FALSE
  # )
  # 
  # # Main loop through each site
  # for (site in sites) {
  #   # Subset data for current site
  #   site_data <- tmp1_site[tmp1_site$SurveyAreaIdentifier == site, ]
  #   
  #   # Skip sites with <2 years of data
  #   if (nrow(site_data) < 2) {
  #     message("Skipping site ", site, " - insufficient data")
  #     next
  #   }
  #   
  #   # Extract and sort years
  #   site_data <- site_data[order(site_data$wyear), ]
  #   years <- site_data$wyear
  #   
  #   # Initialize storage for slope estimates
  #   n_samples <- length(sample_cols)
  #   slopes <- numeric(n_samples)
  #   
  #   # Loop through posterior samples
  #   for (i in seq_along(sample_cols)) {
  #     # Get predictions for this sample
  #     pred <- site_data[[sample_cols[i]]]
  #     
  #     # Calculate log-linear slope
  #     tryCatch({
  #       model <- lm(log(pred) ~ years)
  #       slopes[i] <- coef(model)[2]
  #     }, error = function(e) {
  #       slopes[i] <<- NA  # Handle model errors
  #     })
  #   }
  #   
  #   # Remove failed fits
  #   slopes <- slopes[!is.na(slopes)]
  #   
  #   # Convert to percentage change
  #   slopes_pct <- (exp(slopes) - 1) * 100
  #   
  #   # Calculate summary statistics
  #   new_row <- data.frame(
  #     area_code = site,
  #     trnd = mean(slopes_pct),
  #     sd = sd(slopes_pct),
  #     lower_ci = quantile(slopes_pct, 0.025),
  #     upper_ci = quantile(slopes_pct, 0.975)
  #   )
  #   
  #   # Add to results
  #   slope_results <- rbind(slope_results, new_row)
  # }
  # 
  # slope_trends<-slope_results %>% mutate(
  #     Width_of_Credible_Interval = upper_ci-lower_ci, 
  #     per_trnd = trnd/100, 
  #     percent_change = ((1+per_trnd)^period_num-1)*100
  #   )
  # 
  # slope_trends$precision_cat = ifelse(slope_trends$Width_of_Credible_Interval<3.5, "High", ifelse(slope_trends$Width_of_Credible_Interval>=3.5 & slope_trends$Width_of_Credible_Interval<=6.7, "Medium", "Low"))
  # 
  # #write output to table   
  # trend.out<-NULL
  # trend.out <- slope_trends %>%
  #   mutate(model_type="GLM MONTH AR1 ALPHA+TAU SPATIAL", 
  #          model_family = fam,
  #          years = paste(Y1, "-", Y2, sep = ""),
  #          year_start=Y1, 
  #          year_end=Y2,
  #          period ="all years",
  #          season = "winter",
  #          results_code = "BCCWS+PSSS",
  #          version=2025, 
  #          species_code = sp.code,
  #          species_id=sp.id, 
  #          species_name=species_name,
  #          species_sci_name=species_sci_name,
  #          stderr = "",
  #          index_type= "Slope trend", 
  #          model_fit = "", 	
  #          percent_change_low ="", 
  #          percent_change_high = "",
  #          prob_decrease_0 = "",
  #          prob_decrease_25 = "",
  #          prob_decrease_30 = "",
  #          prob_decrease_50 = "",
  #          prob_increase_0 = "",
  #          prob_increase_33 = "",	
  #          prob_increase_100 = "",
  #          confidence = "",
  #          precision_num = "",
  #          suitability="",
  #          precision_cat = ifelse(slope_trends$Width_of_Credible_Interval<3.5, "High", ifelse(slope_trends$Width_of_Credible_Interval>=3.5 & slope_trends$Width_of_Credible_Interval<=6.7, "Medium", "Low")),
  #          coverage_num = "",
  #          coverage_cat = "",
  #          goal = "",
  #          goal_lower = "",
  #          sample_size = sample_size,
  #          sample_size_units="Number of Routes",
  #          sample_total = "",
  #          subtitle = "",
  #          pval = "",
  #          pval_str = "",
  #          post_prob = "",
  #          trnd_order = "",
  #          dq = "",
  #          prob_LD = "",
  #          prob_MD = "",
  #          prob_LC = "",
  #          prob_MI = "",
  #          prob_LI = "",
  #          quantile_050 = "",
  #          quantile_165 = "",
  #          quantile_835 = "",
  #          quantile_950 = "",
  #          trend_id = "",
  #          upload_dt = "")
  # 
  # write.trend<-trend.out %>% dplyr::select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, index_type, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
  # 
  # 
  # write.table(write.trend, 
  #             file = paste(out.dir, name, "_TrendsSlope.csv", sep = ""), 
  #             row.names = FALSE, 
  #             append = TRUE, 
  #             quote = FALSE, 
  #             sep = ",", 
  #             col.names = FALSE)  
  # 
  # 
  # 
  # #########################SVC Maps###################################
  # ##https://inla.r-inla-download.org/r-inla.org/doc/vignettes/svc.html
  # 
  # 
  # if(area=="SalishSea"){ #only make map if full Salish Sea Analysis
  # # get easting and northing limits
  # xlim <- range( boundary_segment$loc[, 1])
  # ylim <- range( boundary_segment$loc[, 2])
  # grd_dims <- round(c(x = diff(range(xlim)), y = diff(range(ylim))) / 5) #5 km mapping grid
  # 
  # # make mesh projector to get model summaries from the mesh to the mapping grid
  # mesh_proj <- inla.mesh.projector(
  #   mesh2,
  #   xlim = xlim, ylim = ylim, dims = grd_dims)
  # 
  #   # pull data
  #   # kappa <- data.frame(
  #   #   median = exp(M1$summary.random$kappa$"0.5quant"),
  #   #   range95 = exp(M1$summary.random$kappa$"0.975quant") -
  #   #     exp(M1$summary.random$kappa$"0.025quant")
  #   # )
  #   # 
  #   if(guild=="Yes"){
  #   sp_idx <- data.frame(
  #     median = exp(M1$summary.random$sp_idx$"0.5quant"),
  #     range95 = exp(M1$summary.random$sp_idx$"0.975quant") -
  #       exp(M1$summary.random$sp_idx$"0.025quant")
  #   )
  #   }
  # 
  #    alph <- data.frame(
  #     median = exp(M1$summary.random$alpha$"0.5quant"),
  #     range95 = exp(M1$summary.random$alpha$"0.975quant") -
  #       exp(M1$summary.random$alpha$"0.025quant")
  #   )
  # 
  #   # taus <- data.frame(
  #   #   median = (exp(M1$summary.random$tau$"0.5quant") - 1) * 100,
  #   #   range95 = (exp(M1$summary.random$tau$"0.975quant") -
  #   #                exp(M1$summary.random$tau$"0.025quant")) * 100
  #   # )
  # 
  #   # loop to get estimates on a mapping grid
  #   pred_grids <- lapply(
  #     list(alpha = alph), # tau = taus),
  #     function(x) as.matrix(inla.mesh.project(mesh_proj, x))
  #   )
  # 
  #   # make a terra raster stack with the posterior median and range95
  #   out_stk<-NULL
  #   out_stk <- rast()
  #   for (j in 1:1) {
  #     mean_j <- cbind(expand.grid(x = mesh_proj$x, y = mesh_proj$y),
  #                     Z = c(matrix(pred_grids[[j]][, 1], grd_dims[1]))
  #     )
  #     mean_j <- rast(mean_j, crs = epsg6703km)
  #     range95_j <- cbind(expand.grid(X = mesh_proj$x, Y = mesh_proj$y),
  #                        Z = c(matrix(pred_grids[[j]][, 2], grd_dims[1]))
  #     )
  #     range95_j <- rast(range95_j, crs = epsg6703km)
  #     out_j <- c(mean_j, range95_j)
  #     terra::add(out_stk) <- out_j
  #   }
  #   
  #   names(out_stk) <- c("alpha_median", "alpha_range95") #, "tau_median", "tau_range95")
  #   map<-st_transform(map, crs = epsg6703km)
  #   out_stk <- terra::mask(out_stk, map, touches = FALSE)
  # 
  #   # #change the crs of map to epsg6703km
  #   # map <- st_transform(map, crs = epsg6703km)
  #   # out_stk <- terra::mask(out_stk, map, touches = FALSE)
  #   # 
  #   # medians
  #   # fields alpha_s, tau_s
  #   pa <- make_plot_field(
  #     data_stk = out_stk[["alpha_median"]],
  #     scale_label = "posterior\nmedian\nalpha"
  #   )
  #   
  #   # pt <- make_plot_field(
  #   #   data_stk = out_stk[["tau_median"]],
  #   #   scale_label = "posterior\nmedian\ntau"
  #   # )
  #   # # sites kappa_s
  #   # ps <- make_plot_site(
  #   #   data = cbind(site_map, data.frame(value = kappa$median)),
  #   #   scale_label = "posterior\nmedian\nkappa"
  #   # )
  #  
  #    # range95
  #   # fields alpha_s, tau_s
  #   pa_range95 <- make_plot_field(
  #     data_stk = out_stk[["alpha_range95"]],
  #     scale_label = "posterior\nrange95\nexp(alpha_s)"
  #   )
  #   
  #   # pt_range95 <- make_plot_field(
  #   #   data_stk = out_stk[["tau_range95"]],
  #   #   scale_label = "posterior\nrange95\n100(exp(tau_s)-1)"
  #   # )
  #   # 
  #   # # sites kappa_s
  #   # ps_range95 <- make_plot_site(
  #   #   data = cbind(site_map, data.frame(value = kappa$range95)),
  #   #   scale_label = "posterior\nrange95\nexp(kappa_s)"
  #   #)
  #   
  #   # plot together
  #   #multiplot(pa, pt, cols = 2)
  #   
  #   # plot together
  #   multiplot(pa, pa_range95, cols = 2)
  #   
  #   # plot together
  #   # multiplot(ps, pa, pt, ps_range95, pa_range95, pt_range95, cols = 2)
  #   
  #   pdf(paste(plot.dir, species_name, "_spdePlot.pdf", sep=""))
  #   multiplot(pa, pt, pa_range95, pt_range95, cols=2)
  #   while(!is.null(dev.list())) dev.off()
  #  
  #   } # end if Salish Sea create map
  } #end nrow data 
  } #end min.data  
  }#end SpeciesLoop


