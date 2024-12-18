#Analysis scripts

if(site=="BCCWS"){

  sp.dat<-sp.data %>% filter(ProjectCode=="BCCWS")
  
}  

if(site=="PSSS"){
  
  sp.dat<-sp.data %>% filter(ProjectCode=="PSSS")
  
} 

#Specify the spatial extent of the analysis
if(site=="SalishSea"){
  
  sp.dat<-sp.data 

}
  
  #Create a loop for the species list
  for(i in 1:length(sp.list)){
    
    #i<-1 #for testing
    
    #Subset the data for the species
    dat <- sp.dat %>% filter(SpeciesCode==sp.list[i])
    dat<-dat %>% distinct(ProjectCode, SurveyAreaIdentifier, wyear, YearCollected, MonthCollected, DayCollected, .keep_all = TRUE)
    sp.code<-sp.list[i]
    
##zero-fill the dat using the events dataframe##
    dat<-left_join(events, dat, by= c("ProjectCode", "SurveyAreaIdentifier", "wyear", "YearCollected", "MonthCollected", "DayCollected"))
#Observation Counts will be backfilled with a 0 whenever it is NA
    dat$ObservationCount[is.na(dat$ObservationCount)]<-0
    
#Remove SurveyAreaIdentifier from the data on where the sum of ObersevationCount is 0 across all years
#If a species was never detected on a route, we will not include that route in the species specific analysis
#This is considered out of range or in unsuitable habitat
    dat<-dat %>% group_by(SurveyAreaIdentifier) %>% filter(sum(ObservationCount)>0) %>% ungroup()
    trends.csv$sample_size<-n_distinct(dat$SurveyAreaIdentifier) #assign number of routes to output table
    
    # create date and day of year columns
    dat$date <- as.Date(paste(dat$YearCollected, dat$MonthCollected, 
                              dat$DayCollected, sep = "-"))
    dat$doy <- as.numeric(format(dat$date, "%j"))
    
#Minimum Data Requirements##
    
    #Now we will check that the minimum data requirements are met. 
    #We will want to ensure that the species was detected a minimum of X times in a year
    #That the species was detected in at least 1/2 of the survey years
    #And that greater than a certain % of routes has non-zero counts
    
    SpeciesMean<- dat %>% group_by(YearCollected) %>% summarize(YearMean = mean(ObservationCount)) %>% ungroup() %>% summarize(MeanMean = mean(YearMean))
    SpeciesMean$NumYears <- n_distinct(sp.data$YearCollected)
    
    #calculate percent of SurveyAreaIdentifiers with zero counts
    routestotal<-n_distinct(dat$SurveyAreaIdentifier)
    routeszero<-dat %>% filter(ObservationCount==0) 
    routesnotzero<-n_distinct(routeszero$SurveyAreaIdentifier)
    SpeciesMean$ZeroRoutes<-(routestotal-routesnotzero)/routestotal

    #Now cheek the SpeciesMean object to see if the species meets the minimum data requirements 
    #all the variable must be large than the values min.abundance, min.years, zero.count, if TRUE continue with the analysis
    
    if(SpeciesMean$MeanMean>=min.abundance & SpeciesMean$NumYears>=min.years & SpeciesMean$ZeroRoutes<=zero.count){
  min.data <- TRUE 
      }else{
  min.data <- FALSE
    }

    #only continue if the species meets the minimum data requirements      
if(min.data==TRUE){
    
##Assign species names
  sp.codes<-meta_species_codes()
  sp.codes<-sp.codes %>% filter(authority=="BSCDATA") %>% dplyr::select(species_id, species_code)
  sp.id<-sp.codes$species_id[sp.codes$species_code==sp.list[i]]
  
  sp.names<-meta_species_taxonomy()
  species_name<-sp.names$english_name[sp.names$species_id==sp.id]
  species_sci_name<-sp.names$scientific_name[sp.names$species_id==sp.id]
  
###Model without spatail effect on abundance 
    
#Create index variables
    dat <- dat %>% mutate( 
      std_yr = wyear - Y2,
      kappa = as.integer(factor(dat$SurveyAreaIdentifier)),
      year_idx = as.integer(wyear),
      doy_idx = as.integer(doy))%>%
      st_as_sf(coords = c("DecimalLongitude", "DecimalLatitude"), crs = 4326, remove = FALSE)
    
    dat <- st_transform(dat, crs = utm_crs) %>% 
      mutate(
        easting = st_coordinates(.)[, 1]/1000,
        northing = st_coordinates(.)[, 2]/1000) %>% 
      arrange(SurveyAreaIdentifier, wyear)
  
    #Set prior for the random effects
    prec.prior<- list(prec = list(prior = "gaussian", param=c(0,0.1))) 
    hyper.iid<-list(prec=list(prior="pc.prec", param=c(2,0.05)))
    inla.setOption(scale.model.default=TRUE)
    
    formula<- ObservationCount ~ -1 + DurationInHours + 
      f(kappa, model="iid", hyper=hyper.iid) + f(year_idx, model = "ar1", hyper=prec.prior) + 
      f(doy_idx, model = "ar1", hyper=prec.prior)

    M0<-try(inla(formula, family = "nbinomial", data = dat, 
                 control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), verbose =TRUE), silent = T)
    

    #Dispersion Statistic to determiner is nbinomial is value
    mu1<-M0$summary.fitted.values[,"mean"]
    E1<-(dat$ObservationCount-mu1)/ sqrt(mu1 + mu1^2) #Pearson residuals
    N<-nrow(dat)
    p<-nrow(M0$summary.fixed + 2) # +1 for each the idd random effect
    Dispersion1<-sum(E1^2)/(N-p)
    print(paste("Dispersions Statistic out1 = ", Dispersion1, sep = ""))

    #write the dispersion statistic to the output file
    dispersion.csv$area_code<-site
    dispersion.csv$SpeciesCode<-sp.list[i]
    dispersion.csv$dispersion<-Dispersion1
    
    write.table(dispersion.csv, file = paste(out.dir,  "DispersionStat",".csv", sep = ""),
                col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE, sep = ",")
    
    dat$mu1<-mu1
    #plot ObservationCount and mu1 using ggplot, with 1:1 line
     q <- ggplot(dat, aes(x = ObservationCount, y = mu1)) + 
      geom_point() + 
      geom_abline(intercept = 0, slope = 1)
    
    # Save the plot with specified width, height, and dpi
    ggsave(filename = paste(plot.dir, sp.list[i], "_FitPlot.jpeg", sep = ""), 
           plot = q, 
           width = 8,    # Adjust width as needed
           height = 6,   # Adjust height as needed
           dpi = 150)    # Lower dpi for faster saving
    
     #Plot the data for visual inspection

     #Convert the data to a spatial object
     dat_sf <- st_as_sf(dat, coords = c("DecimalLongitude", "DecimalLatitude"), crs = 4326)
     #remove zero ObservationCounts from the output plot
     dat_sf<-dat_sf %>% group_by(SurveyAreaIdentifier, geometry) %>% summarise(ObservationCount = mean(ObservationCount))

     #write plot to Plots folder in Output
       p<-ggplot(data = dat_sf) +
       # Select a basemap
       annotation_map_tile(type = "cartolight", zoom = NULL, progress = "none") +
       # Plot the points, size by the sum of ObservationCount within a year
       geom_sf(aes(size=ObservationCount)) +
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

     ggsave(paste(plot.dir, sp.list[i], "_SumCountPlot.jpeg", sep = ""), plot = p, width = 10, height = 6, units = "in")

#create the datframe of covariates
N<-nrow(dat)

    Covariates<- data.frame(
      Intercept=rep(1, N),
      DurationInHours=dat$DurationInHours,
      kappa = dat$kappa,
      #year_idx = dat$year_idx,
      doy_idx = dat$doy_idx
    )

    #Create the Mesh
    #Make a set of distinct study sites for mapping
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
    
    Bound<-inla.nonconvex.hull(Loc_unique)

    mesh2<-fm_mesh_2d_inla(Loc_unique, 
                           #mesh2<-inla.mesh.2d(Loc, 
                           boundary = Bound,
                           max.edge = c(150, 200), # km inside and outside
                           cutoff = 0,
                           crs = fm_crs(dat))
    #SPDE
    spde <- inla.spde2.pcmatern(  #could also use inla.spde2.pcmatern
      mesh = mesh2,
      prior.range = c(500, 0.5),
      prior.sigma = c(1, 0.5)
    )

    #Spatial Fields
    # make index sets for the spatial model
    alpha_idx <- inla.spde.make.index(name = "alpha", n.spde = spde$n.spde) #n.repl is used to account for repeated measure at the same location over time.
    tau_idx <- inla.spde.make.index(name = "tau", n.spde = spde$n.spde) #n.repl is used to account for repeated measure at the same location over time.
    
    #Projection matrix A using all locations
    A_alph <- inla.spde.make.A(mesh=mesh2, loc=Loc_all)  # pg 218 this formula should include an alpha, default is 2 if the model does not include times.
    A_tau <- inla.spde.make.A(mesh = mesh2, loc = Loc_all, weights = dat$std_yr) # note weights argument
    
    #Create Stack Object for INLA
    Stack <- inla.stack (
      tag="FitGAM",
      data=list(count=as.vector(dat$ObservationCount)), #response from the dataframe
      effects = list(Covariates=Covariates, alpha = alpha_idx, tau=tau_idx),  #covariate list
      A = list(
        1, #value of 1 to non-spatial terms
        A_alph, 
        A_tau
      )
    )
    
    formula.sp<- count ~ -1 + Intercept + DurationInHours + 
      f(kappa, model="iid", hyper=hyper.iid) + f(doy_idx, model = "ar1", hyper=prec.prior) + f(alpha, model =spde)+ f(tau, model =spde)
       


    #fit the non-spatial model using INLA
    M1<-inla(formula.sp, family = "nbinomial", data = inla.stack.data(Stack),
                    control.predictor = list(A=inla.stack.A(Stack)),
                    control.compute = list(dic=TRUE, waic=TRUE, config = TRUE),
                    verbose =TRUE)

    #Compare the DIC and WIC values
   z.out<-NULL
   dic<-c(M0$dic$dic, M1$dic$dic)
   wic<-c(M0$waic$waic, M1$waic$waic)
   ModelType<-c("GAM", "GAM + SPATIAL")
   z.out<-cbind(ModelType, dic, wic)
   z.out<-as.data.frame(z.out)
   z.out$SpeciesCode<-sp.code

   write.table(z.out, file = paste(out.dir,  "ModelComparison.csv", sep = ""),
               col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE, sep = ",")
   

#Calculate Posterior estimate of abundance
nsamples<- 100
post.sample1 <-NULL #clear previous
post.sample1<-inla.posterior.sample(nsamples, M1)

tmp1<-NULL
tmp1 <- dat %>% dplyr::select(wyear) %>% st_drop_geometry() 

#for each sample in the posterior we want to join the predicted to tmp so that the predictions line up with doy/year and we can get the mean count by year
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
years = paste(min(dat$YearCollected), "-", max(dat$YearCollected), sep = ""),
year = wyear,
period ="all years",
season = "winter",
area_code = site,
model_type = "GLM DOY AR1 ALPHA+TAU SPATIAL",
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
family="nbinomial",
results_code = "BCCWS+PSSS",
version = "2025",
season="Winter",
area_code=site,
trend_index="") #look at CMMN code to generate in next round of analysis

# Run LOESS function
indices.csv$LOESS_index = loess_func(indices.csv$index, indices.csv$wyear)

# Order output before printing to table
indices.csv<-indices.csv %>% dplyr::select(results_code, version, area_code, season, period, species_code, species_id, year, index, stderr, stdev, upper_ci, lower_ci, LOESS_index, trend_index)

# Write data to table
write.table(indices.csv, 
            file = paste(out.dir,	site, "_AnnualIndices.csv", sep = ""),
            row.names = FALSE, 
            append = TRUE, 
            quote = FALSE, 
            sep = ",", 
            col.names = FALSE)


##TRENDS: SPECIFIC TO  SPECIES and TIME PERIOD
#AUTO-GENERATE TIME PERIODS TO ANALYZE TRENDS
time.period = NA
nyears=length(unique(dat$wyear))
list.years<-unique(dat$wyear)
rev.years<-rev(list.years)

#Generate all-years, 10 years, 20 years and 3 generation length.

#Fetch the generation length from the NatureCounts Database
gen_years <- nc_query_table(table = "SpeciesLifeHistory") %>%
  filter(subcategDescr == "Average generation length (years)") %>% 
  filter(speciesID == sp.id) %>% pull(value) %>% as.numeric()
gen.length <- round(gen_years  *  3)

if(length(gen.length)>0){
  gen.length<-min(gen.length)
}

if(is.na(time.period)) {
  endyr <- max(dat$wyear)
  startyr <- min(dat$wyear)
  totyr<- endyr-startyr
  
  #if gen.length is missing assign 10
  if(is.na(gen.length)){
    gen.length<-10
  }
  
  #if 3 gen < 10 years, keep 10 years
  if(gen.length<10){
    gen.length<-10
    #threegen<-endyr-gen.length+1  
    threegen<-rev.years[gen.length]
    yrthreegen<-nyears-gen.length+1
  }else{
    threegen<-rev.years[gen.length]  
    yrthreegen<-nyears-gen.length+1
  }
  
  #if 3 gen is longer than the available dataset, keep all years  
  if(gen.length>nyears){
    threegen<-startyr
    yrthreegen<-1  
  }
  
  tenyr<-rev.years[10]
  yrten<-nyears-9
  
  if(totyr>20){
    twentyyr<-rev.years[20]
    yrtwenty<-nyears-19
    
    time.period = c("all years", "20-years", "10-years", "3Gen-Recent")
    Y1.trend <- c(startyr, twentyyr, tenyr, threegen)
    Y2.trend <- c(endyr, endyr, endyr, endyr)
    
    y1.trend <- c(1, yrtwenty, yrten, yrthreegen)
    y2.trend <- c(nyears, nyears, nyears, nyears)
  }else{
    
    time.period = c("all years", "10-years", "3Gen-Recent")
    Y1.trend <- c(startyr, tenyr, threegen)
    Y2.trend <- c(endyr, endyr, endyr)
    
    y1.trend <- c(1, yrten, yrthreegen)
    y2.trend <- c(nyears, nyears, nyears)
    
  }
  
} # end is.na(time.period) 

for(p in 1:length(time.period)) {
  
  period <- time.period[p]
  Y1 <- Y1.trend[p]
  Y2 <- Y2.trend[p]
  y1 <- y1.trend[p]
  y2 <- y2.trend[p]
  
##END POINT TRENDS  
  pred.ch<-tmp1 %>% filter(wyear %in% c(Y1, Y2)) %>% dplyr::select(-wyear)
  pred.ch<-t(pred.ch)
  pred.ch<-as.data.frame(pred.ch)
  
  pred.ch<-pred.ch %>% mutate(ch=(V2/V1), max_year=Y2, min_year=Y1, tr=(100*((ch^(1/(max_year-min_year)))-1)))
  pred.ch<-pred.ch %>% reframe(trnd=median(tr), percent_change=100*(median(ch)-1), lower_ci=quantile(tr, probs=0.025), upper_ci=quantile(tr, probs=0.95), sd=sd(tr), Width_of_Credible_Interval=upper_ci-lower_ci) %>% distinct()
  
  #write output to table   
  trend.out<-NULL
  trend.out <- pred.ch %>%
    mutate(model_type="GLM DOY AR1 ALPHA+TAU SPATIAL", 
           model_family = "nbinomial",
           years = paste(Y1, "-", Y2, sep = ""),
           year_start=Y1, 
           year_end=Y2,
           period =period,
           season = "winter",
           results_code = "BCCWS+PSSS",
           area_code = site,
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
              file = paste(out.dir, site, "_TrendsEndpoint.csv", sep = ""), 
              row.names = FALSE, 
              append = TRUE, 
              quote = FALSE, 
              sep = ",", 
              col.names = FALSE)  
  
#SLOPE TRENDS
#Summary of the GAM smooth on year
  wy=c(y1:y2)
  pred.yr<-tmp1 %>% dplyr::select(-wyear)
  pred.yr<-t(pred.yr)
  ne = log(pred.yr[,wy]) 
  
  #This is the slope function. 
  #It calculates the coefficient of the lm slope for each row in the smoothed output. 
  
  #slope function 1 
  slope  <-  function(x){
    return(coef(lm(x~I(y1:y2)))[2])
  }
  
  m =  apply(ne,1,slope)
  m = as.vector((exp(m)-1)*100)
  
  #include slop output in new table
  trend.out$index_type="Slope Trend"
  trend.out$trnd<-median(m, na.rm=TRUE)
  trend.out$lower_ci<-quantile(m, prob=0.025)
  trend.out$upper_ci<-quantile(m, prob=0.950)
  trend.out$sd<-sd(m, na.rm=TRUE)
  
  per_trend=trend.out$trnd/100
  period_num=Y2-Y1
  trend.out$percent_change<-((1+per_trend)^period_num-1)*100
  trend.out$Width_of_Credible_Interval_slope<-trend.out$upper_ci-trend.out$lower_ci
  trend.out$precision_cat = ifelse(pred.ch$Width_of_Credible_Interval<3.5, "High", ifelse(pred.ch$Width_of_Credible_Interval>=3.5 & pred.ch$Width_of_Credible_Interval<=6.7, "Medium", "Low"))
  
  write.trend<-trend.out %>% dplyr::select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, index_type, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
  
  
  write.table(write.trend, 
              file = paste(out.dir, site, "_TrendsSlope.csv", sep = ""), 
              row.names = FALSE, 
              append = TRUE, 
              quote = FALSE, 
              sep = ",", 
              col.names = FALSE)  

      } #end period loop
    } #end min.data  
  }#end SpeciesLoop



