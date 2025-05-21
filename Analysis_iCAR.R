#Analysis iCAR


if(length(species.list) == 1){
  sp.list<-unique(sp.data$CommonName)
}else{
  sp.list<-species.list
}

  sp.dat<-sp.data 
  event<-events
  

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


#grid data are only those cells containing data. This layers is created in ArcGIS. 
nb1 <- spdep::poly2nb(Grid, row.names=Grid$data); nb1
is.symmetric.nb(nb1, verbose = FALSE, force = TRUE)
nb2INLA("nb1.graph", nb1)
nb1.adj <- paste(getwd(),"/nb1.graph", sep="")
g1 <- inla.read.graph("nb1.graph")


#Create a loop for the species list
for(i in 1:length(sp.list)){
  dat<-NULL 
  #i<-1 #for testing
  print(paste("Currently analyzing species ", i, "/", sp.list[i], sep = "")) 
  
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
    
    #Only retains routes on which a species was detected >1 years   
    # Count the number of years each site had detection
    site_years_detected <- aggregate(ObservationCount ~ SurveyAreaIdentifier + wyear, data = dat, FUN = sum)
    site_years_detected$Detected <- site_years_detected$ObservationCount > 0
    
    # Sum the number of years with detections for each site
    site_detect_years <- aggregate(Detected ~ SurveyAreaIdentifier, data = site_years_detected, FUN = sum)
    
    # Filter for sites with detections in more than 1 years
    sites_to_keep <- subset(site_detect_years, Detected > 1)$SurveyAreaIdentifier
    
    # Filter the main dataset
    dat <- subset(dat, SurveyAreaIdentifier %in% sites_to_keep)    
    
    #Remove SurveyAreaIdentifier from the data on where the sum of ObersevationCount is 0 across all years
    #If a species was never detected on a route, we will not include that route in the species specific analysis
    #This is considered out of range or in unsuitable habitat
    dat<-dat %>% group_by(SurveyAreaIdentifier) %>% filter(sum(ObservationCount)>0) %>% ungroup()
    routes<-n_distinct(dat$SurveyAreaIdentifier)
    
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
    
    print(paste("Did", sp.list[i], "meet minimum data requirements:", min.data))
    
    #only continue if the species meets the minimum data requirements      
    if(min.data==TRUE){
      
      #Prepare the parameters
      wyears <- unique(dat$wyear)
      mean_wyear <- max(wyears)
      
      #Create index variables
      dat <- dat %>% mutate( 
        std_yr = wyear - Y2,
        alpha_i = as.integer(factor(dat$Name)),
        protocol = factor(ProjectCode), 
        kappa = as.integer(factor(dat$SurveyAreaIdentifier)),
        year_idx = as.integer(wyear - mean_wyear), #intercept is the expected count during the most recent year of data collection. 
        wmonth_idx = as.factor(wmonth), 
        sp_idx = as.integer(factor(CommonName)))%>%
        arrange(SurveyAreaIdentifier, wyear)
      
      # Aggregate data to annual max count
      dat <- dat %>%
        group_by(SurveyAreaIdentifier, wyear) %>%
        slice_max(ObservationCount, n = 1, with_ties = FALSE) %>%
        ungroup()
      
      #Calucalte sample size per polygon alpha_i
      sample_size<-dat %>% group_by(alpha_i) %>% summarise(sample_size=n_distinct(SurveyAreaIdentifier))
      
      # Last, remove alpha_i with incomplete sampling over all years. 
      # Indicates that the polygon is not well sampled for a given species and end point trend may not work
      period_num = Y2-Y1 
      
       complete_sites <- dat %>%
        group_by(alpha_i) %>%
        summarise(has_all_years = n_distinct(wyear)) %>%
        filter(has_all_years == period_num) %>%
        select(alpha_i)
      
      # Filter original data frame to keep only complete sites
      dat <- dat %>%
        filter(alpha_i %in% complete_sites$alpha_i)
      
      #Model Formula
      if(guild=="Yes"){
        
        dat$sp_idx[is.na(dat$sp_idx)] <- 999 #replace NA which are zero counts with generic sp_idx
        
        formula<- ObservationCount ~ -1 + 
          f(year_idx, model = "iid", hyper = hyper.iid) + 
          f(sp_idx, model="iid", hyper=hyper.iid) +
          # cell ICAR random intercepts
          f(alpha_i, model="besag", graph=g1, constr=FALSE, scale.model=TRUE, hyper = hyper.iid) +
          # random route intercepts
          f(kappa, model = "iid", hyper = hyper.iid)
       
      }else{
        
        formula<- ObservationCount ~ -1 + 
          f(year_idx, model = "iid", hyper = hyper.iid) + 
          # cell ICAR random intercepts
          f(alpha_i, model="besag", graph=g1, constr=FALSE, scale.model=TRUE, hyper = hyper.iid) +
          # random route intercepts
          f(kappa, model="iid", hyper = hyper.iid)
        
      }
      
      M2<-try(inla(formula, family = fam, data = dat, offset = log(dat$DurationInHours),
                   control.predictor = list(compute = TRUE),
                   control.compute = list(
                     dic = TRUE,                # For model comparison
                     waic = TRUE,               # For model comparison
                     config = TRUE,             # Enable posterior sampling
                     cpo = TRUE                 # Optional: cross-validated PIT
                   ),
                   control.fixed = list(
                     mean = 0,                  # Prior mean for fixed effects
                     prec = 0.001               # Prior precision for fixed effects
                   ), 
                   control.family = list(
                     hyper = list(theta = list(prior = "loggamma", param = c(3, 0.1)))
                   )
      ))
  
      #Dispersion Statistic
      mu1<-M2$summary.fitted.values[,"mean"]
      E1<-(dat$ObservationCount-mu1)/ sqrt(mu1 + mu1^2) #Pearson residuals
      N<-nrow(dat)
      p<-nrow(M2$summary.fixed + 2) # +1 for each the idd random effect
      Dispersion1<-sum(E1^2)/(N-p)
      print(paste(sp.list[i], " Dispersions Statistic = ", Dispersion1, sep = ""))
      
      #write the dispersion statistic to the output file
      dispersion.csv$area_code<-area
      dispersion.csv$SpeciesCode<-sp.list[i]
      dispersion.csv$dispersion<-Dispersion1
      
      write.table(dispersion.csv, file = paste(out.dir,  name, "_DispersionStat_iCAR.csv", sep = ""),
                  col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE, sep = ",")
      dat$mu1<-mu1
      
      #plot ObservationCount and mu1 using ggplot, with 1:1 line
      q <- ggplot(dat, aes(x = ObservationCount, y = mu1)) + 
        geom_point() + 
        geom_abline(intercept = 0, slope = 1)
      
      # Save the plot with specified width, height, and dpi
      ggsave(filename = paste(plot.dir, area, sp.list[i], "_FitPlot_iCAR.jpeg", sep = ""), 
             plot = q, 
             width = 8,    # Adjust width as needed
             height = 6,   # Adjust height as needed
             dpi = 150)    # Lower dpi for faster saving
      
      ##Remove polygons with no survey sites
      cells_with_counts <- unique(dat$alpha_i[which(!is.na(dat$ObservationCount))]) 
      
      #Calculate Posterior estimate of abundance
      nsamples<- 100
      post.sample1 <-NULL #clear previous
      post.sample1<-inla.posterior.sample(nsamples, M2)
      
      tmp1<-NULL
      tmp1 <- dat %>% dplyr::select(wyear, alpha_i) %>% st_drop_geometry() 
      
      #for each sample in the posterior we want to join the predicted to tmp so that the predictions line up with year and we can get the mean count by year
      for (h in 1:nsamples){
        pred <- exp(post.sample1[[h]]$latent[1:nrow(dat)])
        tmp1[[paste0("V", h+1)]] <- pred
      }
    
      #will want to adjust V to match the posterior sample size   
      tmp1<-tmp1 %>% group_by(wyear, alpha_i) %>% summarise_all(mean, na.rm=TRUE)
      tmp1<-tmp1 %>% rowwise() %>% mutate(index = median(c_across(starts_with("V"))), 
                                          lower_ci=quantile(c_across(starts_with("V")), 0.025), 
                                          upper_ci=quantile(c_across(starts_with("V")), 0.975), 
                                          stdev=sd(c_across(starts_with("V"))), 
                                          stderr = stdev / sqrt(nsamples))  
      
      #link back this the site name using the grid_key
      grid3<-grid %>% st_drop_geometry() %>% 
        select(alpha_i, Name) %>% 
        mutate(area_code = Name) %>% 
        distinct()
      
      tmp1<-left_join(tmp1, grid3, by="alpha_i")
      
      #Assign data to output table 
      indices.csv<-tmp1 %>% dplyr::select(wyear, index, lower_ci, upper_ci, stdev, stderr, area_code) %>% mutate(
        species_code = sp.code,
        years = paste(min(dat$wyear), "-", max(dat$wyear), sep = ""),
        year = wyear,
        period ="all years",
        season = "winter",
        model_type = "iCAR ALPHA SPATIAL",
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
        results_code = "BCCWS/PSSS",
        version = Sys.Date(),
        season="Winter",
        trend_index="")
      
      # Run LOESS function
      indices.csv$LOESS_index = loess_func(indices.csv$index, indices.csv$wyear)
      
      # Order output before printing to table
      indices.csv<-indices.csv %>% ungroup %>% dplyr::select(results_code, version, area_code, season, period, species_code, species_id, year, index, stderr, stdev, upper_ci, lower_ci, LOESS_index, trend_index)
     
      # Write data to table
      write.table(indices.csv, 
                  file = paste(out.dir,	name, "_AnnualIndices_iCAR.csv", sep = ""),
                  row.names = FALSE, 
                  append = TRUE, 
                  quote = FALSE, 
                  sep = ",", 
                  col.names = FALSE)
      
      
      ##############################################################################
      ##END POINT TRENDS for each site based on simulated data
      
      tmp2<-tmp1 %>% select(-index, -upper_ci, -lower_ci, -stdev) %>% filter(wyear==Y1 | wyear==Y2)
     
      tmp2 <- tmp2 %>%
          pivot_longer(
          cols = V3:V102,
          names_to = "simulation",
          values_to = "abundance"
        )
      
      tmp2 <- tmp2 %>%
        pivot_wider(
          id_cols = c(alpha_i, simulation),
          names_from = wyear,
          values_from = abundance
        )
      
      y1_col <- as.character(Y1)
      y2_col <- as.character(Y2)
      
      df_trends <- tmp2 %>%
        mutate(
          trend = 100 * (
            (!!sym(y2_col) / !!sym(y1_col))^(1 / (Y2 - Y1)) - 1
          ))
      
      df_trends <- df_trends %>%
        group_by(alpha_i) %>%
        summarise(
          trnd = mean(trend, na.rm = TRUE),
          lower_ci = quantile(trend, 0.025, na.rm = TRUE),
          upper_ci = quantile(trend, 0.975, na.rm = TRUE),
          stdev = sd(trend, na.rm=TRUE),
          stderr = sd(trend, na.rm = TRUE) / sqrt(n())) %>% 
        mutate(
          Width_of_Credible_Interval = upper_ci-lower_ci, 
          per_trend = trnd/100, 
          percent_change = ((1+per_trend)^period_num-1)*100,
          )
      
      #link back this the site name using the grid_key
      df_trends<-left_join(df_trends, grid3, by="alpha_i")
      
      #write output to table   
      trend.out<-NULL
      trend.out <- df_trends %>%
        mutate(model_type="iCAR ALPHA SPATIAL", 
               model_family = fam,
               years = paste(Y1, "-", Y2, sep = ""),
               year_start=Y1, 
               year_end=Y2,
               period ="all years",
               season = "winter",
               results_code = "BCCWS/PSSS",
               version=Sys.Date(), 
               area_code=df_trends$area_code,
               species_code = sp.code,
               species_id=sp.id, 
               species_name=species_name,
               species_sci_name=species_sci_name,
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
               precision_cat = ifelse(df_trends$Width_of_Credible_Interval<3.5, "High", ifelse(df_trends$Width_of_Credible_Interval>=3.5 & df_trends$Width_of_Credible_Interval<=6.7, "Medium", "Low")),
               coverage_num = "",
               coverage_cat = "",
               goal = "",
               goal_lower = "",
               #sample_size = sample_size,
               sample_size_units="Number of Sites",
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
      
      trend.out<-left_join(trend.out, sample_size, by="alpha_i")
      
      write.trend<-trend.out %>% dplyr::select(results_code,	version,	area_code,	season,	period, species_code,	species_id,	years,year_start,	year_end,	trnd,	lower_ci, upper_ci, index_type, stderr,	model_type,	model_fit,	percent_change,	percent_change_low,	percent_change_high,	prob_decrease_0,	prob_decrease_25,	prob_decrease_30,	prob_decrease_50,	prob_increase_0,	prob_increase_33,	prob_increase_100, suitability, precision_num,	precision_cat,	coverage_num,	coverage_cat,	sample_size, sample_size_units, prob_LD, prob_MD, prob_LC, prob_MI, prob_LI)
      
      write.table(write.trend, 
                  file = paste(out.dir, name, "_TrendsEndpoint_iCAR.csv", sep = ""), 
                  row.names = FALSE, 
                  append = TRUE, 
                  quote = FALSE, 
                  sep = ",", 
                  col.names = FALSE)  
      
      
      #if the analysis if for the full Salish Sea, create an output maps
      if(area=="SalishSea"){
        
        } #end if SalishSea
      } #end min data
    } #end if nrows = 0
  } #end sp.list