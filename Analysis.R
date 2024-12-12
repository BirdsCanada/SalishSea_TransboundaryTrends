#Analysis scripts


#Specify the spatial extent of the analysis
if(site=="SalishSea"){
  
  #Create a loop for the species list
  for(i in 1:length(species_list)){
    
    #i<-1 #for testing
    
    #Subset the data for the species
    dat <- sp.data %>% filter(SpeciesCode==sp.list[i])
    
    sp.code<-sp.list[i]
    
##zero-fill the dat using the events dataframe##
    dat<-events %>% left_join(dat, by= c("ProjectCode", "SurveyAreaIdentifier", "wyear", "YearCollected", "MonthCollected", "DayCollected"))
    
    dat$SpeciesCode<-sp.list[i]
    
    #Observation Counts will be backfilled with a 0 whenever it is NA
    dat$ObservationCount[is.na(dat$ObservationCount)]<-0
    
##Minimum Data Requirements##
    
    #Now we will check that the minimum data requirements are met. 
    #We will want to ensure that the species was detected a minimum of X times in a year
    #That the species was detected in at least 1/2 of the survey years
    #And that greater than a certain % of routes has non-zero counts
    
    SpeciesMean<- sp.data %>% group_by(YearCollected) %>% summarize(YearMean = mean(ObservationCount)) %>% ungroup() %>% summarize(MeanMean = mean(YearMean))
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
    
    #Remove SurveyAreaIdentifier from the data on where the sum of ObersevationCount is 0 across all years
    #If a species was never detected on a route, we will not include that route in the species specific analysis
    #This is considered out of range or in unsuitable habitat
    dat<-dat %>% group_by(SurveyAreaIdentifier) %>% filter(sum(ObservationCount)>0) %>% ungroup()
    trends.csv$sample_size<-n_distinct(dat$SurveyAreaIdentifier)
    
    # create date and day of year columns
    dat$date <- as.Date(paste(dat$YearCollected, dat$MonthCollected, 
                                   dat$DayCollected, sep = "-"))
    dat$doy <- as.numeric(format(dat$date, "%j"))
    
###Model without spatail effect on abundance    
    #standardize year to max, prepare index variables
    #where i = grid cell, k = route, t = year, e = protocol_id
    dat <- dat %>% mutate(std_yr = wyear - Y2)
    #dat$ellip_e <- as.integer(factor(dat$ProjectCode))#index for random protocol effect
    dat$kappa_k <- as.integer(factor(dat$SurveyAreaIdentifier))#index for the random route effect
    dat$yearfac = as.factor(dat$wyear)

    # MODEL FORMULA with GAM for doy and year effects

    #determine the number of knots for year
    N<-nrow(dat)
    kyears<- 4 #add one knot for every 4 years of the time series follow Smith and Edwards
    kdays<-3 #polynomial doy effect

    #smooth years
    smy<-smoothCon(s(wyear, bs="cr", k=kyears), data=dat)[[1]]
    Xsmy<-smy$X #basis function
    colnames(Xsmy)<-paste("BasisYR", 1:ncol(Xsmy), sep="")#need unique names
    
    lcs.y<-inla.make.lincombs(data.frame(Xsmy))
    names(lcs.y)<-paste(names(lcs.y), "YR", sep="")#need unique names

    dat<-cbind(dat, Xsmy)

    #smooth day of year
    sdy<-smoothCon(s(doy, bs="cr", k=kdays), data=dat)[[1]]
    Xsdy<-sdy$X #basis function
    colnames(Xsdy)<-paste("BasisDay", 1:ncol(Xsdy), sep="")#need unique names

    lcs.d<-inla.make.lincombs(data.frame(Xsdy))
    names(lcs.d)<-paste(names(lcs.d), "DAY", sep="")#need unique names

    dat<-cbind(dat, Xsdy)
    lcs<-c(lcs.y, lcs.d)
    
    Covariates<- data.frame(
      Intercept=rep(1, N), 
      YR1= Xsmy[,"BasisYR1"],
      YR2= Xsmy[,"BasisYR2"],
      YR3= Xsmy[,"BasisYR3"],
      YR4= Xsmy[,"BasisYR4"],
      DAY1= Xsdy[,"BasisDay1"],
      DAY2= Xsdy[,"BasisDay2"],
      DAY3= Xsdy[,"BasisDay3"],
      DurationInHours=dat$DurationInHours
    )
    

    #Use penalised complex prior for random year effect
    hyper.iid<-list(prec=list(prior="pc.prec", param=c(2,0.05)))
    inla.setOption(scale.model.default=TRUE)

    formula<- ObservationCount ~ -1 + Xsmy + Xsdy + DurationInHours + f(kappa_k, model="iid", hyper=hyper.iid) + f(yearfac, model="iid", hyper=hyper.iid)


    #fit the model using INLA
    model<-try(inla(formula, family = "nbinomial", data = dat,
                        control.predictor = list(compute = TRUE),
                        control.compute = list(dic=TRUE, config = TRUE),
                        lincomb=lcs, verbose =TRUE), silent = T)


    
###Model with spatial effect on Abundance
    #Create index variables
    dat <- dat %>% mutate(site_idx = factor(paste(SurveyAreaIdentifier)), 
        std_yr = wyear - Y2,
        year_idx = as.numeric(factor(wyear))) %>% 
      st_as_sf(coords = c("DecimalLongitude", "DecimalLatitude"), crs = 4326, remove = FALSE)
     
      dat <- st_transform(dat, crs = utm_crs) %>% 
        mutate(
        easting = st_coordinates(.)[, 1]/1000,
        northing = st_coordinates(.)[, 2]/1000) %>% 
      arrange(SurveyAreaIdentifier, wyear)
    
      #determine the number of knots for year
      #add one knot for every 4 years of the time series follow Smith and Edwards
      nyears <- length(unique(dat$wyear))
      kyears<- ceiling(nyears/4) #round up to nearest whole number
      kdays<-3 #polynomial doy effect

      #smooth years
      smy<-smoothCon(s(wyear, bs="cr", k=kyears), data=dat)[[1]]
      Xsmy<-smy$X #basis function
      colnames(Xsmy)<-paste("BasisYR", 1:ncol(Xsmy), sep="")#need unique names

      lcs.y<-inla.make.lincombs(data.frame(Xsmy))
      names(lcs.y)<-paste(names(lcs.y), "YR", sep="")#need unique names

      dat<-cbind(dat, Xsmy)

      #smooth day of year
      sdy<-smoothCon(s(doy, bs="cr", k=kdays), data=dat)[[1]]
      Xsdy<-sdy$X #basis function
      colnames(Xsdy)<-paste("BasisDay", 1:ncol(Xsdy), sep="")#need unique names

      lcs.d<-inla.make.lincombs(data.frame(Xsdy))
      names(lcs.d)<-paste(names(lcs.d), "DAY", sep="")#need unique names

      dat<-cbind(dat, Xsdy)
      lcs<-c(lcs.y, lcs.d)

    
    #Make a set of distinct study sites for mapping
    site_map <- dat %>%
      dplyr::select(SurveyAreaIdentifier, easting, northing) %>% distinct()

    ##-----------------------------------------------------------
    #Make a set of distinct study sites for mapping    
    #Make a two extension hulls and mesh for spatial model
    # 
    # hull <- fm_extensions(
    #   site_map,
    #   convex = c(20, 50),
    #   concave = c(35, 50)
    # )
    
    #make the mesh this way so that the point fall on the vertices of the lattice
    Loc<-site_map%>% dplyr::select(easting, northing) %>% 
      st_drop_geometry() %>% distinct %>%  as.matrix()
    
    Bound<-inla.nonconvex.hull(Loc)
   
    mesh2<-inla.mesh.2d(Loc, 
                           boundary = Bound,
                           max.edge = c(100, 120), # km inside and outside
                           cutoff = 0,
                           crs = fm_crs(dat))
    
    A <- inla.spde.make.A(mesh2, Loc)  # pg 218 this formula should include an alpha, default is 2 if the model does not include times. 
    
    spde <- inla.spde2.pcmatern(
      mesh = mesh2,
      prior.range = c(500, 0.5),
      prior.sigma = c(1, 0.5)
    )
    
    # make index sets
    alpha_idx <- inla.spde.make.index(name = "alpha", n.spde = mesh2$n)
    
    # make projector matrices
    A_alpha <- inla.spde.make.A(mesh = mesh2, loc = Loc)
    
    # stack observed data
    stack_fit <- inla.stack(
      tag = "obs",
      data = list(count = as.vector(dat$ObservationCount)), # response from data frame
      effects = list(data.frame(
        intercept = 1,
        kappa = dat$site_idx
      ), # predictors from data frame
      alpha = alpha_idx, # or index sets if spatial
      DurationInHours = dat$DurationInHours,
      Xsmy = dat$Xsmy,
      Xsdy = dat$Xsdy
      ),
      A = list(
        1, # a value of 1 is given for non-spatial terms
        A_alpha,
        A_eps,
        A_tau
      )
    )
   
###Model Formula
    # iid prior
    pc_prec <- list(prior = "pcprec", param = c(1, 0.1))
    
    # components
    svc_components <- ObservationCounts ~ -1 + Xsdy + Xsmy + DurationInHours + 
      kappa(site_idx, model = "iid", constr = TRUE, hyper = list(prec = pc_prec)) +
      alpha(geometry, model = spde)
      ) 
    
    #Run Model
    res <- bru(
      svc_components,
      like(
        formula = svc_formula,
        family = fam,
        data = sp.data
      ),
      options = list(
        control.compute = list(waic = TRUE, cpo = FALSE),
        control.inla = list(int.strategy = "eb"),
        verbose = FALSE
      )
    )
    
    
    
   #Dispersion Statistic
    mu1<-model$summary.fitted.values[,"mean"]
    E1<-(dat$ObservationCount-mu1)/ sqrt(mu1 + mu1^2) #Pearson residuals
    N<-nrow(dat)
    p<-nrow(model$summary.fixed + 2) # +1 for each the idd random effect
    Dispersion1<-sum(E1^2)/(N-p)
    print(paste("Dispersions Statistic out1 = ", Dispersion1, sep = ""))
    
    dat$mu1<-mu1
    #plot ObservationCount and mu1 using ggplot, with 1:1 line
    q<-ggplot(dat, aes(x=ObservationCount, y=mu1)) + geom_point() + geom_abline(intercept = 0, slope = 1)
    ggsave(paste(plot.dir, sp.list[i], "_FitPlot.jpeg", plot=q, sep = ""))

    #write the dispersion statistic to the output file
    dispersion.csv$area_code<-site
    dispersion.csv$SpeciesCode<-sp.list[i]
    dispersion.csv$dispersion<-Dispersion1
    
    write.table(dispersion.csv, file = paste(out.dir,  "DispersionStat",".csv", sep = ""), 
               col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE, sep = ",")
    
    #Plot the data for visual inspection
    
    #Convert the data to a spatial object
    dat_sf <- st_as_sf(dat, coords = c("DecimalLongitude", "DecimalLatitude"), crs = 4326)
    #remove zero ObservationCounts from the output plot
    dat_sf<-dat_sf %>% group_by(SurveyAreaIdentifier, geometry) %>% summarise(ObservationCount = sum(ObservationCount)) 
    
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
      scale_size_continuous(name = "Sum Observation Count")
 
    ggsave(paste(plot.dir, sp.list[i], "_SumCountPlot.jpeg", sep = ""), plot = p, width = 10, height = 6, units = "in")
      
    #turn off device
    dev.off()
    
    
      
    
    } #end min.data  
   }#end SpeciesLoop
} #end if SalishSea