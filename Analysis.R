#Analysis scripts


#Specify the spatial extent of the analysis
if(site=="SalishSea"){
  
  #Create a loop for the species list
  for(i in 1:length(species_list)){
    
    #i<-1 #for testing
    
    #Subset the data for the species
    dat <- sp.data %>% filter(SpeciesCode==sp.list[i])
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
    
###Model without spatail effect on abundance 
    
#Create index variables
    dat <- dat %>% mutate( 
      std_yr = wyear - Y2,
      kappa = as.integer(factor(dat$SurveyAreaIdentifier)),
      year_idx = as.numeric(factor(wyear)),
      doy_idx = as.numeric(factor(doy))) %>%
      st_as_sf(coords = c("DecimalLongitude", "DecimalLatitude"), crs = 4326, remove = FALSE)
    
    dat <- st_transform(dat, crs = utm_crs) %>% 
      mutate(
        easting = st_coordinates(.)[, 1]/1000,
        northing = st_coordinates(.)[, 2]/1000) %>% 
      arrange(SurveyAreaIdentifier, wyear)

  # #GAM for doy and year effects
  #   #determine the number of knots for year
  #   N<-nrow(dat)
  #   kyears<- 4 #add one knot for every 4 years of the time series follow Smith and Edwards
  #   kdays<-3 #polynomial doy effect
  # 
  #   #smooth years
  #   smy<-smoothCon(s(wyear, bs="cr", k=kyears), data=dat)[[1]]
  #   Xsmy<-smy$X #basis function
  #   colnames(Xsmy)<-paste("BasisYR", 1:ncol(Xsmy), sep="")#need unique names
  #   
  #   lcs.y<-inla.make.lincombs(data.frame(Xsmy))
  #   names(lcs.y)<-paste(names(lcs.y), "YR", sep="")#need unique names
  # 
  #   dat<-cbind(dat, Xsmy)
  # 
  #   #smooth day of year
  #   sdy<-smoothCon(s(doy, bs="cr", k=kdays), data=dat)[[1]]
  #   Xsdy<-sdy$X #basis function
  #   colnames(Xsdy)<-paste("BasisDAY", 1:ncol(Xsdy), sep="")#need unique names
  # 
  #   lcs.d<-inla.make.lincombs(data.frame(Xsdy))
  #   names(lcs.d)<-paste(names(lcs.d), "DAY", sep="")#need unique names
  # 
  #   dat<-cbind(dat, Xsdy)
  #   lcs<-c(lcs.y, lcs.d)
  #   
    #Use penalised complex prior for random year effect
    hyper.iid<-list(prec=list(prior="pc.prec", param=c(2,0.05)))
    inla.setOption(scale.model.default=TRUE)
    
    formula.gam<- ObservationCount ~ -1 + wyear + DurationInHours + 
      f(kappa, model="iid", hyper=hyper.iid) + + f(year_idx, model = "ar1", hyper=prec.prior) + 
      f(doy_idx, model = "ar1", hyper=prec.prior)

    M0<-try(inla(formula.gam, family = "nbinomial", data = dat, 
                 control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, 
                 config = TRUE), #lincomb=lcs, 
                 verbose =TRUE), silent = T)
    

    #Dispersion Statistic to determiner is nbinomial is value
    mu1<-M0$summary.fitted.values[,"mean"]
    E1<-(dat$ObservationCount-mu1)/ sqrt(mu1 + mu1^2) #Pearson residuals
    N<-nrow(dat)
    p<-nrow(M0$summary.fixed + 2) # +1 for each the idd random effect
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

    
    # Summary of the GAM smooth on year
    # I have checked that this is reflected in the latent posterior samples. Looks good. 
    
    CR<-NULL
    nr<-nrow(dat) #Year is stored first in the lc output
    
    smooth<-M0$summary.lincomb.derived
    
    f.yr<-smooth[1:nr+0*nr, "mean"]
    selo.yr<-smooth[1:nr+0*nr, "0.025quant"]
    seup.yr<-smooth[1:nr+0*nr, "0.975quant"]
    
    f.doy<-smooth[1:nr+1*nr, "mean"]
    selo.doy<-smooth[1:nr+1*nr, "0.025quant"]
    seup.doy<-smooth[1:nr+1*nr, "0.975quant"]
    
    oyr<-order(dat$year_idx)
    odoy<-order(dat$doy_idx)
    
    MyData <- data.frame(
      mu   = c(   f.yr[oyr],    f.doy[odoy]), 
      SeUp = c(seup.yr[oyr], seup.doy[odoy]), 
      SeLo = c(selo.yr[oyr], selo.doy[odoy]), 
      Xaxis = c(sort(dat$year_idx), sort(dat$doy_idx)),
      ID    = factor(rep(c("Year smoother", "DOY smoother"), each = nrow(dat))))
    
    #plot the smooths
    p<-ggplot(MyData, aes(x = Xaxis, y = mu, colour = ID)) +
      geom_line() +
      facet_wrap(~ ID, scales = "free") +
      #geom_ribbon(aes(ymin = SeLo, ymax = SeUp), alpha = 0.2) +
      labs(title = sp.list[i]) +
      theme_minimal()
   
     ggsave(paste(plot.dir, sp.list[i], "_SmoothPlot.jpeg", plot=p, sep = ""))
    
    
    
    
    
    
    
    
    
    
    
    
    
##TO COMPLEX##    
    #create the datframe of covariates
    Covariates<- data.frame(
      Intercept=rep(1, N), 
      BasisYR1= Xsmy[,"BasisYR1"],
      BasisYR2= Xsmy[,"BasisYR2"],
      BasisYR3= Xsmy[,"BasisYR3"],
      BasisYR4= Xsmy[,"BasisYR4"],
      #year_idx = dat$year_idx,
      BasisDAY1= Xsdy[,"BasisDAY1"],
      BasisDAY2= Xsdy[,"BasisDAY2"],
      BasisDAY3= Xsdy[,"BasisDAY3"],
      DurationInHours=dat$DurationInHours, 
      kappa = dat$kappa
    )
    
    #Create the Mesh
    #Make a set of distinct study sites for mapping
    site_map <- dat %>%
      dplyr::select(SurveyAreaIdentifier, easting, northing) %>% distinct()

   
    #make the mesh this way so that the point fall on the vertices of the lattice
    Loc_all<-as.matrix(st_coordinates(dat)) 
    
    #make unique locations
    Loc_unique<-dat %>% dplyr::select(SurveyAreaIdentifier, easting, northing) %>%
      distinct() %>%
      dplyr::select(easting, northing) %>% distinct() %>% 
      st_drop_geometry() %>% 
      as.matrix()
    
    Bound<-inla.nonconvex.hull(Loc_unique)
    # 
    # #Create the mesh with unique locations
     mesh2<-inla.mesh.2d(boundary = Bound,
                            max.edge = c(100, 150), # km inside and outside
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
    
    #Projection matrix A using all locations
    A <- inla.spde.make.A(mesh=mesh2, loc=Loc_all)  # pg 218 this formula should include an alpha, default is 2 if the model does not include times. 
    
    #Create Stack Object for INLA
    Stack <- inla.stack (
      tag="FitGAM", 
      data=list(count=as.vector(dat$ObservationCount)), #response from the dataframe
      effects = list(Covariates=Covariates, alpha = alpha_idx),  #covariate list
      A = list(
        1, #value of 1 to non-spatial terms
        A
      )
    )
    
    #Use penalised complex prior for random year effect
    pc_prec <- list(prior = "pcprec", param = c(1, 0.1))
    
    #Create Model Formula which does not include the spatial varying effect
    formulaGAM<- count ~ -1 + Intercept + BasisYR1 + BasisYR2 + BasisYR3 + BasisYR4 + BasisDAY1 + BasisDAY2 + BasisDAY3 + DurationInHours + f(kappa, model="iid") 
    
    #fit the non-spatial model using INLA
    M1<-inla(formulaGAM, family = "nbinomial", data = inla.stack.data(Stack), 
                    control.predictor = list(A=inla.stack.A(Stack)),
                    control.compute = list(dic=TRUE, waic=TRUE, config = TRUE),
                    lincomb=lcs, verbose =TRUE)
    
    
    
    
    #Create Model Formula which includes the spatial varying effect
    formulaGAMsp<- count ~ -1 + Intercept + BasisYR1 + BasisYR2 + BasisYR3 + BasisYR4 + BasisDAY1 + BasisDAY2 + BasisDAY3 + DurationInHours + f(kappa, model="iid") + f(alpha_idx, model =spde)
    
    #fit the non-spatial model using INLA
    M2<-inla(formulaGAMsp, family = "nbinomial", data = inla.stack.data(Stack), 
             control.predictor = list(A=inla.stack.A(Stack)),
             control.compute = list(dic=TRUE, waic=TRUE, config = TRUE),
             lincomb=lcs, verbose =TRUE)
    
    #Compare the DIC and WIC values
   dic<-c(M1$dic$dic, M2$dic$dic)
   wic<-c(M1$waic$waic, M2$waic$waic)  
   z.out<-cbind(dic, wic)
   rownames(z.out)<-c("GAM", "GAM + SPATIAL")
   z.out<-as.data.frame(z.out)
   z.out$SpeciesCode<-sp.code
  
   
   
    
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


