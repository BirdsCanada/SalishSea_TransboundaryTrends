#Analysis iCAR

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

}  

if(area=="PSSS"){
  
  sp.dat<-sp.data %>% filter(ProjectCode=="PSSS")
  event<-events %>% filter(ProjectCode=="PSSS")

  #This is fixed in the data cleaning script and can be removed during next round. 
  event<-event %>% filter(DurationInHours<=3)
  
} 

#Specify the spatial extent of the analysis
if(area=="SalishSea"){
  
  sp.dat<-sp.data 
  event<-events
  
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
    
    print(paste(sp.list[i], min.data))
    
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
      
      #Model Formula
      
      if(guild=="Yes"){
        
        dat$sp_idx[is.na(dat$sp_idx)] <- 999 #replace NA which are zero counts with generic sp_idx
        
        formula<- ObservationCount ~ -1 + 
          f(year_idx, model = "iid", hyper = hyper.iid) + 
          f(sp_idx, model="iid", hyper=hyper.iid) +
          # cell ICAR random intercepts
          f(alpha_i, model="besag", graph=g1, constr=FALSE, scale.model=TRUE, hyper = hyper.iid) +
          # random route intercepts
          f(kappa, model = "iid", hyper = hyper.iid,)
       
      }else{
        
        formula<- ObservationCount ~ -1 + 
          f(year_idx, model = "iid", hyper = hyper.iid) + 
          # cell ICAR random intercepts
          f(alpha_i, model="besag", graph=g1, constr=FALSE, scale.model=TRUE, hyper = hyper.iid) +
          # random route intercepts
          f(kappa, model="iid", hyper = hyper.iid)
        
      }
      
      M0<-try(inla(formula, family = fam, data = dat, offset = log(dat$DurationInHours),
                   control.predictor = list(compute = TRUE), control.compute = list(dic=TRUE, config = TRUE), verbose =TRUE), silent = T)
  
      ##Remove polygons with no survey sites
      cells_with_counts <- unique(dat$alpha_i[which(!is.na(dat$ObservationCount))]) 
      
      
      
      
      
      } #end min data
    } #end if nrows = 0
  } #end sp.list