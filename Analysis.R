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
    
    #standardize year to max, prepare index variables 
    #where i = grid cell, k = route, t = year, e = protocol_id
    dat <- dat %>% mutate(std_yr = wyear - Y2)
    #dat$ellip_e <- as.integer(factor(dat$ProjectCode))#index for random protocol effect
    dat$kappa_k <- as.integer(factor(dat$SurveyAreaIdentifier))#index for the random route effect
    dat$yearfac = as.factor(dat$wyear)

    # MODEL FORMULA with GAM for doy and year effects
    
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
    
    #Use penalised complex prior for random year effect
    hyper.iid<-list(prec=list(prior="pc.prec", param=c(2,0.05)))
    inla.setOption(scale.model.default=TRUE)
    
    formula<- ObservationCount ~ -1 + Xsmy + Xsdy + DurationInHours + f(kappa_k, model="iid", hyper=hyper.iid)  + f(yearfac, model="iid", hyper=hyper.iid) 
    
    
    #fit the model using INLA
    model<-try(inla(formula, family = "nbinomial", data = dat, 
                        control.predictor = list(compute = TRUE), 
                        control.compute = list(dic=TRUE, config = TRUE), 
                        lincomb=lcs, verbose =TRUE), silent = T)

    #Dispersion Statistic
    mu1<-model$summary.fitted.values[,"mean"]
    E1<-(dat$ObservationCount-mu1)/ sqrt(mu1 + mu1^2) #Pearson residuals
    N<-nrow(dat)
    p<-nrow(model$summary.fixed)
    Dispersion1<-sum(E1^2)/(N-p)
    print(paste("Dispersions Statistic out1 = ", Dispersion1, sep = ""))

    
       # Repeat the whole thing 1000 times.
    NSim <- 1000
    SimData <- inla.posterior.sample(n = NSim, result = model)
    N  <- nrow(dat)
    Ysim <- matrix(nrow = N, ncol = NSim)
    mu.i <- matrix(nrow = N, ncol = NSim)
    
    MyParams <- rownames(model$summary.fixed)
    MyID <- function(x){ which(rownames(SimData[[1]]$latent) == x) }
   
    ###HERE## 
    RowNum.Betas<-lapply(MyParams, function(x) grep(x, rownames(SimData[[1]]$latent), fixed=TRUE))
    RowNum.Betas<-as.numeric(RowNum.Betas)
    
    MyParams <- paste("kappa_k:", levels(dat$kappa_k), sep = "")
    
    RowNum.ai<-lapply(MyParams, function(x) grep(x, rownames(SimData[[1]]$latent), fixed=TRUE))
    RowNum.ai<-as.numeric(RowNum.ai)
    
    RowNum.ai <- unlist(RowNum.ai)
    RowNum.ai
    
    
    
    for (i in 1: NSim){
      Betas <- SimData[[i]]$latent[RowNum.Betas]
      ai    <- SimData[[i]]$latent[RowNum.ai]
      FixedPart   <- Xm %*% Betas
      aiPart      <- ai[as.numeric(dat$kappa_k)]
      mu.i[,i]    <- exp(FixedPart + aiPart)
      Ysim[,i]    <- rpois(n = nrow(Owls2), lambda = mu.i[,i])
    }
    table(Ysim[,1])
    table(Ysim[,2])
    table(Ysim[,3])
    
    
    # We could calculate the number of zeros in each of the 1,000
    # data sets.
    zeros <- vector(length = NSim)
    for(i in 1:NSim){
      zeros[i] <- sum(Ysim[,i] == 0)
    }
    
    table(zeros)
    
    # From the 1,000 simulated data sets, in 2 simulated
    # data sets we had 1141 zeros. In 2 simulated data sets
    # we had 1148 zeros,etc......
    # Your results will be different as mine.
    
    # Just type 
    Ysim[,1]
    Ysim[,2]
    Ysim[,3]
    #etc
    
    # Let's plot this as a table
    # Figure 20.24
    par(mfrow = c(2,1), mar = c(5,5,2,2), cex.lab = 1.5)
    
    plot(T1, ylim = c(-4,5), lwd = 2, cex.lab = 1.5,
         xlab = "Arrival time")
    abline(h = 0, lty = 2)
    points(x = Owls2$ArrivalTime,
           y = E1, 
           pch = 16,
           cex = 0.5,
           col = grey(0.1))
    
    
    
    plot(table(zeros), 
         #axes = FALSE,
         xlab = "How often do we have 0, 1, 2, 3, etc. number of zeros",
         ylab = "Number of zeros in simulated data sets",
         xlim = c(0, 170),
         main = "Simulation results")
    points(x = sum(Owls2$NCalls == 0), 
           y = 0, 
           pch = 16, 
           cex = 5, 
           col = 2)
    # The red dot is the number of zeros in the original data set.
    # The data simulated from the Poisson model
    # contains to many zeros.
    #########################################
    
    
    
    
    
      } #end min.data  
   }#end SpeciesLoop
} #end if SalishSea