#Code used to clean the BCCWS and prepare it for analysis 

#Created by Danielle Ethier
#December 2024


in.BCCWS <- read.csv("Data/BCCWS.csv") # reads in back-up copy of database 
  
  #     ObservationCount2 (Inland)
  #     ObservationCount3 (NearShore)
  #     ObservationCount4 (Offshore)
  #     ObservationCount5 (Unknown Habitat - might include flyovers? Protocol says not to count fly overs)
  
  in.BCCWS$ObservationCount<-as.numeric(in.BCCWS$ObservationCount3)  ##WILLL WANT TO KEEP JUST THE NEARSHORE DATA TO MAKE THIS COMPARABLE TO PSSS
  
  
  #Thayer's Gull missing SpeciesCode. Need to assign here species_id == 5190
  in.BCCWS$SpeciesCode<-as.character(in.BCCWS$SpeciesCode)
  in.BCCWS$SpeciesCode[in.BCCWS$species_id == 5190] <- "ICGU"  ## CHANGE "Iceland Gull (Thayer's)" TO "Ivory Gull"

  ##MEW GULL NEEDS CHANGE TO SHORT BILLED GULL IN THE DATA
  ##This has two species_id 5140 and 5142
  in.BCCWS$SpeciesCode[in.BCCWS$species_id == 5140] <- "SBIG"
  in.BCCWS$SpeciesCode[in.BCCWS$species_id == 5142] <- "SBIG"
 
  
  in.BCCWS$SpeciesCode<-as.factor(in.BCCWS$SpeciesCode)
  in.BCCWS <- subset(in.BCCWS, !is.na(SpeciesCode))
  
  # filter data by years 
  in.BCCWS <- subset(in.BCCWS, YearCollected >= Y1 & YearCollected <= Y2)
  # filter data by months October to April
  in.BCCWS <- subset(in.BCCWS, MonthCollected %in% c(10:12, 1:4))
    #filter bad dates
  in.BCCWS <- in.BCCWS %>% filter(!(SurveyAreaIdentifier == "LMSQ-3" & SpeciesCode == "BAEA" & YearCollected == "1999" & MonthCollected == "12" & DayCollected == "12"))
  
  # create date and day of year columns
  in.BCCWS$date <- as.Date(paste(in.BCCWS$YearCollected, in.BCCWS$MonthCollected, 
                                 in.BCCWS$DayCollected, sep = "-"))
  in.BCCWS$doy <- as.numeric(format(in.BCCWS$date, "%j"))
  
  # parse out form ID number and delete bad forms
  in.BCCWS$form.id <- gsub("BCCWS-", "", in.BCCWS$SamplingEventIdentifier)
  in.BCCWS <- subset(in.BCCWS, form.id != 3794 & form.id != 5469 &
                      form.id != 5063 & form.id != 6945)
  
  # Remove inland sites
    in.BCCWS <- in.BCCWS %>%
    filter(!(RouteIdentifier %in% c("GIHO-20", "GIPE-12", "LMSQ-10", "LMSQ-14", "LMSQ-15",
                                    "LMSQ-16", "LMSQ-2", "LMSQ-20", "LMSQ-3", "LMSQ-5", 
                                    "LMSQ-6", "LMSQ-7", "LMSQ-8", "LMSQ-9", "VICV-4")))
  
  # Correct some sites with multiple surveys sharing a single identifier. These
  # identifiers differ only in what species counts they have, and only vary slightly.
  # We select the one with the most complete list of species bearing the most
  # generous levels of uncertainty in species ids.
    
  in.BCCWS <- in.BCCWS[!(in.BCCWS$SamplingEventIdentifier %in% c("BCCWS-130875-1", "BCCWS-130872-1", "BCCWS-131007-1")),]
  in.BCCWS <- in.BCCWS[!(in.BCCWS$SamplingEventIdentifier %in% c("BCCWS-279326-2", "BCCWS-279328-2", "BCCWS-279327-2")),]
    
    # seems to be a few duplicate records in data file; this keeps only one
  in.BCCWS <- unique(in.BCCWS)
  
  # keep only one survey per month by selecting the first survey if there are duplicates
  in.BCCWS <- in.BCCWS %>% group_by(SurveyAreaIdentifier, YearCollected, MonthCollected) %>% slice_min(doy) %>% ungroup()
  
  # create an events matrix for future zero filling
  event.BCCWS <- in.BCCWS %>% select(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, DecimalLatitude, DecimalLongitude) %>% unique()

  # create a new column called wyear, which groups surveys by winter year
  # for example, January-April 2005 would be wyear 2004
  in.BCCWS$wyear <- ifelse(in.BCCWS$MonthCollected %in% c(1:4), in.BCCWS$YearCollected-1, 
                           in.BCCWS$YearCollected)
  
  # Because there are errors in the Duration in Hours column, I will adjust the TimeObservationsEnded and TimeObservationStarted column. 
  # Specifically, any value that is < 6 AM I will add 12 to bring it into 24 hour time. 
  in.BCCWS$TimeObservationsEnded <- as.numeric(in.BCCWS$TimeObservationsEnded)
  in.BCCWS$TimeObservationsStarted <- as.numeric(in.BCCWS$TimeObservationsStarted)
  in.BCCWS$TimeObservationsEnded <- ifelse(in.BCCWS$TimeObservationsEnded < 7, in.BCCWS$TimeObservationsEnded + 12, in.BCCWS$TimeObservationsEnded)
  in.BCCWS$TimeObservationsStarted <- ifelse(in.BCCWS$TimeObservationsStarted < 7, in.BCCWS$TimeObservationsStarted + 12, in.BCCWS$TimeObservationsStarted)
  
  # Remove TimeObservationEnds that is >24:59 hours
  in.BCCWS <- in.BCCWS %>% filter(TimeObservationsEnded < 24.59)
  # If the TimeObservationStart is >= 18:00 and the TimeObservationEnds <= 10.68, add 12 to the TimeObservationEnds
  in.BCCWS$TimeObservationsEnded <- ifelse(in.BCCWS$TimeObservationsStarted >= 18 & in.BCCWS$TimeObservationsEnded <= 10.68, in.BCCWS$TimeObservationsEnded + 12, in.BCCWS$TimeObservationsEnded)
  
  # recalculate duration in hours using the start and end times
  in.BCCWS$DurationInHours2 <- in.BCCWS$TimeObservationsEnded - in.BCCWS$TimeObservationsStarted
  
  # if DurationinHours is negative, we want to swap the survey start time and survey end time as they seem to be reversed. 
  in.BCCWS<- in.BCCWS %>% mutate(TimeObservationsStarted2= ifelse(in.BCCWS$DurationInHours2 < 0, TimeObservationsEnded, TimeObservationsStarted), TimeObservationsEnded2= ifelse(in.BCCWS$DurationInHours2 < 0, TimeObservationsStarted, TimeObservationsEnded))
   
  # recalculate duration in hours using the start and end times
  in.BCCWS$DurationInHours <- in.BCCWS$TimeObservationsEnded2 - in.BCCWS$TimeObservationsStarted2hours
  
  
  # retain columns that are needed for the analysis
  in.BCCWS <- in.BCCWS %>% select(SpeciesCode, ObservationCount, SurveyAreaIdentifier, wyear, YearCollected, MonthCollected, DayCollected, DurationInHours)

    # write index.data to file
   write.csv(in.BCCWS, "Data/BCCWS.clean.csv")
   write.csv(events.BCCWS, "Data/BCCWS.events.csv")