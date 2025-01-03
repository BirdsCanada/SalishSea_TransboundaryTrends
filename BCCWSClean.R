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
  
  # filter data by months October to April
  in.BCCWS <- subset(in.BCCWS, MonthCollected %in% c(10:12, 1:4))
  # filter bad dates
  in.BCCWS <- in.BCCWS %>% filter(!(SurveyAreaIdentifier == "LMSQ-3" & SpeciesCode == "BAEA" & YearCollected == "1999" & MonthCollected == "12" & DayCollected == "12"))
  # parse out form ID number and delete bad forms
  in.BCCWS$form.id <- gsub("BCCWS-", "", in.BCCWS$SamplingEventIdentifier)
  in.BCCWS <- subset(in.BCCWS, form.id != 3794 & form.id != 5469 &
                       form.id != 5063 & form.id != 6945)
    
  # seems to be a few duplicate records in data file; this keeps only one
  in.BCCWS <- distinct(in.BCCWS)
  
  #create day of year column 
  in.BCCWS$doy <- as.numeric(format(as.Date(paste(in.BCCWS$YearCollected, in.BCCWS$MonthCollected, in.BCCWS$DayCollected, sep="-")), "%j"))
  
  
  # keep only one survey per month by selecting the first survey if there are duplicates
  in.BCCWS <- in.BCCWS %>% group_by(SurveyAreaIdentifier, YearCollected, MonthCollected) %>% slice_min(doy) %>% ungroup()
  
  # create a new column called wyear, which groups surveys by winter year
  # for example, January-April 2005 would be wyear 2004
  in.BCCWS$wyear <- ifelse(in.BCCWS$MonthCollected %in% c(1:4), in.BCCWS$YearCollected-1, 
                           in.BCCWS$YearCollected)
  
  # filter data by years 
  in.BCCWS <- subset(in.BCCWS, wyear >= Y1 & wyear <= Y2)
  
  
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
  # First make sure these are time fields for that the calculation happens correclty
  in.BCCWS$DurationInHours2 <- calculate_duration(in.BCCWS$TimeObservationsEnded, in.BCCWS$TimeObservationsStarted)

  # if DurationinHours is negative, we want to swap the survey start time and survey end time as they seem to be reversed.
  in.BCCWS<- in.BCCWS %>% mutate(TimeObservationsStarted2= ifelse(in.BCCWS$DurationInHours2 < 0, TimeObservationsEnded, TimeObservationsStarted), TimeObservationsEnded2= ifelse(in.BCCWS$DurationInHours2 < 0, TimeObservationsStarted, TimeObservationsEnded))

  # recalculate duration in hours using the start and end times
  in.BCCWS$DurationInHours <- calculate_duration(in.BCCWS$TimeObservationsEnded2, in.BCCWS$TimeObservationsStarted2)
  
  # remove rows with missing DurationInHours
  in.BCCWS <- in.BCCWS %>% filter(!is.na(DurationInHours))
  # remove missing SurveyAreaIdentifier
  in.BCCWS <- in.BCCWS %>% filter(!is.na(SurveyAreaIdentifier))
  # remove missing DecimalLatitude and DecimalLongitude
  in.BCCWS <- in.BCCWS %>% filter(!is.na(DecimalLatitude) & !is.na(DecimalLongitude))
  #Filter events to 45.06N to 50.64N latitude and 125.07W to 115.15W longitude
  in.BCCWS <- in.BCCWS %>% filter(DecimalLatitude >= 45.06 & DecimalLatitude <= 50.64 & DecimalLongitude >= -125.07 & DecimalLongitude <= -115.15)
  #remove the sampling point on the outside coastal edge of Vaconcover Isannd which in <-124 DecimialLongitude and <48.5 DecimialLatitude
  in.BCCWS <- in.BCCWS %>% filter(!(DecimalLatitude < 48.5 & DecimalLongitude < -124))
  
  #Filter Duration in hours greater than 0.3 and less than 10
  in.BCCWS<-in.BCCWS[in.BCCWS$DurationInHours > 0.3 & in.BCCWS$DurationInHours < 10,]
  
  # removed NA ObservationCounts
  in.BCCWS <- in.BCCWS %>% filter(!is.na(ObservationCount))
  
  # create an events matrix for future zero filling
  event.BCCWS <- in.BCCWS %>% dplyr::select(ProjectCode, SurveyAreaIdentifier, wyear, YearCollected, MonthCollected, DayCollected, DecimalLatitude, DecimalLongitude, DurationInHours) %>% distinct()
  # if there are multiple events in a single day (now caused by Duration in Hours), take the minimum
  event.BCCWS <- event.BCCWS %>% group_by(ProjectCode, SurveyAreaIdentifier, wyear, YearCollected, MonthCollected, DayCollected) %>% slice_min(DurationInHours) %>% ungroup()
  # ensure that each SurveyAreaIdentifier has a single decimal latitude and longitude. If multiple, take the first
  event.BCCWS <- event.BCCWS %>% group_by(ProjectCode, SurveyAreaIdentifier) %>% slice_min(DecimalLatitude) %>% ungroup()
  
  # retain columns that are needed for the analysis
  in.BCCWS <- in.BCCWS %>% dplyr::select(ProjectCode, SurveyAreaIdentifier, SpeciesCode, ObservationCount,  wyear, YearCollected, MonthCollected, DayCollected)

  # write index.data to file
   write.csv(in.BCCWS, "Data/BCCWS.clean.csv")
   write.csv(event.BCCWS, "Data/BCCWS.events.csv")
   