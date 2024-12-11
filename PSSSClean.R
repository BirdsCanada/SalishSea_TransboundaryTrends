#Code used to clean the PSSS and prepare it for analysis 

#Created by Danielle Ethier
#December 2024


in.PSSS <- read.csv("Data/PSSS.csv") # reads in back-up copy of database 

in.PSSS$SpeciesCode<-as.factor(in.PSSS$SpeciesCode)
in.PSSS <- subset(in.PSSS, !is.na(SpeciesCode))

# filter data by years 
in.PSSS <- subset(in.PSSS, YearCollected >= Y1 & YearCollected <= Y2)
# filter data by months October to April
in.PSSS <- subset(in.PSSS, MonthCollected %in% c(10:12, 1:4))

# create date and day of year columns
in.PSSS$date <- as.Date(paste(in.PSSS$YearCollected, in.PSSS$MonthCollected, 
                               in.PSSS$DayCollected, sep = "-"))
in.PSSS$doy <- as.numeric(format(in.PSSS$date, "%j"))

# if duplicate records in data file; this keeps only one
in.PSSS <- distinct(in.PSSS)

# keep only one survey per month by selecting the first survey if there are duplicates
in.PSSS <- in.PSSS %>% group_by(SurveyAreaIdentifier, YearCollected, MonthCollected) %>% slice_min(doy) %>% ungroup()

# create a new column called wyear, which groups surveys by winter year
# for example, January-April 2005 would be wyear 2004
in.PSSS$wyear <- ifelse(in.PSSS$MonthCollected %in% c(1:4), in.PSSS$YearCollected-1, 
                         in.PSSS$YearCollected)

# Because there are errors in the Duration in Hours column, I will adjust the TimeObservationsEnded and TimeObservationStarted column. 
# Specifically, any value that is < 6 AM I will add 12 to bring it into 24 hour time. 

# Remove missing TimeObservationsStarted and TimeObservationsEnded
in.PSSS <- in.PSSS %>% filter(!is.na(TimeObservationsStarted) & !is.na(TimeObservationsEnded))
# Remove times with less than 3 nchar
in.PSSS <- in.PSSS %>% filter(nchar(TimeObservationsStarted) > 2 & nchar(TimeObservationsEnded) > 2)
# REmove times with greater tahn 4 nchar
in.PSSS <- in.PSSS %>% filter(nchar(TimeObservationsStarted) < 5 & nchar(TimeObservationsEnded) < 5)

#convert time into decimal hours
in.PSSS <- in.PSSS %>% mutate(DecimalTimeObservationsStarted = (TimeObservationsStarted %/% 100)+((TimeObservationsStarted %% 100)/60), DecimalTimeObservationsEnded = (TimeObservationsEnded %/% 100)+((TimeObservationsEnded %% 100)/60))

# Calculate the duration
  in.PSSS$DurationInHours2 <- calculate_duration(in.PSSS$DecimalTimeObservationsStarted, in.PSSS$DecimalTimeObservationsEnded)                      
            
# if DurationinHours is negative, we want to swap the survey start time and survey end time as they seem to be reversed.
  in.PSSS<- in.PSSS %>% mutate(DecimalTimeObservationsStarted2= ifelse(in.PSSS$DurationInHours2 < 0, DecimalTimeObservationsEnded, DecimalTimeObservationsStarted), DecimalTimeObservationsEnded2= ifelse(in.PSSS$DurationInHours2 < 0, DecimalTimeObservationsStarted, DecimalTimeObservationsEnded))

# recalculate duration in hours using the start and end times
  in.PSSS$DurationInHours <- calculate_duration(in.PSSS$DecimalTimeObservationsStarted2, in.PSSS$DecimalTimeObservationsEnded2)                      
  
#remove remaining negative DurationInHours
  in.PSSS <- in.PSSS %>% filter(DurationInHours >= 0)  
#Filter Duration in hours greater than 0.3 and less than 10
  in.PSSS<-in.PSSS[in.PSSS$DurationInHours > 0.3 & in.PSSS$DurationInHours < 10,]  
  
# create a new column called wyear, which groups surveys by winter year
# for example, January-April 2005 would be wyear 2004
  in.PSSS$wyear <- ifelse(in.PSSS$MonthCollected %in% c(1:4), in.PSSS$YearCollected-1, 
                           in.PSSS$YearCollected)
  
  # filter data by years 
  in.PSSS <- subset(in.PSSS, wyear >= Y1 & wyear <= Y2)

  #Remove ObservationCounts that are NA
  in.PSSS <- in.PSSS %>% filter(!is.na(ObservationCount))
  #Remove missing SurveyAreaIdentifiers
  in.PSSS <- in.PSSS %>% filter(!is.na(SurveyAreaIdentifier))
  #Remove missing DecimalLatitude or DecimalLongitude
  in.PSSS <- in.PSSS %>% filter(!is.na(DecimalLatitude) & !is.na(DecimalLongitude))
  #Filter events to 45.06N to 50.64N latitude and 125.07W to 115.15W longitude
  in.PSSS <- in.PSSS %>% filter(DecimalLatitude >= 45.06 & DecimalLatitude <= 50.64 & DecimalLongitude >= -125.07 & DecimalLongitude <= -115.15)
                           

  # create an events matrix for future zero filling
  event.PSSS <- in.PSSS %>% dplyr::select(ProjectCode, SurveyAreaIdentifier, wyear, YearCollected, MonthCollected, DayCollected, DecimalLatitude, DecimalLongitude) %>% distinct()
  
  # retain columns that are needed for the analysis
  in.PSSS <- in.PSSS %>% dplyr::select(ProjectCode, SurveyAreaIdentifier, SpeciesCode, ObservationCount,  wyear, YearCollected, MonthCollected, DayCollected, DurationInHours)

  # write index.data to file
  write.csv(in.PSSS, "Data/PSSS.clean.csv")
  write.csv(event.PSSS, "Data/PSSS.events.csv")
