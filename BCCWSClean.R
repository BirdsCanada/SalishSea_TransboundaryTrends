#Code used to clean the BCCWS 


#     ObservationCount2 (Inland)
#     ObservationCount3 (NearShore)
#     ObservationCount4 (Offshore)
#     ObservationCount5 (Unknown Habitat - might include flyovers? Protocol says not to count fly overs)

BCCWS$ObservationCount<-as.numeric(BCCWS$ObservationCount3)  ##WILLL WANT TO KEEP JUST THE NEARSHORE DATA TO MAKE THIS COMPARABLE TO PSSS

#will want to include DURATION in hours because this is higly variable and will want to use as a offset or covariate .

bccws.manip <- function(Y1, Y2, write.out) {
  
  ################
  # READ IN DATA #
  ################ 
  
 
  in.data <- read.csv("Data/BCCWS.csv") # reads in back-up copy of database 
  
  #Thayer's Gull missing SpeciesCode. Need to assign here species_id == 5190
  in.data$SpeciesCode<-as.character(in.data$SpeciesCode)
  in.data$SpeciesCode[in.data$species_id == 5190] <- "THGU"  ## CHANGE "Iceland Gull (Thayer's)" TO "Ivory Gull"
  
  ##MEW GULL NEEDS CHANGE TO SHORT BILLED GULL IN THE DATA
  ##This has two species_id 5140 and 5142
  
  in.data$SpeciesCode<-as.factor(in.data$SpeciesCode)
  
  in.data <- subset(in.data, !is.na(SpeciesCode))
  
  # filter data by years to analyze
  in.data <- subset(in.data, YearCollected >= Y1 & YearCollected <= Y2)
  
  #filter bad dates
  in.data <- in.data %>% filter(!(SurveyAreaIdentifier == "LMSQ-3" & SpeciesCode == "BAEA" & YearCollected == "1999" & MonthCollected == "12" & DayCollected == "12"))
  
  # create date and doy variables (ignoring time collected)
  in.data$date <- as.Date(ISOdate(in.data$YearCollected, in.data$MonthCollected,
                                  in.data$DayCollected))
  in.data$doy <- as.POSIXlt(in.data$date)$yday  
  
  # parse out form ID number and delete bad forms
  in.data$form.id <- gsub("BCCWS-", "", in.data$SamplingEventIdentifier)
  in.data <- subset(in.data, form.id != 3794 & form.id != 5469 &
                      form.id != 5063 & form.id != 6945)
  
  # seems to be a few duplicate records in data file; this keeps only one
  
  in.data <- unique(in.data)
  
  ##########################################################
  # LIMIT TO SURVEY CLOSEST TO SECOND SUNDAY OF EACH MONTH #
  ##########################################################
  # For months in which more than one survey was conducted at a route, 
  # need to choose the date that falls closest to the second sunday of the month
  # First trim down the dataframe to the list of variables required,
  # then create date variable that corresponds to the second Sunday of each month, 
  # each year.  The survey conducted closest to this date is used in analysis, the rest
  # are dropped.  This keeps comparisons among sites as similar as possible.
  
  # create vector of months (1-12), repeated for all years in dataset
  # this will give the doy of the second sunday of each month/year combination
  # merge this back with main dataframe, and subset based on minimum distance from 
  # this date each month
  
  sunday <- NULL
  sunday$year <- rep(seq(min(unique(in.data$YearCollected)), 
                         max(unique(in.data$YearCollected)), by = 1), each = 12)
  #ERROR
  #manually entered the following code to bypass the error, which is caused by an incomplete data set
  #sunday$month <- rep(seq(1,12, by = 1), each = 20)
  #need to reverse once I egt full dataset
  sunday$month <- rep(seq(1,12, by = 1), times = length(unique(in.data$YearCollected)))
  sunday <- as.data.frame(sunday)
  sunday$first.doy <- as.POSIXlt(as.Date(ISOdate(sunday$year, sunday$month,1)))$yday
  sunday$first.wkday <- weekdays(ISOdate(sunday$year, sunday$month, 1))
  sunday$day2[sunday$first.wkday == "Sunday"] <- sunday$first.doy[sunday$first.wkday == 
                                                                    "Sunday"] + 7  
  sunday$day2[sunday$first.wkday == "Monday"] <- sunday$first.doy[sunday$first.wkday == 
                                                                    "Monday"] + 13
  sunday$day2[sunday$first.wkday == "Tuesday"] <- sunday$first.doy[sunday$first.wkday == 
                                                                     "Tuesday"] + 12
  sunday$day2[sunday$first.wkday == "Wednesday"] <- sunday$first.doy[sunday$first.wkday == 
                                                                       "Wednesday"] + 11
  sunday$day2[sunday$first.wkday == "Thursday"] <- sunday$first.doy[sunday$first.wkday == 
                                                                      "Thursday"] + 10
  sunday$day2[sunday$first.wkday == "Friday"] <- sunday$first.doy[sunday$first.wkday == 
                                                                    "Friday"] + 9
  sunday$day2[sunday$first.wkday == "Saturday"] <- sunday$first.doy[sunday$first.wkday == 
                                                                      "Saturday"] + 8
  sunday <- subset(sunday, select = c("year","month","day2"))
  
  # need to pick the survey visit that falls closest to the second sunday of each month 
  # at each site.  Make this in seperate data file, showing unique dates surveyed at each site
  
  survey.dates <- unique(subset(in.data, select = c("SurveyAreaIdentifier", "YearCollected", 
                                                    "MonthCollected", "date","doy")))
  survey.dates <- merge(survey.dates, sunday, by.x = c("YearCollected", "MonthCollected"),
                        by.y = c("year","month"), all.x = T)
  survey.dates$diff <- survey.dates$doy - survey.dates$day2
  survey.dates$diff.abs <- abs(survey.dates$diff)
  
  # first takes the absolute difference (above), to figure out the closest date(s)
  
  survey.dates.2 <- with(survey.dates, aggregate(diff.abs, list(YearCollected=YearCollected, 
                                                                MonthCollected=MonthCollected, SurveyAreaIdentifier=SurveyAreaIdentifier), min))
  names(survey.dates.2) <- c("YearCollected", "MonthCollected", "SurveyAreaIdentifier", "min.diff.abs")
  survey.dates.2 <- merge(survey.dates.2, survey.dates, all.x=T, by.x=c("YearCollected",
                                                                        "MonthCollected", "SurveyAreaIdentifier", "min.diff.abs"), 
                          by.y=c("YearCollected", "MonthCollected", "SurveyAreaIdentifier", "diff.abs"))
  
  # if there are two surveys conducted within an equal distance (in days) from the suggested second 
  # sunday, the following code keeps the survey date that occured PRIOR to the second sunday, and drops 
  # the survey conducted after the second sunday (i.e., picks the first of two). This is NOT based on the
  # absolute difference in this case.
  
  survey.dates.3 <- with(survey.dates.2, aggregate(diff, list(YearCollected=YearCollected, 
                                                              MonthCollected=MonthCollected, SurveyAreaIdentifier=SurveyAreaIdentifier), min))
  names(survey.dates.3) <- c("YearCollected", "MonthCollected", "SurveyAreaIdentifier", "min.diff")
  
  survey.dates.3 <- merge(survey.dates.3, survey.dates.2, all.x=T, by.x=c("YearCollected",
                                                                          "MonthCollected", "SurveyAreaIdentifier", "min.diff"), 
                          by.y=c("YearCollected", "MonthCollected", "SurveyAreaIdentifier", "diff"))
  
  survey.dates.3 <- subset(survey.dates.3, select = c("YearCollected","MonthCollected","SurveyAreaIdentifier",
                                                      "date"))
  
  # remerge selected survey dates to keep (survey.dates.3) with data,
  # omitting observations in data that do not correspond to survey.dates.3
  
  in.data <- merge(in.data, survey.dates.3, by.x=c("YearCollected", "MonthCollected", 
                                                   "SurveyAreaIdentifier", "date"), by.y = c("YearCollected", "MonthCollected", "SurveyAreaIdentifier",
                                                                                             "date"))
  
  
  
  ###############################
  # CREATE A ZERO-FILLED MATRIX #
  ###############################
  # Now use the reshape package to add zeroes for the date/time combinations
  # in the dataset when a species was not observed (but at least one observation of 
  # another species was made). Note that we assume that if no species were observed, 
  # but observations were made, that there is a zero count for 'something' in the database. 
  # This appears to normally be true. 
  
  # We do not remove NAs in 'recast' because we want to include those 
  # dates/times in which no birds were observed. The following assumes that 
  # these dates/times are included, but with a bird name of NA. 
  # This may not always be true, but it should be 'mostly' correct.
  
  # The following creates a 'wide' data frame, with a variable for each species. 
  # We use fun.aggregate = "max" to take max count where there are multiple observations 
  # the same species, in the same date/time.  This, however, should not be an issue,
  # given we pick a date for each month above.  Indeed, there was only one observation
  # per month when I checked this. Can ignore Day collected, since we already
  # chose one survey day per month
  
  
  #ERROR
  # fun.aggregate will not work on "ObervationCount" if not an integer  
  in.data$ObservationCount<-as.integer(in.data$ObservationCount)
  
  test <- recast(in.data, SurveyAreaIdentifier + YearCollected + MonthCollected ~ SpeciesCode, 
                 fill=0, fun.aggregate = "max", 
                 id.var = c("SurveyAreaIdentifier","YearCollected","MonthCollected", "SpeciesCode"), 
                 measure.var = "ObservationCount")
  
  
  # and then we need to take that 'wide' data frame, and re-organize in a 'long' format
  # e.g. a separate line for each observation of each species in each date/time (including zeros). 
  
  test <- melt(test, id.var = c("SurveyAreaIdentifier","YearCollected","MonthCollected"))
  
  test <- data.frame(test, row.names = c(1:nrow(test)))
  names(test)[names(test) == "value"] <- "ObservationCount" #which now includes zero counts
  
  ################################################## 
  # create new year and month variable, for winter #
  ##################################################
  #First winter months is September and it will be assigned 1
  #Last winter month is Arpil and it will be assigned 8  
  
  #ERROR
  #will not work if MonthCollected is a character since a numberic opperator is being applied
  test$MonthCollected<-as.integer(test$MonthCollected)  
  
  test$wmonth <- ifelse(test$MonthCollected %in% c(9:12), test$MonthCollected - 8, 
                        ifelse(test$MonthCollected %in% c(1:4), test$MonthCollected + 4, NA) )
  
  # subset only winter months
  # remove all NA
  test <- subset(test, !is.na(wmonth))
  
  #ERROR
  #will not work if YearCollected is a character since a numberic opperator is being applied
  test$YearCollected<-as.integer(test$YearCollected)
  
  # naming winter year 'wyear', which takes the year of fall months 
  # i.e., January 2005 would be wmonth 5, wyear 2004.
  test$wyear <- ifelse(test$wmonth %in% c(1:4), test$YearCollected, 
                       test$YearCollected - 1) 
  
  
  
  ####################################################
  # limit data to migration windows for each species #
  ####################################################
  
  # these are 'permanent' windows based on expert opinion (Pete Davidson et al?)
  # and are not updated annually.
  
  survey.windows <- read.csv("./sp.survey.windows.csv") # this was updated in April 2020. BAEA was changed and KILL added. 
  # replace with new sp.names files which includes species code  
  species.codes <- read.csv("./sp.names.csv")
  names(species.codes)[names(species.codes) == "english_name"] <- "SpeciesName"
  names(species.codes)[names(species.codes) == "species_code"] <- "SpeciesCode"
  species.codes <- species.codes %>% select(SpeciesCode, SpeciesName, sort_order)
  
  survey.windows <- merge(survey.windows, species.codes, all.x = T,
                          by = "SpeciesName")
  
  #species with ' in the name do not copy properly. Need to manual fix. 
  survey.windows[9, 13]<-"BOGU"
  survey.windows[9, 14]<-3497
  survey.windows[10, 13]<-"BRAC"
  survey.windows[10, 14]<-4083
  survey.windows[61, 13]<-"THGU"
  survey.windows[61, 14]<-3570
  
  all.species <- as.character(unique(survey.windows$SpeciesCode))
  
  # restrict data to species with seasonal windows (all.species)
  all.species.df <- as.data.frame(all.species)
  names(all.species.df) <- "SpeciesCode"
  all.species.df <- na.omit(all.species.df)
  test <- merge(test, all.species.df, by = "SpeciesCode")
  
  # for each species with seasonal windows, drop data outside window
  for (i in 1:length(all.species)) {
    test$ObservationCount[test$SpeciesCode == all.species[i] &
                            (test$wmonth < survey.windows$StartWMonth[survey.windows$SpeciesCode ==
                                                                        all.species[i]]|test$wmonth > survey.windows$EndWMonth[
                                                                          survey.windows$SpeciesCode == all.species[i]])] <- NA }
  
  # drop all NA Observation Counts from step above
  test <- subset(test, !is.na(ObservationCount))
  
  #####################################################
  # Limit to species observed at least once per route #
  #####################################################
  
  # summarize survey site, to determine which species have
  # been observed at least once - those with sum <= 1 across months/years
  # will be dropped from analysis (implies never observed on a route)
  
  site.summ <- melt(test, id.var = c("SurveyAreaIdentifier", "SpeciesCode"),
                    measure.var = "ObservationCount")
  site.summ <- cast(site.summ, SurveyAreaIdentifier + SpeciesCode ~ variable,
                    sum)
  site.sp.list <- unique(subset(site.summ, select = c("SurveyAreaIdentifier",
                                                      "SpeciesCode"), ObservationCount > 1))
  
  # limit raw data to these species, i.e., those that were observed
  # at least once on a route during the species' survey window
  #(this may need to be modified to be more restrictive)
  
  test <- merge(test, site.sp.list, by = c("SurveyAreaIdentifier","SpeciesCode"))
  
  
  
  #############################################################################
  # Limit data to routes run for entire survey window of a particular species #
  #############################################################################
  
  # for each species and year, count number of months a survey was conducted for each route
  # if this is below the total number of months for a species survey window,
  # drop that route/year from analysis
  
  survey.summ <- unique(subset(test, select = c("SurveyAreaIdentifier",
                                                "SpeciesCode","wmonth","wyear")))
  survey.summ <- melt(survey.summ, id.var = c("SurveyAreaIdentifier","SpeciesCode", "wyear"))
  survey.summ <- cast(survey.summ, SurveyAreaIdentifier + SpeciesCode + wyear ~ variable, 
                      length)  # for each species, this gives total number of months each route was surveyed each year
  names(survey.summ) <- c("SurveyAreaIdentifier","SpeciesCode","wyear","nmonths")
  
  for (i in 1:length(all.species)) {
    survey.summ$nmonths[survey.summ$SpeciesCode == all.species[i] &
                          (survey.summ$nmonths < survey.windows$TotMonths[survey.windows$SpeciesCode ==
                                                                            all.species[i]])] <- NA }
  
  # give list of species by site, year, in which all survey window months were surveyed
  survey.summ <- subset(survey.summ, !is.na(nmonths)) 
  
  # limit data to these species/site/year combinations
  test.2 <- merge(test, subset(survey.summ, select = c(1:3)), by = c("SurveyAreaIdentifier",
                                                                     "SpeciesCode","wyear"))
  
  
  ################################################################
  # Limit data to routes run at least 1 winters for each species #
  ################################################################
  
  # count total number of winters each route was run for each species
  # this works because we first dropped years for each species where
  # all survey window months were not covered. Originally this code was set to 
  # years>1, however, chose to use this as a scaling factor in the analysis rather 
  # than removing routes only run once  
  
  winter.summ <- unique(subset(test.2, select = c("SurveyAreaIdentifier","SpeciesCode","wyear")))
  winter.summ <- aggregate(x = winter.summ$wyear, list(SurveyAreaIdentifier = 
                                                         winter.summ$SurveyAreaIdentifier, SpeciesCode = winter.summ$SpeciesCode), FUN = length)
  names(winter.summ) <- c("SurveyAreaIdentifier","SpeciesCode","nyears")
  
  #nyears can be used as the scaling factor in the analysis
  test.2 <- merge(test.2, subset(winter.summ, nyears > 0, select = c(
    "SurveyAreaIdentifier","SpeciesCode", "nyears")), 
    by = c("SurveyAreaIdentifier","SpeciesCode"))
  
  ##############################################################################
  # Drop species not detected on more than one route within region of interest #
  ##############################################################################
  
  # count total number routes per species
  # first need to limit the data to the region of interest for analysis, OR
  # summarize number routes per region.  
  
  # Limit data to sites within the Georgian Basin
  # to keep all sites, include ",all.x = T" in merge statement
  
  # parse out the region, area codes from the SurveyAreaIdentifier field
  
  test.2$area.reg <- substring(test.2$SurveyAreaIdentifier, 1,4)
  
  #NOTE: There are two different CVS files: Georgian Basin and Salish Sea
  
  #  the following drops sites not classified as georgian basin
  #  g.basin.sites <- read.csv("./bccws.georgian.basin.sites.csv")
  #  test.2 <- merge(test.2, g.basin.sites, by = "area.reg")
  
  # the following drops sites not classified as the Salish Sea. Updated April 2020
  SS.sites <- read.csv("./bccws.salishsea.csv")
  test.2 <- merge(test.2, SS.sites, by = "area.reg")
  
  #for regions outside the SS  
  test.2 <- merge(test.2, SS.sites, by = "area.reg", all.x=TRUE)
  test.2$region.name<-fct_explicit_na(test.2$region.name, na_level = "OUTER")
  test.2<-subset(test.2, region.name=="OUTER")
  
  # Did not include this since we are adding the scaling factor. 
  # drops species detected on less that 2 routes
  route.summ <- unique(subset(test.2, select = c("SurveyAreaIdentifier","SpeciesCode", "wyear")))
  route.summ <- aggregate(x <- route.summ$SurveyAreaIdentifier, list(SpeciesCode = 
                                                                       route.summ$SpeciesCode, wyear = route.summ$wyear), FUN = length)
  test.2 <- merge(test.2, route.summ, by = c("SpeciesCode", "wyear"))
  
  names(test.2)[names(test.2) == "x"] <- "nroute"
  
  # now need to limit by species again, because the above data filters will have dropped
  # particular species from particular stations after the zero-filling occurred.
  # for the purpose of trend analysis, don't want to include species with a mean of 0
  # at a route across years.
  
  sp.site.list <- unique(subset(test.2, ObservationCount > 0, select = c("SurveyAreaIdentifier",
                                                                         "SpeciesCode")))
  
  # option to output test file, which will be used for graphing purposes
  
  test.2 <- merge(test.2, sp.site.list, by = c("SurveyAreaIdentifier", "SpeciesCode"))
  names(test.2) <- c("SurveyAreaIdentifier", "SpeciesCode", "wyear","YearCollected", "MonthCollected", "ObservationCount", "wmonth", "nyear", "region.name", "nroute") 
  
  #if are.reg
  names(test.2) <- c("SurveyAreaIdentifier", "SpeciesCode", "wyear","area.reg","YearCollected", "MonthCollected", "ObservationCount", "wmonth", "nyear", "region.name", "nroute") 
  
  # write.csv(test.2, file = "bccws.data.filtered.csv", row.names = F)
  
  # now get mean across months for each year at a site
  
  index.data <- with(test.2, aggregate(ObservationCount, list(SurveyAreaIdentifier = 
                                                                SurveyAreaIdentifier, SpeciesCode = SpeciesCode, wyear = wyear, nyear=nyear, nroute=nroute),
                                       FUN = mean))
  
  names(index.data) <- c("SurveyAreaIdentifier", "SpeciesCode", "wyear", "nyear", "nroute", "mean.ObservationCount")
  
  index.data$mean.ObservationCount <- round(index.data$mean.ObservationCount, digits = 0)
  
  names(index.data) <- c("SurveyAreaIdentifier", "SpeciesCode", "wyear", "nyear", "nroute","mncount")
  
  # for current purposes, drop data from winter year = Y2, because this will
  # only include half the winter (Fall months... jan & rest of winter months from 2009 
  # not in dataset).  I.e., if analyzing up to 2009, only need up to wyear 2008.
  
  index.data <- subset(index.data, wyear < Y2)
  
  # and return index.data
  return(index.data)  # returns index.data for use in subsequent functions
  # write index.data to file
  # write.csv(index.data, "index.data.csv")
  
} # END of bccws.manip function
