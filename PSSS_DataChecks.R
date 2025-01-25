#sample code to access PSSS data from your working Data directory
PSSS <- read_excel("Data/PSSS_2008-2025.xlsx") #copy contains wyear 2023

sp.code<-meta_species_codes()
sp.tax<-meta_species_taxonomy()
sp.code<-sp.code %>% filter(authority=="BSCDATA") %>% dplyr::select(-authority, -species_id2, -rank) %>% distinct()
sp.tax<-sp.tax %>% dplyr::select(species_id, scientific_name, english_name) %>% distinct()

sp<-left_join(sp.code, sp.tax, by="species_id")
sp<-sp %>% distinct(english_name, .keep_all = TRUE)

PSSS$lat<-sub(" W.*", "", PSSS$position)  
PSSS$long<-sub(".*W", "", PSSS$position)

PSSS$lat = gsub('N', '', PSSS$lat)
PSSS$long = gsub('W', '', PSSS$long)

PSSS$DecimalLatitude = measurements::conv_unit(PSSS$lat, from = 'deg_dec_min', to = 'dec_deg')
PSSS$DecimalLatitude<-as.numeric((PSSS$DecimalLatitude))
PSSS$DecimalLongitude = measurements::conv_unit(PSSS$long, from = 'deg_dec_min', to = 'dec_deg')
PSSS$DecimalLongitude<-as.numeric(PSSS$DecimalLongitude)
PSSS$DecimalLongitude=PSSS$DecimalLongitude*(-1)

##three sites have lat and long in a different format, which means they manually need fixed. 
#I have flagged this to Toby to fix in the underlying data. 

#If the DeccimalLatitude or DecimalLongitude is NA, then we need to assign the value from the lat column by subsetting the column where there is "," and taking the values before the comma
PSSS$DecimalLatitude[is.na(PSSS$DecimalLatitude)]<-as.numeric(sub(",.*", "", PSSS$lat[is.na(PSSS$DecimalLatitude)]))
#Do the same for longitude but take the values after "," 
PSSS$DecimalLongitude[is.na(PSSS$DecimalLongitude)]<-as.numeric(sub(".*, ", "", PSSS$long[is.na(PSSS$DecimalLongitude)]))

#remove survey_site_id >184 as these are data entry errors according to Toby
PSSS<-PSSS %>% filter(survey_site_id<=189)

#break apart survey_date and reform into day, month, year
PSSS<-PSSS %>% separate(survey_date, into=c("Date", "del"), sep=" ") %>% dplyr::select(-del) %>% 
  separate(Date, into=c("YearCollected", "MonthCollected", "DayCollected"), sep="-") 

#wrangle raptor data into the long format since each species identification should be in a unique row. 
raptor1<-PSSS %>% filter(!is.na(raptor1)) %>% mutate(common_name = raptor1, bird_count = raptor1_count, notes= raptor1_affect)%>%  dplyr::select(-raptor1, -raptor2, -raptor3, -raptor1_count, -raptor2_count, -raptor3_count, -raptor1_affect, -raptor2_affect, -raptor3_affect) 

raptor1<-raptor1 %>% group_by(site_name, common_name, YearCollected, MonthCollected, DayCollected) %>% mutate(bird_count=sum(bird_count)) %>% distinct(common_name, site_name, YearCollected, MonthCollected, DayCollected, .keep_all=TRUE) %>% filter(common_name == "Bald Eagle")

raptor2<-PSSS %>% filter(!is.na(raptor2)) %>% mutate(common_name = raptor2, bird_count = raptor2_count, notes= raptor2_affect)%>%  dplyr::select(-raptor1, -raptor2, -raptor3, -raptor1_count, -raptor2_count, -raptor3_count, -raptor1_affect, -raptor2_affect, -raptor3_affect) 

raptor2<-raptor2 %>% group_by(site_name, common_name, YearCollected, MonthCollected, DayCollected) %>% mutate(bird_count=sum(bird_count)) %>% distinct(common_name, site_name, YearCollected, MonthCollected, DayCollected, .keep_all=TRUE) %>% filter(common_name == "Bald Eagle")

raptor3<-PSSS %>% filter(!is.na(raptor3)) %>% mutate(common_name = raptor3, bird_count = raptor3_count, notes= raptor3_affect) %>%  dplyr::select(-raptor1, -raptor2, -raptor3, -raptor1_count, -raptor2_count, -raptor3_count, -raptor1_affect, -raptor2_affect, -raptor3_affect) 

raptor3<-raptor3 %>% group_by(site_name, common_name, YearCollected, MonthCollected, DayCollected) %>% mutate(bird_count=sum(bird_count)) %>% distinct(common_name, site_name, YearCollected, MonthCollected, DayCollected, .keep_all=TRUE) %>% filter(common_name == "Bald Eagle")

PSSS<-PSSS %>%  dplyr::select(-raptor1, -raptor2, -raptor3, -raptor1_count, -raptor2_count, -raptor3_count, -raptor1_affect, -raptor2_affect, -raptor3_affect) 

#bind raptor data back with PSSS data
PSSS<-rbind(PSSS, raptor1)
PSSS<-rbind(PSSS, raptor2)
PSSS<-rbind(PSSS, raptor3)

#remove rows with missing common name
PSSS<-PSSS %>% filter(common_name !="")

#remove bearing and distance because we want each species/ site/ date to be a single row in the data set similar to BBCWS
PSSS<-PSSS %>% dplyr::select(-bearing, -dist)

#replace Thayer's Gull with Ivory Gull
PSSS<-PSSS %>% mutate(common_name = ifelse(common_name == "Thayer's Gull", "Ivory Gull", common_name))
#replace Iceland (Thayer's) Gull with Ivory Gull
PSSS<-PSSS %>% mutate(common_name = ifelse(common_name == "Iceland (Thayer's) Gull", "Ivory Gull", common_name))

#Merge with species ID
PSSS<-merge(PSSS, sp, by.x=c("common_name"), by.y= ("english_name"), all.x=TRUE)

#create a species list and remove any species with fewer than 50 distinct survey_ids
sp.list<-PSSS %>% group_by(common_name) %>% summarise(n=n_distinct(survey_id)) %>% filter(n>=50) %>% dplyr::select(common_name)

#drop species from the PSSS that are not in the species list
PSSS<-PSSS %>% semi_join(sp.list, by="common_name")

###Create code to identify problematic times
# Look for missing start or end times
test1 <- PSSS %>% filter(is.na(start_time) | is.na(end_time))
# Look for times with less than 3 nchar
test2 <- PSSS %>% filter(nchar(start_time) <= 2 & nchar(end_time) <= 2)
# Look for times with greater than 4 nchar
test3 <- PSSS %>% filter(nchar(start_time) > 4 & nchar(end_time) >4)
# Look for instances where the start time is greater than the end time
test4 <- PSSS %>% filter(start_time > end_time)
#combine test 1-4
test5 <- rbind(test1, test2)
test5 <- rbind(test5, test3)
test5 <- rbind(test5, test4)
test5 <- test5 %>% distinct()

test5<-test5 %>% select(survey_site_id, survey_id, YearCollected, start_time, end_time) %>% distinct()
