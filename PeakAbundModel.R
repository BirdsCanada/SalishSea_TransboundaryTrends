#Code to model peak abundance for each species across the Salish Sea from October to April

library(readxl)
library(writexl)
library(lubridate)

sp.tb<-read_excel("Data/survey.window.new.xlsx") #list of target species

BCCWS<-read.csv("Data/BCCWS.csv")
PSSS<-read.csv("Data/PSSS.csv")
dat<-rbind(BCCWS, PSSS)
dat<-dat %>% mutate(survey_year=YearCollected, survey_month=MonthCollected, survey_day=DayCollected)
dat<-dat %>% format_dates()

#create events matrix
events<-dat %>% select(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, doy) %>% distinct()

#filter species data by list of tagets species
sp.list<-unique(sp.tb$species_code)
dat<-dat %>% select(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, doy, CommonName, SpeciesCode, ObservationCount)
dat<-dat %>% filter(SpeciesCode %in% sp.list)

#Create output table for results
##NOT COMPLETE
sp.data <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 4, byrow = FALSE,
                                dimnames = NULL))
names(sp.data) <- c("SurveyAreaIdentifier", "species", "doy", "mean_Obs")  

for(k in 1:length(species.list)) {
  
  k<-1 #for testing
  sp.data<-NULL
  sp.data <- dat %>% filter(SpeciesCode == sp.list[k]) %>% distinct(SurveyAreaIdentifier,  YearCollected, MonthCollected, DayCollected, .keep_all = TRUE)
  species <- sp.list[k]
  
  #zero-fill
  sp.data <- left_join(events, sp.data, by = c("SurveyAreaIdentifier",  "YearCollected", "MonthCollected", "DayCollected", "doy")) %>%
    mutate(ObservationCount = replace(ObservationCount, is.na(ObservationCount), 0))
  
  test<-sp.data %>% group_by(YearCollected, MonthCollected) %>% summarise(MeanCount=mean(ObservationCount))
  test$Month<-month(test$MonthCollected, label = TRUE, abbr=FALSE)
  test<-test %>% filter(MonthCollected %in% c(10, 11, 12, 1, 2, 3, 4))
  
  #reorder the months ##HERE
  data <- data.frame(
    category = c(1, 2, 3, 4, 10, 11, 12),
    value = c(10, 15, 20, 25, 30, 35, 40, 45)
  )
  
  ggplot(test, aes(x=MonthCollected, y=MeanCount))+
    geom_point()+
    xlab("Month")+
    ylab("Mean Count")+
    ggtitle(sp.list[k])+
    scale_x_continuous(breaks=scales::breaks_width(width=1))+
    theme_classic()
    
  
} #end loop