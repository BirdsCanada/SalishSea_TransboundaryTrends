#Code to model peak abundance for each species across the Salish Sea from October to April

library(readxl)
library(writexl)
library(lubridate)
library(tidyverse)
library(naturecounts)

plot.dir <- paste("./Plots/", sep = "")
dir.create(plot.dir, showWarnings=FALSE, recursive=TRUE)

sp.tb<-read.csv("Data/survey.window.new2.csv") #list of target species

BCCWS<-read.csv("Data/BCCWS.csv")
PSSS<-read.csv("Data/PSSS.csv")
dat<-rbind(BCCWS, PSSS)
dat<-dat %>% mutate(survey_year=YearCollected, survey_month=MonthCollected, survey_day=DayCollected)
dat<-dat %>% format_dates()

#create events matrix
events<-dat %>% select(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, doy) %>% distinct()

#filter species data by list of targets species, these are the 80 sp that the program shares
sp.list<-c("COGO","RNGR","SBIG","COME","PALO","BAEA","BUFF","SUSC","DCCO","WWSC","GWGU","MAMU","AMWI","HOGR","HARD","PECO","MALL","COLO","BAGO","LTDU","RBME","HERG","GBHE","RTLO","WEGR","PIGU","GADW","GWTE","RBGU","LESC","BLSC","NOPI","HOME","BRAN","CACG","PEFA","NSHO","MERL","RNDU","GRSC","NOHA","SNGO","RHAU","PBGR","HEEG","EUWI","CANV","OSPR","BRAC","CAGU","COMU","CAGO","BOGU","WEGU","COHA","SSHA","EAGR","ANMU","GWFG","AMCO","GLGU","CITE","TUSW","RUDU","REDH","CATE","COTE","KIEI","YBLO","RNPH","CAAU","POJA","SOSH","NOFU","REPH","CLGR","BRPE","SAGU","AWPE","IVGU")
#sp.list <- sort(unlist(sp.list))
dat<-dat %>% select(SurveyAreaIdentifier, YearCollected, MonthCollected, DayCollected, doy, CommonName, SpeciesCode, species_id, ObservationCount)
dat<-dat %>% filter(SpeciesCode %in% sp.list)
dat<-dat %>% filter(!is.na(SpeciesCode))

#Create output table for results
out.plot<-NULL


for(k in 1:length(sp.list)) {
  
  #k<-2 #for testing
  sp.data<-NULL
  sp.data <- dat %>% filter(SpeciesCode == sp.list[k]) %>% distinct(SurveyAreaIdentifier,  YearCollected, MonthCollected, DayCollected, .keep_all = TRUE)
  species <- sp.list[k]
  
  #zero-fill
  sp.data <- left_join(events, sp.data, by = c("SurveyAreaIdentifier",  "YearCollected", "MonthCollected", "DayCollected", "doy")) %>%
    mutate(ObservationCount = replace(ObservationCount, is.na(ObservationCount), 0))
  
  test<-sp.data %>% group_by(YearCollected, MonthCollected) %>% summarise(MeanCount=mean(ObservationCount))
  test<-test %>% filter(MonthCollected %in% c(1, 2, 3, 4, 10, 11, 12))
  test$MonthCollected<-factor(test$MonthCollected, levels=c(10, 11, 12, 1, 2, 3, 4), 
                              labels=c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"))
  
  
  out.plot[[k]]<-ggplot(test, aes(x=MonthCollected, y=MeanCount))+
    geom_point()+
    #geom_smooth(method="loess")+
    xlab("Month")+
    ylab("Mean Count")+
    ggtitle(sp.list[k])+
    theme_classic()
  
  } #end sp loop

pdf(paste(plot.dir, "Abundance.pdf", sep = ""),
    height = 10, width = 8, paper = "letter")

#    print(out.plot0)
for(r in 1:length(out.plot)){
  try(print(out.plot[[r]]), silent = TRUE)
}

while(!is.null(dev.list())) dev.off()


