
# To read from local Data directory
in.data<-read.csv("Data/in.data.csv")
events<-read.csv("Data/events.csv")

#Determine which SpeciesCode each ProjectCode share in the full dataset
sp.bsss<-in.data %>% filter(ProjectCode == "BCCWS") %>% dplyr::select(SpeciesCode) %>% distinct()
sp.psss<-in.data %>% filter(ProjectCode == "PSSS") %>% dplyr::select(SpeciesCode) %>% distinct()

common.species<-intersect(sp.bsss$SpeciesCode, sp.psss$SpeciesCode) #There are 74 species in common between BCCWS and PSSS

#filter the full dataset to only include the common species
in.data<-in.data[in.data$SpeciesCode %in% common.species,]

#Some species are detected infrequently. We will want to set a minimum data requirement for species to be included in the analysis.Species that have a mean abundance per year < 5, and species that were detected in fewer than one-half of the survey years will be removed from the analysis. 

#First, calculate the mean abundance per year for each species
SpeciesMean<-in.data %>% group_by(SpeciesCode, YearCollected) %>% summarise(MeanYrAbundance = mean(ObservationCount))%>% group_by(SpeciesCode) %>% summarise(MeanAbundance = mean(MeanYrAbundance))
#Then, we will calculate the number of years each species was detected in
SpeciesYears<-in.data %>% group_by(SpeciesCode) %>% summarise(NumYears = length(unique(YearCollected)))
#Merge the two datasets
SpeciesData<-merge(SpeciesMean, SpeciesYears, by = c("SpeciesCode"))
#Filter the data to only include species that meet the minimum data requirements
SpeciesData<-SpeciesData[SpeciesData$MeanAbundance >= min.abundance & SpeciesData$NumYears >= min.years,]
#create species list from SpeciesData
sp.list<-unique(SpeciesData$SpeciesCode)