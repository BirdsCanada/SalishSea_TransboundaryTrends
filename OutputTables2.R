## Set up output tables
#This is in a separate file because if loop through species fails, don't want to re-write these files and delete everything that has already been done. Can then just re-start the loop at the next species, and keep going.

## Create .csv file for indices

indices.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 15, byrow = FALSE,
                                    dimnames = NULL))
names(indices.csv) <- c("results_code", "version", "area_code", "season", "period", "species_code", "species_id", "year", "index", "stderr", "stdev", "upper_ci", "lower_ci", "LOESS_index", "trend_index")


write.table(indices.csv, file = paste(out.dir, 
                                      name, "_AnnualIndices_iCAR.csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")


## Create .csv file for trends
trends.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 39, 
                                   byrow = FALSE, dimnames = NULL))
names(trends.csv) <- c("results_code",	"version",	"area_code",	"season",	"period", "species_code",	"species_id",	"years", "year_start",	"year_end",	"trnd",	"lower_ci", "upper_ci", "index_type", "stderr",	"model_type",	"model_fit",	"percent_change",	"percent_change_low",	"percent_change_high",	"prob_decrease_0",	"prob_decrease_25",	"prob_decrease_30",	"prob_decrease_50",	"prob_increase_0",	"prob_increase_33",	"prob_increase_100", "suitability", "precision_num",	"precision_cat",	"coverage_num",	"coverage_cat",	"sample_size", "sample_size_units", "prob_LD", "prob_MD", "prob_LC", "prob_MI", "prob_LI")


#Endpoint Trends
write.table(trends.csv, file = paste(out.dir, 
                                     name, "_TrendsEndpoint_iCAR.csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")

## Create .csv file for dispersion stat

dispersion.csv <- as.data.frame(matrix(data = NA, nrow = 1, ncol = 3, byrow = FALSE,
                                    dimnames = NULL))
names(dispersion.csv) <- c("area_code", "SpeciesCode", "dispersion")

write.table(dispersion.csv, file = paste(out.dir,  name, "_DispersionStat_iCAR.csv", sep = ""), 
            row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")


