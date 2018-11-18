# Script for combining functional information and fish data

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 9 September 2018

# Libraries required
library(dplyr)

# Set working directory for input files
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/08_AddEnvironmentalVariables/")
getwd()
dir()

# Load input files
totalMaxN <- read.table("Analysis_MaxN_SpUpdated_HabitatInc_EnviroInc.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
totalLength <- read.table("Analysis_3D_Point_Length_Measurements_SpUpdated_BiomassIncluded_HabitatInc_EnviroInc.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 

# Reset working directory for outputs
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/09_FunctionalInformation/")
getwd()
dir()

#### Load database of functional information and combine with MaxN and Lengths ####

Honours_Functional_Information <- read.table("01_Honours_Functional_Information.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

temp.totalMaxN <- (merge(totalMaxN, Honours_Functional_Information, by.x = "Genus_species", by.y = "TAXONOMIC_NAME", all.x = TRUE, sort = FALSE))

temp.totalLength <- (merge(totalLength, Honours_Functional_Information, by.x = "Genus_species", by.y = "TAXONOMIC_NAME", all.x = TRUE, sort = FALSE))

colnames(temp.totalMaxN)
temp.totalMaxN = temp.totalMaxN[, c(2:6, 1, 7:16, 220, 221, 17:219, 222:250)]

colnames(temp.totalLength)
temp.totalLength = temp.totalLength[, c(2:6, 1, 7:28, 235, 236, 29:234, 237:265)]


#### QUALITY CONTROL: Check for NAs ####
# As this is the final step for formatting data for analysis. This indicates processes that may not have worked.

NAs.MaxN <- temp.totalMaxN %>%
  filter(is.na(HonoursID) | is.na(Sample.No) | is.na(Genus_species) | is.na(Count) | is.na(Latitude.x) | is.na(Longitude.x) | is.na(Date) | is.na(Time.x) | is.na(Relief) | is.na(Sponge) | is.na(Reef) | is.na(Vegetation) | is.na(Sediment) | is.na(Year) | is.na(vrm_neigh3_mean_5_clip_01) | is.na(slope_mean_5_clip_01) | is.na(rugosity_mean_500_clip_01) | is.na(profile_curv_mean_500_clip_01) | is.na(plan_curv_mean_500_clip_01) | is.na(northness_mean_500_clip_01) | is.na(SPECIES_NAME) | is.na(Lmax) | is.na(Trophic.Level) | is.na(Water.column) | is.na(Substrate.type) | is.na(complexity) | is.na(Gregariousness) | is.na(Vulnerability))

NAs.Length <- temp.totalLength %>%
  filter(is.na(HonoursID) | is.na(Original.Sample.Code) | is.na(Genus_species) | is.na(Length) | is.na(Biomass..g.) | is.na(Latitude.x) | is.na(Longitude.x) | is.na(Date) | is.na(Time.x) | is.na(Relief) | is.na(Sponge) | is.na(Reef) | is.na(Vegetation) | is.na(Sediment) | is.na(Year) | is.na(vrm_neigh3_mean_5_clip_01) | is.na(slope_mean_5_clip_01) | is.na(rugosity_mean_500_clip_01) | is.na(profile_curv_mean_500_clip_01) | is.na(plan_curv_mean_500_clip_01) | is.na(northness_mean_500_clip_01) | is.na(SPECIES_NAME) | is.na(Lmax) | is.na(Trophic.Level) | is.na(Water.column) | is.na(Substrate.type) | is.na(complexity) | is.na(Gregariousness) | is.na(Vulnerability))

NAs.Habitat <- temp.totalMaxN %>%
  filter(is.na(Relief)) %>%
  dplyr::select(c(HonoursID, Sample.No, Genus_species, Relief, Sponge, Macroalgae, Reef, Vegetation, Sediment, CampaignID))

NAs.Functional <- temp.totalMaxN %>%
  filter(is.na(SPECIES_NAME))
NAs.Functional.Species <- data.frame(unique(NAs.Functional$Genus_species))


#### Export output tables. Descriptions of contents below: ####

# Combined fish MaxN observations, habitat characteristics, environmental variables and functional information for all years ####
write.table(temp.totalMaxN, file = "Analysis_MaxN_SpUpdated_HabitatInc_EnviroInc_FunctionalInc.txt" , sep = "\t", row.names = FALSE)

# Combined fish length observations, habitat characteristics, environmental variables and functional information for all years ####
write.table(temp.totalLength, file = "Analysis_3D_Point_Length_Measurements_SpUpdated_BiomassIncluded_HabitatInc_EnviroInc_FunctionalInc.txt" , sep = "\t", row.names = FALSE)
