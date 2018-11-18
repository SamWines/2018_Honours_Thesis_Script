# Script for combining broad habitat characteristics with fish data

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 6 September 2018

rm(list=ls())

# Libraries required
library(dplyr)

# Load input files from multiple directories
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/05_UpdatingSpeciesNames_RemovingNonFish/")
getwd()
dir()

MaxN <- read.table("Analysis_MaxN_SpUpdated.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
MaxN$ID <- paste(MaxN$Sample.No, MaxN$CampaignID, sep="_")


getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/06_Biomass/")
getwd()
dir()

Lengths <- read.table("Analysis_3D_Point_Length_Measurements_SpUpdated_BiomassIncluded.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
Lengths$ID <- paste(Lengths$Sample.No, Lengths$CampaignID, sep="_")


getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/07_HabitatData/Step2/")
getwd()
dir()

Habitat <- read.table("Analysis_Habitat.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)


# Reset Working Directory for Outputs
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/07_HabitatData/Step3_JoinToFishData/")
getwd()



#### Join broad habitat characteristics with fish observations ####
MaxN.HabInc <- (merge(MaxN, Habitat, by = 'HonoursID', all.x = TRUE, sort = FALSE))
Length.HabInc <- (merge(Lengths, Habitat, by = 'HonoursID', all.x = TRUE, sort = FALSE))



#### Export combined site habitat characteristics and fish observations for all years ####
write.table(MaxN.HabInc, file = "Analysis_MaxN_SpUpdated_HabitatInc.txt" , sep = "\t", row.names = FALSE)
write.table(Length.HabInc, file = "Analysis_3D_Point_Length_Measurements_SpUpdated_BiomassIncluded_HabitatInc.txt" , sep = "\t", row.names = FALSE)




