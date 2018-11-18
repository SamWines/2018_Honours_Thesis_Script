# Script for Updating Species Names and Removing Non-Fish Species

# This script is used to:
  # 1. Make sure that species names are consistent across all years
  # 2. Where some species could only be classified to genus, terminology is made consistent
  # 3. Remove non-fish species
  # 4. Remove SRMP sites that do not overlap with multibeam bathymetry information
  # 5. Remove BRUV deployments that had an estimated visibility of less than 4 m

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 6 September 2018

# Libraries required:
library(dplyr)

# Set working directory
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/05_UpdatingSpeciesNames_RemovingNonFish")
getwd()
dir()

# Specify Output Filenames
Analysis_MaxN <- "Analysis_MaxN_SpUpdated.txt"
Analysis_3DPointLengthMeasurements <- "Analysis_3D_Point_Length_Measurements_SpUpdated.txt"

# Load combined Global Archive formatted files
MaxN <- read.table("Analysis_MaxN.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
PointLengthMeasurements <- read.table("Analysis_3D_Point_Length_Measurements.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)



#### Update to latest species names ####

MaxN$Genus_species[MaxN$Genus_species=="Aulopus purpurissatus"]<-"Latropiscis purpurissatus"
MaxN$Genus_species[MaxN$Genus_species=="Chyrosophyrs auratus"]<-"Chrysophrys auratus"
MaxN$Genus_species[MaxN$Genus_species=="Nelusetta ayraudi"]<-"Nelusetta ayraud"
MaxN$Genus_species[MaxN$Genus_species=="Odax acroptilus"]<-"Heteroscarus acroptilus"
MaxN$Genus_species[MaxN$Genus_species=="Odax cyanomelas"]<-"Olisthops cyanomelas"
MaxN$Genus_species[MaxN$Genus_species=="Pseudolabrus mortonii"]<-"Pseudolabrus rubicundus"
MaxN$Genus_species[MaxN$Genus_species=="Sillaginodes punctata"]<-"Sillaginodes punctatus"

PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Aulopus purpurissatus"]<-"Latropiscis purpurissatus"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Chyrosophyrs auratus"]<-"Chrysophrys auratus"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Nelusetta ayraudi"]<-"Nelusetta ayraud"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Odax acroptilus"]<-"Heteroscarus acroptilus"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Odax cyanomelas"]<-"Olisthops cyanomelas"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Pseudolabrus mortonii"]<-"Pseudolabrus rubicundus"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Sillaginodes punctata"]<-"Sillaginodes punctatus"



#### Tidy observations only identified to genus level ####

unique(MaxN$Genus_species)

MaxN$Genus_species[MaxN$Genus_species=="Platycephalus "]<-"Platycephalus sp"
MaxN$Genus_species[MaxN$Genus_species=="Platycephalus bassensis"]<-"Platycephalus sp"
MaxN$Genus_species[MaxN$Genus_species=="Caesioperca rasor"]<-"Caesioperca sp"
MaxN$Genus_species[MaxN$Genus_species=="Platycephalus bassensis"]<-"Platycephalus sp"
MaxN$Genus_species[MaxN$Genus_species=="Trachurus novaezelandiae"]<-"Trachurus sp"
MaxN$Genus_species[MaxN$Genus_species=="Trachurus declivis"]<-"Trachurus sp"
MaxN$Genus_species[MaxN$Genus_species=="Parma victoriae"]<-"Parma sp"
MaxN$Genus_species[MaxN$Genus_species=="Pseudocaranx georgianus"]<-"Pseudocaranx sp"
MaxN$Genus_species[MaxN$Genus_species=="Pseudocaranx dentex"]<-"Pseudocaranx sp"
MaxN$Genus_species[MaxN$Genus_species=="Pseudocaranx wrighti"]<-"Pseudocaranx sp"
MaxN$Genus_species[MaxN$Genus_species=="Platycephalus "]<-"Platycephalus sp"
MaxN$Genus_species[MaxN$Genus_species=="Platycephalus bassensis"]<-"Platycephalus sp"
MaxN$Genus_species[MaxN$Genus_species=="Dasyatis brevicaudata"]<-"Dasyatis sp"
MaxN$Genus_species[MaxN$Genus_species=="Achoerodus gouldii"]<-"Achoerodus sp"
MaxN$Genus_species[MaxN$Genus_species=="Scorpis aequipinnis"]<-"Scorpis sp"
MaxN$Genus_species[MaxN$Genus_species=="Scorpis lineolata"]<-"Scorpis sp"
MaxN$Genus_species[MaxN$Genus_species=="Pseudophycis bachus"]<-"Pseudophycis sp"
MaxN$Genus_species[MaxN$Genus_species=="Pseudophycis barbata"]<-"Pseudophycis sp"
MaxN$Genus_species[MaxN$Genus_species=="Urolophus paucimaculatus"]<-"Urolophus sp"
MaxN$Genus_species[MaxN$Genus_species=="Parascyllium variolatum"]<-"Parascyllium sp"
MaxN$Genus_species[MaxN$Genus_species=="Parascyllium ferrugineum"]<-"Parascyllium sp"

PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Platycephalus "]<-"Platycephalus sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Platycephalus bassensis"]<-"Platycephalus sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Caesioperca rasor"]<-"Caesioperca sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Trachurus novaezelandiae"]<-"Trachurus sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Trachurus declivis"]<-"Trachurus sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Parma victoriae"]<-"Parma sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Pseudocaranx georgianus"]<-"Pseudocaranx sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Pseudocaranx dentex"]<-"Pseudocaranx sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Pseudocaranx wrighti"]<-"Pseudocaranx sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Platycephalus "]<-"Platycephalus sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Platycephalus bassensis"]<-"Platycephalus sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Dasyatis brevicaudata"]<-"Dasyatis sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Achoerodus gouldii"]<-"Achoerodus sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Scorpis aequipinnis"]<-"Scorpis sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Scorpis lineolata"]<-"Scorpis sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Pseudophycis bachus"]<-"Pseudophycis sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Pseudophycis barbata"]<-"Pseudophycis sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Urolophus paucimaculatus"]<-"Urolophus sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Parascyllium variolatum"]<-"Parascyllium sp"
PointLengthMeasurements$Genus_species[PointLengthMeasurements$Genus_species=="Parascyllium ferrugineum"]<-"Parascyllium sp"



#### Remove observations only identified to family level ####

MaxN <- MaxN[MaxN$Genus_species!=" sp" & MaxN$Genus_species!=" ", ]

PointLengthMeasurements <- PointLengthMeasurements[PointLengthMeasurements$Genus_species!=" sp" 
                                                   & PointLengthMeasurements$Genus_species!=" ", ]

PointLengthMeasurements <- PointLengthMeasurements[PointLengthMeasurements$Genus_species!=" sp" 
             & PointLengthMeasurements$Genus_species!=" ", ]



#### Remove Non-Fish Species ####

MaxN <- MaxN[MaxN$Genus_species!="Jasus edwardsii" 
             & MaxN$Genus_species!="Sepioteuthis australis" 
             & MaxN$Genus_species!="Nototodarus gouldi" 
             & MaxN$Genus_species!="Sepia apama" 
             & MaxN$Genus_species!="Arctocephalus pusillus", ]

PointLengthMeasurements <- PointLengthMeasurements[PointLengthMeasurements$Genus_species!="Jasus edwardsii" 
                                                   & PointLengthMeasurements$Genus_species!="Sepioteuthis australis" 
                                                   & PointLengthMeasurements$Genus_species!="Nototodarus gouldi" 
                                                   & PointLengthMeasurements$Genus_species!="Sepia apama" 
                                                   & PointLengthMeasurements$Genus_species!="Arctocephalus pusillus", ]



#### Remove SRMP Torquay Drops (Dont have Multibeam Data) ####

MaxN <- MaxN %>%
  filter(HonoursID != "8_2017-03-PA-BRUV" & 
           HonoursID != "8_2018-03-PA-BRUV" & 
           HonoursID != "9_2017-03-PA-BRUV" & 
           HonoursID != "9_2018-03-PA-BRUV" & 
           HonoursID != "10_2017-03-PA-BRUV" & 
           HonoursID != "10_2018-03-PA-BRUV" &
           HonoursID != "25_2017-03-PA-BRUV" & 
           HonoursID != "25_2018-03-PA-BRUV" & 
           HonoursID != "26_2017-03-PA-BRUV" & 
           HonoursID != "26_2018-03-PA-BRUV" & 
           HonoursID != "27_2017-03-PA-BRUV" & 
           HonoursID != "27_2018-03-PA-BRUV")

PointLengthMeasurements <- PointLengthMeasurements %>%
  filter(HonoursID != "8_2017-03-PA-BRUV" & 
           HonoursID != "8_2018-03-PA-BRUV" & 
           HonoursID != "9_2017-03-PA-BRUV" & 
           HonoursID != "9_2018-03-PA-BRUV" & 
           HonoursID != "10_2017-03-PA-BRUV" & 
           HonoursID != "10_2018-03-PA-BRUV" &
           HonoursID != "25_2017-03-PA-BRUV" & 
           HonoursID != "25_2018-03-PA-BRUV" & 
           HonoursID != "26_2017-03-PA-BRUV" & 
           HonoursID != "26_2018-03-PA-BRUV" & 
           HonoursID != "27_2017-03-PA-BRUV" & 
           HonoursID != "27_2018-03-PA-BRUV")



#### Remove drops with poor visibility ####
  # Used minimum of 4m visibility

MaxN <- MaxN %>%
  filter(EstimatedVisibility >= 4)

PointLengthMeasurements <- PointLengthMeasurements %>%
  filter(EstimatedVisibility >= 4)



#### Check how many samples remaining ###

unique(MaxN$HonoursID)
unique(PointLengthMeasurements$HonoursID)



#### Export Tables ####

write.table(MaxN, file = Analysis_MaxN , sep = "\t", row.names = FALSE)

write.table(PointLengthMeasurements, file = Analysis_3DPointLengthMeasurements , sep = "\t", row.names = FALSE)


