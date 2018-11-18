# Script for taking EventMeasure raw output and changing it into Global Archive format

# This specfic file is for the 2018-03-PA-BRUV campaign

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated: 27 April 2018

# Libraries required:
library(stringr)
library(dplyr)

# Set working directory
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/02_EMtoGA/")
getwd()

# Set Campaign ID
CampaignID <- "2018-03-PA-BRUV"

# Specify Input Filenames
EM_MaxN <- "AD_2018_MaxN.txt"
EM_3DPointLengthMeasurements <- "AD_2018_3D_Point_Length_Measurements.txt"

# Specify Output Filenames
GA_Metadata <- "2018-03_PointAddis_stereoBRUVs_Metadata.txt"
GA_Count <- "2018-03_PointAddis_stereoBRUVs_Count.txt"
GA_Length <- "2018-03_PointAddis_stereoBRUVs_Length.txt"

# Load EventMeasure Output Files
MaxN <- read.table(EM_MaxN, header = TRUE, skip = 4, sep = "\t", stringsAsFactors = FALSE)
PointLengthMeasurements <- read.table(EM_3DPointLengthMeasurements, header = TRUE, skip = 4, sep = "\t", stringsAsFactors = FALSE)
ProcessingLog <- read.table("Final_CompletionsLOG.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)



#### Make Global Archive Count File ####

MaxN$Genus_species <- with(MaxN, paste(Genus, Species, sep = " "))
MaxN <- subset(MaxN, Genus_species != " ")
MaxN$Others <- NA
split.Count <- data.frame(do.call('rbind', str_split(as.character(MaxN$Filename),'_')))
split.Count$Cams <- with(split.Count, paste(X2, X3, X4, sep = "_"))

# Add estimated visibility
MaxN$VizID <- paste(MaxN$OpCode, split.Count$Cams, sep = "_")
ProcessingLog$VizID <- paste(ProcessingLog$Drop.Code, ProcessingLog$Cameras, sep = "_")
MaxN <- merge(MaxN, ProcessingLog, by = 'VizID', sort = FALSE, all.x = TRUE)

Draft.Count <- data.frame(MaxN$OpCode, 
                          MaxN$Family, 
                          MaxN$Genus_species, 
                          MaxN$MaxN, 
                          MaxN$Others, 
                          MaxN$Comment, 
                          split.Count$Cams, 
                          MaxN$Filename, 
                          MaxN$Period.time..mins., 
                          MaxN$Code, 
                          MaxN$TapeReader, 
                          MaxN$Has.fed, 
                          CampaignID)
colnames(Draft.Count) <-  c("Sample", 
                            "Family", 
                            "Genus_species", 
                            "Count", 
                            "Others", 
                            "Comment", 
                            "Cams", 
                            "Filename", 
                            "Time_MaxN(mins)", 
                            "sp_Code", 
                            "Tape_Reader", 
                            "Has_Fed", 
                            "CampaignID")



#### Make Global Archive Length File ####

PointLengthMeasurements$Genus_species <- with(PointLengthMeasurements, paste(Genus, Species, sep = " "))
PointLengthMeasurements$Others <- NA
split.Lengths <- data.frame(do.call('rbind', strsplit(as.character(PointLengthMeasurements$Filename),'_',fixed=TRUE)))
split.Lengths$Cams <- with(split.Lengths, paste(X2, X3, X4, sep = "_"))

Draft.Length <- data.frame(PointLengthMeasurements$OpCode, 
                           PointLengthMeasurements$Family, 
                           PointLengthMeasurements$Genus_species, 
                           PointLengthMeasurements$Number, 
                           PointLengthMeasurements$Length,
                           PointLengthMeasurements$Others, 
                           PointLengthMeasurements$Frame,
                           PointLengthMeasurements$Period.time..mins.,
                           PointLengthMeasurements$Length..mm.,
                           PointLengthMeasurements$Precision..mm.,
                           PointLengthMeasurements$RMS..mm.,
                           PointLengthMeasurements$Range..mm.,
                           PointLengthMeasurements$Direction..deg.,
                           PointLengthMeasurements$Horz..Dir...deg.,
                           PointLengthMeasurements$Vert..Dir...deg.,
                           PointLengthMeasurements$Mid.X..mm.,
                           PointLengthMeasurements$Mid.Y..mm.,
                           PointLengthMeasurements$Mid.Z..mm.,
                           PointLengthMeasurements$TapeReader,
                           PointLengthMeasurements$Code,
                           PointLengthMeasurements$Stage,
                           PointLengthMeasurements$Comment,
                           PointLengthMeasurements$Filename,
                           split.Lengths$Cams,
                           CampaignID)

colnames(Draft.Length) <-  c("Sample",
                             "Family",
                             "Genus_species",
                             "Count",
                             "Length",
                             "Others",
                             "Frame",
                             "Time (mins)",
                             "Length (mm)",
                             "Precision (mm)",
                             "RMS (mm)",
                             "Range (mm)",
                             "Direction (deg)",
                             "Horz. Dir. (deg)",
                             "Vert. Dir. (deg)",
                             "Mid X (mm)",
                             "Mid Y (mm)",
                             "Mid Z (mm)",
                             "TapeReader",
                             "Code",
                             "Stage",
                             "Comment",
                             "Filename",
                             "Cameras",
                             "CampaignID")



#### Make Global Archive Metadata File ####

Draft.Metadata <- data.frame(CampaignID,
                             Draft.Count$Sample,
                             MaxN$Latitude,
                             MaxN$Longitude,
                             MaxN$Date,
                             MaxN$Time,
                             MaxN$Site,
                             MaxN$MPAStatus,
                             split.Count$X1,
                             MaxN$Depth,
                             MaxN$TapeReader,
                             MaxN$Comment,
                             MaxN$Estimated.Viz,
                             MaxN$Others)

                             
colnames(Draft.Metadata) <- c("CampaignID", 
                              "Sample",	
                              "Latitude",	
                              "Longitude",	
                              "Date",	
                              "Time",	
                              "Location",	
                              "Status",	
                              "Site",	
                              "Depth",	
                              "Observer",	
                              "Successful",
                              "EstimatedVisibility",
                              "Others")

Draft.Metadata.2 <- Draft.Metadata[!duplicated(Draft.Metadata$Sample), ]



#### Export Global Archive Formatted Tables ####

write.table(Draft.Count, file = GA_Count , sep = "\t", row.names = FALSE)
write.table(Draft.Length, file = GA_Length , sep = "\t", row.names = FALSE)
write.table(Draft.Metadata.2, file = GA_Metadata , sep = "\t", row.names = FALSE)

