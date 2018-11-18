# Script for combining Global Archive Count and Length Data with Assosciated Metadata

# This specific file combines metadata with count and lengths for 2013, 2017 and 2018 Point Addis Parks Victoria BRUV campaigns
# It then merges observations from all three years into a single dataframe

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 27 April 2018

# Libraries required:
library(dplyr)

# Set working directory
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/04_JoiningMetadata")
getwd()

# Specify Output Filenames
Analysis_MaxN <- "Analysis_MaxN.txt"
Analysis_3DPointLengthMeasurements <- "Analysis_3D_Point_Length_Measurements.txt"



#### Join Metadata with 2018 Global Archive MaxN and Length Files ####

# Specify Input Filenames
GA_Metadata_2018 <- "2018-03_PointAddis_stereoBRUVs_Metadata.txt"
GA_Count_2018 <- "2018-03_PointAddis_stereoBRUVs_Count.txt"
GA_Length_2018 <- "2018-03_PointAddis_stereoBRUVs_Length.txt"

# Load Global Archive Output Files
MaxN_2018 <- read.table(GA_Count_2018, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
PointLengthMeasurements_2018 <- read.table(GA_Length_2018, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
Metadata_2018 <- read.table(GA_Metadata_2018, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Join Fish Observations with Metadata for Each Site
Analysis_MaxN_2018 <- (merge(MaxN_2018, Metadata_2018, by = 'Sample', sort = FALSE))
Analysis_PointLengthMeasurements_2018 <- (merge(PointLengthMeasurements_2018, Metadata_2018, by = 'Sample', sort = FALSE))



#### Join Metadata with 2017 Global Archive MaxN and Length Files ####

# Specify Input Filenames
GA_Metadata_2017 <- "2017-03_PointAddis_stereoBRUVs_Metadata.txt"
GA_Count_2017 <- "2017-03_PointAddis_stereoBRUVs_Count.txt"
GA_Length_2017 <- "2017-03_PointAddis_stereoBRUVs_Length.txt"

# Load Global Archive Output Files
MaxN_2017 <- read.table(GA_Count_2017, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
PointLengthMeasurements_2017 <- read.table(GA_Length_2017, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
Metadata_2017 <- read.table(GA_Metadata_2017, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Join Fish Observations with Metadata for Each Site
Analysis_MaxN_2017 <- (merge(MaxN_2017, Metadata_2017, by = 'Sample', sort = FALSE))
Analysis_PointLengthMeasurements_2017 <- (merge(PointLengthMeasurements_2017, Metadata_2017, by = 'Sample', sort = FALSE))



#### Join Metadata with 2013 Global Archive MaxN and Length Files ####

# Specify Input Filenames
GA_Metadata_2013 <- "2013-04_PointAddis_stereoBRUVs_Metadata.txt"
GA_Count_2013 <- "2013-04_PointAddis_stereoBRUVs_Count.txt"
GA_Length_2013 <- "2013-04_PointAddis_stereoBRUVs_Length.txt"

# Load Global Archive Output Files
MaxN_2013 <- read.table(GA_Count_2013, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
PointLengthMeasurements_2013 <- read.table(GA_Length_2013, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
Metadata_2013 <- read.table(GA_Metadata_2013, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Join Fish Observations with Metadata for Each Site
Analysis_MaxN_2013 <- (merge(MaxN_2013, Metadata_2013, by = 'Sample', sort = FALSE))
Analysis_PointLengthMeasurements_2013 <- (merge(PointLengthMeasurements_2013, Metadata_2013, by = 'Sample', sort = FALSE))



#### Make sure that all years have a common drop number column ####

Analysis_MaxN_2013$Sample.No <- substring(Analysis_MaxN_2013$Sample, 3)
Analysis_MaxN_2017$Sample.No <- substring(Analysis_MaxN_2017$Sample, 3)
Analysis_MaxN_2018$Sample.No <- substring(Analysis_MaxN_2018$Sample, 3)

Analysis_PointLengthMeasurements_2013$Sample.No <- substring(Analysis_PointLengthMeasurements_2013$Sample, 3)
Analysis_PointLengthMeasurements_2017$Sample.No <- substring(Analysis_PointLengthMeasurements_2017$Sample, 3)
Analysis_PointLengthMeasurements_2018$Sample.No <- substring(Analysis_PointLengthMeasurements_2018$Sample, 3)



#### Convert dataframes from all years into a common format so that they can be merged together ####

# Make common format for MaxN dataframes

common_Analysis_MaxN_2018 <- data.frame(Analysis_MaxN_2018$Sample,
                                        Analysis_MaxN_2018$Sample.No,
                                        Analysis_MaxN_2018$Family, 
                                        Analysis_MaxN_2018$Genus_species, 
                                        Analysis_MaxN_2018$Count, 
                                        Analysis_MaxN_2018$Others.x, 
                                        Analysis_MaxN_2018$Comment, 
                                        Analysis_MaxN_2018$Cams, 
                                        Analysis_MaxN_2018$Filename, 
                                        Analysis_MaxN_2018$Time_MaxN.mins., 
                                        Analysis_MaxN_2018$sp_Code, 
                                        Analysis_MaxN_2018$Tape_Reader, 
                                        Analysis_MaxN_2018$Has_Fed, 
                                        Analysis_MaxN_2018$Latitude,
                                        Analysis_MaxN_2018$Longitude,
                                        Analysis_MaxN_2018$Date,
                                        Analysis_MaxN_2018$Time,
                                        Analysis_MaxN_2018$Location,
                                        Analysis_MaxN_2018$Status,
                                        Analysis_MaxN_2018$Site,
                                        Analysis_MaxN_2018$Depth,
                                        Analysis_MaxN_2018$Observer,
                                        Analysis_MaxN_2018$Successful,
                                        Analysis_MaxN_2018$EstimatedVisibility,
                                        Analysis_MaxN_2018$CampaignID.x)

colnames(common_Analysis_MaxN_2018) <-  c("Sample",
                                          "Sample.No",
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
                                          "CampaignID")


common_Analysis_MaxN_2017 <- data.frame(Analysis_MaxN_2017$Sample,
                                        Analysis_MaxN_2017$Sample.No,
                                        Analysis_MaxN_2017$Family, 
                                        Analysis_MaxN_2017$Genus_species, 
                                        Analysis_MaxN_2017$Count, 
                                        Analysis_MaxN_2017$Others.x, 
                                        Analysis_MaxN_2017$Comment, 
                                        Analysis_MaxN_2017$Cams, 
                                        Analysis_MaxN_2017$FileName, 
                                        Analysis_MaxN_2017$Time_MaxN.mins., 
                                        Analysis_MaxN_2017$sp_Code, 
                                        Analysis_MaxN_2017$Tape_Reader, 
                                        Analysis_MaxN_2017$Has_Fed, 
                                        Analysis_MaxN_2017$Latitude,
                                        Analysis_MaxN_2017$Longitude,
                                        Analysis_MaxN_2017$Date,
                                        Analysis_MaxN_2017$Time,
                                        Analysis_MaxN_2017$Location,
                                        Analysis_MaxN_2017$Status,
                                        Analysis_MaxN_2017$Site,
                                        Analysis_MaxN_2017$Depth,
                                        Analysis_MaxN_2017$Observer,
                                        Analysis_MaxN_2017$Successful,
                                        Analysis_MaxN_2017$EstimatedVisibility,
                                        Analysis_MaxN_2017$CampaignID.x)

colnames(common_Analysis_MaxN_2017) <-  c("Sample",
                                          "Sample.No",
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
                                          "CampaignID")


common_Analysis_MaxN_2013 <- data.frame(Analysis_MaxN_2013$Sample,
                                        Analysis_MaxN_2013$Sample.No,
                                        Analysis_MaxN_2013$Family, 
                                        Analysis_MaxN_2013$Genus_species, 
                                        Analysis_MaxN_2013$Count, 
                                        Analysis_MaxN_2013$Others.x, 
                                        NA, 
                                        Analysis_MaxN_2013$Cams, 
                                        NA, 
                                        Analysis_MaxN_2013$Time_MaxN..mins., 
                                        Analysis_MaxN_2013$sp_Code, 
                                        Analysis_MaxN_2013$Tape_Reader, 
                                        NA, 
                                        Analysis_MaxN_2013$Latitude,
                                        Analysis_MaxN_2013$Longitude,
                                        Analysis_MaxN_2013$Date,
                                        Analysis_MaxN_2013$Time,
                                        Analysis_MaxN_2013$Location,
                                        Analysis_MaxN_2013$Status,
                                        Analysis_MaxN_2013$Site,
                                        Analysis_MaxN_2013$Depth,
                                        Analysis_MaxN_2013$Observer,
                                        Analysis_MaxN_2013$Successful,
                                        Analysis_MaxN_2013$EstimatedVisibility,
                                        Analysis_MaxN_2013$CampaignID.x)

colnames(common_Analysis_MaxN_2013) <-  c("Sample",
                                          "Sample.No",
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
                                          "CampaignID")


# Make common format for Length dataframes

common_Analysis_PointLengthMeasurements_2018 <- data.frame(Analysis_PointLengthMeasurements_2018$Sample,
                                                           Analysis_PointLengthMeasurements_2018$Sample.No, 
                                                           Analysis_PointLengthMeasurements_2018$Family, 
                                                           Analysis_PointLengthMeasurements_2018$Genus_species,
                                                           Analysis_PointLengthMeasurements_2018$Count, 
                                                           Analysis_PointLengthMeasurements_2018$Length, 
                                                           Analysis_PointLengthMeasurements_2018$Others.x,
                                                           Analysis_PointLengthMeasurements_2018$Frame, 
                                                           Analysis_PointLengthMeasurements_2018$Time..mins.,
                                                           Analysis_PointLengthMeasurements_2018$Length..mm., 
                                                           Analysis_PointLengthMeasurements_2018$Precision..mm., 
                                                           Analysis_PointLengthMeasurements_2018$RMS..mm.,
                                                           Analysis_PointLengthMeasurements_2018$Range..mm., 
                                                           Analysis_PointLengthMeasurements_2018$Direction..deg.,
                                                           Analysis_PointLengthMeasurements_2018$Horz..Dir...deg., 
                                                           Analysis_PointLengthMeasurements_2018$Vert..Dir...deg.,
                                                           Analysis_PointLengthMeasurements_2018$Mid.X..mm.,
                                                           Analysis_PointLengthMeasurements_2018$Mid.Y..mm.,
                                                           Analysis_PointLengthMeasurements_2018$Mid.Z..mm.,
                                                           Analysis_PointLengthMeasurements_2018$TapeReader,
                                                           Analysis_PointLengthMeasurements_2018$Code,
                                                           Analysis_PointLengthMeasurements_2018$Stage,
                                                           Analysis_PointLengthMeasurements_2018$Comment,
                                                           Analysis_PointLengthMeasurements_2018$Filename,
                                                           Analysis_PointLengthMeasurements_2018$Cameras,
                                                           Analysis_PointLengthMeasurements_2018$Latitude,
                                                           Analysis_PointLengthMeasurements_2018$Longitude,
                                                           Analysis_PointLengthMeasurements_2018$Date,
                                                           Analysis_PointLengthMeasurements_2018$Time,
                                                           Analysis_PointLengthMeasurements_2018$Location,
                                                           Analysis_PointLengthMeasurements_2018$Status,
                                                           Analysis_PointLengthMeasurements_2018$Site,
                                                           Analysis_PointLengthMeasurements_2018$Depth,
                                                           Analysis_PointLengthMeasurements_2018$Observer,
                                                           Analysis_PointLengthMeasurements_2018$Successful,
                                                           Analysis_PointLengthMeasurements_2018$EstimatedVisibility,
                                                           Analysis_PointLengthMeasurements_2018$CampaignID.x)
colnames(common_Analysis_PointLengthMeasurements_2018) <-  c("Sample",
                                                             "Sample.No",
                                                             "Family", 
                                                             "Genus_species",
                                                             "Count", 
                                                             "Length", 
                                                             "Others",
                                                             "Frame", 
                                                             "Time..mins.",
                                                             "Length..mm.", 
                                                             "Precision..mm.", 
                                                             "RMS..mm.",
                                                             "Range..mm.", 
                                                             "Direction..deg.",
                                                             "Horz..Dir...deg.", 
                                                             "Vert..Dir...deg.",
                                                             "Mid.X..mm.",
                                                             "Mid.Y..mm.",
                                                             "Mid.Z..mm.",
                                                             "TapeReader",
                                                             "Code",
                                                             "Stage",
                                                             "Comment",
                                                             "Filename",
                                                             "Cameras",
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
                                                             "CampaignID")

common_Analysis_PointLengthMeasurements_2017 <- data.frame(Analysis_PointLengthMeasurements_2017$Sample,
                                                           Analysis_PointLengthMeasurements_2017$Sample.No,
                                                           Analysis_PointLengthMeasurements_2017$Family, 
                                                           Analysis_PointLengthMeasurements_2017$Genus_species,
                                                           Analysis_PointLengthMeasurements_2017$Count, 
                                                           Analysis_PointLengthMeasurements_2017$Length, 
                                                           Analysis_PointLengthMeasurements_2017$Others.x,
                                                           Analysis_PointLengthMeasurements_2017$Frame, 
                                                           Analysis_PointLengthMeasurements_2017$Time..mins.,
                                                           Analysis_PointLengthMeasurements_2017$Length..mm., 
                                                           Analysis_PointLengthMeasurements_2017$Precision..mm., 
                                                           Analysis_PointLengthMeasurements_2017$RMS..mm.,
                                                           Analysis_PointLengthMeasurements_2017$Range..mm., 
                                                           Analysis_PointLengthMeasurements_2017$Direction..deg.,
                                                           Analysis_PointLengthMeasurements_2017$Horz..Dir...deg., 
                                                           Analysis_PointLengthMeasurements_2017$Vert..Dir...deg.,
                                                           Analysis_PointLengthMeasurements_2017$Mid.X..mm.,
                                                           Analysis_PointLengthMeasurements_2017$Mid.Y..mm.,
                                                           Analysis_PointLengthMeasurements_2017$Mid.Z..mm.,
                                                           Analysis_PointLengthMeasurements_2017$TapeReader,
                                                           Analysis_PointLengthMeasurements_2017$Code,
                                                           Analysis_PointLengthMeasurements_2017$Stage,
                                                           Analysis_PointLengthMeasurements_2017$Comment,
                                                           Analysis_PointLengthMeasurements_2017$Filename,
                                                           Analysis_PointLengthMeasurements_2017$Cameras,
                                                           Analysis_PointLengthMeasurements_2017$Latitude,
                                                           Analysis_PointLengthMeasurements_2017$Longitude,
                                                           Analysis_PointLengthMeasurements_2017$Date,
                                                           Analysis_PointLengthMeasurements_2017$Time,
                                                           Analysis_PointLengthMeasurements_2017$Location.x,
                                                           Analysis_PointLengthMeasurements_2017$Status,
                                                           Analysis_PointLengthMeasurements_2017$Site,
                                                           Analysis_PointLengthMeasurements_2017$Depth,
                                                           Analysis_PointLengthMeasurements_2017$Observer,
                                                           Analysis_PointLengthMeasurements_2017$Successful,
                                                           Analysis_PointLengthMeasurements_2017$EstimatedVisibility,
                                                           Analysis_PointLengthMeasurements_2017$CampaignID.x)
colnames(common_Analysis_PointLengthMeasurements_2017) <-  c("Sample",
                                                             "Sample.No",
                                                             "Family", 
                                                             "Genus_species",
                                                             "Count", 
                                                             "Length", 
                                                             "Others",
                                                             "Frame", 
                                                             "Time..mins.",
                                                             "Length..mm.", 
                                                             "Precision..mm.", 
                                                             "RMS..mm.",
                                                             "Range..mm.", 
                                                             "Direction..deg.",
                                                             "Horz..Dir...deg.", 
                                                             "Vert..Dir...deg.",
                                                             "Mid.X..mm.",
                                                             "Mid.Y..mm.",
                                                             "Mid.Z..mm.",
                                                             "TapeReader",
                                                             "Code",
                                                             "Stage",
                                                             "Comment",
                                                             "Filename",
                                                             "Cameras",
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
                                                             "CampaignID")

common_Analysis_PointLengthMeasurements_2013 <- data.frame(Analysis_PointLengthMeasurements_2013$Sample,
                                                           Analysis_PointLengthMeasurements_2013$Sample.No,
                                                           Analysis_PointLengthMeasurements_2013$Family, 
                                                           Analysis_PointLengthMeasurements_2013$Genus_species,
                                                           Analysis_PointLengthMeasurements_2013$Count, 
                                                           Analysis_PointLengthMeasurements_2013$Length, 
                                                           Analysis_PointLengthMeasurements_2013$Others.x,
                                                           Analysis_PointLengthMeasurements_2013$Frame, 
                                                           Analysis_PointLengthMeasurements_2013$Time..mins.,
                                                           Analysis_PointLengthMeasurements_2013$Length..mm., 
                                                           Analysis_PointLengthMeasurements_2013$Precision..mm., 
                                                           Analysis_PointLengthMeasurements_2013$RMS.Pt..1..mm.,
                                                           Analysis_PointLengthMeasurements_2013$Range..mm., 
                                                           Analysis_PointLengthMeasurements_2013$Direction..degrees.,
                                                           Analysis_PointLengthMeasurements_2013$Horz..Dir...degrees., 
                                                           Analysis_PointLengthMeasurements_2013$Vert..Dir...degrees.,
                                                           NA,
                                                           NA,
                                                           NA,
                                                           Analysis_PointLengthMeasurements_2013$TapeReader,
                                                           Analysis_PointLengthMeasurements_2013$Code,
                                                           Analysis_PointLengthMeasurements_2013$Stage,
                                                           Analysis_PointLengthMeasurements_2013$Comment,
                                                           Analysis_PointLengthMeasurements_2013$Filename,
                                                           Analysis_PointLengthMeasurements_2013$Cameras,
                                                           Analysis_PointLengthMeasurements_2013$Latitude,
                                                           Analysis_PointLengthMeasurements_2013$Longitude,
                                                           Analysis_PointLengthMeasurements_2013$Date,
                                                           Analysis_PointLengthMeasurements_2013$Time,
                                                           Analysis_PointLengthMeasurements_2013$Location.x,
                                                           Analysis_PointLengthMeasurements_2013$Status,
                                                           Analysis_PointLengthMeasurements_2013$Site,
                                                           Analysis_PointLengthMeasurements_2013$Depth,
                                                           Analysis_PointLengthMeasurements_2013$Observer,
                                                           Analysis_PointLengthMeasurements_2013$Successful,
                                                           Analysis_PointLengthMeasurements_2013$EstimatedVisibility,
                                                           Analysis_PointLengthMeasurements_2013$CampaignID.x)
colnames(common_Analysis_PointLengthMeasurements_2013) <-  c("Sample",
                                                             "Sample.No",
                                                             "Family", 
                                                             "Genus_species",
                                                             "Count", 
                                                             "Length", 
                                                             "Others",
                                                             "Frame", 
                                                             "Time..mins.",
                                                             "Length..mm.", 
                                                             "Precision..mm.", 
                                                             "RMS..mm.",
                                                             "Range..mm.", 
                                                             "Direction..deg.",
                                                             "Horz..Dir...deg.", 
                                                             "Vert..Dir...deg.",
                                                             "Mid.X..mm.",
                                                             "Mid.Y..mm.",
                                                             "Mid.Z..mm.",
                                                             "TapeReader",
                                                             "Code",
                                                             "Stage",
                                                             "Comment",
                                                             "Filename",
                                                             "Cameras",
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
                                                             "CampaignID")



#### Combine All Years ####

totalMaxN <- rbind(common_Analysis_MaxN_2018, common_Analysis_MaxN_2017, common_Analysis_MaxN_2013)
totalPointLengthMeasurements <- rbind(common_Analysis_PointLengthMeasurements_2018, common_Analysis_PointLengthMeasurements_2017, common_Analysis_PointLengthMeasurements_2013)


# Make Unique Site Codes For Each Drop (Site Code needs to include Year of sampling and Site Code) (Needed as some sites were sampled multiple times throughout the years)

Honours_Common_Site_Codes <- read.table("Honours_Site_Codes_Common13_17_18.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) # Common Codes as Site Codes were different for 2013 compared to 2017 and 2018

totalMaxN$ID <- paste(totalMaxN$Sample.No, totalMaxN$CampaignID, sep="_")
totalPointLengthMeasurements$ID <- paste(totalPointLengthMeasurements$Sample.No, totalPointLengthMeasurements$CampaignID, sep="_")

temp.totalMaxN <- (merge(totalMaxN, Honours_Common_Site_Codes, by = 'ID', sort = FALSE))
temp.totalPointLengthMeasurement <- (merge(totalPointLengthMeasurements, Honours_Common_Site_Codes, by = 'ID', sort = FALSE))

temp.totalMaxN$HonoursID <- paste(temp.totalMaxN$HonoursSiteCode, temp.totalMaxN$CampaignID, sep="_")
temp.totalPointLengthMeasurement$HonoursID <- paste(temp.totalPointLengthMeasurement$HonoursSiteCode, temp.totalPointLengthMeasurement$CampaignID, sep="_")

totalMaxN <- data.frame(temp.totalMaxN$HonoursID,
                        temp.totalMaxN$HonoursSiteCode,
                        temp.totalMaxN$Sample,
                        temp.totalMaxN$Sample.No,
                        temp.totalMaxN$Family,
                        temp.totalMaxN$Genus_species,
                        temp.totalMaxN$Count,
                        temp.totalMaxN$Others,
                        temp.totalMaxN$Comment,
                        temp.totalMaxN$Cams,
                        temp.totalMaxN$Filename,
                        temp.totalMaxN$`Time_MaxN(mins)`,
                        temp.totalMaxN$sp_Code, 
                        temp.totalMaxN$Tape_Reader, 
                        temp.totalMaxN$Has_Fed, 
                        temp.totalMaxN$Latitude,
                        temp.totalMaxN$Longitude,
                        temp.totalMaxN$Date,
                        temp.totalMaxN$Time,
                        temp.totalMaxN$Location,
                        temp.totalMaxN$Status,
                        temp.totalMaxN$Site,
                        temp.totalMaxN$Depth,
                        temp.totalMaxN$Observer,
                        temp.totalMaxN$Successful,
                        temp.totalMaxN$EstimatedVisibility,
                        temp.totalMaxN$CampaignID)
colnames(totalMaxN) <-  c("HonoursID",
                          "HonoursSiteCode",
                          "Sample",
                          "Sample.No",
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
                          "CampaignID")

totalPointLengthMeasurements <- data.frame(temp.totalPointLengthMeasurement$HonoursID,
                                           temp.totalPointLengthMeasurement$HonoursSiteCode,
                                           temp.totalPointLengthMeasurement$Sample,
                                           temp.totalPointLengthMeasurement$Sample.No,
                                           temp.totalPointLengthMeasurement$Family, 
                                           temp.totalPointLengthMeasurement$Genus_species,
                                           temp.totalPointLengthMeasurement$Count, 
                                           temp.totalPointLengthMeasurement$Length, 
                                           temp.totalPointLengthMeasurement$Others,
                                           temp.totalPointLengthMeasurement$Frame, 
                                           temp.totalPointLengthMeasurement$Time..mins.,
                                           temp.totalPointLengthMeasurement$Length..mm.,
                                           temp.totalPointLengthMeasurement$Precision..mm.,
                                           temp.totalPointLengthMeasurement$RMS..mm.,
                                           temp.totalPointLengthMeasurement$Range..mm.,
                                           temp.totalPointLengthMeasurement$Direction..deg.,
                                           temp.totalPointLengthMeasurement$Horz..Dir...deg., 
                                           temp.totalPointLengthMeasurement$Vert..Dir...deg.,
                                           temp.totalPointLengthMeasurement$Mid.X..mm.,
                                           temp.totalPointLengthMeasurement$Mid.Y..mm.,
                                           temp.totalPointLengthMeasurement$Mid.Z..mm.,
                                           temp.totalPointLengthMeasurement$TapeReader,
                                           temp.totalPointLengthMeasurement$Code,
                                           temp.totalPointLengthMeasurement$Stage,
                                           temp.totalPointLengthMeasurement$Comment,
                                           temp.totalPointLengthMeasurement$Filename,
                                           temp.totalPointLengthMeasurement$Cameras,
                                           temp.totalPointLengthMeasurement$Latitude,
                                           temp.totalPointLengthMeasurement$Longitude,
                                           temp.totalPointLengthMeasurement$Date,
                                           temp.totalPointLengthMeasurement$Time,
                                           temp.totalPointLengthMeasurement$Location,
                                           temp.totalPointLengthMeasurement$Status,
                                           temp.totalPointLengthMeasurement$Site,
                                           temp.totalPointLengthMeasurement$Depth,
                                           temp.totalPointLengthMeasurement$Observer,
                                           temp.totalPointLengthMeasurement$EstimatedVisibility,
                                           temp.totalPointLengthMeasurement$CampaignID)
colnames(totalPointLengthMeasurements) <-  c("HonoursID",
                                             "HonoursSiteCode",
                                             "Original.Sample.Code",
                                             "Original.Sample.No",
                                             "Family", 
                                             "Genus_species",
                                             "Count", 
                                             "Length", 
                                             "Others",
                                             "Frame", 
                                             "Time..mins.",
                                             "Length..mm.",
                                             "Precision..mm.",
                                             "RMS..mm.",
                                             "Range..mm.",
                                             "Direction..deg.",
                                             "Horz..Dir...deg.", 
                                             "Vert..Dir...deg.",
                                             "Mid.X..mm.",
                                             "Mid.Y..mm.",
                                             "Mid.Z..mm.",
                                             "TapeReader",
                                             "Code",
                                             "Stage",
                                             "Comment",
                                             "Filename",
                                             "Cameras",
                                             "Latitude",
                                             "Longitude",
                                             "Date",
                                             "Time",
                                             "Location",
                                             "Status",
                                             "Site",
                                             "Depth",
                                             "Observer",
                                             "EstimatedVisibility",
                                             "CampaignID")

# Change Date format
totalMaxN$Date <- as.Date(totalMaxN$Date)
unique(totalMaxN$Date)

totalPointLengthMeasurements$Date <- as.Date(totalPointLengthMeasurements$Date)
unique(totalPointLengthMeasurements$Date)


#### Write Text Files ####

write.table(totalMaxN, file = Analysis_MaxN , sep = "\t", row.names = FALSE)

write.table(totalPointLengthMeasurements, file = Analysis_3DPointLengthMeasurements , sep = "\t", row.names = FALSE)
