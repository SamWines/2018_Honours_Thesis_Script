# Script for Taking TransectMeasure (Langlois Script) Products and Formatting for Use With Broad Habitat Groups

# This script is used to:
# 1. Combine TransectMeasure output from multiple years
# 2. From Habitat % Cover, split data into broad habitat groups

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 6 September 2018

rm(list=ls())

# Set working directory for input files
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/07_HabitatData/Step2") # Set folder that contains raw data file and that you want to write output file to
getwd()
list.files()

Honours_Common_Site_Codes <- read.table("Honours_Site_Codes_Common13_17_18.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) # Common Codes as Site Codes were different for 2013 compared to 2017 and 2018



#### Split 2018 Habitat Classifications into Broad Habitat Groups ####

InputFile_2018 <- "x_PointAddis2018_R_habitat.percent.cover.and.mean.and.sd.of.relief.txt"
OutputFile_2018 <- "2018_Addis_Habitat.txt"

# Set working directory for input files
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/07_HabitatData/Step2") # Set folder that contains raw data file and that you want to write output file to
getwd()
list.files()

# Load TransectMeasure (Langlois Script) Output Files
Input_2018 <- read.table(InputFile_2018, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Format Habitat Dataframe
Input_2018$CampaignID <- paste("2018-03-PA-BRUV")
Input_2018$Sample.No <- substring(Input_2018$site.., 4)
Input_2018$Sample.No <- as.numeric(as.character(Input_2018$Sample.No))
Input_2018$ID <- paste(Input_2018$Sample.No, Input_2018$CampaignID, sep="_")
Input_2018 <- (merge(Input_2018, Honours_Common_Site_Codes, by = 'ID', sort = FALSE))
Input_2018$Honours_Code <- paste(Input_2018$Honours_Code, Input_2018$CampaignID, sep="_")

# Make columms to specify vegetation or sediment
Input_2018$Vegetation <- Input_2018$Biota..Invertebrate.complex + Input_2018$Biota..Macroalgae
Input_2018$Sediment <- Input_2018$Biota..Unconsolidated

# Create and format output file containing broad habitat characteristics
Output_2018 <- data.frame(Input_2018$Honours_Code,Input_2018$site..)
colnames(Output_2018)[1] <- "HonoursID"
colnames(Output_2018)[2] <- "OriginalSample"
Output_2018$Relief <- ifelse(Input_2018$mean.relief>=1, "Complex", "Flat")
Output_2018$Sponge <- ifelse(Input_2018$Biota..Invertebrate.complex>0, "Sponge", "NoSponge")
Output_2018$Macroalgae <- ifelse(Input_2018$Biota..Macroalgae>0, "Macroalgae", "NoMacroalgae")
Output_2018$Reef <- ifelse(Input_2018$Biota..Reef>0, "Reef", "NoReef")
Output_2018$Vegetation <- ifelse(Input_2018$Vegetation>0, "Vegetation", "NoVegetation")
Output_2018$Sediment <- ifelse(Input_2018$Sediment>0, "Sediment", "NotSediment")
Output_2018$LimitedViz <- ifelse(Input_2018$fieldofview.Limited>0.1, "LimitedViz", "ClearViz")
Output_2018$Year <- paste("2018")



#### Split 2017 Habitat Classifications into Broad Habitat Groups ####

InputFile_2017 <- "x_PointAddis2017_R_habitat.percent.cover.and.mean.and.sd.of.relief.txt"
OutputFile_2017 <- "2017_Addis_Habitat.txt"

# Set working directory for input files
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/07_HabitatData/Step2") # Set folder that contains raw data file and that you want to write output file to
getwd()
list.files()

# Load TransectMeasure (Langlois Script) Output Files
Input_2017 <- read.table(InputFile_2017, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Format Habitat Dataframe
Input_2017$CampaignID <- paste("2017-03-PA-BRUV")
Input_2017$Sample.No <- substring(Input_2017$Sample, 3)
Input_2017$Sample.No <- as.numeric(as.character(Input_2017$Sample.No))
Input_2017$ID <- paste(Input_2017$Sample.No, Input_2017$CampaignID, sep="_")
Input_2017 <- (merge(Input_2017, Honours_Common_Site_Codes, by = 'ID', sort = FALSE))
Input_2017$Honours_Code <- paste(Input_2017$Honours_Code, Input_2017$CampaignID, sep="_")

# Make columms to specify vegetation or sediment
Input_2017$Vegetation <- Input_2017$Biota..Invertebrate.complex + Input_2017$Biota..Macroalgae
Input_2017$Sediment <- Input_2017$Biota..Unconsolidated

# Create and format output file containing broad habitat characteristics
Output_2017 <- data.frame(Input_2017$Honours_Code,Input_2017$Sample)
colnames(Output_2017)[1] <- "HonoursID"
colnames(Output_2017)[2] <- "OriginalSample"
Output_2017$Relief <- ifelse(Input_2017$mean.relief>=1, "Complex", "Flat")
Output_2017$Sponge <- ifelse(Input_2017$Biota..Invertebrate.complex>0, "Sponge", "NoSponge")
Output_2017$Macroalgae <- ifelse(Input_2017$Biota..Macroalgae>0, "Macroalgae", "NoMacroalgae")
Output_2017$Reef <- ifelse(Input_2017$Biota..Reef>0, "Reef", "NoReef")
Output_2017$Vegetation <- ifelse(Input_2017$Vegetation>0, "Vegetation", "NoVegetation")
Output_2017$Sediment <- ifelse(Input_2017$Sediment>0, "Sediment", "NotSediment")
Output_2017$LimitedViz <- ifelse(Input_2017$fieldofview.Limited>0.1, "LimitedViz", "ClearViz")
Output_2017$Year <- paste("2017")



#### Split 2013 Habitat Classifications into Broad Habitat Groups ####

InputFile_2013 <- "x_PointAddis2013_R_habitat.percent.cover.and.mean.and.sd.of.relief.txt"
OutputFile_2013 <- "2013_Addis_Habitat.txt"

# Set working directory for input files
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/07_HabitatData/Step2") # Set folder that contains raw data file and that you want to write output file to
getwd()
list.files()

# Load TransectMeasure (Langlois Script) Output Files
Input_2013 <- read.table(InputFile_2013, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# Format Habitat Dataframe
Input_2013$CampaignID <- paste("2013-04-PA-BRUV")
Input_2013$Sample.No <- substring(Input_2013$OpCode, 12)
Input_2013$Sample.No <- as.numeric(gsub("([0-9]+).*$", "\\1", Input_2013$Sample.No)) #Remove letters from sample number
Input_2013$Sample.No <- as.numeric(as.character(Input_2013$Sample.No))
Input_2013$ID <- paste(Input_2013$Sample.No, Input_2013$CampaignID, sep="_")
Input_2013 <- (merge(Input_2013, Honours_Common_Site_Codes, by = 'ID', sort = FALSE))
Input_2013$Honours_Code <- paste(Input_2013$Honours_Code, Input_2013$CampaignID, sep="_")

# Make columms to specify vegetation or sediment
Input_2013$Vegetation <- Input_2013$Biota..Invertebrate.complex + Input_2013$Biota..Macroalgae
Input_2013$Sediment <- Input_2013$Biota..Unconsolidated

# Create and format output file containing broad habitat characteristics
Output_2013 <- data.frame(Input_2013$Honours_Code,Input_2013$Sample)
colnames(Output_2013)[1] <- "HonoursID"
colnames(Output_2013)[2] <- "OriginalSample"
Output_2013$Relief <- ifelse(Input_2013$mean.relief>=1, "Complex", "Flat")
Output_2013$Sponge <- ifelse(Input_2013$Biota..Invertebrate.complex>0, "Sponge", "NoSponge")
Output_2013$Macroalgae <- ifelse(Input_2013$Biota..Macroalgae>0, "Macroalgae", "NoMacroalgae")
Output_2013$Reef <- ifelse(Input_2013$Biota..Reef>0, "Reef", "NoReef")
Output_2013$Vegetation <- ifelse(Input_2013$Vegetation>0, "Vegetation", "NoVegetation")
Output_2013$Sediment <- ifelse(Input_2013$Sediment>0, "Sediment", "NotSediment")
Output_2013$LimitedViz <- ifelse(Input_2013$fieldofview.Limited>0.1, "LimitedViz", "ClearViz")
Output_2013$Year <- paste("2013")



#### Combine site habitat characteristics for all years ####

OutputFile <- "Analysis_Habitat.txt"
Analysis_Habitat <- rbind(Output_2013, Output_2017, Output_2018)



#### Export combined site habitat characteristics for all years ####

# Reset working directory for output files
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/07_HabitatData/Step2") # Set folder that contains raw data file and that you want to write output file to
getwd()
list.files()

# Export table
write.table(Analysis_Habitat, file = OutputFile , sep = "\t", row.names = FALSE)

