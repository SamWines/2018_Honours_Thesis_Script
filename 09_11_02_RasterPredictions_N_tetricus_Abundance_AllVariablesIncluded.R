# Script for predicting rasters of best performing GAMs

# This script is for modelling abundance of N. tetricus

# This script is used to:
# 1. Split data into training and test
# 2. Predict rasters for each year sampled

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 14 September 2018

# Libraries required
library(plyr)
library(dplyr)
library(mgcv)
library(ROCR)
library(nlme)
library(MuMIn)
library(gamm4)
library(lme4)
library(ggplot2)
library(GGally)
library(corrplot)
library(raster)
library(rgdal)


# Set working directory
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/11_Modelling_Subsets/02_BrokenDownIntoScales/")
getwd()
dir()

# Import Files
Import.5m.Site.Summaries <- read.table("01_5m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.10m.Site.Summaries <- read.table("01_10m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.25m.Site.Summaries <- read.table("01_25m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.50m.Site.Summaries <- read.table("01_50m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.75m.Site.Summaries <- read.table("01_75m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.100m.Site.Summaries <- read.table("01_100m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.150m.Site.Summaries <- read.table("01_150m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.200m.Site.Summaries <- read.table("01_200m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.250m.Site.Summaries <- read.table("01_250m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.300m.Site.Summaries <- read.table("01_300m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.400m.Site.Summaries <- read.table("01_400m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.500m.Site.Summaries <- read.table("01_500m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 


Training.Test <- read.table("01_Training_Test_Split.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 

Import.5m.Site.Summaries <- (merge(Import.5m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.10m.Site.Summaries <- (merge(Import.10m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.25m.Site.Summaries <- (merge(Import.25m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.50m.Site.Summaries <- (merge(Import.50m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.75m.Site.Summaries <- (merge(Import.75m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.100m.Site.Summaries <- (merge(Import.100m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.150m.Site.Summaries <- (merge(Import.150m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.200m.Site.Summaries <- (merge(Import.200m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.250m.Site.Summaries <- (merge(Import.250m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.300m.Site.Summaries <- (merge(Import.300m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.400m.Site.Summaries <- (merge(Import.400m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.500m.Site.Summaries <- (merge(Import.500m.Site.Summaries, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))

# Reset working directory for outputs
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/02_N.tetricus/")
getwd()
dir()


#Import.25m.Site.Summaries$Year <- as.factor(Import.25m.Site.Summaries$Year)
#Year <- as.factor(Import.25m.Site.Summaries$Year)

#Import.25m.Site.Summaries$Status <- as.factor(Import.25m.Site.Summaries$Status)
#Status <- as.factor(Import.25m.Site.Summaries$Status)


#  25m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.5m.N.tetricus.Abundance <- Import.5m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_25_clip_01, 
                #slope_mean_25_clip_01, 
                rugosity_mean_5_clip_01, 
                #profile_curv_mean_25_clip_01, 
                #plan_curv_mean_25_clip_01,
                northness_mean_5_clip_01,
                #fbpi25_50_mean_25_clip_01,
                fbpi10_30_mean_5_clip_01,
                eastness_mean_5_clip_01,
                curvature_mean_5_clip_01,
                #bbpi100_200_mean_25_clip_01,
                bathy_mean_5_clip_01,
                #bathy_std_25_clip_01,
                #avg_wave_orb_vel_mean_25_clip_161,
                #SST_mean_25_clip_01,
                dist2reef_mean_5_clip_01
  )

TestData.5m.C.auratus.Abundance <- Import.5m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_5_clip_01, 
                slope_mean_55_clip_01, 
                rugosity_mean_5_clip_01, 
                profile_curv_mean_5_clip_01, 
                plan_curv_mean_5_clip_01,
                northness_mean_5_clip_01,
                fbpi25_50_mean_5_clip_01,
                fbpi10_30_mean_5_clip_01,
                eastness_mean_5_clip_01,
                curvature_mean_5_clip_01,
                bbpi100_200_mean_5_clip_01,
                bathy_mean_5_clip_01,
                bathy_std_5_clip_01,
                avg_wave_orb_vel_mean_5_clip_01,
                #SST_mean_25_clip_01,
                dist2reef_mean_5_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.5m.N.tetricus.Abundance[, 2:length(TrainingData.5m.N.tetricus.Abundance)], geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.5m.N.tetricus.Abundance)

#Check Response Variable

plot(TrainingData.5m.N.tetricus.Abundance$N.tetricus.Abundance)


### Transformations
# Example: TrainingData.25m.C.auratus.Abundance$sqrt.rugosity_mean_25_clip_01=sqrt(TrainingData.25m.C.auratus.Abundance$rugosity_mean_25_clip_01)



# Models loop

data.5m <- TrainingData.5m.N.tetricus.Abundance


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html






## Create Raster Stacks for each year

getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/02_N.tetricus/Abundance_SiteExc/Year_2013_Rasters/")
getwd()
dir()

#make a list of the rasters in the working directory
dir()

raster.list.2013 <- list.files(pattern="*.asc")
raster.list.2013

#create a stack of rasters
raster.stack.2013 <- stack(raster.list.2013)


getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/02_N.tetricus/Abundance_SiteExc/Year_2017_Rasters/")
getwd()
dir()

#make a list of the rasters in the working directory
dir()

raster.list.2017 <- list.files(pattern="*.asc")
raster.list.2017

#create a stack of rasters
raster.stack.2017 <- stack(raster.list.2017)


getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/02_N.tetricus/Abundance_SiteExc/Year_2018_Rasters/")
getwd()
dir()

#make a list of the rasters in the working directory
dir()

raster.list.2018 <- list.files(pattern="*.asc")
raster.list.2018

#create a stack of rasters
raster.stack.2018 <- stack(raster.list.2018)


# Reset Directory for Outputs
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/02_N.tetricus/Abundance_SiteExc/Output_Rasters/")
getwd()
dir()

# RunModels



testmod.Year <- gam(N.tetricus.Abundance ~ Year + s(fbpi10_30_mean_5_clip_01, k = 9) + s(eastness_mean_5_clip_01, k = 9) + s(bathy_mean_5_clip_01, k = 9) + s(dist2reef_mean_5_clip_01, k = 9), data = data.5m)


test2013 <- predict(raster.stack.2013, testmod.Year, filename='prediction2013', progress='text', type='response', format='ascii', overwrite =TRUE)

test2017 <- predict(raster.stack.2017, testmod.Year, filename='prediction2017', progress='text', type='response', format='ascii', overwrite =TRUE)

test2018 <- predict(raster.stack.2018, testmod.Year, filename='prediction2018', progress='text', type='response', format='ascii', overwrite =TRUE)

plot(test2018)

