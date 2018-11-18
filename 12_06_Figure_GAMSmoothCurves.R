# Script for producing GAM smooth curves for predictor variables from the best performing models in the study

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

Training.Test <- read.table("01_Training_Test_Split.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 


# Import Files
Import.5m.Site.Summaries.ENTIRE <- read.table("01_5m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.10m.Site.Summaries.ENTIRE <- read.table("01_10m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.25m.Site.Summaries.ENTIRE <- read.table("01_25m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.50m.Site.Summaries.ENTIRE <- read.table("01_50m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.75m.Site.Summaries.ENTIRE <- read.table("01_75m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.100m.Site.Summaries.ENTIRE <- read.table("01_100m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.150m.Site.Summaries.ENTIRE <- read.table("01_150m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.200m.Site.Summaries.ENTIRE <- read.table("01_200m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.250m.Site.Summaries.ENTIRE <- read.table("01_250m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.300m.Site.Summaries.ENTIRE <- read.table("01_300m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.400m.Site.Summaries.ENTIRE <- read.table("01_400m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.500m.Site.Summaries.ENTIRE <- read.table("01_500m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 

getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/11_Modelling_Subsets/04_Sediment/")
getwd()
dir()


Import.5m.Site.Summaries.SEDIMENT <- read.table("01_5m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.10m.Site.Summaries.SEDIMENT <- read.table("01_10m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.25m.Site.Summaries.SEDIMENT <- read.table("01_25m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.50m.Site.Summaries.SEDIMENT <- read.table("01_50m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.75m.Site.Summaries.SEDIMENT <- read.table("01_75m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.100m.Site.Summaries.SEDIMENT <- read.table("01_100m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.150m.Site.Summaries.SEDIMENT <- read.table("01_150m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.200m.Site.Summaries.SEDIMENT <- read.table("01_200m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.250m.Site.Summaries.SEDIMENT <- read.table("01_250m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.300m.Site.Summaries.SEDIMENT <- read.table("01_300m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.400m.Site.Summaries.SEDIMENT <- read.table("01_400m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.500m.Site.Summaries.SEDIMENT <- read.table("01_500m_SiteSummariesSEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 


getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/11_Modelling_Subsets/03_Reef/")
getwd()
dir()


Import.5m.Site.Summaries.REEF <- read.table("01_5m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.10m.Site.Summaries.REEF <- read.table("01_10m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.25m.Site.Summaries.REEF <- read.table("01_25m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.50m.Site.Summaries.REEF <- read.table("01_50m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.75m.Site.Summaries.REEF <- read.table("01_75m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.100m.Site.Summaries.REEF <- read.table("01_100m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.150m.Site.Summaries.REEF <- read.table("01_150m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.200m.Site.Summaries.REEF <- read.table("01_200m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.250m.Site.Summaries.REEF <- read.table("01_250m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.300m.Site.Summaries.REEF <- read.table("01_300m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.400m.Site.Summaries.REEF <- read.table("01_400m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.500m.Site.Summaries.REEF <- read.table("01_500m_SiteSummariesREEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 


getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/11_Modelling_Subsets/06_Macroalgae/")
getwd()
dir()


Import.5m.Site.Summaries.MACROALGAE <- read.table("01_5m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.10m.Site.Summaries.MACROALGAE <- read.table("01_10m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.25m.Site.Summaries.MACROALGAE <- read.table("01_25m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.50m.Site.Summaries.MACROALGAE <- read.table("01_50m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.75m.Site.Summaries.MACROALGAE <- read.table("01_75m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.100m.Site.Summaries.MACROALGAE <- read.table("01_100m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.150m.Site.Summaries.MACROALGAE <- read.table("01_150m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.200m.Site.Summaries.MACROALGAE <- read.table("01_200m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.250m.Site.Summaries.MACROALGAE <- read.table("01_250m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.300m.Site.Summaries.MACROALGAE <- read.table("01_300m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.400m.Site.Summaries.MACROALGAE <- read.table("01_400m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.500m.Site.Summaries.MACROALGAE <- read.table("01_500m_SiteSummariesMACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 


getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/11_Modelling_Subsets/05_Sponge/")
getwd()
dir()



Import.5m.Site.Summaries.SPONGE <- read.table("01_5m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.10m.Site.Summaries.SPONGE <- read.table("01_10m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.25m.Site.Summaries.SPONGE <- read.table("01_25m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.50m.Site.Summaries.SPONGE <- read.table("01_50m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.75m.Site.Summaries.SPONGE <- read.table("01_75m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.100m.Site.Summaries.SPONGE <- read.table("01_100m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.150m.Site.Summaries.SPONGE <- read.table("01_150m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.200m.Site.Summaries.SPONGE <- read.table("01_200m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.250m.Site.Summaries.SPONGE <- read.table("01_250m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.300m.Site.Summaries.SPONGE <- read.table("01_300m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.400m.Site.Summaries.SPONGE <- read.table("01_400m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.500m.Site.Summaries.SPONGE <- read.table("01_500m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 



# Merge Training Data

Import.5m.Site.Summaries.ENTIRE <- (merge(Import.5m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.10m.Site.Summaries.ENTIRE <- (merge(Import.10m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.25m.Site.Summaries.ENTIRE <- (merge(Import.25m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.50m.Site.Summaries.ENTIRE <- (merge(Import.50m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.75m.Site.Summaries.ENTIRE <- (merge(Import.75m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.100m.Site.Summaries.ENTIRE <- (merge(Import.100m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.150m.Site.Summaries.ENTIRE <- (merge(Import.150m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.200m.Site.Summaries.ENTIRE <- (merge(Import.200m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.250m.Site.Summaries.ENTIRE <- (merge(Import.250m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.300m.Site.Summaries.ENTIRE <- (merge(Import.300m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.400m.Site.Summaries.ENTIRE <- (merge(Import.400m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.500m.Site.Summaries.ENTIRE <- (merge(Import.500m.Site.Summaries.ENTIRE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))

Import.5m.Site.Summaries.SEDIMENT <- (merge(Import.5m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.10m.Site.Summaries.SEDIMENT <- (merge(Import.10m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.25m.Site.Summaries.SEDIMENT <- (merge(Import.25m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.50m.Site.Summaries.SEDIMENT <- (merge(Import.50m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.75m.Site.Summaries.SEDIMENT <- (merge(Import.75m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.100m.Site.Summaries.SEDIMENT <- (merge(Import.100m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.150m.Site.Summaries.SEDIMENT <- (merge(Import.150m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.200m.Site.Summaries.SEDIMENT <- (merge(Import.200m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.250m.Site.Summaries.SEDIMENT <- (merge(Import.250m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.300m.Site.Summaries.SEDIMENT <- (merge(Import.300m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.400m.Site.Summaries.SEDIMENT <- (merge(Import.400m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.500m.Site.Summaries.SEDIMENT <- (merge(Import.500m.Site.Summaries.SEDIMENT, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))

Import.5m.Site.Summaries.REEF <- (merge(Import.5m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.10m.Site.Summaries.REEF <- (merge(Import.10m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.25m.Site.Summaries.REEF <- (merge(Import.25m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.50m.Site.Summaries.REEF <- (merge(Import.50m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.75m.Site.Summaries.REEF <- (merge(Import.75m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.100m.Site.Summaries.REEF <- (merge(Import.100m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.150m.Site.Summaries.REEF <- (merge(Import.150m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.200m.Site.Summaries.REEF <- (merge(Import.200m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.250m.Site.Summaries.REEF <- (merge(Import.250m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.300m.Site.Summaries.REEF <- (merge(Import.300m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.400m.Site.Summaries.REEF <- (merge(Import.400m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.500m.Site.Summaries.REEF <- (merge(Import.500m.Site.Summaries.REEF, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))

Import.5m.Site.Summaries.MACROALGAE <- (merge(Import.5m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.10m.Site.Summaries.MACROALGAE <- (merge(Import.10m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.25m.Site.Summaries.MACROALGAE <- (merge(Import.25m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.50m.Site.Summaries.MACROALGAE <- (merge(Import.50m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.75m.Site.Summaries.MACROALGAE <- (merge(Import.75m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.100m.Site.Summaries.MACROALGAE <- (merge(Import.100m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.150m.Site.Summaries.MACROALGAE <- (merge(Import.150m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.200m.Site.Summaries.MACROALGAE <- (merge(Import.200m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.250m.Site.Summaries.MACROALGAE <- (merge(Import.250m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.300m.Site.Summaries.MACROALGAE <- (merge(Import.300m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.400m.Site.Summaries.MACROALGAE <- (merge(Import.400m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.500m.Site.Summaries.MACROALGAE <- (merge(Import.500m.Site.Summaries.MACROALGAE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))

Import.5m.Site.Summaries.SPONGE <- (merge(Import.5m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.10m.Site.Summaries.SPONGE <- (merge(Import.10m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.25m.Site.Summaries.SPONGE <- (merge(Import.25m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.50m.Site.Summaries.SPONGE <- (merge(Import.50m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.75m.Site.Summaries.SPONGE <- (merge(Import.75m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.100m.Site.Summaries.SPONGE <- (merge(Import.100m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.150m.Site.Summaries.SPONGE <- (merge(Import.150m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.200m.Site.Summaries.SPONGE <- (merge(Import.200m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.250m.Site.Summaries.SPONGE <- (merge(Import.250m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.300m.Site.Summaries.SPONGE <- (merge(Import.300m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.400m.Site.Summaries.SPONGE <- (merge(Import.400m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))
Import.500m.Site.Summaries.SPONGE <- (merge(Import.500m.Site.Summaries.SPONGE, Training.Test, by = "HonoursID", all.x = TRUE, sort = TRUE))


# Reset working directory for outputs
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/15_Figures/06_GAMSmoothCurves/")
getwd()
dir()



#  ENTIRE ASSEMBLAGE 25m    #################################################################################################################################   

# Select Columns for analysis

Data.ENTIRE <- Import.25m.Site.Summaries.ENTIRE %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_25_clip_01, 
                #slope_mean_25_clip_01, 
                rugosity_mean_25_clip_01, 
                #profile_curv_mean_25_clip_01, 
                #plan_curv_mean_25_clip_01,
                northness_mean_25_clip_01,
                #fbpi25_50_mean_25_clip_01,
                fbpi10_30_mean_25_clip_01,
                eastness_mean_25_clip_01,
                curvature_mean_25_clip_01,
                #bbpi100_200_mean_25_clip_01,
                bathy_mean_25_clip_01,
                #bathy_std_25_clip_01,
                #avg_wave_orb_vel_mean_25_clip_161,
                SST_mean_25_clip_01,
                dist2reef_mean_25_clip_01
  )

colnames(Data.ENTIRE) <- c("SpeciesRichness",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "Sea-Surface Temperature",
                           "DistanceToReef")


Model.ENTIRE <- gam(SpeciesRichness ~ s(Rugosity, k = 9) + s(Bathymetry, k = 9) + s(DistanceToReef, k = 9), data = Data.ENTIRE)


# Make GAM Plots
pdf(file = "ENTIRE_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.ENTIRE)
dev.off()



# MOBILE SP REMOVED 25 m

Data.MOBILEREMOVED <- Import.25m.Site.Summaries.ENTIRE %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_25_clip_01, 
                #slope_mean_25_clip_01, 
                rugosity_mean_25_clip_01, 
                #profile_curv_mean_25_clip_01, 
                #plan_curv_mean_25_clip_01,
                northness_mean_25_clip_01,
                #fbpi25_50_mean_25_clip_01,
                fbpi10_30_mean_25_clip_01,
                eastness_mean_25_clip_01,
                curvature_mean_25_clip_01,
                #bbpi100_200_mean_25_clip_01,
                bathy_mean_25_clip_01,
                #bathy_std_25_clip_01,
                #avg_wave_orb_vel_mean_25_clip_161,
                SST_mean_25_clip_01,
                dist2reef_mean_25_clip_01
  )

colnames(Data.MOBILEREMOVED) <- c("SpeciesRichness",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "Sea-Surface Temperature",
                           "DistanceToReef")


Model.MOBILEREMOVED <- gam(SpeciesRichness ~ Year + Status + s(Rugosity, k = 9) + s(Bathymetry, k = 9) + s(DistanceToReef, k = 9), data = Data.MOBILEREMOVED)


# Make GAM Plots
pdf(file = "MOBILEREMOVED_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.MOBILEREMOVED)
dev.off()


#  REEF 25m    #################################################################################################################################   

# Select Columns for analysis

Data.REEF <- Import.10m.Site.Summaries.REEF %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_25_clip_01, 
                #slope_mean_25_clip_01, 
                rugosity_mean_10_clip_01, 
                #profile_curv_mean_25_clip_01, 
                #plan_curv_mean_25_clip_01,
                northness_mean_10_clip_01,
                #fbpi25_50_mean_25_clip_01,
                fbpi10_30_mean_10_clip_01,
                eastness_mean_10_clip_01,
                curvature_mean_10_clip_01,
                #bbpi100_200_mean_25_clip_01,
                bathy_mean_10_clip_01,
                #bathy_std_25_clip_01,
                #avg_wave_orb_vel_mean_25_clip_161,
                SST_mean_10_clip_01,
                dist2reef_mean_10_clip_01
  )

colnames(Data.REEF) <- c("SpeciesRichness",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "Sea-Surface Temperature",
                           "DistanceToReef")


Model.REEF <- gam(SpeciesRichness ~ Year + Status + s(Rugosity, k = 9) + s(Bathymetry, k = 9) + s(DistanceToReef, k = 9), data = Data.REEF)


# Make GAM Plots
pdf(file = "REEF_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.REEF)
dev.off()

#  SEDIMENT 25m    #################################################################################################################################   

# Select Columns for analysis

Data.SEDIMENT <- Import.50m.Site.Summaries.SEDIMENT %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_25_clip_01, 
                #slope_mean_25_clip_01, 
                rugosity_mean_50_clip_01, 
                #profile_curv_mean_25_clip_01, 
                #plan_curv_mean_25_clip_01,
                northness_mean_50_clip_01,
                #fbpi25_50_mean_25_clip_01,
                fbpi10_30_mean_50_clip_01,
                eastness_mean_50_clip_01,
                curvature_mean_50_clip_01,
                #bbpi100_200_mean_25_clip_01,
                bathy_mean_50_clip_01,
                #bathy_std_25_clip_01,
                #avg_wave_orb_vel_mean_25_clip_161,
                SST_mean_50_clip_01,
                dist2reef_mean_50_clip_01
  )

colnames(Data.SEDIMENT) <- c("SpeciesRichness",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "Sea-Surface Temperature",
                           "DistanceToReef")


Model.SEDIMENT <- gam(SpeciesRichness ~ Year + Status + s(Rugosity, k = 9) + s(Eastness, k = 9) + s(Bathymetry, k = 9) + s(DistanceToReef, k = 9), data = Data.SEDIMENT)


# Make GAM Plots
pdf(file = "SEDIMENT_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.ENTIRE)
dev.off()


#  MACROALGAE 25m    #################################################################################################################################   

# Select Columns for analysis

Data.MACROALGAE <- Import.50m.Site.Summaries.MACROALGAE %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_25_clip_01, 
                #slope_mean_25_clip_01, 
                rugosity_mean_50_clip_01, 
                #profile_curv_mean_25_clip_01, 
                #plan_curv_mean_25_clip_01,
                northness_mean_50_clip_01,
                #fbpi25_50_mean_25_clip_01,
                fbpi10_30_mean_50_clip_01,
                eastness_mean_50_clip_01,
                curvature_mean_50_clip_01,
                #bbpi100_200_mean_25_clip_01,
                bathy_mean_50_clip_01,
                #bathy_std_25_clip_01,
                #avg_wave_orb_vel_mean_25_clip_161,
                SST_mean_50_clip_01,
                dist2reef_mean_50_clip_01
  )

colnames(Data.MACROALGAE) <- c("SpeciesRichness",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "Sea-Surface Temperature",
                           "DistanceToReef")


Model.MACROALGAE <- gam(SpeciesRichness ~ Year + s(Northness, k = 5) + s(FineBPI, k = 5) + s(Curvature, k = 5) + s(DistanceToReef, k = 5), data = Data.MACROALGAE)


# Make GAM Plots
pdf(file = "MACROALGAE_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.MACROALGAE)
dev.off()


#  SPONGE 25m    #################################################################################################################################   

# Select Columns for analysis

Data.SPONGE <- Import.400m.Site.Summaries.SPONGE %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_25_clip_01, 
                #slope_mean_25_clip_01, 
                rugosity_mean_400_clip_01, 
                #profile_curv_mean_25_clip_01, 
                #plan_curv_mean_25_clip_01,
                northness_mean_400_clip_01,
                #fbpi25_50_mean_25_clip_01,
                fbpi10_30_mean_400_clip_01,
                eastness_mean_400_clip_01,
                curvature_mean_400_clip_01,
                #bbpi100_200_mean_25_clip_01,
                bathy_mean_400_clip_01,
                #bathy_std_25_clip_01,
                #avg_wave_orb_vel_mean_25_clip_161,
                SST_mean_400_clip_01,
                dist2reef_mean_400_clip_01
  )


colnames(Data.SPONGE) <- c("SpeciesRichness",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "Sea-Surface Temperature",
                           "DistanceToReef")

Model.SPONGE <- gam(SpeciesRichness ~ s(FineBPI, k = 3) + s(Curvature, k = 3) + s(DistanceToReef, k = 3), data = Data.SPONGE)


# Make GAM Plots
pdf(file = "SPONGE_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.SPONGE)
dev.off()


#  CARNIVORES 100m ####

Data.CARNIVORES <- Import.100m.Site.Summaries.ENTIRE %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness.CARNIVORES,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_100_clip_01, 
                #slope_mean_100_clip_01, 
                rugosity_mean_100_clip_01, 
                #profile_curv_mean_100_clip_01, 
                #plan_curv_mean_100_clip_01,
                northness_mean_100_clip_01,
                #fbpi25_50_mean_100_clip_01,
                fbpi10_30_mean_100_clip_01,
                eastness_mean_100_clip_01,
                curvature_mean_100_clip_01,
                #bbpi100_200_mean_100_clip_01,
                bathy_mean_100_clip_01,
                #bathy_std_100_clip_01,
                #avg_wave_orb_vel_mean_100_clip_161,
                #SST_mean_100_clip_01,
                dist2reef_mean_100_clip_01
  )

colnames(Data.CARNIVORES) <- c("SpeciesRichness",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "DistanceToReef")


Model.CARNIVORES <- gam(SpeciesRichness ~ Year + s(FineBPI, k = 5) + s(Curvature, k = 5) + s(Bathymetry, k = 5), data = Data.CARNIVORES)


# Make GAM Plots
pdf(file = "CARNIVORES_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.CARNIVORES)
dev.off()


#  HERBIVORES 25m ####


Data.HERBIVORES <- Import.25m.Site.Summaries.ENTIRE %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness.HERBIVORES,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_25_clip_01, 
                #slope_mean_25_clip_01, 
                rugosity_mean_25_clip_01, 
                #profile_curv_mean_25_clip_01, 
                #plan_curv_mean_25_clip_01,
                northness_mean_25_clip_01,
                #fbpi25_50_mean_25_clip_01,
                fbpi10_30_mean_25_clip_01,
                eastness_mean_25_clip_01,
                curvature_mean_25_clip_01,
                #bbpi100_200_mean_25_clip_01,
                bathy_mean_25_clip_01,
                #bathy_std_25_clip_01,
                #avg_wave_orb_vel_mean_25_clip_161,
                #SST_mean_25_clip_01,
                dist2reef_mean_25_clip_01
  )

colnames(Data.HERBIVORES) <- c("SpeciesRichness",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "DistanceToReef")


Model.HERBIVORES <- gam(SpeciesRichness ~ Year + Status + s(Rugosity, k = 5) + s(Northness, k = 5) + s(Curvature, k = 5) + s(Bathymetry, k = 5) + s(DistanceToReef, k = 5), data = Data.HERBIVORES)

# Make GAM Plots
pdf(file = "HERBIVORES_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.HERBIVORES)
dev.off()



# INVERTIVORES 50m ####

Data.INVERTIVORES <- Import.50m.Site.Summaries.ENTIRE %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness.INVERTIVORES,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_50_clip_01, 
                #slope_mean_50_clip_01, 
                rugosity_mean_50_clip_01, 
                #profile_curv_mean_50_clip_01, 
                #plan_curv_mean_50_clip_01,
                northness_mean_50_clip_01,
                #fbpi25_50_mean_50_clip_01,
                fbpi10_30_mean_50_clip_01,
                eastness_mean_50_clip_01,
                curvature_mean_50_clip_01,
                #bbpi100_200_mean_50_clip_01,
                bathy_mean_50_clip_01,
                #bathy_std_50_clip_01,
                #avg_wave_orb_vel_mean_50_clip_161,
                #SST_mean_50_clip_01,
                dist2reef_mean_50_clip_01
  )

colnames(Data.INVERTIVORES) <- c("SpeciesRichness",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "DistanceToReef")


Model.INVERTIVORES <- gam(SpeciesRichness ~ Year + s(Northness, k = 9) + s(FineBPI, k = 9) + s(Eastness, k = 9) + s(Bathymetry, k = 9) + s(DistanceToReef, k = 9), data = Data.INVERTIVORES)

# Make GAM Plots
pdf(file = "INVERTIVORES_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.INVERTIVORES)
dev.off()




#  C.AURATUS 25m  ####

Data.C.AURATUS <- Import.25m.Site.Summaries.ENTIRE %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(C.auratus.Abundance,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_25_clip_01, 
                #slope_mean_25_clip_01, 
                rugosity_mean_25_clip_01, 
                #profile_curv_mean_25_clip_01, 
                #plan_curv_mean_25_clip_01,
                northness_mean_25_clip_01,
                #fbpi25_50_mean_25_clip_01,
                fbpi10_30_mean_25_clip_01,
                eastness_mean_25_clip_01,
                curvature_mean_25_clip_01,
                #bbpi100_200_mean_25_clip_01,
                bathy_mean_25_clip_01,
                #bathy_std_25_clip_01,
                #avg_wave_orb_vel_mean_25_clip_161,
                #SST_mean_25_clip_01,
                dist2reef_mean_25_clip_01
  )

colnames(Data.C.AURATUS) <- c("Abundance",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "DistanceToReef")


Model.C.AURATUS <- gam(Abundance ~ Year + Status + s(Eastness, k = 9) + s(Bathymetry, k = 9) + s(DistanceToReef, k = 9), data = Data.C.AURATUS)

# Make GAM Plots
pdf(file = "C_AURATUS_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.C.AURATUS)
dev.off()



# N.TETRICUS 5m ####

Data.N.TETRICUS <- Import.5m.Site.Summaries.ENTIRE %>%
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

colnames(Data.N.TETRICUS) <- c("Abundance",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "DistanceToReef")



Model.N.TETRICUS <- gam(Abundance ~ Year + s(FineBPI, k = 9) + s(Eastness, k = 9) + s(Bathymetry, k = 9) + s(DistanceToReef, k = 9), data = Data.N.TETRICUS)

# Make GAM Plots
pdf(file = "N_TETRICUS_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.N.TETRICUS)
dev.off()


# M.HIPPOCREPIS  150m ####

Data.M.HIPPOCREPIS <- Import.150m.Site.Summaries.ENTIRE %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(M.hippocrepis.Abundance,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_2150_clip_01, 
                #slope_mean_2150_clip_01, 
                rugosity_mean_150_clip_01, 
                #profile_curv_mean_2150_clip_01, 
                #plan_curv_mean_2150_clip_01,
                northness_mean_150_clip_01,
                #fbpi2150_1500_mean_2150_clip_01,
                fbpi10_30_mean_150_clip_01,
                eastness_mean_150_clip_01,
                curvature_mean_150_clip_01,
                #bbpi100_200_mean_2150_clip_01,
                bathy_mean_150_clip_01,
                #bathy_std_2150_clip_01,
                #avg_wave_orb_vel_mean_2150_clip_161,
                #SST_mean_2150_clip_01,
                dist2reef_mean_150_clip_01
  )

colnames(Data.M.HIPPOCREPIS) <- c("Abundance",
                           "Year",
                           "Status",
                           "Rugosity",
                           "Northness",
                           "FineBPI",
                           "Eastness",
                           "Curvature",
                           "Bathymetry",
                           "DistanceToReef")



Model.M.HIPPOCREPIS <- gam(Abundance ~ Year + Status + s(Rugosity, k = 9) + s(Eastness, k = 9) + s(Curvature, k = 9), data = Data.M.HIPPOCREPIS)

# Make GAM Plots
pdf(file = "M_HIPPOCREPIS_Best_Plots.pdf", width = 4, height = 4)
plot.gam(Model.M.HIPPOCREPIS)
dev.off()
