# Script for producing correlation matrix figures for predictor variables

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 14 September 2018

# Libraries required
library(plyr)
library(dplyr)
library(mgcv)
library(ROCR)
library(AUC)
library(ncf)
library(nlme)
library(MuMIn)
library(gamm4)
library(lme4)
library(devtools)
library(ggplot2)
library(GGally)
library(corrplot)
library(Hmisc)
library(raster)

# Set working directory
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/11_Modelling_Subsets/02_BrokenDownIntoScales/")
getwd()
dir()

# Import Files
Import.5m.EnvironmentalVariables <- read.table("01_5m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 


setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/15_Figures/01_CorrelationMatrix/")
getwd()
dir()



#  5m    #################################################################################################################################   

# Select Columns for analysis

All_Correlations_5m_01 <- Import.5m.EnvironmentalVariables %>%
  dplyr::select(SpeciesRichness,
                Year,
                Status,
         vrm_neigh3_mean_5_clip_01, 
         slope_mean_5_clip_01, 
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
         SST_mean_5_clip_01,
         dist2reef_mean_5_clip_01
         )
colnames(All_Correlations_5m_01) <- c("SpeciesRichness",
                                      "Year",
                                      "Status",
                                      "VRM",
                                      "Slope",
                                      "Rugosity",
                                      "Profile_Curvature",
                                      "Plan_Curvature",
                                      "Northness",
                                      "Fine_BPI_25_50)",
                                      "Fine_BPI_10_30",
                                      "Eastness",
                                      "Curvature",
                                      "Broad_BPI_100_200",
                                      "Bathymetry_Mean",
                                      "Bathymetry_Std._Deviation",
                                      "Average_Wave_Orbital_Velocity",
                                      "Sea_Surface_Temperature",
                                      "Distance_to_Reef")


# Make Correlation Matrix

ggcorr(All_Correlations_5m_01[, 2:length(All_Correlations_5m_01)], method = c("pairwise", "pearson"), geom = "blank", label = TRUE, hjust = 1, label_size = 4, size = 5, layout.exp = 1) +
  geom_point(size = 12, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE) +
  theme_classic() +
  theme()

ggsave("All_Correlations_5m_01.png", plot = last_plot(), device = "png",
      dpi = 300, height = 200, width = 250, units = "mm", limitsize = TRUE)


# Select Columns for analysis

Limited_Correlations_5m_01 <- Import.5m.EnvironmentalVariables %>%
  dplyr::select(SpeciesRichness,
                Year,
                Status,
                #vrm_neigh3_mean_5_clip_01, 
                #slope_mean_5_clip_01, 
                rugosity_mean_5_clip_01, 
                #profile_curv_mean_5_clip_01, 
                #plan_curv_mean_5_clip_01,
                northness_mean_5_clip_01,
                #fbpi25_50_mean_5_clip_01,
                fbpi10_30_mean_5_clip_01,
                eastness_mean_5_clip_01,
                curvature_mean_5_clip_01,
                #bbpi100_200_mean_5_clip_01,
                bathy_mean_5_clip_01,
                #bathy_std_5_clip_01,
                #avg_wave_orb_vel_mean_5_clip_01,
                #SST_mean_5_clip_01,
                dist2reef_mean_5_clip_01
  )
colnames(Limited_Correlations_5m_01) <- c("SpeciesRichness",
                                          "Year",
                                          "Status",
                                      #"VRM",
                                      #"Slope",
                                      "Rugosity",
                                      #"Profile Curvature",
                                      #"Plan Curvature",
                                      "Northness",
                                      #"Fine BPI (25-50)",
                                      "Fine_BPI_10_30",
                                      "Eastness",
                                      "Curvature",
                                      #"Broad BPI (100-200)",
                                      "Bathymetry_Mean",
                                      #"Bathymetry (Std. Deviation)",
                                      #"Average Wave Orbital Velocity",
                                      #"Sea_Surface_Temperature",
                                      "Distance_to_Reef")


# Make Correlation Matrix

ggcorr(Limited_Correlations_5m_01[, 2:length(Limited_Correlations_5m_01)], method = c("pairwise", "pearson"), geom = "blank", label = TRUE, hjust = 1, label_size = 4, size = 5, layout.exp = 1) +
  geom_point(size = 12, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE) +
  theme_classic() +
  theme()

ggsave("Limited_Correlations_5m_01.png", plot = last_plot(), device = "png",
       dpi = 300, height = 200, width = 250, units = "mm", limitsize = TRUE)
