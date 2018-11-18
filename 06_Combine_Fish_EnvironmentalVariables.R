# Script for combining environmental variables and fish data (using yearly SST)

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 9 September 2018

# Libraries required:
detach("package:plyr", unload=TRUE)#will error - no worries
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all columns
library(ggplot2)
library(stringr)


# Set working directory for input files
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/07_HabitatData/Step3_JoinToFishData/")
getwd()
dir()

# Load input files
totalMaxN <- read.table("Analysis_MaxN_SpUpdated_HabitatInc.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 

totalLength <- read.table("Analysis_3D_Point_Length_Measurements_SpUpdated_BiomassIncluded_HabitatInc.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 

# Reset working directory for outputs
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/08_AddEnvironmentalVariables/")
getwd()
dir()

InputEnvironmentalVariables <- read.table("03_Honours_Environmental_Variables.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

InputUTM <- read.table("01_Honours_Environmental_Variables.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

InputUTM <- InputUTM %>%
  dplyr::select(c(HonoursID, Easting, Northing))

temp.totalMaxN <- (merge(totalMaxN, InputEnvironmentalVariables, by.x = "HonoursID", by.y = "HonoursID", all.x = TRUE, sort = FALSE))
temp.totalMaxN <- (merge(temp.totalMaxN, InputUTM, by.x = "HonoursID", by.y = "HonoursID", all.x = TRUE, sort = FALSE))

temp.totalLength <- (merge(totalLength, InputEnvironmentalVariables, by.x = "HonoursID", by.y = "HonoursID", all.x = TRUE, sort = FALSE))
temp.totalLength <- (merge(temp.totalLength, InputUTM, by.x = "HonoursID", by.y = "HonoursID", all.x = TRUE, sort = FALSE))


#### Make SST columns for correct dates ####
# To enable creation of yearly predictive rasters, a single monthly SST raster was chosen for each year. This raster was chosen as the best descriptor of sampling dates for that year.

Dates <- unique(temp.totalMaxN$Date)


## Assign Correct SST for MaxN ##

Date_2018_03_07 <- temp.totalMaxN %>%
  filter(Date == "2018-03-07")
Date_2018_03_07$SST_mean_5_clip_01 <- Date_2018_03_07$sst_m_201803_mean_5_clip_01
Date_2018_03_07$SST_mean_10_clip_01 <- Date_2018_03_07$sst_m_201803_mean_10_clip_01
Date_2018_03_07$SST_mean_25_clip_01 <- Date_2018_03_07$sst_m_201803_mean_25_clip_01
Date_2018_03_07$SST_mean_50_clip_01 <- Date_2018_03_07$sst_m_201803_mean_50_clip_01
Date_2018_03_07$SST_mean_75_clip_01 <- Date_2018_03_07$sst_m_201803_mean_75_clip_01
Date_2018_03_07$SST_mean_100_clip_01 <- Date_2018_03_07$sst_m_201803_mean_100_clip_01
Date_2018_03_07$SST_mean_150_clip_01 <- Date_2018_03_07$sst_m_201803_mean_150_clip_01
Date_2018_03_07$SST_mean_200_clip_01 <- Date_2018_03_07$sst_m_201803_mean_200_clip_01
Date_2018_03_07$SST_mean_250_clip_01 <- Date_2018_03_07$sst_m_201803_mean_250_clip_01
Date_2018_03_07$SST_mean_300_clip_01 <- Date_2018_03_07$sst_m_201803_mean_300_clip_01
Date_2018_03_07$SST_mean_400_clip_01 <- Date_2018_03_07$sst_m_201803_mean_400_clip_01
Date_2018_03_07$SST_mean_500_clip_01 <- Date_2018_03_07$sst_m_201803_mean_500_clip_01

colnames(Date_2018_03_07)
Date_2018_03_07 = Date_2018_03_07[, c(1:207, 268:293)]


Date_2018_04_09 <- temp.totalMaxN %>%
  filter(Date == "2018-04-09")
Date_2018_04_09$SST_mean_5_clip_01 <- Date_2018_04_09$sst_m_201804_mean_5_clip_01
Date_2018_04_09$SST_mean_10_clip_01 <- Date_2018_04_09$sst_m_201804_mean_10_clip_01
Date_2018_04_09$SST_mean_25_clip_01 <- Date_2018_04_09$sst_m_201804_mean_25_clip_01
Date_2018_04_09$SST_mean_50_clip_01 <- Date_2018_04_09$sst_m_201804_mean_50_clip_01
Date_2018_04_09$SST_mean_75_clip_01 <- Date_2018_04_09$sst_m_201804_mean_75_clip_01
Date_2018_04_09$SST_mean_100_clip_01 <- Date_2018_04_09$sst_m_201804_mean_100_clip_01
Date_2018_04_09$SST_mean_150_clip_01 <- Date_2018_04_09$sst_m_201804_mean_150_clip_01
Date_2018_04_09$SST_mean_200_clip_01 <- Date_2018_04_09$sst_m_201804_mean_200_clip_01
Date_2018_04_09$SST_mean_250_clip_01 <- Date_2018_04_09$sst_m_201804_mean_250_clip_01
Date_2018_04_09$SST_mean_300_clip_01 <- Date_2018_04_09$sst_m_201804_mean_300_clip_01
Date_2018_04_09$SST_mean_400_clip_01 <- Date_2018_04_09$sst_m_201804_mean_400_clip_01
Date_2018_04_09$SST_mean_500_clip_01 <- Date_2018_04_09$sst_m_201804_mean_500_clip_01

Date_2018_04_09 = Date_2018_04_09[, c(1:207, 268:293)]


Date_2018_03_08 <- temp.totalMaxN %>%
  filter(Date == "2018-03-08")
Date_2018_03_08$SST_mean_5_clip_01 <- Date_2018_03_08$sst_m_201803_mean_5_clip_01
Date_2018_03_08$SST_mean_10_clip_01 <- Date_2018_03_08$sst_m_201803_mean_10_clip_01
Date_2018_03_08$SST_mean_25_clip_01 <- Date_2018_03_08$sst_m_201803_mean_25_clip_01
Date_2018_03_08$SST_mean_50_clip_01 <- Date_2018_03_08$sst_m_201803_mean_50_clip_01
Date_2018_03_08$SST_mean_75_clip_01 <- Date_2018_03_08$sst_m_201803_mean_75_clip_01
Date_2018_03_08$SST_mean_100_clip_01 <- Date_2018_03_08$sst_m_201803_mean_100_clip_01
Date_2018_03_08$SST_mean_150_clip_01 <- Date_2018_03_08$sst_m_201803_mean_150_clip_01
Date_2018_03_08$SST_mean_200_clip_01 <- Date_2018_03_08$sst_m_201803_mean_200_clip_01
Date_2018_03_08$SST_mean_250_clip_01 <- Date_2018_03_08$sst_m_201803_mean_250_clip_01
Date_2018_03_08$SST_mean_300_clip_01 <- Date_2018_03_08$sst_m_201803_mean_300_clip_01
Date_2018_03_08$SST_mean_400_clip_01 <- Date_2018_03_08$sst_m_201803_mean_400_clip_01
Date_2018_03_08$SST_mean_500_clip_01 <- Date_2018_03_08$sst_m_201803_mean_500_clip_01

Date_2018_03_08 = Date_2018_03_08[, c(1:207, 268:293)]


Date_2017_03_26 <- temp.totalMaxN %>%
  filter(Date == "2017-03-26")
Date_2017_03_26$SST_mean_5_clip_01 <- Date_2017_03_26$sst_m_201703_mean_5_clip_01
Date_2017_03_26$SST_mean_10_clip_01 <- Date_2017_03_26$sst_m_201703_mean_10_clip_01
Date_2017_03_26$SST_mean_25_clip_01 <- Date_2017_03_26$sst_m_201703_mean_25_clip_01
Date_2017_03_26$SST_mean_50_clip_01 <- Date_2017_03_26$sst_m_201703_mean_50_clip_01
Date_2017_03_26$SST_mean_75_clip_01 <- Date_2017_03_26$sst_m_201703_mean_75_clip_01
Date_2017_03_26$SST_mean_100_clip_01 <- Date_2017_03_26$sst_m_201703_mean_100_clip_01
Date_2017_03_26$SST_mean_150_clip_01 <- Date_2017_03_26$sst_m_201703_mean_150_clip_01
Date_2017_03_26$SST_mean_200_clip_01 <- Date_2017_03_26$sst_m_201703_mean_200_clip_01
Date_2017_03_26$SST_mean_250_clip_01 <- Date_2017_03_26$sst_m_201703_mean_250_clip_01
Date_2017_03_26$SST_mean_300_clip_01 <- Date_2017_03_26$sst_m_201703_mean_300_clip_01
Date_2017_03_26$SST_mean_400_clip_01 <- Date_2017_03_26$sst_m_201703_mean_400_clip_01
Date_2017_03_26$SST_mean_500_clip_01 <- Date_2017_03_26$sst_m_201703_mean_500_clip_01

Date_2017_03_26 = Date_2017_03_26[, c(1:207, 268:293)]


Date_2017_03_25 <- temp.totalMaxN %>%
  filter(Date == "2017-03-25")
Date_2017_03_25$SST_mean_5_clip_01 <- Date_2017_03_25$sst_m_201703_mean_5_clip_01
Date_2017_03_25$SST_mean_10_clip_01 <- Date_2017_03_25$sst_m_201703_mean_10_clip_01
Date_2017_03_25$SST_mean_25_clip_01 <- Date_2017_03_25$sst_m_201703_mean_25_clip_01
Date_2017_03_25$SST_mean_50_clip_01 <- Date_2017_03_25$sst_m_201703_mean_50_clip_01
Date_2017_03_25$SST_mean_75_clip_01 <- Date_2017_03_25$sst_m_201703_mean_75_clip_01
Date_2017_03_25$SST_mean_100_clip_01 <- Date_2017_03_25$sst_m_201703_mean_100_clip_01
Date_2017_03_25$SST_mean_150_clip_01 <- Date_2017_03_25$sst_m_201703_mean_150_clip_01
Date_2017_03_25$SST_mean_200_clip_01 <- Date_2017_03_25$sst_m_201703_mean_200_clip_01
Date_2017_03_25$SST_mean_250_clip_01 <- Date_2017_03_25$sst_m_201703_mean_250_clip_01
Date_2017_03_25$SST_mean_300_clip_01 <- Date_2017_03_25$sst_m_201703_mean_300_clip_01
Date_2017_03_25$SST_mean_400_clip_01 <- Date_2017_03_25$sst_m_201703_mean_400_clip_01
Date_2017_03_25$SST_mean_500_clip_01 <- Date_2017_03_25$sst_m_201703_mean_500_clip_01

Date_2017_03_25 = Date_2017_03_25[, c(1:207, 268:293)]



Date_2017_03_06 <- temp.totalMaxN %>%
  filter(Date == "2017-03-06")
Date_2017_03_06$SST_mean_5_clip_01 <- Date_2017_03_06$sst_m_201703_mean_5_clip_01
Date_2017_03_06$SST_mean_10_clip_01 <- Date_2017_03_06$sst_m_201703_mean_10_clip_01
Date_2017_03_06$SST_mean_25_clip_01 <- Date_2017_03_06$sst_m_201703_mean_25_clip_01
Date_2017_03_06$SST_mean_50_clip_01 <- Date_2017_03_06$sst_m_201703_mean_50_clip_01
Date_2017_03_06$SST_mean_75_clip_01 <- Date_2017_03_06$sst_m_201703_mean_75_clip_01
Date_2017_03_06$SST_mean_100_clip_01 <- Date_2017_03_06$sst_m_201703_mean_100_clip_01
Date_2017_03_06$SST_mean_150_clip_01 <- Date_2017_03_06$sst_m_201703_mean_150_clip_01
Date_2017_03_06$SST_mean_200_clip_01 <- Date_2017_03_06$sst_m_201703_mean_200_clip_01
Date_2017_03_06$SST_mean_250_clip_01 <- Date_2017_03_06$sst_m_201703_mean_250_clip_01
Date_2017_03_06$SST_mean_300_clip_01 <- Date_2017_03_06$sst_m_201703_mean_300_clip_01
Date_2017_03_06$SST_mean_400_clip_01 <- Date_2017_03_06$sst_m_201703_mean_400_clip_01
Date_2017_03_06$SST_mean_500_clip_01 <- Date_2017_03_06$sst_m_201703_mean_500_clip_01

Date_2017_03_06 = Date_2017_03_06[, c(1:207, 268:293)]


Date_2013_05_21 <- temp.totalMaxN %>%
  filter(Date == "2013-05-21")
Date_2013_05_21$SST_mean_5_clip_01 <- Date_2013_05_21$sst_m_201305_mean_5_clip_01
Date_2013_05_21$SST_mean_10_clip_01 <- Date_2013_05_21$sst_m_201305_mean_10_clip_01
Date_2013_05_21$SST_mean_25_clip_01 <- Date_2013_05_21$sst_m_201305_mean_25_clip_01
Date_2013_05_21$SST_mean_50_clip_01 <- Date_2013_05_21$sst_m_201305_mean_50_clip_01
Date_2013_05_21$SST_mean_75_clip_01 <- Date_2013_05_21$sst_m_201305_mean_75_clip_01
Date_2013_05_21$SST_mean_100_clip_01 <- Date_2013_05_21$sst_m_201305_mean_100_clip_01
Date_2013_05_21$SST_mean_150_clip_01 <- Date_2013_05_21$sst_m_201305_mean_150_clip_01
Date_2013_05_21$SST_mean_200_clip_01 <- Date_2013_05_21$sst_m_201305_mean_200_clip_01
Date_2013_05_21$SST_mean_250_clip_01 <- Date_2013_05_21$sst_m_201305_mean_250_clip_01
Date_2013_05_21$SST_mean_300_clip_01 <- Date_2013_05_21$sst_m_201305_mean_300_clip_01
Date_2013_05_21$SST_mean_400_clip_01 <- Date_2013_05_21$sst_m_201305_mean_400_clip_01
Date_2013_05_21$SST_mean_500_clip_01 <- Date_2013_05_21$sst_m_201305_mean_500_clip_01

Date_2013_05_21 = Date_2013_05_21[, c(1:207, 268:293)]


Date_2013_05_20 <- temp.totalMaxN %>%
  filter(Date == "2013-05-20")
Date_2013_05_20$SST_mean_5_clip_01 <- Date_2013_05_20$sst_m_201305_mean_5_clip_01
Date_2013_05_20$SST_mean_10_clip_01 <- Date_2013_05_20$sst_m_201305_mean_10_clip_01
Date_2013_05_20$SST_mean_25_clip_01 <- Date_2013_05_20$sst_m_201305_mean_25_clip_01
Date_2013_05_20$SST_mean_50_clip_01 <- Date_2013_05_20$sst_m_201305_mean_50_clip_01
Date_2013_05_20$SST_mean_75_clip_01 <- Date_2013_05_20$sst_m_201305_mean_75_clip_01
Date_2013_05_20$SST_mean_100_clip_01 <- Date_2013_05_20$sst_m_201305_mean_100_clip_01
Date_2013_05_20$SST_mean_150_clip_01 <- Date_2013_05_20$sst_m_201305_mean_150_clip_01
Date_2013_05_20$SST_mean_200_clip_01 <- Date_2013_05_20$sst_m_201305_mean_200_clip_01
Date_2013_05_20$SST_mean_250_clip_01 <- Date_2013_05_20$sst_m_201305_mean_250_clip_01
Date_2013_05_20$SST_mean_300_clip_01 <- Date_2013_05_20$sst_m_201305_mean_300_clip_01
Date_2013_05_20$SST_mean_400_clip_01 <- Date_2013_05_20$sst_m_201305_mean_400_clip_01
Date_2013_05_20$SST_mean_500_clip_01 <- Date_2013_05_20$sst_m_201305_mean_500_clip_01

Date_2013_05_20 = Date_2013_05_20[, c(1:207, 268:293)]


Date_2013_04_10 <- temp.totalMaxN %>%
  filter(Date == "2013-04-10")
Date_2013_04_10$SST_mean_5_clip_01 <- Date_2013_04_10$sst_m_201304_mean_5_clip_01
Date_2013_04_10$SST_mean_10_clip_01 <- Date_2013_04_10$sst_m_201304_mean_10_clip_01
Date_2013_04_10$SST_mean_25_clip_01 <- Date_2013_04_10$sst_m_201304_mean_25_clip_01
Date_2013_04_10$SST_mean_50_clip_01 <- Date_2013_04_10$sst_m_201304_mean_50_clip_01
Date_2013_04_10$SST_mean_75_clip_01 <- Date_2013_04_10$sst_m_201304_mean_75_clip_01
Date_2013_04_10$SST_mean_100_clip_01 <- Date_2013_04_10$sst_m_201304_mean_100_clip_01
Date_2013_04_10$SST_mean_150_clip_01 <- Date_2013_04_10$sst_m_201304_mean_150_clip_01
Date_2013_04_10$SST_mean_200_clip_01 <- Date_2013_04_10$sst_m_201304_mean_200_clip_01
Date_2013_04_10$SST_mean_250_clip_01 <- Date_2013_04_10$sst_m_201304_mean_250_clip_01
Date_2013_04_10$SST_mean_300_clip_01 <- Date_2013_04_10$sst_m_201304_mean_300_clip_01
Date_2013_04_10$SST_mean_400_clip_01 <- Date_2013_04_10$sst_m_201304_mean_400_clip_01
Date_2013_04_10$SST_mean_500_clip_01 <- Date_2013_04_10$sst_m_201304_mean_500_clip_01

Date_2013_04_10 = Date_2013_04_10[, c(1:207, 268:293)]



Date_2013_04_17 <- temp.totalMaxN %>%
  filter(Date == "2013-04-17")
Date_2013_04_17$SST_mean_5_clip_01 <- Date_2013_04_17$sst_m_201304_mean_5_clip_01
Date_2013_04_17$SST_mean_10_clip_01 <- Date_2013_04_17$sst_m_201304_mean_10_clip_01
Date_2013_04_17$SST_mean_25_clip_01 <- Date_2013_04_17$sst_m_201304_mean_25_clip_01
Date_2013_04_17$SST_mean_50_clip_01 <- Date_2013_04_17$sst_m_201304_mean_50_clip_01
Date_2013_04_17$SST_mean_75_clip_01 <- Date_2013_04_17$sst_m_201304_mean_75_clip_01
Date_2013_04_17$SST_mean_100_clip_01 <- Date_2013_04_17$sst_m_201304_mean_100_clip_01
Date_2013_04_17$SST_mean_150_clip_01 <- Date_2013_04_17$sst_m_201304_mean_150_clip_01
Date_2013_04_17$SST_mean_200_clip_01 <- Date_2013_04_17$sst_m_201304_mean_200_clip_01
Date_2013_04_17$SST_mean_250_clip_01 <- Date_2013_04_17$sst_m_201304_mean_250_clip_01
Date_2013_04_17$SST_mean_300_clip_01 <- Date_2013_04_17$sst_m_201304_mean_300_clip_01
Date_2013_04_17$SST_mean_400_clip_01 <- Date_2013_04_17$sst_m_201304_mean_400_clip_01
Date_2013_04_17$SST_mean_500_clip_01 <- Date_2013_04_17$sst_m_201304_mean_500_clip_01

Date_2013_04_17 = Date_2013_04_17[, c(1:207, 268:293)]


Date_2013_05_29 <- temp.totalMaxN %>%
  filter(Date == "2013-05-29")
Date_2013_05_29$SST_mean_5_clip_01 <- Date_2013_05_29$sst_m_201305_mean_5_clip_01
Date_2013_05_29$SST_mean_10_clip_01 <- Date_2013_05_29$sst_m_201305_mean_10_clip_01
Date_2013_05_29$SST_mean_25_clip_01 <- Date_2013_05_29$sst_m_201305_mean_25_clip_01
Date_2013_05_29$SST_mean_50_clip_01 <- Date_2013_05_29$sst_m_201305_mean_50_clip_01
Date_2013_05_29$SST_mean_75_clip_01 <- Date_2013_05_29$sst_m_201305_mean_75_clip_01
Date_2013_05_29$SST_mean_100_clip_01 <- Date_2013_05_29$sst_m_201305_mean_100_clip_01
Date_2013_05_29$SST_mean_150_clip_01 <- Date_2013_05_29$sst_m_201305_mean_150_clip_01
Date_2013_05_29$SST_mean_200_clip_01 <- Date_2013_05_29$sst_m_201305_mean_200_clip_01
Date_2013_05_29$SST_mean_250_clip_01 <- Date_2013_05_29$sst_m_201305_mean_250_clip_01
Date_2013_05_29$SST_mean_300_clip_01 <- Date_2013_05_29$sst_m_201305_mean_300_clip_01
Date_2013_05_29$SST_mean_400_clip_01 <- Date_2013_05_29$sst_m_201305_mean_400_clip_01
Date_2013_05_29$SST_mean_500_clip_01 <- Date_2013_05_29$sst_m_201305_mean_500_clip_01

Date_2013_05_29 = Date_2013_05_29[, c(1:207, 268:293)]

# Combine Dates Back Together
temp.totalMaxN <- rbind(Date_2013_05_29, Date_2013_04_17, Date_2013_04_10, Date_2013_05_20, Date_2013_05_21, Date_2017_03_06, Date_2017_03_25, Date_2017_03_26, Date_2018_03_08, Date_2018_04_09, Date_2018_03_07)


## Assign Correct SST for MaxN ##

Date_2018_03_07 <- temp.totalLength %>%
  filter(Date == "2018-03-07")
Date_2018_03_07$SST_mean_5_clip_01 <- Date_2018_03_07$sst_m_201803_mean_5_clip_01
Date_2018_03_07$SST_mean_10_clip_01 <- Date_2018_03_07$sst_m_201803_mean_10_clip_01
Date_2018_03_07$SST_mean_25_clip_01 <- Date_2018_03_07$sst_m_201803_mean_25_clip_01
Date_2018_03_07$SST_mean_50_clip_01 <- Date_2018_03_07$sst_m_201803_mean_50_clip_01
Date_2018_03_07$SST_mean_75_clip_01 <- Date_2018_03_07$sst_m_201803_mean_75_clip_01
Date_2018_03_07$SST_mean_100_clip_01 <- Date_2018_03_07$sst_m_201803_mean_100_clip_01
Date_2018_03_07$SST_mean_150_clip_01 <- Date_2018_03_07$sst_m_201803_mean_150_clip_01
Date_2018_03_07$SST_mean_200_clip_01 <- Date_2018_03_07$sst_m_201803_mean_200_clip_01
Date_2018_03_07$SST_mean_250_clip_01 <- Date_2018_03_07$sst_m_201803_mean_250_clip_01
Date_2018_03_07$SST_mean_300_clip_01 <- Date_2018_03_07$sst_m_201803_mean_300_clip_01
Date_2018_03_07$SST_mean_400_clip_01 <- Date_2018_03_07$sst_m_201803_mean_400_clip_01
Date_2018_03_07$SST_mean_500_clip_01 <- Date_2018_03_07$sst_m_201803_mean_500_clip_01

colnames(Date_2018_03_07)
Date_2018_03_07 = Date_2018_03_07[, c(1:222, 283:308)]


Date_2018_04_09 <- temp.totalLength %>%
  filter(Date == "2018-04-09")
Date_2018_04_09$SST_mean_5_clip_01 <- Date_2018_04_09$sst_m_201804_mean_5_clip_01
Date_2018_04_09$SST_mean_10_clip_01 <- Date_2018_04_09$sst_m_201804_mean_10_clip_01
Date_2018_04_09$SST_mean_25_clip_01 <- Date_2018_04_09$sst_m_201804_mean_25_clip_01
Date_2018_04_09$SST_mean_50_clip_01 <- Date_2018_04_09$sst_m_201804_mean_50_clip_01
Date_2018_04_09$SST_mean_75_clip_01 <- Date_2018_04_09$sst_m_201804_mean_75_clip_01
Date_2018_04_09$SST_mean_100_clip_01 <- Date_2018_04_09$sst_m_201804_mean_100_clip_01
Date_2018_04_09$SST_mean_150_clip_01 <- Date_2018_04_09$sst_m_201804_mean_150_clip_01
Date_2018_04_09$SST_mean_200_clip_01 <- Date_2018_04_09$sst_m_201804_mean_200_clip_01
Date_2018_04_09$SST_mean_250_clip_01 <- Date_2018_04_09$sst_m_201804_mean_250_clip_01
Date_2018_04_09$SST_mean_300_clip_01 <- Date_2018_04_09$sst_m_201804_mean_300_clip_01
Date_2018_04_09$SST_mean_400_clip_01 <- Date_2018_04_09$sst_m_201804_mean_400_clip_01
Date_2018_04_09$SST_mean_500_clip_01 <- Date_2018_04_09$sst_m_201804_mean_500_clip_01

Date_2018_04_09 = Date_2018_04_09[, c(1:222, 283:308)]


Date_2018_03_08 <- temp.totalLength %>%
  filter(Date == "2018-03-08")
Date_2018_03_08$SST_mean_5_clip_01 <- Date_2018_03_08$sst_m_201803_mean_5_clip_01
Date_2018_03_08$SST_mean_10_clip_01 <- Date_2018_03_08$sst_m_201803_mean_10_clip_01
Date_2018_03_08$SST_mean_25_clip_01 <- Date_2018_03_08$sst_m_201803_mean_25_clip_01
Date_2018_03_08$SST_mean_50_clip_01 <- Date_2018_03_08$sst_m_201803_mean_50_clip_01
Date_2018_03_08$SST_mean_75_clip_01 <- Date_2018_03_08$sst_m_201803_mean_75_clip_01
Date_2018_03_08$SST_mean_100_clip_01 <- Date_2018_03_08$sst_m_201803_mean_100_clip_01
Date_2018_03_08$SST_mean_150_clip_01 <- Date_2018_03_08$sst_m_201803_mean_150_clip_01
Date_2018_03_08$SST_mean_200_clip_01 <- Date_2018_03_08$sst_m_201803_mean_200_clip_01
Date_2018_03_08$SST_mean_250_clip_01 <- Date_2018_03_08$sst_m_201803_mean_250_clip_01
Date_2018_03_08$SST_mean_300_clip_01 <- Date_2018_03_08$sst_m_201803_mean_300_clip_01
Date_2018_03_08$SST_mean_400_clip_01 <- Date_2018_03_08$sst_m_201803_mean_400_clip_01
Date_2018_03_08$SST_mean_500_clip_01 <- Date_2018_03_08$sst_m_201803_mean_500_clip_01

Date_2018_03_08 = Date_2018_03_08[, c(1:222, 283:308)]


Date_2017_03_26 <- temp.totalLength %>%
  filter(Date == "2017-03-26")
Date_2017_03_26$SST_mean_5_clip_01 <- Date_2017_03_26$sst_m_201703_mean_5_clip_01
Date_2017_03_26$SST_mean_10_clip_01 <- Date_2017_03_26$sst_m_201703_mean_10_clip_01
Date_2017_03_26$SST_mean_25_clip_01 <- Date_2017_03_26$sst_m_201703_mean_25_clip_01
Date_2017_03_26$SST_mean_50_clip_01 <- Date_2017_03_26$sst_m_201703_mean_50_clip_01
Date_2017_03_26$SST_mean_75_clip_01 <- Date_2017_03_26$sst_m_201703_mean_75_clip_01
Date_2017_03_26$SST_mean_100_clip_01 <- Date_2017_03_26$sst_m_201703_mean_100_clip_01
Date_2017_03_26$SST_mean_150_clip_01 <- Date_2017_03_26$sst_m_201703_mean_150_clip_01
Date_2017_03_26$SST_mean_200_clip_01 <- Date_2017_03_26$sst_m_201703_mean_200_clip_01
Date_2017_03_26$SST_mean_250_clip_01 <- Date_2017_03_26$sst_m_201703_mean_250_clip_01
Date_2017_03_26$SST_mean_300_clip_01 <- Date_2017_03_26$sst_m_201703_mean_300_clip_01
Date_2017_03_26$SST_mean_400_clip_01 <- Date_2017_03_26$sst_m_201703_mean_400_clip_01
Date_2017_03_26$SST_mean_500_clip_01 <- Date_2017_03_26$sst_m_201703_mean_500_clip_01

Date_2017_03_26 = Date_2017_03_26[, c(1:222, 283:308)]


Date_2017_03_25 <- temp.totalLength %>%
  filter(Date == "2017-03-25")
Date_2017_03_25$SST_mean_5_clip_01 <- Date_2017_03_25$sst_m_201703_mean_5_clip_01
Date_2017_03_25$SST_mean_10_clip_01 <- Date_2017_03_25$sst_m_201703_mean_10_clip_01
Date_2017_03_25$SST_mean_25_clip_01 <- Date_2017_03_25$sst_m_201703_mean_25_clip_01
Date_2017_03_25$SST_mean_50_clip_01 <- Date_2017_03_25$sst_m_201703_mean_50_clip_01
Date_2017_03_25$SST_mean_75_clip_01 <- Date_2017_03_25$sst_m_201703_mean_75_clip_01
Date_2017_03_25$SST_mean_100_clip_01 <- Date_2017_03_25$sst_m_201703_mean_100_clip_01
Date_2017_03_25$SST_mean_150_clip_01 <- Date_2017_03_25$sst_m_201703_mean_150_clip_01
Date_2017_03_25$SST_mean_200_clip_01 <- Date_2017_03_25$sst_m_201703_mean_200_clip_01
Date_2017_03_25$SST_mean_250_clip_01 <- Date_2017_03_25$sst_m_201703_mean_250_clip_01
Date_2017_03_25$SST_mean_300_clip_01 <- Date_2017_03_25$sst_m_201703_mean_300_clip_01
Date_2017_03_25$SST_mean_400_clip_01 <- Date_2017_03_25$sst_m_201703_mean_400_clip_01
Date_2017_03_25$SST_mean_500_clip_01 <- Date_2017_03_25$sst_m_201703_mean_500_clip_01

Date_2017_03_25 = Date_2017_03_25[, c(1:222, 283:308)]



Date_2017_03_06 <- temp.totalLength %>%
  filter(Date == "2017-03-06")
Date_2017_03_06$SST_mean_5_clip_01 <- Date_2017_03_06$sst_m_201703_mean_5_clip_01
Date_2017_03_06$SST_mean_10_clip_01 <- Date_2017_03_06$sst_m_201703_mean_10_clip_01
Date_2017_03_06$SST_mean_25_clip_01 <- Date_2017_03_06$sst_m_201703_mean_25_clip_01
Date_2017_03_06$SST_mean_50_clip_01 <- Date_2017_03_06$sst_m_201703_mean_50_clip_01
Date_2017_03_06$SST_mean_75_clip_01 <- Date_2017_03_06$sst_m_201703_mean_75_clip_01
Date_2017_03_06$SST_mean_100_clip_01 <- Date_2017_03_06$sst_m_201703_mean_100_clip_01
Date_2017_03_06$SST_mean_150_clip_01 <- Date_2017_03_06$sst_m_201703_mean_150_clip_01
Date_2017_03_06$SST_mean_200_clip_01 <- Date_2017_03_06$sst_m_201703_mean_200_clip_01
Date_2017_03_06$SST_mean_250_clip_01 <- Date_2017_03_06$sst_m_201703_mean_250_clip_01
Date_2017_03_06$SST_mean_300_clip_01 <- Date_2017_03_06$sst_m_201703_mean_300_clip_01
Date_2017_03_06$SST_mean_400_clip_01 <- Date_2017_03_06$sst_m_201703_mean_400_clip_01
Date_2017_03_06$SST_mean_500_clip_01 <- Date_2017_03_06$sst_m_201703_mean_500_clip_01

Date_2017_03_06 = Date_2017_03_06[, c(1:222, 283:308)]


Date_2013_05_21 <- temp.totalLength %>%
  filter(Date == "2013-05-21")
Date_2013_05_21$SST_mean_5_clip_01 <- Date_2013_05_21$sst_m_201305_mean_5_clip_01
Date_2013_05_21$SST_mean_10_clip_01 <- Date_2013_05_21$sst_m_201305_mean_10_clip_01
Date_2013_05_21$SST_mean_25_clip_01 <- Date_2013_05_21$sst_m_201305_mean_25_clip_01
Date_2013_05_21$SST_mean_50_clip_01 <- Date_2013_05_21$sst_m_201305_mean_50_clip_01
Date_2013_05_21$SST_mean_75_clip_01 <- Date_2013_05_21$sst_m_201305_mean_75_clip_01
Date_2013_05_21$SST_mean_100_clip_01 <- Date_2013_05_21$sst_m_201305_mean_100_clip_01
Date_2013_05_21$SST_mean_150_clip_01 <- Date_2013_05_21$sst_m_201305_mean_150_clip_01
Date_2013_05_21$SST_mean_200_clip_01 <- Date_2013_05_21$sst_m_201305_mean_200_clip_01
Date_2013_05_21$SST_mean_250_clip_01 <- Date_2013_05_21$sst_m_201305_mean_250_clip_01
Date_2013_05_21$SST_mean_300_clip_01 <- Date_2013_05_21$sst_m_201305_mean_300_clip_01
Date_2013_05_21$SST_mean_400_clip_01 <- Date_2013_05_21$sst_m_201305_mean_400_clip_01
Date_2013_05_21$SST_mean_500_clip_01 <- Date_2013_05_21$sst_m_201305_mean_500_clip_01

Date_2013_05_21 = Date_2013_05_21[, c(1:222, 283:308)]


Date_2013_05_20 <- temp.totalLength %>%
  filter(Date == "2013-05-20")
Date_2013_05_20$SST_mean_5_clip_01 <- Date_2013_05_20$sst_m_201305_mean_5_clip_01
Date_2013_05_20$SST_mean_10_clip_01 <- Date_2013_05_20$sst_m_201305_mean_10_clip_01
Date_2013_05_20$SST_mean_25_clip_01 <- Date_2013_05_20$sst_m_201305_mean_25_clip_01
Date_2013_05_20$SST_mean_50_clip_01 <- Date_2013_05_20$sst_m_201305_mean_50_clip_01
Date_2013_05_20$SST_mean_75_clip_01 <- Date_2013_05_20$sst_m_201305_mean_75_clip_01
Date_2013_05_20$SST_mean_100_clip_01 <- Date_2013_05_20$sst_m_201305_mean_100_clip_01
Date_2013_05_20$SST_mean_150_clip_01 <- Date_2013_05_20$sst_m_201305_mean_150_clip_01
Date_2013_05_20$SST_mean_200_clip_01 <- Date_2013_05_20$sst_m_201305_mean_200_clip_01
Date_2013_05_20$SST_mean_250_clip_01 <- Date_2013_05_20$sst_m_201305_mean_250_clip_01
Date_2013_05_20$SST_mean_300_clip_01 <- Date_2013_05_20$sst_m_201305_mean_300_clip_01
Date_2013_05_20$SST_mean_400_clip_01 <- Date_2013_05_20$sst_m_201305_mean_400_clip_01
Date_2013_05_20$SST_mean_500_clip_01 <- Date_2013_05_20$sst_m_201305_mean_500_clip_01

Date_2013_05_20 = Date_2013_05_20[, c(1:222, 283:308)]


Date_2013_04_10 <- temp.totalLength %>%
  filter(Date == "2013-04-10")
Date_2013_04_10$SST_mean_5_clip_01 <- Date_2013_04_10$sst_m_201304_mean_5_clip_01
Date_2013_04_10$SST_mean_10_clip_01 <- Date_2013_04_10$sst_m_201304_mean_10_clip_01
Date_2013_04_10$SST_mean_25_clip_01 <- Date_2013_04_10$sst_m_201304_mean_25_clip_01
Date_2013_04_10$SST_mean_50_clip_01 <- Date_2013_04_10$sst_m_201304_mean_50_clip_01
Date_2013_04_10$SST_mean_75_clip_01 <- Date_2013_04_10$sst_m_201304_mean_75_clip_01
Date_2013_04_10$SST_mean_100_clip_01 <- Date_2013_04_10$sst_m_201304_mean_100_clip_01
Date_2013_04_10$SST_mean_150_clip_01 <- Date_2013_04_10$sst_m_201304_mean_150_clip_01
Date_2013_04_10$SST_mean_200_clip_01 <- Date_2013_04_10$sst_m_201304_mean_200_clip_01
Date_2013_04_10$SST_mean_250_clip_01 <- Date_2013_04_10$sst_m_201304_mean_250_clip_01
Date_2013_04_10$SST_mean_300_clip_01 <- Date_2013_04_10$sst_m_201304_mean_300_clip_01
Date_2013_04_10$SST_mean_400_clip_01 <- Date_2013_04_10$sst_m_201304_mean_400_clip_01
Date_2013_04_10$SST_mean_500_clip_01 <- Date_2013_04_10$sst_m_201304_mean_500_clip_01

Date_2013_04_10 = Date_2013_04_10[, c(1:222, 283:308)]



Date_2013_04_17 <- temp.totalLength %>%
  filter(Date == "2013-04-17")
Date_2013_04_17$SST_mean_5_clip_01 <- Date_2013_04_17$sst_m_201304_mean_5_clip_01
Date_2013_04_17$SST_mean_10_clip_01 <- Date_2013_04_17$sst_m_201304_mean_10_clip_01
Date_2013_04_17$SST_mean_25_clip_01 <- Date_2013_04_17$sst_m_201304_mean_25_clip_01
Date_2013_04_17$SST_mean_50_clip_01 <- Date_2013_04_17$sst_m_201304_mean_50_clip_01
Date_2013_04_17$SST_mean_75_clip_01 <- Date_2013_04_17$sst_m_201304_mean_75_clip_01
Date_2013_04_17$SST_mean_100_clip_01 <- Date_2013_04_17$sst_m_201304_mean_100_clip_01
Date_2013_04_17$SST_mean_150_clip_01 <- Date_2013_04_17$sst_m_201304_mean_150_clip_01
Date_2013_04_17$SST_mean_200_clip_01 <- Date_2013_04_17$sst_m_201304_mean_200_clip_01
Date_2013_04_17$SST_mean_250_clip_01 <- Date_2013_04_17$sst_m_201304_mean_250_clip_01
Date_2013_04_17$SST_mean_300_clip_01 <- Date_2013_04_17$sst_m_201304_mean_300_clip_01
Date_2013_04_17$SST_mean_400_clip_01 <- Date_2013_04_17$sst_m_201304_mean_400_clip_01
Date_2013_04_17$SST_mean_500_clip_01 <- Date_2013_04_17$sst_m_201304_mean_500_clip_01

Date_2013_04_17 = Date_2013_04_17[, c(1:222, 283:308)]


Date_2013_05_29 <- temp.totalLength %>%
  filter(Date == "2013-05-29")
Date_2013_05_29$SST_mean_5_clip_01 <- Date_2013_05_29$sst_m_201305_mean_5_clip_01
Date_2013_05_29$SST_mean_10_clip_01 <- Date_2013_05_29$sst_m_201305_mean_10_clip_01
Date_2013_05_29$SST_mean_25_clip_01 <- Date_2013_05_29$sst_m_201305_mean_25_clip_01
Date_2013_05_29$SST_mean_50_clip_01 <- Date_2013_05_29$sst_m_201305_mean_50_clip_01
Date_2013_05_29$SST_mean_75_clip_01 <- Date_2013_05_29$sst_m_201305_mean_75_clip_01
Date_2013_05_29$SST_mean_100_clip_01 <- Date_2013_05_29$sst_m_201305_mean_100_clip_01
Date_2013_05_29$SST_mean_150_clip_01 <- Date_2013_05_29$sst_m_201305_mean_150_clip_01
Date_2013_05_29$SST_mean_200_clip_01 <- Date_2013_05_29$sst_m_201305_mean_200_clip_01
Date_2013_05_29$SST_mean_250_clip_01 <- Date_2013_05_29$sst_m_201305_mean_250_clip_01
Date_2013_05_29$SST_mean_300_clip_01 <- Date_2013_05_29$sst_m_201305_mean_300_clip_01
Date_2013_05_29$SST_mean_400_clip_01 <- Date_2013_05_29$sst_m_201305_mean_400_clip_01
Date_2013_05_29$SST_mean_500_clip_01 <- Date_2013_05_29$sst_m_201305_mean_500_clip_01

Date_2013_05_29 = Date_2013_05_29[, c(1:222, 283:308)]

# Combine Dates Back Together
temp.totalLength <- rbind(Date_2013_05_29, Date_2013_04_17, Date_2013_04_10, Date_2013_05_20, Date_2013_05_21, Date_2017_03_06, Date_2017_03_25, Date_2017_03_26, Date_2018_03_08, Date_2018_04_09, Date_2018_03_07)

# Tidy columns for output table
Updated.EnvionmentalVariables <- temp.totalMaxN %>%
  dplyr::select(-c(Sample, Sample.No, Family, Genus_species, Count, Others, Comment, Cams, Filename, Time_MaxN.mins., sp_Code, Tape_Reader, Has_Fed))

#### Export output tables. Descriptions of contents below: ####

# Combined fish MaxN observations, habitat characteristics and environmental variables for all years ####
write.table(temp.totalMaxN, file = "Analysis_MaxN_SpUpdated_HabitatInc_EnviroInc.txt" , sep = "\t", row.names = FALSE)

# Combined fish length observations, habitat characteristics and environmental variables for all years ####
write.table(temp.totalLength, file = "Analysis_3D_Point_Length_Measurements_SpUpdated_BiomassIncluded_HabitatInc_EnviroInc.txt" , sep = "\t", row.names = FALSE)

# Updated environmental variables with single SST column, as opposed to monthly SST columns ####
write.table(Updated.EnvionmentalVariables, file = "Updated.EnvionmentalVariables.txt" , sep = "\t", row.names = FALSE)

