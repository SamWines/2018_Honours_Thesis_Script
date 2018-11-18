# Script for running GAMs on all combinations of predictor variables at multiple spatial scales

# This script is for modelling abundance of Notolabrus tetricus (Blue-throat Wrasse)
# Consistent variables have been used to ensure that differences between predictor variables aren't causing varying trends across scales

# This script is used to:
# 1. Split data into training and test
# 2. Run model for consistent variables (year, status, rugosity, bathymetry and distance to reef)
# 3. Predict for test data
# 4. Repeat steps (1-3) for all 12 spatial scales 
# 5. Make summary of all models
# 6. Plot metrics of model performance across spatial scales

# Author: Sam Wines (slwines@deakin.edu.au)

# Resources Used:
# https://kevintshoemaker.github.io/NRES-746/GAMs.html

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
library(caret)
library(purrr)

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


getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/02_N.tetricus/Abundance_ConsVars/")
getwd()
dir()

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


# 5m

TrainingData.5m.N.tetricus.Abundance <- Import.5m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_5_clip_01,
                bathy_mean_5_clip_01,
                dist2reef_mean_5_clip_01
  )

TestData.5m.N.tetricus.Abundance <- Import.5m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_5_clip_01,
                bathy_mean_5_clip_01,
                dist2reef_mean_5_clip_01
  )

TrainingData.5m.N.tetricus.Abundance$Year <- factor(TrainingData.5m.N.tetricus.Abundance$Year)
TrainingData.5m.N.tetricus.Abundance$Status <- factor(TrainingData.5m.N.tetricus.Abundance$Status)
TestData.5m.N.tetricus.Abundance$Year <- factor(TestData.5m.N.tetricus.Abundance$Year)
TestData.5m.N.tetricus.Abundance$Status <- factor(TestData.5m.N.tetricus.Abundance$Status)


# Models loop

data.5m <- TrainingData.5m.N.tetricus.Abundance

vars.5m <- c("N.tetricus.Abundance",
             "Year",
             "Status",
             "s(rugosity_mean_5_clip_01)",
             "s(bathy_mean_5_clip_01)",
             "s(dist2reef_mean_5_clip_01)")

Response.5m <- paste(vars.5m[1], " ~ ")
Predictor.5m <- paste(vars.5m[2], vars.5m[3], vars.5m[4], vars.5m[5], sep = " + ")
Formula.5m <- formula(paste(Response.5m, Predictor.5m))

Cons_5m_SpRichness_ENTIRE <- gam(Formula.5m, data = data.5m)

# Predict for test data
TestData.5m.N.tetricus.Abundance$Predicted.Species.Richness.5m <- as.numeric(predict.gam(Cons_5m_SpRichness_ENTIRE, TestData.5m.N.tetricus.Abundance))
TestData.5m.N.tetricus.Abundance$Difference.Pred.True.5m <- abs((TestData.5m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.5m.N.tetricus.Abundance$Predicted.Species.Richness.5m))
Mean.5m.Species.Richness.Predictive.Difference.5m <- mean(TestData.5m.N.tetricus.Abundance$Difference.Pred.True.5m)


# 10m

TrainingData.10m.N.tetricus.Abundance <- Import.10m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_10_clip_01,
                bathy_mean_10_clip_01,
                dist2reef_mean_10_clip_01
  )

TestData.10m.N.tetricus.Abundance <- Import.10m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_10_clip_01,
                bathy_mean_10_clip_01,
                dist2reef_mean_10_clip_01
  )

TrainingData.10m.N.tetricus.Abundance$Year <- factor(TrainingData.10m.N.tetricus.Abundance$Year)
TrainingData.10m.N.tetricus.Abundance$Status <- factor(TrainingData.10m.N.tetricus.Abundance$Status)
TestData.10m.N.tetricus.Abundance$Year <- factor(TestData.10m.N.tetricus.Abundance$Year)
TestData.10m.N.tetricus.Abundance$Status <- factor(TestData.10m.N.tetricus.Abundance$Status)


# Models loop

data.10m <- TrainingData.10m.N.tetricus.Abundance

vars.10m <- c("N.tetricus.Abundance",
             "Year",
             "Status",
             "s(rugosity_mean_10_clip_01)",
             "s(bathy_mean_10_clip_01)",
             "s(dist2reef_mean_10_clip_01)")

Response.10m <- paste(vars.10m[1], " ~ ")
Predictor.10m <- paste(vars.10m[2], vars.10m[3], vars.10m[4], vars.10m[5], sep = " + ")
Formula.10m <- formula(paste(Response.10m, Predictor.10m))

Cons_10m_SpRichness_ENTIRE <- gam(Formula.10m, data = data.10m)

# Predict for test data
TestData.10m.N.tetricus.Abundance$Predicted.Species.Richness.10m <- as.numeric(predict.gam(Cons_10m_SpRichness_ENTIRE, TestData.10m.N.tetricus.Abundance))
TestData.10m.N.tetricus.Abundance$Difference.Pred.True.10m <- abs((TestData.10m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.10m.N.tetricus.Abundance$Predicted.Species.Richness.10m))
Mean.10m.Species.Richness.Predictive.Difference.10m <- mean(TestData.10m.N.tetricus.Abundance$Difference.Pred.True.10m)



# 25m

TrainingData.25m.N.tetricus.Abundance <- Import.25m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_25_clip_01,
                bathy_mean_25_clip_01,
                dist2reef_mean_25_clip_01
  )

TestData.25m.N.tetricus.Abundance <- Import.25m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_25_clip_01,
                bathy_mean_25_clip_01,
                dist2reef_mean_25_clip_01
  )

TrainingData.25m.N.tetricus.Abundance$Year <- factor(TrainingData.25m.N.tetricus.Abundance$Year)
TrainingData.25m.N.tetricus.Abundance$Status <- factor(TrainingData.25m.N.tetricus.Abundance$Status)
TestData.25m.N.tetricus.Abundance$Year <- factor(TestData.25m.N.tetricus.Abundance$Year)
TestData.25m.N.tetricus.Abundance$Status <- factor(TestData.25m.N.tetricus.Abundance$Status)


# Models loop

data.25m <- TrainingData.25m.N.tetricus.Abundance

vars.25m <- c("N.tetricus.Abundance",
              "Year",
              "Status",
              "s(rugosity_mean_25_clip_01)",
              "s(bathy_mean_25_clip_01)",
              "s(dist2reef_mean_25_clip_01)")

Response.25m <- paste(vars.25m[1], " ~ ")
Predictor.25m <- paste(vars.25m[2], vars.25m[3], vars.25m[4], vars.25m[5], sep = " + ")
Formula.25m <- formula(paste(Response.25m, Predictor.25m))

Cons_25m_SpRichness_ENTIRE <- gam(Formula.25m, data = data.25m)

# Predict for test data
TestData.25m.N.tetricus.Abundance$Predicted.Species.Richness.25m <- as.numeric(predict.gam(Cons_25m_SpRichness_ENTIRE, TestData.25m.N.tetricus.Abundance))
TestData.25m.N.tetricus.Abundance$Difference.Pred.True.25m <- abs((TestData.25m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.25m.N.tetricus.Abundance$Predicted.Species.Richness.25m))
Mean.25m.Species.Richness.Predictive.Difference.25m <- mean(TestData.25m.N.tetricus.Abundance$Difference.Pred.True.25m)




# 50m

TrainingData.50m.N.tetricus.Abundance <- Import.50m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_50_clip_01,
                bathy_mean_50_clip_01,
                dist2reef_mean_50_clip_01
  )

TestData.50m.N.tetricus.Abundance <- Import.50m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_50_clip_01,
                bathy_mean_50_clip_01,
                dist2reef_mean_50_clip_01
  )

TrainingData.50m.N.tetricus.Abundance$Year <- factor(TrainingData.50m.N.tetricus.Abundance$Year)
TrainingData.50m.N.tetricus.Abundance$Status <- factor(TrainingData.50m.N.tetricus.Abundance$Status)
TestData.50m.N.tetricus.Abundance$Year <- factor(TestData.50m.N.tetricus.Abundance$Year)
TestData.50m.N.tetricus.Abundance$Status <- factor(TestData.50m.N.tetricus.Abundance$Status)


# Models loop

data.50m <- TrainingData.50m.N.tetricus.Abundance

vars.50m <- c("N.tetricus.Abundance",
              "Year",
              "Status",
              "s(rugosity_mean_50_clip_01)",
              "s(bathy_mean_50_clip_01)",
              "s(dist2reef_mean_50_clip_01)")

Response.50m <- paste(vars.50m[1], " ~ ")
Predictor.50m <- paste(vars.50m[2], vars.50m[3], vars.50m[4], vars.50m[5], sep = " + ")
Formula.50m <- formula(paste(Response.50m, Predictor.50m))

Cons_50m_SpRichness_ENTIRE <- gam(Formula.50m, data = data.50m)

# Predict for test data
TestData.50m.N.tetricus.Abundance$Predicted.Species.Richness.50m <- as.numeric(predict.gam(Cons_50m_SpRichness_ENTIRE, TestData.50m.N.tetricus.Abundance))
TestData.50m.N.tetricus.Abundance$Difference.Pred.True.50m <- abs((TestData.50m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.50m.N.tetricus.Abundance$Predicted.Species.Richness.50m))
Mean.50m.Species.Richness.Predictive.Difference.50m <- mean(TestData.50m.N.tetricus.Abundance$Difference.Pred.True.50m)




# 75m

TrainingData.75m.N.tetricus.Abundance <- Import.75m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_75_clip_01,
                bathy_mean_75_clip_01,
                dist2reef_mean_75_clip_01
  )

TestData.75m.N.tetricus.Abundance <- Import.75m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_75_clip_01,
                bathy_mean_75_clip_01,
                dist2reef_mean_75_clip_01
  )

TrainingData.75m.N.tetricus.Abundance$Year <- factor(TrainingData.75m.N.tetricus.Abundance$Year)
TrainingData.75m.N.tetricus.Abundance$Status <- factor(TrainingData.75m.N.tetricus.Abundance$Status)
TestData.75m.N.tetricus.Abundance$Year <- factor(TestData.75m.N.tetricus.Abundance$Year)
TestData.75m.N.tetricus.Abundance$Status <- factor(TestData.75m.N.tetricus.Abundance$Status)


# Models loop

data.75m <- TrainingData.75m.N.tetricus.Abundance

vars.75m <- c("N.tetricus.Abundance",
              "Year",
              "Status",
              "s(rugosity_mean_75_clip_01)",
              "s(bathy_mean_75_clip_01)",
              "s(dist2reef_mean_75_clip_01)")

Response.75m <- paste(vars.75m[1], " ~ ")
Predictor.75m <- paste(vars.75m[2], vars.75m[3], vars.75m[4], vars.75m[5], sep = " + ")
Formula.75m <- formula(paste(Response.75m, Predictor.75m))

Cons_75m_SpRichness_ENTIRE <- gam(Formula.75m, data = data.75m)

# Predict for test data
TestData.75m.N.tetricus.Abundance$Predicted.Species.Richness.75m <- as.numeric(predict.gam(Cons_75m_SpRichness_ENTIRE, TestData.75m.N.tetricus.Abundance))
TestData.75m.N.tetricus.Abundance$Difference.Pred.True.75m <- abs((TestData.75m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.75m.N.tetricus.Abundance$Predicted.Species.Richness.75m))
Mean.75m.Species.Richness.Predictive.Difference.75m <- mean(TestData.75m.N.tetricus.Abundance$Difference.Pred.True.75m)




# 100m

TrainingData.100m.N.tetricus.Abundance <- Import.100m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_100_clip_01,
                bathy_mean_100_clip_01,
                dist2reef_mean_100_clip_01
  )

TestData.100m.N.tetricus.Abundance <- Import.100m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_100_clip_01,
                bathy_mean_100_clip_01,
                dist2reef_mean_100_clip_01
  )

TrainingData.100m.N.tetricus.Abundance$Year <- factor(TrainingData.100m.N.tetricus.Abundance$Year)
TrainingData.100m.N.tetricus.Abundance$Status <- factor(TrainingData.100m.N.tetricus.Abundance$Status)
TestData.100m.N.tetricus.Abundance$Year <- factor(TestData.100m.N.tetricus.Abundance$Year)
TestData.100m.N.tetricus.Abundance$Status <- factor(TestData.100m.N.tetricus.Abundance$Status)


# Models loop

data.100m <- TrainingData.100m.N.tetricus.Abundance

vars.100m <- c("N.tetricus.Abundance",
              "Year",
              "Status",
              "s(rugosity_mean_100_clip_01)",
              "s(bathy_mean_100_clip_01)",
              "s(dist2reef_mean_100_clip_01)")

Response.100m <- paste(vars.100m[1], " ~ ")
Predictor.100m <- paste(vars.100m[2], vars.100m[3], vars.100m[4], vars.100m[5], sep = " + ")
Formula.100m <- formula(paste(Response.100m, Predictor.100m))

Cons_100m_SpRichness_ENTIRE <- gam(Formula.100m, data = data.100m)

# Predict for test data
TestData.100m.N.tetricus.Abundance$Predicted.Species.Richness.100m <- as.numeric(predict.gam(Cons_100m_SpRichness_ENTIRE, TestData.100m.N.tetricus.Abundance))
TestData.100m.N.tetricus.Abundance$Difference.Pred.True.100m <- abs((TestData.100m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.100m.N.tetricus.Abundance$Predicted.Species.Richness.100m))
Mean.100m.Species.Richness.Predictive.Difference.100m <- mean(TestData.100m.N.tetricus.Abundance$Difference.Pred.True.100m)




# 150m

TrainingData.150m.N.tetricus.Abundance <- Import.150m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_150_clip_01,
                bathy_mean_150_clip_01,
                dist2reef_mean_150_clip_01
  )

TestData.150m.N.tetricus.Abundance <- Import.150m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_150_clip_01,
                bathy_mean_150_clip_01,
                dist2reef_mean_150_clip_01
  )

TrainingData.150m.N.tetricus.Abundance$Year <- factor(TrainingData.150m.N.tetricus.Abundance$Year)
TrainingData.150m.N.tetricus.Abundance$Status <- factor(TrainingData.150m.N.tetricus.Abundance$Status)
TestData.150m.N.tetricus.Abundance$Year <- factor(TestData.150m.N.tetricus.Abundance$Year)
TestData.150m.N.tetricus.Abundance$Status <- factor(TestData.150m.N.tetricus.Abundance$Status)


# Models loop

data.150m <- TrainingData.150m.N.tetricus.Abundance

vars.150m <- c("N.tetricus.Abundance",
              "Year",
              "Status",
              "s(rugosity_mean_150_clip_01)",
              "s(bathy_mean_150_clip_01)",
              "s(dist2reef_mean_150_clip_01)")

Response.150m <- paste(vars.150m[1], " ~ ")
Predictor.150m <- paste(vars.150m[2], vars.150m[3], vars.150m[4], vars.150m[5], sep = " + ")
Formula.150m <- formula(paste(Response.150m, Predictor.150m))

Cons_150m_SpRichness_ENTIRE <- gam(Formula.150m, data = data.150m)

# Predict for test data
TestData.150m.N.tetricus.Abundance$Predicted.Species.Richness.150m <- as.numeric(predict.gam(Cons_150m_SpRichness_ENTIRE, TestData.150m.N.tetricus.Abundance))
TestData.150m.N.tetricus.Abundance$Difference.Pred.True.150m <- abs((TestData.150m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.150m.N.tetricus.Abundance$Predicted.Species.Richness.150m))
Mean.150m.Species.Richness.Predictive.Difference.150m <- mean(TestData.150m.N.tetricus.Abundance$Difference.Pred.True.150m)




# 200m

TrainingData.200m.N.tetricus.Abundance <- Import.200m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_200_clip_01,
                bathy_mean_200_clip_01,
                dist2reef_mean_200_clip_01
  )

TestData.200m.N.tetricus.Abundance <- Import.200m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_200_clip_01,
                bathy_mean_200_clip_01,
                dist2reef_mean_200_clip_01
  )

TrainingData.200m.N.tetricus.Abundance$Year <- factor(TrainingData.200m.N.tetricus.Abundance$Year)
TrainingData.200m.N.tetricus.Abundance$Status <- factor(TrainingData.200m.N.tetricus.Abundance$Status)
TestData.200m.N.tetricus.Abundance$Year <- factor(TestData.200m.N.tetricus.Abundance$Year)
TestData.200m.N.tetricus.Abundance$Status <- factor(TestData.200m.N.tetricus.Abundance$Status)


# Models loop

data.200m <- TrainingData.200m.N.tetricus.Abundance

vars.200m <- c("N.tetricus.Abundance",
              "Year",
              "Status",
              "s(rugosity_mean_200_clip_01)",
              "s(bathy_mean_200_clip_01)",
              "s(dist2reef_mean_200_clip_01)")

Response.200m <- paste(vars.200m[1], " ~ ")
Predictor.200m <- paste(vars.200m[2], vars.200m[3], vars.200m[4], vars.200m[5], sep = " + ")
Formula.200m <- formula(paste(Response.200m, Predictor.200m))

Cons_200m_SpRichness_ENTIRE <- gam(Formula.200m, data = data.200m)

# Predict for test data
TestData.200m.N.tetricus.Abundance$Predicted.Species.Richness.200m <- as.numeric(predict.gam(Cons_200m_SpRichness_ENTIRE, TestData.200m.N.tetricus.Abundance))
TestData.200m.N.tetricus.Abundance$Difference.Pred.True.200m <- abs((TestData.200m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.200m.N.tetricus.Abundance$Predicted.Species.Richness.200m))
Mean.200m.Species.Richness.Predictive.Difference.200m <- mean(TestData.200m.N.tetricus.Abundance$Difference.Pred.True.200m)




# 250m

TrainingData.250m.N.tetricus.Abundance <- Import.250m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_250_clip_01,
                bathy_mean_250_clip_01,
                dist2reef_mean_250_clip_01
  )

TestData.250m.N.tetricus.Abundance <- Import.250m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_250_clip_01,
                bathy_mean_250_clip_01,
                dist2reef_mean_250_clip_01
  )

TrainingData.250m.N.tetricus.Abundance$Year <- factor(TrainingData.250m.N.tetricus.Abundance$Year)
TrainingData.250m.N.tetricus.Abundance$Status <- factor(TrainingData.250m.N.tetricus.Abundance$Status)
TestData.250m.N.tetricus.Abundance$Year <- factor(TestData.250m.N.tetricus.Abundance$Year)
TestData.250m.N.tetricus.Abundance$Status <- factor(TestData.250m.N.tetricus.Abundance$Status)


# Models loop

data.250m <- TrainingData.250m.N.tetricus.Abundance

vars.250m <- c("N.tetricus.Abundance",
              "Year",
              "Status",
              "s(rugosity_mean_250_clip_01)",
              "s(bathy_mean_250_clip_01)",
              "s(dist2reef_mean_250_clip_01)")

Response.250m <- paste(vars.250m[1], " ~ ")
Predictor.250m <- paste(vars.250m[2], vars.250m[3], vars.250m[4], vars.250m[5], sep = " + ")
Formula.250m <- formula(paste(Response.250m, Predictor.250m))

Cons_250m_SpRichness_ENTIRE <- gam(Formula.250m, data = data.250m)

# Predict for test data
TestData.250m.N.tetricus.Abundance$Predicted.Species.Richness.250m <- as.numeric(predict.gam(Cons_250m_SpRichness_ENTIRE, TestData.250m.N.tetricus.Abundance))
TestData.250m.N.tetricus.Abundance$Difference.Pred.True.250m <- abs((TestData.250m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.250m.N.tetricus.Abundance$Predicted.Species.Richness.250m))
Mean.250m.Species.Richness.Predictive.Difference.250m <- mean(TestData.250m.N.tetricus.Abundance$Difference.Pred.True.250m)




# 300m

TrainingData.300m.N.tetricus.Abundance <- Import.300m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_300_clip_01,
                bathy_mean_300_clip_01,
                dist2reef_mean_300_clip_01
  )

TestData.300m.N.tetricus.Abundance <- Import.300m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_300_clip_01,
                bathy_mean_300_clip_01,
                dist2reef_mean_300_clip_01
  )

TrainingData.300m.N.tetricus.Abundance$Year <- factor(TrainingData.300m.N.tetricus.Abundance$Year)
TrainingData.300m.N.tetricus.Abundance$Status <- factor(TrainingData.300m.N.tetricus.Abundance$Status)
TestData.300m.N.tetricus.Abundance$Year <- factor(TestData.300m.N.tetricus.Abundance$Year)
TestData.300m.N.tetricus.Abundance$Status <- factor(TestData.300m.N.tetricus.Abundance$Status)


# Models loop

data.300m <- TrainingData.300m.N.tetricus.Abundance

vars.300m <- c("N.tetricus.Abundance",
              "Year",
              "Status",
              "s(rugosity_mean_300_clip_01)",
              "s(bathy_mean_300_clip_01)",
              "s(dist2reef_mean_300_clip_01)")

Response.300m <- paste(vars.300m[1], " ~ ")
Predictor.300m <- paste(vars.300m[2], vars.300m[3], vars.300m[4], vars.300m[5], sep = " + ")
Formula.300m <- formula(paste(Response.300m, Predictor.300m))

Cons_300m_SpRichness_ENTIRE <- gam(Formula.300m, data = data.300m)

# Predict for test data
TestData.300m.N.tetricus.Abundance$Predicted.Species.Richness.300m <- as.numeric(predict.gam(Cons_300m_SpRichness_ENTIRE, TestData.300m.N.tetricus.Abundance))
TestData.300m.N.tetricus.Abundance$Difference.Pred.True.300m <- abs((TestData.300m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.300m.N.tetricus.Abundance$Predicted.Species.Richness.300m))
Mean.300m.Species.Richness.Predictive.Difference.300m <- mean(TestData.300m.N.tetricus.Abundance$Difference.Pred.True.300m)




# 400m

TrainingData.400m.N.tetricus.Abundance <- Import.400m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_400_clip_01,
                bathy_mean_400_clip_01,
                dist2reef_mean_400_clip_01
  )

TestData.400m.N.tetricus.Abundance <- Import.400m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_400_clip_01,
                bathy_mean_400_clip_01,
                dist2reef_mean_400_clip_01
  )

TrainingData.400m.N.tetricus.Abundance$Year <- factor(TrainingData.400m.N.tetricus.Abundance$Year)
TrainingData.400m.N.tetricus.Abundance$Status <- factor(TrainingData.400m.N.tetricus.Abundance$Status)
TestData.400m.N.tetricus.Abundance$Year <- factor(TestData.400m.N.tetricus.Abundance$Year)
TestData.400m.N.tetricus.Abundance$Status <- factor(TestData.400m.N.tetricus.Abundance$Status)


# Models loop

data.400m <- TrainingData.400m.N.tetricus.Abundance

vars.400m <- c("N.tetricus.Abundance",
              "Year",
              "Status",
              "s(rugosity_mean_400_clip_01)",
              "s(bathy_mean_400_clip_01)",
              "s(dist2reef_mean_400_clip_01)")

Response.400m <- paste(vars.400m[1], " ~ ")
Predictor.400m <- paste(vars.400m[2], vars.400m[3], vars.400m[4], vars.400m[5], sep = " + ")
Formula.400m <- formula(paste(Response.400m, Predictor.400m))

Cons_400m_SpRichness_ENTIRE <- gam(Formula.400m, data = data.400m)

# Predict for test data
TestData.400m.N.tetricus.Abundance$Predicted.Species.Richness.400m <- as.numeric(predict.gam(Cons_400m_SpRichness_ENTIRE, TestData.400m.N.tetricus.Abundance))
TestData.400m.N.tetricus.Abundance$Difference.Pred.True.400m <- abs((TestData.400m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.400m.N.tetricus.Abundance$Predicted.Species.Richness.400m))
Mean.400m.Species.Richness.Predictive.Difference.400m <- mean(TestData.400m.N.tetricus.Abundance$Difference.Pred.True.400m)


# 500m

TrainingData.500m.N.tetricus.Abundance <- Import.500m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_500_clip_01,
                bathy_mean_500_clip_01,
                dist2reef_mean_500_clip_01
  )

TestData.500m.N.tetricus.Abundance <- Import.500m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(N.tetricus.Abundance,
                Year,
                Status,
                rugosity_mean_500_clip_01,
                bathy_mean_500_clip_01,
                dist2reef_mean_500_clip_01
  )

TrainingData.500m.N.tetricus.Abundance$Year <- factor(TrainingData.500m.N.tetricus.Abundance$Year)
TrainingData.500m.N.tetricus.Abundance$Status <- factor(TrainingData.500m.N.tetricus.Abundance$Status)
TestData.500m.N.tetricus.Abundance$Year <- factor(TestData.500m.N.tetricus.Abundance$Year)
TestData.500m.N.tetricus.Abundance$Status <- factor(TestData.500m.N.tetricus.Abundance$Status)


# Models loop

data.500m <- TrainingData.500m.N.tetricus.Abundance

vars.500m <- c("N.tetricus.Abundance",
              "Year",
              "Status",
              "s(rugosity_mean_500_clip_01)",
              "s(bathy_mean_500_clip_01)",
              "s(dist2reef_mean_500_clip_01)")

Response.500m <- paste(vars.500m[1], " ~ ")
Predictor.500m <- paste(vars.500m[2], vars.500m[3], vars.500m[4], vars.500m[5], sep = " + ")
Formula.500m <- formula(paste(Response.500m, Predictor.500m))

Cons_500m_SpRichness_ENTIRE <- gam(Formula.500m, data = data.500m)

# Predict for test data
TestData.500m.N.tetricus.Abundance$Predicted.Species.Richness.500m <- as.numeric(predict.gam(Cons_500m_SpRichness_ENTIRE, TestData.500m.N.tetricus.Abundance))
TestData.500m.N.tetricus.Abundance$Difference.Pred.True.500m <- abs((TestData.500m.N.tetricus.Abundance$N.tetricus.Abundance) - (TestData.500m.N.tetricus.Abundance$Predicted.Species.Richness.500m))
Mean.500m.Species.Richness.Predictive.Difference.500m <- mean(TestData.500m.N.tetricus.Abundance$Difference.Pred.True.500m)



#### RESULTS SUMMARIES ####

best.model.selection.5m <- data.frame(as.numeric(5))
best.model.selection.10m <- data.frame(as.numeric(10))
best.model.selection.25m <- data.frame(as.numeric(25))
best.model.selection.50m <- data.frame(as.numeric(50))
best.model.selection.75m <- data.frame(as.numeric(75))
best.model.selection.100m <- data.frame(as.numeric(100))
best.model.selection.150m <- data.frame(as.numeric(150))
best.model.selection.200m <- data.frame(as.numeric(200))
best.model.selection.250m <- data.frame(as.numeric(250))
best.model.selection.300m <- data.frame(as.numeric(300))
best.model.selection.400m <- data.frame(as.numeric(400))
best.model.selection.500m <- data.frame(as.numeric(500))

colnames(best.model.selection.5m)[1] <- "Scale"
colnames(best.model.selection.10m)[1] <- "Scale"
colnames(best.model.selection.25m)[1] <- "Scale"
colnames(best.model.selection.50m)[1] <- "Scale"
colnames(best.model.selection.75m)[1] <- "Scale"
colnames(best.model.selection.100m)[1] <- "Scale"
colnames(best.model.selection.150m)[1] <- "Scale"
colnames(best.model.selection.200m)[1] <- "Scale"
colnames(best.model.selection.250m)[1] <- "Scale"
colnames(best.model.selection.300m)[1] <- "Scale"
colnames(best.model.selection.400m)[1] <- "Scale"
colnames(best.model.selection.500m)[1] <- "Scale"

best.model.selection.5m$Formula <- as.character(c(Cons_5m_SpRichness_ENTIRE$formula))
best.model.selection.10m$Formula <- as.character(c(Cons_10m_SpRichness_ENTIRE$formula))
best.model.selection.25m$Formula <- as.character(c(Cons_25m_SpRichness_ENTIRE$formula))
best.model.selection.50m$Formula <- as.character(c(Cons_50m_SpRichness_ENTIRE$formula))
best.model.selection.75m$Formula <- as.character(c(Cons_75m_SpRichness_ENTIRE$formula))
best.model.selection.100m$Formula <- as.character(c(Cons_100m_SpRichness_ENTIRE$formula))
best.model.selection.150m$Formula <- as.character(c(Cons_150m_SpRichness_ENTIRE$formula))
best.model.selection.200m$Formula <- as.character(c(Cons_200m_SpRichness_ENTIRE$formula))
best.model.selection.250m$Formula <- as.character(c(Cons_250m_SpRichness_ENTIRE$formula))
best.model.selection.300m$Formula <- as.character(c(Cons_300m_SpRichness_ENTIRE$formula))
best.model.selection.400m$Formula <- as.character(c(Cons_400m_SpRichness_ENTIRE$formula))
best.model.selection.500m$Formula <- as.character(c(Cons_500m_SpRichness_ENTIRE$formula))

best.model.selection.5m$AIC.mgcv <- as.character(c(Cons_5m_SpRichness_ENTIRE$aic))
best.model.selection.10m$AIC.mgcv <- as.character(c(Cons_10m_SpRichness_ENTIRE$aic))
best.model.selection.25m$AIC.mgcv <- as.character(c(Cons_25m_SpRichness_ENTIRE$aic))
best.model.selection.50m$AIC.mgcv <- as.character(c(Cons_50m_SpRichness_ENTIRE$aic))
best.model.selection.75m$AIC.mgcv <- as.character(c(Cons_75m_SpRichness_ENTIRE$aic))
best.model.selection.100m$AIC.mgcv <- as.character(c(Cons_100m_SpRichness_ENTIRE$aic))
best.model.selection.150m$AIC.mgcv <- as.character(c(Cons_150m_SpRichness_ENTIRE$aic))
best.model.selection.200m$AIC.mgcv <- as.character(c(Cons_200m_SpRichness_ENTIRE$aic))
best.model.selection.250m$AIC.mgcv <- as.character(c(Cons_250m_SpRichness_ENTIRE$aic))
best.model.selection.300m$AIC.mgcv <- as.character(c(Cons_300m_SpRichness_ENTIRE$aic))
best.model.selection.400m$AIC.mgcv <- as.character(c(Cons_400m_SpRichness_ENTIRE$aic))
best.model.selection.500m$AIC.mgcv <- as.character(c(Cons_500m_SpRichness_ENTIRE$aic))

best.model.selection.5m$Deviance.Explained <- as.numeric(((Cons_5m_SpRichness_ENTIRE$null.deviance - Cons_5m_SpRichness_ENTIRE$deviance)/Cons_5m_SpRichness_ENTIRE$null.deviance)*100)
best.model.selection.10m$Deviance.Explained <- as.numeric(((Cons_10m_SpRichness_ENTIRE$null.deviance - Cons_10m_SpRichness_ENTIRE$deviance)/Cons_10m_SpRichness_ENTIRE$null.deviance)*100)
best.model.selection.25m$Deviance.Explained <- as.numeric(((Cons_25m_SpRichness_ENTIRE$null.deviance - Cons_25m_SpRichness_ENTIRE$deviance)/Cons_25m_SpRichness_ENTIRE$null.deviance)*100)
best.model.selection.50m$Deviance.Explained <- as.numeric(((Cons_50m_SpRichness_ENTIRE$null.deviance - Cons_50m_SpRichness_ENTIRE$deviance)/Cons_50m_SpRichness_ENTIRE$null.deviance)*100)
best.model.selection.75m$Deviance.Explained <- as.numeric(((Cons_75m_SpRichness_ENTIRE$null.deviance - Cons_75m_SpRichness_ENTIRE$deviance)/Cons_75m_SpRichness_ENTIRE$null.deviance)*100)
best.model.selection.100m$Deviance.Explained <- as.numeric(((Cons_100m_SpRichness_ENTIRE$null.deviance - Cons_100m_SpRichness_ENTIRE$deviance)/Cons_100m_SpRichness_ENTIRE$null.deviance)*100)
best.model.selection.150m$Deviance.Explained <- as.numeric(((Cons_150m_SpRichness_ENTIRE$null.deviance - Cons_150m_SpRichness_ENTIRE$deviance)/Cons_150m_SpRichness_ENTIRE$null.deviance)*100)
best.model.selection.200m$Deviance.Explained <- as.numeric(((Cons_200m_SpRichness_ENTIRE$null.deviance - Cons_200m_SpRichness_ENTIRE$deviance)/Cons_200m_SpRichness_ENTIRE$null.deviance)*100)
best.model.selection.250m$Deviance.Explained <- as.numeric(((Cons_250m_SpRichness_ENTIRE$null.deviance - Cons_250m_SpRichness_ENTIRE$deviance)/Cons_250m_SpRichness_ENTIRE$null.deviance)*100)
best.model.selection.300m$Deviance.Explained <- as.numeric(((Cons_300m_SpRichness_ENTIRE$null.deviance - Cons_300m_SpRichness_ENTIRE$deviance)/Cons_300m_SpRichness_ENTIRE$null.deviance)*100)
best.model.selection.400m$Deviance.Explained <- as.numeric(((Cons_400m_SpRichness_ENTIRE$null.deviance - Cons_400m_SpRichness_ENTIRE$deviance)/Cons_400m_SpRichness_ENTIRE$null.deviance)*100)
best.model.selection.500m$Deviance.Explained <- as.numeric(((Cons_500m_SpRichness_ENTIRE$null.deviance - Cons_500m_SpRichness_ENTIRE$deviance)/Cons_500m_SpRichness_ENTIRE$null.deviance)*100)


BestModelSelection <- rbind(
  best.model.selection.5m,
  best.model.selection.10m,
  best.model.selection.25m,
  best.model.selection.50m,
  best.model.selection.75m,
  best.model.selection.100m,
  best.model.selection.150m,
  best.model.selection.200m,
  best.model.selection.250m,
  best.model.selection.300m,
  best.model.selection.400m,
  best.model.selection.500m
)

write.table(BestModelSelection, file = "01_BestModelSelection_SpeciesRichness.txt" , sep = "\t", row.names = FALSE)


N.tetricus.Abundance_AIC.mgcv_01 <- ggplot(data=BestModelSelection, aes(x=BestModelSelection$Scale, y=BestModelSelection$AIC.mgcv, group=1)) +
  geom_line()+
  geom_point()+
  xlab("Scale (m)") + ylab("AIC (mgcv)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
N.tetricus.Abundance_AIC.mgcv_01
ggsave("AICmgcv_N.tetricus.Abundance_01.png", plot = last_plot(), device = "png",
       scale = 1, dpi = 300, limitsize = TRUE)


N.tetricus.Abundance_DevExplained_01 <- ggplot(data=BestModelSelection, aes(x=BestModelSelection$Scale, y=BestModelSelection$Deviance.Explained, group=1)) +
  geom_line()+
  geom_point()+
  xlab("Scale (m)") + ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
N.tetricus.Abundance_DevExplained_01
ggsave("DevExplained_N.tetricus.Abundance_01.png", plot = last_plot(), device = "png",
       scale = 1, dpi = 300, limitsize = TRUE)






