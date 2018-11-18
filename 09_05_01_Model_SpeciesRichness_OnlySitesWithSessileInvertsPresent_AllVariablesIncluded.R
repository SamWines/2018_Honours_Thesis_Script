# Script for running GAMs on all combinations of predictor variables at multiple spatial scales

# This script is for modelling species richness of sites with sessile invertebrates present

# This script is used to:
# 1. Split data into training and test
# 2. Run tests of correlations and perform transformations if needed
# 3. Run models for all combinations of predictor variables
# 4. Use the MuMIn package to select the best performing model
# 5. Predict for test data
# 6. Repeat steps (1-5) for all 12 spatial scales 
# 7. Make summary of best models for each scale
# 8. Calculate variable importance of best model for script
# 9. Plot curves of predictors of best model for script
# 10. Plot metrics of model performance across spatial scales

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

# Set working directory
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/11_Modelling_Subsets/05_Sponge/")
getwd()
dir()

# Import Files
Import.5m.Site.Summaries <- read.table("01_5m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.10m.Site.Summaries <- read.table("01_10m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.25m.Site.Summaries <- read.table("01_25m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.50m.Site.Summaries <- read.table("01_50m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.75m.Site.Summaries <- read.table("01_75m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.100m.Site.Summaries <- read.table("01_100m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.150m.Site.Summaries <- read.table("01_150m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.200m.Site.Summaries <- read.table("01_200m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.250m.Site.Summaries <- read.table("01_250m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.300m.Site.Summaries <- read.table("01_300m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.400m.Site.Summaries <- read.table("01_400m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
Import.500m.Site.Summaries <- read.table("01_500m_SiteSummariesSPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 


getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/05_Sponge/SpeciesRichness_SiteExc/")
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


#  5m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.5m.SpeciesRichness <- Import.5m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
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
                #avg_wave_orb_vel_mean_5_clip_161,
                #SST_mean_5_clip_01,
                dist2reef_mean_5_clip_01
  )

TestData.5m.SpeciesRichness <- Import.5m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
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
                #SST_mean_5_clip_01,
                dist2reef_mean_5_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.5m.SpeciesRichness[, 2:length(TrainingData.5m.SpeciesRichness)], method = c("pairwise", "spearman"), geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.5m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.5m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.5m.SpeciesRichness$sqrt.rugosity_mean_5_clip_01=sqrt(TrainingData.5m.SpeciesRichness$rugosity_mean_5_clip_01)



# Models loop

data.5m <- TrainingData.5m.SpeciesRichness

vars.5m <- c("SpeciesRichness",
             #"Training0.Test1",
             "Year",
             "Status",
             #vrm_neigh3_mean_5_clip_01, 
             #slope_mean_5_clip_01, 
             "s(rugosity_mean_5_clip_01, k=3)", 
             #profile_curv_mean_5_clip_01, 
             #plan_curv_mean_5_clip_01,
             "s(northness_mean_5_clip_01, k=3)",
             #fbpi25_50_mean_5_clip_01,
             "s(fbpi10_30_mean_5_clip_01, k=3)",
             "s(eastness_mean_5_clip_01, k=3)",
             "s(curvature_mean_5_clip_01, k=3)",
             #bbpi100_200_mean_5_clip_01,
             "s(bathy_mean_5_clip_01, k=3)",
             #bathy_std_5_clip_01,
             #avg_wave_orb_vel_mean_5_clip_161,
             #"s(SST_mean_5_clip_01, k=3)",
             "s(dist2reef_mean_5_clip_01, k=3)")

N.5m <- list(1,2,3,4)

COMB.5m <- sapply(N.5m, function(m) combn(x=vars.5m[2:length(vars.5m)], m))

COMB2.5m <- list()
k.5m=0
for(i in seq(COMB.5m)){
  tmp.5m <- COMB.5m[[i]]
  for(j in seq(ncol(tmp.5m))){
    k.5m <- k.5m + 1
    COMB2.5m[[k.5m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.5m[,j], collapse=" + ")))
  }
}

ModelsResults.5m <- lapply(COMB2.5m,
                            function(x) gam(x, data=data.5m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.5m <-model.sel(ModelsResults.5m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.5m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.5m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.5m, 1/8 < weight/max(output.5m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.5m, cumsum(output.5m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.5m <-as.data.frame(output.5m)[(length(output.5m)-4):length(output.5m)] 
output.2.5m 

# a little clean-up, lets round things a bit 
output.2.5m[,2:3]<- round(output.2.5m[,2:3],2) 
output.2.5m[,4:5]<- round(output.2.5m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.5m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.5m$Model<-rownames(output.2.5m) 
output.2.5m

# Get the best model
best.model.5m <- as.integer(rownames(output.2.5m)[1])

summary(ModelsResults.5m[[best.model.5m]])


# Predict for test data
TestData.5m.SpeciesRichness$Predicted.Species.Richness.5m <- predict.gam(ModelsResults.5m[[best.model.5m]], TestData.5m.SpeciesRichness)
TestData.5m.SpeciesRichness$Difference.Pred.True.5m <- abs((TestData.5m.SpeciesRichness$SpeciesRichness) - (TestData.5m.SpeciesRichness$Predicted.Species.Richness.5m))
Mean.5m.Species.Richness.Predictive.Difference.5m <- mean(TestData.5m.SpeciesRichness$Difference.Pred.True.5m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html





#  10m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.10m.SpeciesRichness <- Import.10m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_10_clip_01, 
                #slope_mean_10_clip_01, 
                rugosity_mean_10_clip_01, 
                #profile_curv_mean_10_clip_01, 
                #plan_curv_mean_10_clip_01,
                northness_mean_10_clip_01,
                #fbpi25_50_mean_10_clip_01,
                fbpi10_30_mean_10_clip_01,
                eastness_mean_10_clip_01,
                curvature_mean_10_clip_01,
                #bbpi100_200_mean_10_clip_01,
                bathy_mean_10_clip_01,
                #bathy_std_10_clip_01,
                #avg_wave_orb_vel_mean_10_clip_161,
                #SST_mean_10_clip_01,
                dist2reef_mean_10_clip_01
  )

TestData.10m.SpeciesRichness <- Import.10m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_10_clip_01, 
                slope_mean_10_clip_01, 
                rugosity_mean_10_clip_01, 
                profile_curv_mean_10_clip_01, 
                plan_curv_mean_10_clip_01,
                northness_mean_10_clip_01,
                fbpi25_50_mean_10_clip_01,
                fbpi10_30_mean_10_clip_01,
                eastness_mean_10_clip_01,
                curvature_mean_10_clip_01,
                bbpi100_200_mean_10_clip_01,
                bathy_mean_10_clip_01,
                bathy_std_10_clip_01,
                avg_wave_orb_vel_mean_10_clip_01,
                #SST_mean_10_clip_01,
                dist2reef_mean_10_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.10m.SpeciesRichness[, 2:length(TrainingData.10m.SpeciesRichness)], method = c("pairwise", "spearman"), geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.10m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.10m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.10m.SpeciesRichness$sqrt.rugosity_mean_10_clip_01=sqrt(TrainingData.10m.SpeciesRichness$rugosity_mean_10_clip_01)



# Models loop

data.10m <- TrainingData.10m.SpeciesRichness

vars.10m <- c("SpeciesRichness",
              #"Training0.Test1",
              "Year",
              "Status",
              #vrm_neigh3_mean_5_clip_01, 
              #slope_mean_5_clip_01, 
              "s(rugosity_mean_10_clip_01, k=3)", 
              #profile_curv_mean_5_clip_01, 
              #plan_curv_mean_5_clip_01,
              "s(northness_mean_10_clip_01, k=3)",
              #fbpi25_50_mean_5_clip_01,
              "s(fbpi10_30_mean_10_clip_01, k=3)",
              "s(eastness_mean_10_clip_01, k=3)",
              "s(curvature_mean_10_clip_01, k=3)",
              #bbpi100_200_mean_5_clip_01,
              "s(bathy_mean_10_clip_01, k=3)",
              #bathy_std_5_clip_01,
              #avg_wave_orb_vel_mean_5_clip_161,
              #"s(SST_mean_10_clip_01, k=3)",
              "s(dist2reef_mean_10_clip_01, k=3)")

N.10m <- list(1,2,3,4)

COMB.10m <- sapply(N.10m, function(m) combn(x=vars.10m[2:length(vars.10m)], m))

COMB2.10m <- list()
k.10m=0
for(i in seq(COMB.10m)){
  tmp.10m <- COMB.10m[[i]]
  for(j in seq(ncol(tmp.10m))){
    k.10m <- k.10m + 1
    COMB2.10m[[k.10m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.10m[,j], collapse=" + ")))
  }
}

ModelsResults.10m <- lapply(COMB2.10m,
                           function(x) gam(x, data=data.10m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.10m <-model.sel(ModelsResults.10m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.10m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.10m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.10m, 1/8 < weight/max(output.10m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.10m, cumsum(output.10m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.10m <-as.data.frame(output.10m)[(length(output.10m)-4):length(output.10m)] 
output.2.10m 

# a little clean-up, lets round things a bit 
output.2.10m[,2:3]<- round(output.2.10m[,2:3],2) 
output.2.10m[,4:5]<- round(output.2.10m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.10m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.10m$Model<-rownames(output.2.10m) 
output.2.10m

# Get the best model
best.model.10m <- as.integer(rownames(output.2.10m)[1])

summary(ModelsResults.10m[[best.model.10m]])


# Predict for test data
TestData.10m.SpeciesRichness$Predicted.Species.Richness.10m <- predict.gam(ModelsResults.10m[[best.model.10m]], TestData.10m.SpeciesRichness)
TestData.10m.SpeciesRichness$Difference.Pred.True.10m <- abs((TestData.10m.SpeciesRichness$SpeciesRichness) - (TestData.10m.SpeciesRichness$Predicted.Species.Richness.10m))
Mean.10m.Species.Richness.Predictive.Difference.10m <- mean(TestData.10m.SpeciesRichness$Difference.Pred.True.10m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html




#  25m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.25m.SpeciesRichness <- Import.25m.Site.Summaries %>%
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
                #SST_mean_25_clip_01,
                dist2reef_mean_25_clip_01
  )

TestData.25m.SpeciesRichness <- Import.25m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_25_clip_01, 
                slope_mean_25_clip_01, 
                rugosity_mean_25_clip_01, 
                profile_curv_mean_25_clip_01, 
                plan_curv_mean_25_clip_01,
                northness_mean_25_clip_01,
                fbpi25_50_mean_25_clip_01,
                fbpi10_30_mean_25_clip_01,
                eastness_mean_25_clip_01,
                curvature_mean_25_clip_01,
                bbpi100_200_mean_25_clip_01,
                bathy_mean_25_clip_01,
                bathy_std_25_clip_01,
                avg_wave_orb_vel_mean_25_clip_01,
                #SST_mean_25_clip_01,
                dist2reef_mean_25_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.25m.SpeciesRichness[, 2:length(TrainingData.25m.SpeciesRichness)], geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.25m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.25m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.25m.SpeciesRichness$sqrt.rugosity_mean_25_clip_01=sqrt(TrainingData.25m.SpeciesRichness$rugosity_mean_25_clip_01)



# Models loop

data.25m <- TrainingData.25m.SpeciesRichness

vars.25m <- c("SpeciesRichness",
              #"Training0.Test1",
              "Year",
              "Status",
              #vrm_neigh3_mean_5_clip_01, 
              #slope_mean_5_clip_01, 
              "s(rugosity_mean_25_clip_01, k=3)", 
              #profile_curv_mean_5_clip_01, 
              #plan_curv_mean_5_clip_01,
              "s(northness_mean_25_clip_01, k=3)",
              #fbpi25_50_mean_5_clip_01,
              "s(fbpi10_30_mean_25_clip_01, k=3)",
              "s(eastness_mean_25_clip_01, k=3)",
              "s(curvature_mean_25_clip_01, k=3)",
              #bbpi100_200_mean_5_clip_01,
              "s(bathy_mean_25_clip_01, k=3)",
              #bathy_std_5_clip_01,
              #avg_wave_orb_vel_mean_5_clip_161,
              #"s(SST_mean_25_clip_01, k=3)",
              "s(dist2reef_mean_25_clip_01, k=3)")

N.25m <- list(1,2,3,4)

COMB.25m <- sapply(N.25m, function(m) combn(x=vars.25m[2:length(vars.25m)], m))

COMB2.25m <- list()
k.25m=0
for(i in seq(COMB.25m)){
  tmp.25m <- COMB.25m[[i]]
  for(j in seq(ncol(tmp.25m))){
    k.25m <- k.25m + 1
    COMB2.25m[[k.25m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.25m[,j], collapse=" + ")))
  }
}

ModelsResults.25m <- lapply(COMB2.25m,
                            function(x) gam(x, data=data.25m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.25m <-model.sel(ModelsResults.25m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.25m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.25m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.25m, 1/8 < weight/max(output.25m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.25m, cumsum(output.25m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.25m <-as.data.frame(output.25m)[(length(output.25m)-4):length(output.25m)] 
output.2.25m 

# a little clean-up, lets round things a bit 
output.2.25m[,2:3]<- round(output.2.25m[,2:3],2) 
output.2.25m[,4:5]<- round(output.2.25m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.25m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.25m$Model<-rownames(output.2.25m) 
output.2.25m

# Get the best model
best.model.25m <- as.integer(rownames(output.2.25m)[1])

summary(ModelsResults.25m[[best.model.25m]])


# Predict for test data
TestData.25m.SpeciesRichness$Predicted.Species.Richness.25m <- predict.gam(ModelsResults.25m[[best.model.25m]], TestData.25m.SpeciesRichness)
TestData.25m.SpeciesRichness$Difference.Pred.True.25m <- abs((TestData.25m.SpeciesRichness$SpeciesRichness) - (TestData.25m.SpeciesRichness$Predicted.Species.Richness.25m))
Mean.25m.Species.Richness.Predictive.Difference.25m <- mean(TestData.25m.SpeciesRichness$Difference.Pred.True.25m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html








#  50m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.50m.SpeciesRichness <- Import.50m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
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

TestData.50m.SpeciesRichness <- Import.50m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_50_clip_01, 
                slope_mean_50_clip_01, 
                rugosity_mean_50_clip_01, 
                profile_curv_mean_50_clip_01, 
                plan_curv_mean_50_clip_01,
                northness_mean_50_clip_01,
                fbpi25_50_mean_50_clip_01,
                fbpi10_30_mean_50_clip_01,
                eastness_mean_50_clip_01,
                curvature_mean_50_clip_01,
                bbpi100_200_mean_50_clip_01,
                bathy_mean_50_clip_01,
                bathy_std_50_clip_01,
                avg_wave_orb_vel_mean_50_clip_01,
                #SST_mean_50_clip_01,
                dist2reef_mean_50_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.50m.SpeciesRichness[, 2:length(TrainingData.50m.SpeciesRichness)], geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.50m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.50m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.50m.SpeciesRichness$sqrt.rugosity_mean_50_clip_01=sqrt(TrainingData.50m.SpeciesRichness$rugosity_mean_50_clip_01)



# Models loop

data.50m <- TrainingData.50m.SpeciesRichness

vars.50m <- c("SpeciesRichness",
              #"Training0.Test1",
              "Year",
              "Status",
              #vrm_neigh3_mean_5_clip_01, 
              #slope_mean_5_clip_01, 
              "s(rugosity_mean_50_clip_01, k=3)", 
              #profile_curv_mean_5_clip_01, 
              #plan_curv_mean_5_clip_01,
              "s(northness_mean_50_clip_01, k=3)",
              #fbpi25_50_mean_5_clip_01,
              "s(fbpi10_30_mean_50_clip_01, k=3)",
              "s(eastness_mean_50_clip_01, k=3)",
              "s(curvature_mean_50_clip_01, k=3)",
              #bbpi100_200_mean_5_clip_01,
              "s(bathy_mean_50_clip_01, k=3)",
              #bathy_std_5_clip_01,
              #avg_wave_orb_vel_mean_5_clip_161,
              #"s(SST_mean_50_clip_01, k=3)",
              "s(dist2reef_mean_50_clip_01, k=3)")

N.50m <- list(1,2,3,4)

COMB.50m <- sapply(N.50m, function(m) combn(x=vars.50m[2:length(vars.50m)], m))

COMB2.50m <- list()
k.50m=0
for(i in seq(COMB.50m)){
  tmp.50m <- COMB.50m[[i]]
  for(j in seq(ncol(tmp.50m))){
    k.50m <- k.50m + 1
    COMB2.50m[[k.50m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.50m[,j], collapse=" + ")))
  }
}

ModelsResults.50m <- lapply(COMB2.50m,
                            function(x) gam(x, data=data.50m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.50m <-model.sel(ModelsResults.50m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.50m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.50m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.50m, 1/8 < weight/max(output.50m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.50m, cumsum(output.50m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.50m <-as.data.frame(output.50m)[(length(output.50m)-4):length(output.50m)] 
output.2.50m 

# a little clean-up, lets round things a bit 
output.2.50m[,2:3]<- round(output.2.50m[,2:3],2) 
output.2.50m[,4:5]<- round(output.2.50m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.50m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.50m$Model<-rownames(output.2.50m) 
output.2.50m

# Get the best model
best.model.50m <- as.integer(rownames(output.2.50m)[1])

summary(ModelsResults.50m[[best.model.50m]])


# Predict for test data
TestData.50m.SpeciesRichness$Predicted.Species.Richness.50m <- predict.gam(ModelsResults.50m[[best.model.50m]], TestData.50m.SpeciesRichness)
TestData.50m.SpeciesRichness$Difference.Pred.True.50m <- abs((TestData.50m.SpeciesRichness$SpeciesRichness) - (TestData.50m.SpeciesRichness$Predicted.Species.Richness.50m))
Mean.50m.Species.Richness.Predictive.Difference.50m <- mean(TestData.50m.SpeciesRichness$Difference.Pred.True.50m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html





#  75m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.75m.SpeciesRichness <- Import.75m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_75_clip_01, 
                #slope_mean_75_clip_01, 
                rugosity_mean_75_clip_01, 
                #profile_curv_mean_75_clip_01, 
                #plan_curv_mean_75_clip_01,
                northness_mean_75_clip_01,
                #fbpi25_50_mean_75_clip_01,
                fbpi10_30_mean_75_clip_01,
                eastness_mean_75_clip_01,
                curvature_mean_75_clip_01,
                #bbpi100_200_mean_75_clip_01,
                bathy_mean_75_clip_01,
                #bathy_std_75_clip_01,
                #avg_wave_orb_vel_mean_75_clip_161,
                #SST_mean_75_clip_01,
                dist2reef_mean_75_clip_01
  )

TestData.75m.SpeciesRichness <- Import.75m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_75_clip_01, 
                slope_mean_75_clip_01, 
                rugosity_mean_75_clip_01, 
                profile_curv_mean_75_clip_01, 
                plan_curv_mean_75_clip_01,
                northness_mean_75_clip_01,
                fbpi25_50_mean_75_clip_01,
                fbpi10_30_mean_75_clip_01,
                eastness_mean_75_clip_01,
                curvature_mean_75_clip_01,
                bbpi100_200_mean_75_clip_01,
                bathy_mean_75_clip_01,
                bathy_std_75_clip_01,
                avg_wave_orb_vel_mean_75_clip_01,
                #SST_mean_75_clip_01,
                dist2reef_mean_75_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.75m.SpeciesRichness[, 2:length(TrainingData.75m.SpeciesRichness)], geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.75m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.75m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.75m.SpeciesRichness$sqrt.rugosity_mean_75_clip_01=sqrt(TrainingData.75m.SpeciesRichness$rugosity_mean_75_clip_01)



# Models loop

data.75m <- TrainingData.75m.SpeciesRichness

vars.75m <- c("SpeciesRichness",
              #"Training0.Test1",
              "Year",
              "Status",
              #vrm_neigh3_mean_5_clip_01, 
              #slope_mean_5_clip_01, 
              "s(rugosity_mean_75_clip_01, k=3)", 
              #profile_curv_mean_5_clip_01, 
              #plan_curv_mean_5_clip_01,
              "s(northness_mean_75_clip_01, k=3)",
              #fbpi25_50_mean_5_clip_01,
              "s(fbpi10_30_mean_75_clip_01, k=3)",
              "s(eastness_mean_75_clip_01, k=3)",
              "s(curvature_mean_75_clip_01, k=3)",
              #bbpi100_200_mean_5_clip_01,
              "s(bathy_mean_75_clip_01, k=3)",
              #bathy_std_5_clip_01,
              #avg_wave_orb_vel_mean_5_clip_161,
              #"s(SST_mean_75_clip_01, k=3)",
              "s(dist2reef_mean_75_clip_01, k=3)")

N.75m <- list(1,2,3,4)

COMB.75m <- sapply(N.75m, function(m) combn(x=vars.75m[2:length(vars.75m)], m))

COMB2.75m <- list()
k.75m=0
for(i in seq(COMB.75m)){
  tmp.75m <- COMB.75m[[i]]
  for(j in seq(ncol(tmp.75m))){
    k.75m <- k.75m + 1
    COMB2.75m[[k.75m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.75m[,j], collapse=" + ")))
  }
}

ModelsResults.75m <- lapply(COMB2.75m,
                             function(x) gam(x, data=data.75m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.75m <-model.sel(ModelsResults.75m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.75m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.75m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.75m, 1/8 < weight/max(output.75m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.75m, cumsum(output.75m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.75m <-as.data.frame(output.75m)[(length(output.75m)-4):length(output.75m)] 
output.2.75m 

# a little clean-up, lets round things a bit 
output.2.75m[,2:3]<- round(output.2.75m[,2:3],2) 
output.2.75m[,4:5]<- round(output.2.75m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.75m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.75m$Model<-rownames(output.2.75m) 
output.2.75m

# Get the best model
best.model.75m <- as.integer(rownames(output.2.75m)[1])

summary(ModelsResults.75m[[best.model.75m]])


# Predict for test data
TestData.75m.SpeciesRichness$Predicted.Species.Richness.75m <- predict.gam(ModelsResults.75m[[best.model.75m]], TestData.75m.SpeciesRichness)
TestData.75m.SpeciesRichness$Difference.Pred.True.75m <- abs((TestData.75m.SpeciesRichness$SpeciesRichness) - (TestData.75m.SpeciesRichness$Predicted.Species.Richness.75m))
Mean.75m.Species.Richness.Predictive.Difference.75m <- mean(TestData.75m.SpeciesRichness$Difference.Pred.True.75m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html















#  100m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.100m.SpeciesRichness <- Import.100m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
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

TestData.100m.SpeciesRichness <- Import.100m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_100_clip_01, 
                slope_mean_100_clip_01, 
                rugosity_mean_100_clip_01, 
                profile_curv_mean_100_clip_01, 
                plan_curv_mean_100_clip_01,
                northness_mean_100_clip_01,
                fbpi25_50_mean_100_clip_01,
                fbpi10_30_mean_100_clip_01,
                eastness_mean_100_clip_01,
                curvature_mean_100_clip_01,
                bbpi100_200_mean_100_clip_01,
                bathy_mean_100_clip_01,
                bathy_std_100_clip_01,
                avg_wave_orb_vel_mean_100_clip_01,
                #SST_mean_100_clip_01,
                dist2reef_mean_100_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.100m.SpeciesRichness[, 2:length(TrainingData.100m.SpeciesRichness)], geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.100m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.100m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.100m.SpeciesRichness$sqrt.rugosity_mean_100_clip_01=sqrt(TrainingData.100m.SpeciesRichness$rugosity_mean_100_clip_01)



# Models loop

data.100m <- TrainingData.100m.SpeciesRichness

vars.100m <- c("SpeciesRichness",
               #"Training0.Test1",
               "Year",
               "Status",
               #vrm_neigh3_mean_5_clip_01, 
               #slope_mean_5_clip_01, 
               "s(rugosity_mean_100_clip_01, k=3)", 
               #profile_curv_mean_5_clip_01, 
               #plan_curv_mean_5_clip_01,
               "s(northness_mean_100_clip_01, k=3)",
               #fbpi25_50_mean_5_clip_01,
               "s(fbpi10_30_mean_100_clip_01, k=3)",
               "s(eastness_mean_100_clip_01, k=3)",
               "s(curvature_mean_100_clip_01, k=3)",
               #bbpi100_200_mean_5_clip_01,
               "s(bathy_mean_100_clip_01, k=3)",
               #bathy_std_5_clip_01,
               #avg_wave_orb_vel_mean_5_clip_161,
               #"s(SST_mean_100_clip_01, k=3)",
               "s(dist2reef_mean_100_clip_01, k=3)")

N.100m <- list(1,2,3,4)

COMB.100m <- sapply(N.100m, function(m) combn(x=vars.100m[2:length(vars.100m)], m))

COMB2.100m <- list()
k.100m=0
for(i in seq(COMB.100m)){
  tmp.100m <- COMB.100m[[i]]
  for(j in seq(ncol(tmp.100m))){
    k.100m <- k.100m + 1
    COMB2.100m[[k.100m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.100m[,j], collapse=" + ")))
  }
}

ModelsResults.100m <- lapply(COMB2.100m,
                            function(x) gam(x, data=data.100m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.100m <-model.sel(ModelsResults.100m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.100m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.100m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.100m, 1/8 < weight/max(output.100m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.100m, cumsum(output.100m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.100m <-as.data.frame(output.100m)[(length(output.100m)-4):length(output.100m)] 
output.2.100m 

# a little clean-up, lets round things a bit 
output.2.100m[,2:3]<- round(output.2.100m[,2:3],2) 
output.2.100m[,4:5]<- round(output.2.100m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.100m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.100m$Model<-rownames(output.2.100m) 
output.2.100m

# Get the best model
best.model.100m <- as.integer(rownames(output.2.100m)[1])

summary(ModelsResults.100m[[best.model.100m]])


# Predict for test data
TestData.100m.SpeciesRichness$Predicted.Species.Richness.100m <- predict.gam(ModelsResults.100m[[best.model.100m]], TestData.100m.SpeciesRichness)
TestData.100m.SpeciesRichness$Difference.Pred.True.100m <- abs((TestData.100m.SpeciesRichness$SpeciesRichness) - (TestData.100m.SpeciesRichness$Predicted.Species.Richness.100m))
Mean.100m.Species.Richness.Predictive.Difference.100m <- mean(TestData.100m.SpeciesRichness$Difference.Pred.True.100m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html















#  150m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.150m.SpeciesRichness <- Import.150m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_150_clip_01, 
                #slope_mean_150_clip_01, 
                rugosity_mean_150_clip_01, 
                #profile_curv_mean_150_clip_01, 
                #plan_curv_mean_150_clip_01,
                northness_mean_150_clip_01,
                #fbpi25_50_mean_150_clip_01,
                fbpi10_30_mean_150_clip_01,
                eastness_mean_150_clip_01,
                curvature_mean_150_clip_01,
                #bbpi100_200_mean_150_clip_01,
                bathy_mean_150_clip_01,
                #bathy_std_150_clip_01,
                #avg_wave_orb_vel_mean_150_clip_161,
                #SST_mean_150_clip_01,
                dist2reef_mean_150_clip_01
  )

TestData.150m.SpeciesRichness <- Import.150m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_150_clip_01, 
                slope_mean_150_clip_01, 
                rugosity_mean_150_clip_01, 
                profile_curv_mean_150_clip_01, 
                plan_curv_mean_150_clip_01,
                northness_mean_150_clip_01,
                fbpi25_50_mean_150_clip_01,
                fbpi10_30_mean_150_clip_01,
                eastness_mean_150_clip_01,
                curvature_mean_150_clip_01,
                bbpi100_200_mean_150_clip_01,
                bathy_mean_150_clip_01,
                bathy_std_150_clip_01,
                avg_wave_orb_vel_mean_150_clip_01,
                #SST_mean_150_clip_01,
                dist2reef_mean_150_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.150m.SpeciesRichness[, 2:length(TrainingData.150m.SpeciesRichness)], geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.150m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.150m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.150m.SpeciesRichness$sqrt.rugosity_mean_150_clip_01=sqrt(TrainingData.150m.SpeciesRichness$rugosity_mean_150_clip_01)



# Models loop

data.150m <- TrainingData.150m.SpeciesRichness

vars.150m <- c("SpeciesRichness",
               #"Training0.Test1",
               "Year",
               "Status",
               #vrm_neigh3_mean_5_clip_01, 
               #slope_mean_5_clip_01, 
               "s(rugosity_mean_150_clip_01, k=3)", 
               #profile_curv_mean_5_clip_01, 
               #plan_curv_mean_5_clip_01,
               "s(northness_mean_150_clip_01, k=3)",
               #fbpi25_50_mean_5_clip_01,
               "s(fbpi10_30_mean_150_clip_01, k=3)",
               "s(eastness_mean_150_clip_01, k=3)",
               "s(curvature_mean_150_clip_01, k=3)",
               #bbpi100_200_mean_5_clip_01,
               "s(bathy_mean_150_clip_01, k=3)",
               #bathy_std_5_clip_01,
               #avg_wave_orb_vel_mean_5_clip_161,
               #"s(SST_mean_150_clip_01, k=3)",
               "s(dist2reef_mean_150_clip_01, k=3)")

N.150m <- list(1,2,3,4)

COMB.150m <- sapply(N.150m, function(m) combn(x=vars.150m[2:length(vars.150m)], m))

COMB2.150m <- list()
k.150m=0
for(i in seq(COMB.150m)){
  tmp.150m <- COMB.150m[[i]]
  for(j in seq(ncol(tmp.150m))){
    k.150m <- k.150m + 1
    COMB2.150m[[k.150m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.150m[,j], collapse=" + ")))
  }
}

ModelsResults.150m <- lapply(COMB2.150m,
                             function(x) gam(x, data=data.150m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.150m <-model.sel(ModelsResults.150m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.150m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.150m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.150m, 1/8 < weight/max(output.150m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.150m, cumsum(output.150m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.150m <-as.data.frame(output.150m)[(length(output.150m)-4):length(output.150m)] 
output.2.150m 

# a little clean-up, lets round things a bit 
output.2.150m[,2:3]<- round(output.2.150m[,2:3],2) 
output.2.150m[,4:5]<- round(output.2.150m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.150m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.150m$Model<-rownames(output.2.150m) 
output.2.150m

# Get the best model
best.model.150m <- as.integer(rownames(output.2.150m)[1])

summary(ModelsResults.150m[[best.model.150m]])


# Predict for test data
TestData.150m.SpeciesRichness$Predicted.Species.Richness.150m <- predict.gam(ModelsResults.150m[[best.model.150m]], TestData.150m.SpeciesRichness)
TestData.150m.SpeciesRichness$Difference.Pred.True.150m <- abs((TestData.150m.SpeciesRichness$SpeciesRichness) - (TestData.150m.SpeciesRichness$Predicted.Species.Richness.150m))
Mean.150m.Species.Richness.Predictive.Difference.150m <- mean(TestData.150m.SpeciesRichness$Difference.Pred.True.150m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html















#  200m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.200m.SpeciesRichness <- Import.200m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_200_clip_01, 
                #slope_mean_200_clip_01, 
                rugosity_mean_200_clip_01, 
                #profile_curv_mean_200_clip_01, 
                #plan_curv_mean_200_clip_01,
                northness_mean_200_clip_01,
                #fbpi25_50_mean_200_clip_01,
                fbpi10_30_mean_200_clip_01,
                eastness_mean_200_clip_01,
                curvature_mean_200_clip_01,
                #bbpi100_200_mean_200_clip_01,
                bathy_mean_200_clip_01,
                #bathy_std_200_clip_01,
                #avg_wave_orb_vel_mean_200_clip_161,
                #SST_mean_200_clip_01,
                dist2reef_mean_200_clip_01
  )

TestData.200m.SpeciesRichness <- Import.200m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_200_clip_01, 
                slope_mean_200_clip_01, 
                rugosity_mean_200_clip_01, 
                profile_curv_mean_200_clip_01, 
                plan_curv_mean_200_clip_01,
                northness_mean_200_clip_01,
                fbpi25_50_mean_200_clip_01,
                fbpi10_30_mean_200_clip_01,
                eastness_mean_200_clip_01,
                curvature_mean_200_clip_01,
                bbpi100_200_mean_200_clip_01,
                bathy_mean_200_clip_01,
                bathy_std_200_clip_01,
                avg_wave_orb_vel_mean_200_clip_01,
                #SST_mean_200_clip_01,
                dist2reef_mean_200_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.200m.SpeciesRichness[, 2:length(TrainingData.200m.SpeciesRichness)], geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.200m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.200m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.200m.SpeciesRichness$sqrt.rugosity_mean_200_clip_01=sqrt(TrainingData.200m.SpeciesRichness$rugosity_mean_200_clip_01)



# Models loop

data.200m <- TrainingData.200m.SpeciesRichness

vars.200m <- c("SpeciesRichness",
               #"Training0.Test1",
               "Year",
               "Status",
               #vrm_neigh3_mean_5_clip_01, 
               #slope_mean_5_clip_01, 
               "s(rugosity_mean_200_clip_01, k=3)", 
               #profile_curv_mean_5_clip_01, 
               #plan_curv_mean_5_clip_01,
               "s(northness_mean_200_clip_01, k=3)",
               #fbpi25_50_mean_5_clip_01,
               "s(fbpi10_30_mean_200_clip_01, k=3)",
               "s(eastness_mean_200_clip_01, k=3)",
               "s(curvature_mean_200_clip_01, k=3)",
               #bbpi100_200_mean_5_clip_01,
               "s(bathy_mean_200_clip_01, k=3)",
               #bathy_std_5_clip_01,
               #avg_wave_orb_vel_mean_5_clip_161,
               #"s(SST_mean_200_clip_01, k=3)",
               "s(dist2reef_mean_200_clip_01, k=3)")

N.200m <- list(1,2,3,4)

COMB.200m <- sapply(N.200m, function(m) combn(x=vars.200m[2:length(vars.200m)], m))

COMB2.200m <- list()
k.200m=0
for(i in seq(COMB.200m)){
  tmp.200m <- COMB.200m[[i]]
  for(j in seq(ncol(tmp.200m))){
    k.200m <- k.200m + 1
    COMB2.200m[[k.200m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.200m[,j], collapse=" + ")))
  }
}

ModelsResults.200m <- lapply(COMB2.200m,
                             function(x) gam(x, data=data.200m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.200m <-model.sel(ModelsResults.200m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.200m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.200m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.200m, 1/8 < weight/max(output.200m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.200m, cumsum(output.200m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.200m <-as.data.frame(output.200m)[(length(output.200m)-4):length(output.200m)] 
output.2.200m 

# a little clean-up, lets round things a bit 
output.2.200m[,2:3]<- round(output.2.200m[,2:3],2) 
output.2.200m[,4:5]<- round(output.2.200m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.200m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.200m$Model<-rownames(output.2.200m) 
output.2.200m

# Get the best model
best.model.200m <- as.integer(rownames(output.2.200m)[1])

summary(ModelsResults.200m[[best.model.200m]])


# Predict for test data
TestData.200m.SpeciesRichness$Predicted.Species.Richness.200m <- predict.gam(ModelsResults.200m[[best.model.200m]], TestData.200m.SpeciesRichness)
TestData.200m.SpeciesRichness$Difference.Pred.True.200m <- abs((TestData.200m.SpeciesRichness$SpeciesRichness) - (TestData.200m.SpeciesRichness$Predicted.Species.Richness.200m))
Mean.200m.Species.Richness.Predictive.Difference.200m <- mean(TestData.200m.SpeciesRichness$Difference.Pred.True.200m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html















#  250m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.250m.SpeciesRichness <- Import.250m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_250_clip_01, 
                #slope_mean_250_clip_01, 
                rugosity_mean_250_clip_01, 
                #profile_curv_mean_250_clip_01, 
                #plan_curv_mean_250_clip_01,
                northness_mean_250_clip_01,
                #fbpi25_50_mean_250_clip_01,
                fbpi10_30_mean_250_clip_01,
                eastness_mean_250_clip_01,
                curvature_mean_250_clip_01,
                #bbpi100_200_mean_250_clip_01,
                bathy_mean_250_clip_01,
                #bathy_std_250_clip_01,
                #avg_wave_orb_vel_mean_250_clip_161,
                #SST_mean_250_clip_01,
                dist2reef_mean_250_clip_01
  )

TestData.250m.SpeciesRichness <- Import.250m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_250_clip_01, 
                slope_mean_250_clip_01, 
                rugosity_mean_250_clip_01, 
                profile_curv_mean_250_clip_01, 
                plan_curv_mean_250_clip_01,
                northness_mean_250_clip_01,
                fbpi25_50_mean_250_clip_01,
                fbpi10_30_mean_250_clip_01,
                eastness_mean_250_clip_01,
                curvature_mean_250_clip_01,
                bbpi100_200_mean_250_clip_01,
                bathy_mean_250_clip_01,
                bathy_std_250_clip_01,
                avg_wave_orb_vel_mean_250_clip_01,
                #SST_mean_250_clip_01,
                dist2reef_mean_250_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.250m.SpeciesRichness[, 2:length(TrainingData.250m.SpeciesRichness)], geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.250m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.250m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.250m.SpeciesRichness$sqrt.rugosity_mean_250_clip_01=sqrt(TrainingData.250m.SpeciesRichness$rugosity_mean_250_clip_01)



# Models loop

data.250m <- TrainingData.250m.SpeciesRichness

vars.250m <- c("SpeciesRichness",
               #"Training0.Test1",
               "Year",
               "Status",
               #vrm_neigh3_mean_5_clip_01, 
               #slope_mean_5_clip_01, 
               "s(rugosity_mean_250_clip_01, k=3)", 
               #profile_curv_mean_5_clip_01, 
               #plan_curv_mean_5_clip_01,
               "s(northness_mean_250_clip_01, k=3)",
               #fbpi25_50_mean_5_clip_01,
               "s(fbpi10_30_mean_250_clip_01, k=3)",
               "s(eastness_mean_250_clip_01, k=3)",
               "s(curvature_mean_250_clip_01, k=3)",
               #bbpi100_200_mean_5_clip_01,
               "s(bathy_mean_250_clip_01, k=3)",
               #bathy_std_5_clip_01,
               #avg_wave_orb_vel_mean_5_clip_161,
               #"s(SST_mean_250_clip_01, k=3)",
               "s(dist2reef_mean_250_clip_01, k=3)")

N.250m <- list(1,2,3,4)

COMB.250m <- sapply(N.250m, function(m) combn(x=vars.250m[2:length(vars.250m)], m))

COMB2.250m <- list()
k.250m=0
for(i in seq(COMB.250m)){
  tmp.250m <- COMB.250m[[i]]
  for(j in seq(ncol(tmp.250m))){
    k.250m <- k.250m + 1
    COMB2.250m[[k.250m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.250m[,j], collapse=" + ")))
  }
}

ModelsResults.250m <- lapply(COMB2.250m,
                             function(x) gam(x, data=data.250m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.250m <-model.sel(ModelsResults.250m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.250m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.250m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.250m, 1/8 < weight/max(output.250m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.250m, cumsum(output.250m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.250m <-as.data.frame(output.250m)[(length(output.250m)-4):length(output.250m)] 
output.2.250m 

# a little clean-up, lets round things a bit 
output.2.250m[,2:3]<- round(output.2.250m[,2:3],2) 
output.2.250m[,4:5]<- round(output.2.250m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.250m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.250m$Model<-rownames(output.2.250m) 
output.2.250m

# Get the best model
best.model.250m <- as.integer(rownames(output.2.250m)[1])

summary(ModelsResults.250m[[best.model.250m]])


# Predict for test data
TestData.250m.SpeciesRichness$Predicted.Species.Richness.250m <- predict.gam(ModelsResults.250m[[best.model.250m]], TestData.250m.SpeciesRichness)
TestData.250m.SpeciesRichness$Difference.Pred.True.250m <- abs((TestData.250m.SpeciesRichness$SpeciesRichness) - (TestData.250m.SpeciesRichness$Predicted.Species.Richness.250m))
Mean.250m.Species.Richness.Predictive.Difference.250m <- mean(TestData.250m.SpeciesRichness$Difference.Pred.True.250m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html















#  300m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.300m.SpeciesRichness <- Import.300m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_300_clip_01, 
                #slope_mean_300_clip_01, 
                rugosity_mean_300_clip_01, 
                #profile_curv_mean_300_clip_01, 
                #plan_curv_mean_300_clip_01,
                northness_mean_300_clip_01,
                #fbpi25_50_mean_300_clip_01,
                fbpi10_30_mean_300_clip_01,
                eastness_mean_300_clip_01,
                curvature_mean_300_clip_01,
                #bbpi100_200_mean_300_clip_01,
                bathy_mean_300_clip_01,
                #bathy_std_300_clip_01,
                #avg_wave_orb_vel_mean_300_clip_161,
                #SST_mean_300_clip_01,
                dist2reef_mean_300_clip_01
  )

TestData.300m.SpeciesRichness <- Import.300m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_300_clip_01, 
                slope_mean_300_clip_01, 
                rugosity_mean_300_clip_01, 
                profile_curv_mean_300_clip_01, 
                plan_curv_mean_300_clip_01,
                northness_mean_300_clip_01,
                fbpi25_50_mean_300_clip_01,
                fbpi10_30_mean_300_clip_01,
                eastness_mean_300_clip_01,
                curvature_mean_300_clip_01,
                bbpi100_200_mean_300_clip_01,
                bathy_mean_300_clip_01,
                bathy_std_300_clip_01,
                avg_wave_orb_vel_mean_300_clip_01,
                #SST_mean_300_clip_01,
                dist2reef_mean_300_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.300m.SpeciesRichness[, 2:length(TrainingData.300m.SpeciesRichness)], geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.300m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.300m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.300m.SpeciesRichness$sqrt.rugosity_mean_300_clip_01=sqrt(TrainingData.300m.SpeciesRichness$rugosity_mean_300_clip_01)



# Models loop

data.300m <- TrainingData.300m.SpeciesRichness

vars.300m <- c("SpeciesRichness",
               #"Training0.Test1",
               "Year",
               "Status",
               #vrm_neigh3_mean_5_clip_01, 
               #slope_mean_5_clip_01, 
               "s(rugosity_mean_300_clip_01, k=3)", 
               #profile_curv_mean_5_clip_01, 
               #plan_curv_mean_5_clip_01,
               "s(northness_mean_300_clip_01, k=3)",
               #fbpi25_50_mean_5_clip_01,
               "s(fbpi10_30_mean_300_clip_01, k=3)",
               "s(eastness_mean_300_clip_01, k=3)",
               "s(curvature_mean_300_clip_01, k=3)",
               #bbpi100_200_mean_5_clip_01,
               "s(bathy_mean_300_clip_01, k=3)",
               #bathy_std_5_clip_01,
               #avg_wave_orb_vel_mean_5_clip_161,
               #"s(SST_mean_300_clip_01, k=3)",
               "s(dist2reef_mean_300_clip_01, k=3)")

N.300m <- list(1,2,3,4)

COMB.300m <- sapply(N.300m, function(m) combn(x=vars.300m[2:length(vars.300m)], m))

COMB2.300m <- list()
k.300m=0
for(i in seq(COMB.300m)){
  tmp.300m <- COMB.300m[[i]]
  for(j in seq(ncol(tmp.300m))){
    k.300m <- k.300m + 1
    COMB2.300m[[k.300m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.300m[,j], collapse=" + ")))
  }
}

ModelsResults.300m <- lapply(COMB2.300m,
                             function(x) gam(x, data=data.300m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.300m <-model.sel(ModelsResults.300m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.300m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.300m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.300m, 1/8 < weight/max(output.300m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.300m, cumsum(output.300m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.300m <-as.data.frame(output.300m)[(length(output.300m)-4):length(output.300m)] 
output.2.300m 

# a little clean-up, lets round things a bit 
output.2.300m[,2:3]<- round(output.2.300m[,2:3],2) 
output.2.300m[,4:5]<- round(output.2.300m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.300m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.300m$Model<-rownames(output.2.300m) 
output.2.300m

# Get the best model
best.model.300m <- as.integer(rownames(output.2.300m)[1])

summary(ModelsResults.300m[[best.model.300m]])


# Predict for test data
TestData.300m.SpeciesRichness$Predicted.Species.Richness.300m <- predict.gam(ModelsResults.300m[[best.model.300m]], TestData.300m.SpeciesRichness)
TestData.300m.SpeciesRichness$Difference.Pred.True.300m <- abs((TestData.300m.SpeciesRichness$SpeciesRichness) - (TestData.300m.SpeciesRichness$Predicted.Species.Richness.300m))
Mean.300m.Species.Richness.Predictive.Difference.300m <- mean(TestData.300m.SpeciesRichness$Difference.Pred.True.300m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html















#  400m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.400m.SpeciesRichness <- Import.400m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_400_clip_01, 
                #slope_mean_400_clip_01, 
                rugosity_mean_400_clip_01, 
                #profile_curv_mean_400_clip_01, 
                #plan_curv_mean_400_clip_01,
                northness_mean_400_clip_01,
                #fbpi25_50_mean_400_clip_01,
                fbpi10_30_mean_400_clip_01,
                eastness_mean_400_clip_01,
                curvature_mean_400_clip_01,
                #bbpi100_200_mean_400_clip_01,
                bathy_mean_400_clip_01,
                #bathy_std_400_clip_01,
                #avg_wave_orb_vel_mean_400_clip_161,
                #SST_mean_400_clip_01,
                dist2reef_mean_400_clip_01
  )

TestData.400m.SpeciesRichness <- Import.400m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_400_clip_01, 
                slope_mean_400_clip_01, 
                rugosity_mean_400_clip_01, 
                profile_curv_mean_400_clip_01, 
                plan_curv_mean_400_clip_01,
                northness_mean_400_clip_01,
                fbpi25_50_mean_400_clip_01,
                fbpi10_30_mean_400_clip_01,
                eastness_mean_400_clip_01,
                curvature_mean_400_clip_01,
                bbpi100_200_mean_400_clip_01,
                bathy_mean_400_clip_01,
                bathy_std_400_clip_01,
                avg_wave_orb_vel_mean_400_clip_01,
                #SST_mean_400_clip_01,
                dist2reef_mean_400_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.400m.SpeciesRichness[, 2:length(TrainingData.400m.SpeciesRichness)], geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.400m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.400m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.400m.SpeciesRichness$sqrt.rugosity_mean_400_clip_01=sqrt(TrainingData.400m.SpeciesRichness$rugosity_mean_400_clip_01)



# Models loop

data.400m <- TrainingData.400m.SpeciesRichness

vars.400m <- c("SpeciesRichness",
               #"Training0.Test1",
               "Year",
               "Status",
               #vrm_neigh3_mean_5_clip_01, 
               #slope_mean_5_clip_01, 
               "s(rugosity_mean_400_clip_01, k=3)", 
               #profile_curv_mean_5_clip_01, 
               #plan_curv_mean_5_clip_01,
               "s(northness_mean_400_clip_01, k=3)",
               #fbpi25_50_mean_5_clip_01,
               "s(fbpi10_30_mean_400_clip_01, k=3)",
               "s(eastness_mean_400_clip_01, k=3)",
               "s(curvature_mean_400_clip_01, k=3)",
               #bbpi100_200_mean_5_clip_01,
               "s(bathy_mean_400_clip_01, k=3)",
               #bathy_std_5_clip_01,
               #avg_wave_orb_vel_mean_5_clip_161,
               #"s(SST_mean_400_clip_01, k=3)",
               "s(dist2reef_mean_400_clip_01, k=3)")

N.400m <- list(1,2,3,4)

COMB.400m <- sapply(N.400m, function(m) combn(x=vars.400m[2:length(vars.400m)], m))

COMB2.400m <- list()
k.400m=0
for(i in seq(COMB.400m)){
  tmp.400m <- COMB.400m[[i]]
  for(j in seq(ncol(tmp.400m))){
    k.400m <- k.400m + 1
    COMB2.400m[[k.400m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.400m[,j], collapse=" + ")))
  }
}

ModelsResults.400m <- lapply(COMB2.400m,
                             function(x) gam(x, data=data.400m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.400m <-model.sel(ModelsResults.400m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.400m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.400m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.400m, 1/8 < weight/max(output.400m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.400m, cumsum(output.400m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.400m <-as.data.frame(output.400m)[(length(output.400m)-4):length(output.400m)] 
output.2.400m 

# a little clean-up, lets round things a bit 
output.2.400m[,2:3]<- round(output.2.400m[,2:3],2) 
output.2.400m[,4:5]<- round(output.2.400m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.400m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.400m$Model<-rownames(output.2.400m) 
output.2.400m

# Get the best model
best.model.400m <- as.integer(rownames(output.2.400m)[1])

summary(ModelsResults.400m[[best.model.400m]])


# Predict for test data
TestData.400m.SpeciesRichness$Predicted.Species.Richness.400m <- predict.gam(ModelsResults.400m[[best.model.400m]], TestData.400m.SpeciesRichness)
TestData.400m.SpeciesRichness$Difference.Pred.True.400m <- abs((TestData.400m.SpeciesRichness$SpeciesRichness) - (TestData.400m.SpeciesRichness$Predicted.Species.Richness.400m))
Mean.400m.Species.Richness.Predictive.Difference.400m <- mean(TestData.400m.SpeciesRichness$Difference.Pred.True.400m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html















#  500m    #################################################################################################################################   

# Select Columns for analysis

TrainingData.500m.SpeciesRichness <- Import.500m.Site.Summaries %>%
  filter(Training0.Test1 == "0") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                #vrm_neigh3_mean_500_clip_01, 
                #slope_mean_500_clip_01, 
                rugosity_mean_500_clip_01, 
                #profile_curv_mean_500_clip_01, 
                #plan_curv_mean_500_clip_01,
                northness_mean_500_clip_01,
                #fbpi25_50_mean_500_clip_01,
                fbpi10_30_mean_500_clip_01,
                eastness_mean_500_clip_01,
                curvature_mean_500_clip_01,
                #bbpi100_200_mean_500_clip_01,
                bathy_mean_500_clip_01,
                #bathy_std_500_clip_01,
                #avg_wave_orb_vel_mean_500_clip_161,
                #SST_mean_500_clip_01,
                dist2reef_mean_500_clip_01
  )

TestData.500m.SpeciesRichness <- Import.500m.Site.Summaries %>%
  filter(Training0.Test1 == "1") %>%
  dplyr::select(SpeciesRichness,
                #Training0.Test1,
                Year,
                Status,
                vrm_neigh3_mean_500_clip_01, 
                slope_mean_500_clip_01, 
                rugosity_mean_500_clip_01, 
                profile_curv_mean_500_clip_01, 
                plan_curv_mean_500_clip_01,
                northness_mean_500_clip_01,
                fbpi25_50_mean_500_clip_01,
                fbpi10_30_mean_500_clip_01,
                eastness_mean_500_clip_01,
                curvature_mean_500_clip_01,
                bbpi100_200_mean_500_clip_01,
                bathy_mean_500_clip_01,
                bathy_std_500_clip_01,
                avg_wave_orb_vel_mean_500_clip_01,
                #SST_mean_500_clip_01,
                dist2reef_mean_500_clip_01
  )


# Make Correlation Matrix

ggcorr(TrainingData.500m.SpeciesRichness[, 2:length(TrainingData.500m.SpeciesRichness)], geom = "blank", label = TRUE, hjust = 0.8, label_size = 2, size = 3) +
  geom_point(size = 7, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.5)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# Check data

pairs(TrainingData.500m.SpeciesRichness)

#Check Response Variable

plot(TrainingData.500m.SpeciesRichness$SpeciesRichness)


### Transformations
# Example: TrainingData.500m.SpeciesRichness$sqrt.rugosity_mean_500_clip_01=sqrt(TrainingData.500m.SpeciesRichness$rugosity_mean_500_clip_01)



# Models loop

data.500m <- TrainingData.500m.SpeciesRichness

vars.500m <- c("SpeciesRichness",
               #"Training0.Test1",
               "Year",
               "Status",
               #vrm_neigh3_mean_5_clip_01, 
               #slope_mean_5_clip_01, 
               "s(rugosity_mean_500_clip_01, k=3)", 
               #profile_curv_mean_5_clip_01, 
               #plan_curv_mean_5_clip_01,
               "s(northness_mean_500_clip_01, k=3)",
               #fbpi25_50_mean_5_clip_01,
               "s(fbpi10_30_mean_500_clip_01, k=3)",
               "s(eastness_mean_500_clip_01, k=3)",
               "s(curvature_mean_500_clip_01, k=3)",
               #bbpi100_200_mean_5_clip_01,
               "s(bathy_mean_500_clip_01, k=3)",
               #bathy_std_5_clip_01,
               #avg_wave_orb_vel_mean_5_clip_161,
               #"s(SST_mean_500_clip_01, k=3)",
               "s(dist2reef_mean_500_clip_01, k=3)")

N.500m <- list(1,2,3,4)

COMB.500m <- sapply(N.500m, function(m) combn(x=vars.500m[2:length(vars.500m)], m))

COMB2.500m <- list()
k.500m=0
for(i in seq(COMB.500m)){
  tmp.500m <- COMB.500m[[i]]
  for(j in seq(ncol(tmp.500m))){
    k.500m <- k.500m + 1
    COMB2.500m[[k.500m]] <- formula(paste("SpeciesRichness", "~", paste(tmp.500m[,j], collapse=" + ")))
  }
}

ModelsResults.500m <- lapply(COMB2.500m,
                             function(x) gam(x, data=data.500m))


# MuMIn: Model Selection

# use the mod.sel function to conduct model selection 
# and put output into object output 

output.500m <-model.sel(ModelsResults.500m) 

# what's it look like, hmm AIC with small sample bias adjustment AICc 
# delta AICc, and the model weights 
output.500m 

# create a confidence set of models using the subset function 
# select models with delta AICc less than 5 
# IMPORTANT: Weights have been renormalized!! 
subset(output.500m, delta <5) 

# select models using Royall's 1/8 rule for strength of evidence 
# IMPORTANT: Weights have been renormalized!! 
subset(output.500m, 1/8 < weight/max(output.500m$weight)) 

# select models 95% cumulative weight criteria 
# IMPORTANT: Weights have been renormalized!! 
subset(output.500m, cumsum(output.500m$weight) <= .95)

# coerce the object output into a data frame 
# elements 6-10 in output have what we want 
output.2.500m <-as.data.frame(output.500m)[(length(output.500m)-4):length(output.500m)] 
output.2.500m 

# a little clean-up, lets round things a bit 
output.2.500m[,2:3]<- round(output.2.500m[,2:3],2) 
output.2.500m[,4:5]<- round(output.2.500m[,4:5],3) 
# that’s better 

# how about a little renaming columns to fit proper conventions 
# number of parameters (df) should be K 
names(output.2.500m)[1] = "K" 

## lets be sure to put the model names in a column 
output.2.500m$Model<-rownames(output.2.500m) 
output.2.500m

# Get the best model
best.model.500m <- as.integer(rownames(output.2.500m)[1])

summary(ModelsResults.500m[[best.model.500m]])


# Predict for test data
TestData.500m.SpeciesRichness$Predicted.Species.Richness.500m <- predict.gam(ModelsResults.500m[[best.model.500m]], TestData.500m.SpeciesRichness)
TestData.500m.SpeciesRichness$Difference.Pred.True.500m <- abs((TestData.500m.SpeciesRichness$SpeciesRichness) - (TestData.500m.SpeciesRichness$Predicted.Species.Richness.500m))
Mean.500m.Species.Richness.Predictive.Difference.500m <- mean(TestData.500m.SpeciesRichness$Difference.Pred.True.500m)


# Predict Raster

### Use the following link to work out how to predict rasters
### http://rspatial.org/sdm/rst/5_sdm_models.html
















# Metrics of Best Models    #################################################################################################################################   

# Summaries:

BestModels.Summaries <- list(
  summary(ModelsResults.5m[[best.model.5m]]),
  summary(ModelsResults.10m[[best.model.10m]]),
  summary(ModelsResults.25m[[best.model.25m]]),
  summary(ModelsResults.50m[[best.model.50m]]),
  summary(ModelsResults.75m[[best.model.75m]]),
  summary(ModelsResults.100m[[best.model.100m]]),
  summary(ModelsResults.150m[[best.model.150m]]),
  summary(ModelsResults.200m[[best.model.200m]]),
  summary(ModelsResults.250m[[best.model.250m]]),
  summary(ModelsResults.300m[[best.model.300m]]),
  summary(ModelsResults.400m[[best.model.400m]]),
  summary(ModelsResults.500m[[best.model.500m]])
  )


# Model Selection Metrics

best.model.selection.5m <- head(output.2.5m,1)
best.model.selection.10m <- head(output.2.10m,1)
best.model.selection.25m <- head(output.2.25m,1)
best.model.selection.50m <- head(output.2.50m,1)
best.model.selection.75m <- head(output.2.75m,1)
best.model.selection.100m <- head(output.2.100m,1)
best.model.selection.150m <- head(output.2.150m,1)
best.model.selection.200m <- head(output.2.200m,1)
best.model.selection.250m <- head(output.2.250m,1)
best.model.selection.300m <- head(output.2.300m,1)
best.model.selection.400m <- head(output.2.400m,1)
best.model.selection.500m <- head(output.2.500m,1)

best.model.selection.5m$Scale <- as.numeric('5')
best.model.selection.10m$Scale <- as.numeric('10')
best.model.selection.25m$Scale <- as.numeric('25')
best.model.selection.50m$Scale <- as.numeric('50')
best.model.selection.75m$Scale <- as.numeric('75')
best.model.selection.100m$Scale <- as.numeric('100')
best.model.selection.150m$Scale <- as.numeric('150')
best.model.selection.200m$Scale <- as.numeric('200')
best.model.selection.250m$Scale <- as.numeric('250')
best.model.selection.300m$Scale <- as.numeric('300')
best.model.selection.400m$Scale <- as.numeric('400')
best.model.selection.500m$Scale <- as.numeric('500')

best.model.selection.5m$Formula <- as.character(c(ModelsResults.5m[[best.model.5m]]$formula))
best.model.selection.10m$Formula <- as.character(c(ModelsResults.10m[[best.model.10m]]$formula))
best.model.selection.25m$Formula <- as.character(c(ModelsResults.25m[[best.model.25m]]$formula))
best.model.selection.50m$Formula <- as.character(c(ModelsResults.50m[[best.model.50m]]$formula))
best.model.selection.75m$Formula <- as.character(c(ModelsResults.75m[[best.model.75m]]$formula))
best.model.selection.100m$Formula <- as.character(c(ModelsResults.100m[[best.model.100m]]$formula))
best.model.selection.150m$Formula <- as.character(c(ModelsResults.150m[[best.model.150m]]$formula))
best.model.selection.200m$Formula <- as.character(c(ModelsResults.200m[[best.model.200m]]$formula))
best.model.selection.250m$Formula <- as.character(c(ModelsResults.250m[[best.model.250m]]$formula))
best.model.selection.300m$Formula <- as.character(c(ModelsResults.300m[[best.model.300m]]$formula))
best.model.selection.400m$Formula <- as.character(c(ModelsResults.400m[[best.model.400m]]$formula))
best.model.selection.500m$Formula <- as.character(c(ModelsResults.500m[[best.model.500m]]$formula))

best.model.selection.5m$AIC.mgcv <- as.character(c(ModelsResults.5m[[best.model.5m]]$aic))
best.model.selection.10m$AIC.mgcv <- as.character(c(ModelsResults.10m[[best.model.10m]]$aic))
best.model.selection.25m$AIC.mgcv <- as.character(c(ModelsResults.25m[[best.model.25m]]$aic))
best.model.selection.50m$AIC.mgcv <- as.character(c(ModelsResults.50m[[best.model.50m]]$aic))
best.model.selection.75m$AIC.mgcv <- as.character(c(ModelsResults.75m[[best.model.75m]]$aic))
best.model.selection.100m$AIC.mgcv <- as.character(c(ModelsResults.100m[[best.model.100m]]$aic))
best.model.selection.150m$AIC.mgcv <- as.character(c(ModelsResults.150m[[best.model.150m]]$aic))
best.model.selection.200m$AIC.mgcv <- as.character(c(ModelsResults.200m[[best.model.200m]]$aic))
best.model.selection.250m$AIC.mgcv <- as.character(c(ModelsResults.250m[[best.model.250m]]$aic))
best.model.selection.300m$AIC.mgcv <- as.character(c(ModelsResults.300m[[best.model.300m]]$aic))
best.model.selection.400m$AIC.mgcv <- as.character(c(ModelsResults.400m[[best.model.400m]]$aic))
best.model.selection.500m$AIC.mgcv <- as.character(c(ModelsResults.500m[[best.model.500m]]$aic))

best.model.selection.5m$Deviance.Explained <- as.numeric(((ModelsResults.5m[[best.model.5m]]$null.deviance - ModelsResults.5m[[best.model.5m]]$deviance)/ModelsResults.5m[[best.model.5m]]$null.deviance)*100)
best.model.selection.10m$Deviance.Explained <- as.numeric(((ModelsResults.10m[[best.model.10m]]$null.deviance - ModelsResults.10m[[best.model.10m]]$deviance)/ModelsResults.10m[[best.model.10m]]$null.deviance)*100)
best.model.selection.25m$Deviance.Explained <- as.numeric(((ModelsResults.25m[[best.model.25m]]$null.deviance - ModelsResults.25m[[best.model.25m]]$deviance)/ModelsResults.25m[[best.model.25m]]$null.deviance)*100)
best.model.selection.50m$Deviance.Explained <- as.numeric(((ModelsResults.50m[[best.model.50m]]$null.deviance - ModelsResults.50m[[best.model.50m]]$deviance)/ModelsResults.50m[[best.model.50m]]$null.deviance)*100)
best.model.selection.75m$Deviance.Explained <- as.numeric(((ModelsResults.75m[[best.model.75m]]$null.deviance - ModelsResults.75m[[best.model.75m]]$deviance)/ModelsResults.75m[[best.model.75m]]$null.deviance)*100)
best.model.selection.100m$Deviance.Explained <- as.numeric(((ModelsResults.100m[[best.model.100m]]$null.deviance - ModelsResults.100m[[best.model.100m]]$deviance)/ModelsResults.100m[[best.model.100m]]$null.deviance)*100)
best.model.selection.150m$Deviance.Explained <- as.numeric(((ModelsResults.150m[[best.model.150m]]$null.deviance - ModelsResults.150m[[best.model.150m]]$deviance)/ModelsResults.150m[[best.model.150m]]$null.deviance)*100)
best.model.selection.200m$Deviance.Explained <- as.numeric(((ModelsResults.200m[[best.model.200m]]$null.deviance - ModelsResults.200m[[best.model.200m]]$deviance)/ModelsResults.200m[[best.model.200m]]$null.deviance)*100)
best.model.selection.250m$Deviance.Explained <- as.numeric(((ModelsResults.250m[[best.model.250m]]$null.deviance - ModelsResults.250m[[best.model.250m]]$deviance)/ModelsResults.250m[[best.model.250m]]$null.deviance)*100)
best.model.selection.300m$Deviance.Explained <- as.numeric(((ModelsResults.300m[[best.model.300m]]$null.deviance - ModelsResults.300m[[best.model.300m]]$deviance)/ModelsResults.300m[[best.model.300m]]$null.deviance)*100)
best.model.selection.400m$Deviance.Explained <- as.numeric(((ModelsResults.400m[[best.model.400m]]$null.deviance - ModelsResults.400m[[best.model.400m]]$deviance)/ModelsResults.400m[[best.model.400m]]$null.deviance)*100)
best.model.selection.500m$Deviance.Explained <- as.numeric(((ModelsResults.500m[[best.model.500m]]$null.deviance - ModelsResults.500m[[best.model.500m]]$deviance)/ModelsResults.500m[[best.model.500m]]$null.deviance)*100)


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

BestModelSelection <- BestModelSelection[,c(7,8,6,1,2,3,4,5,9,10)]


BestModelSelection$Pred.Obs.Correlation <- c(with(TestData.5m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.5m, use = "complete.obs")),
                                             with(TestData.10m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.10m, use = "complete.obs")),
                                             with(TestData.25m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.25m, use = "complete.obs")),
                                             with(TestData.50m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.50m, use = "complete.obs")),
                                             with(TestData.75m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.75m, use = "complete.obs")),
                                             with(TestData.100m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.100m, use = "complete.obs")),
                                             with(TestData.150m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.150m, use = "complete.obs")),
                                             with(TestData.200m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.200m, use = "complete.obs")),
                                             with(TestData.250m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.250m, use = "complete.obs")),
                                             with(TestData.300m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.300m, use = "complete.obs")),
                                             with(TestData.400m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.400m, use = "complete.obs")),
                                             with(TestData.500m.SpeciesRichness, cor(SpeciesRichness, Predicted.Species.Richness.500m, use = "complete.obs"))
)



# Add ∆AIC
bestAIC <- min(BestModelSelection$AICc)
BestModelSelection$delta <- (BestModelSelection$AICc) - bestAIC


write.table(BestModelSelection, file = "01_BestModelSelection_SpeciesRichness.SPONGE.txt" , sep = "\t", row.names = FALSE)


#### Variable Importance ####

BestModel <- BestModelSelection %>%
  filter(delta == 0)
BestModel.Scale <- as.numeric(BestModel[1,1])
BestModel.Data <- paste("data.", BestModel.Scale, "m", sep = "")
BestModel.Formula <- as.formula(BestModel[1,2])
BestModel.Name <- as.name(paste("ModelsResults.", BestModel.Scale, "m[[best.model.", BestModel.Scale,"m]]", sep = ""))
BestModel <- gam(BestModel.Formula, data = eval(parse(text = BestModel.Data)))

BestModel.Variables <- data.frame(as.character(BestModel.Formula))
BestModel.Variables <- as.character(BestModel.Variables[3,1])
BestModel.Variables <- unlist(strsplit(as.character(BestModel.Variables), " + ", fixed = TRUE))
combinations <- length(BestModel.Variables) - 1
COMB.VI <- list(combn(BestModel.Variables, combinations))

COMB2.VI <- list()
k.VI=0
for(i in seq(COMB.VI)){
  tmp.VI <- COMB.VI[[i]]
  for(j in seq(ncol(tmp.VI))){
    k.VI <- k.VI + 1
    COMB2.VI[[k.VI]] <- formula(paste("SpeciesRichness", "~", paste(tmp.VI[,j], collapse=" + ")))
  }
}

ModelsResults.VI <- lapply(COMB2.VI,
                           function(x) gam(x, data=eval(parse(text = BestModel.Data))))

BestModel.Variables <- data.frame(BestModel.Variables)
BestModel.Variables <- data.frame(
  BestModel.Variables %>% map_df(rev)
)
BestModel.Variables$ModelNo <- as.character(1:as.numeric(nrow(BestModel.Variables)))
BestModel.Variables$BestModel.Variables <- as.character(BestModel.Variables$BestModel.Variables)

BestModel.DevExp <- as.numeric(((BestModel$null.deviance - BestModel$deviance)/BestModel$null.deviance)*100)

output.VI <- data.frame(model.sel(ModelsResults.VI)) 

output.VI$ModelNo <- rownames(output.VI)

output.VI <- merge(output.VI, BestModel.Variables, by = "ModelNo", sort = FALSE)


output.VI$Deviance.Explained <- c(as.numeric(((ModelsResults.VI[[1]]$null.deviance - ModelsResults.VI[[1]]$deviance)/ModelsResults.VI[[1]]$null.deviance)*100),
                                  as.numeric(((ModelsResults.VI[[2]]$null.deviance - ModelsResults.VI[[2]]$deviance)/ModelsResults.VI[[2]]$null.deviance)*100),
                                  as.numeric(((ModelsResults.VI[[3]]$null.deviance - ModelsResults.VI[[3]]$deviance)/ModelsResults.VI[[3]]$null.deviance)*100)
                                 )

output.VI$Variable.Deviance.Explained <- BestModel.DevExp - output.VI$Deviance.Explained
total.combined.dev.exp <- sum(output.VI$Variable.Deviance.Explained)
output.VI$VariableImportance <- output.VI$Variable.Deviance.Explained / total.combined.dev.exp
output.VI$Subset <- "SPONGE"

VariableImportance <- output.VI %>%
  dplyr::select("BestModel.Variables", "Subset", "VariableImportance")

write.table(VariableImportance, file = "01_VariableImportance_SPONGE.txt" , sep = "\t", row.names = FALSE)



#### PLOTS ####

# Make GAM Plots
pdf(file = "SPONGE_Best_Plots.pdf")
plot.gam(BestModel)
dev.off()




# Plot Change Across Scales
SpeciesRichness.SPONGE_AICc_01 <- ggplot(data=BestModelSelection, aes(x=BestModelSelection$Scale, y=BestModelSelection$AICc, group=1)) +
  geom_line()+
  geom_point()+
  xlab("Scale(m)") + ylab("AICc") +
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
SpeciesRichness.SPONGE_AICc_01
ggsave("AICc_SpeciesRichness.SPONGE_01.png", plot = last_plot(), device = "png",
       scale = 1, dpi = 300, limitsize = TRUE)


SpeciesRichness.SPONGE_Weight_01 <- ggplot(data=BestModelSelection, aes(x=BestModelSelection$Scale, y=BestModelSelection$weight, group=1)) +
  geom_line()+
  geom_point()+
  xlab("Scale(m)") + ylab("Weight") +
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
SpeciesRichness.SPONGE_Weight_01
ggsave("Weight_SpeciesRichness.SPONGE_01.png", plot = last_plot(), device = "png",
       scale = 1, dpi = 300, limitsize = TRUE)


SpeciesRichness.SPONGE_Pred.Obs.Correlation_01 <- ggplot(data=BestModelSelection, aes(x=BestModelSelection$Scale, y=BestModelSelection$Pred.Obs.Correlation, group=1)) +
  geom_line()+
  geom_point()+
  xlab("Scale(m)") + ylab("Mean Species Richness Pred.Obs.Correlation") +
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
SpeciesRichness.SPONGE_Pred.Obs.Correlation_01
ggsave("Pred.Obs.Correlation_SpeciesRichness.SPONGE_01.png", plot = last_plot(), device = "png",
       scale = 1, dpi = 300, limitsize = TRUE)


SpeciesRichness.SPONGE_logLik_01 <- ggplot(data=BestModelSelection, aes(x=BestModelSelection$Scale, y=BestModelSelection$logLik, group=1)) +
  geom_line()+
  geom_point()+
  xlab("Scale(m)") + ylab("Log Likelihood") +
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
SpeciesRichness.SPONGE_logLik_01
ggsave("logLik_SpeciesRichness.SPONGE_01.png", plot = last_plot(), device = "png",
       scale = 1, dpi = 300, limitsize = TRUE)


SpeciesRichness.SPONGE_AIC.mgcv_01 <- ggplot(data=BestModelSelection, aes(x=BestModelSelection$Scale, y=BestModelSelection$AIC.mgcv, group=1)) +
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
SpeciesRichness.SPONGE_AIC.mgcv_01
ggsave("AICmgcv_SpeciesRichness.SPONGE_01.png", plot = last_plot(), device = "png",
       scale = 1, dpi = 300, limitsize = TRUE)


SpeciesRichness.SPONGE_DevExplained_01 <- ggplot(data=BestModelSelection, aes(x=BestModelSelection$Scale, y=BestModelSelection$Deviance.Explained, group=1)) +
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
SpeciesRichness.SPONGE_DevExplained_01
ggsave("DevExplained_SpeciesRichness.SPONGE_01.png", plot = last_plot(), device = "png",
       scale = 1, dpi = 300, limitsize = TRUE)

