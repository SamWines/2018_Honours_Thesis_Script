# Script for calculating the importance of each predictor variable in the various models made in this study

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 14 September 2018

# Libraries required
library(dplyr)
library(ggplot2)
library(reshape)
library(RColorBrewer)

#### Import Files ####

# Entire Dataset
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/01_EntireDataset/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.ENTIRE <- read.table("01_VariableImportance_ENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.ENTIRE$Data <- as.character("Entire Dataset")

# Pelagics Removed
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/02_PelagicsRemoved/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.PELAGICSREMOVED <- read.table("01_VariableImportance_PELAGICSREMOVED.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.PELAGICSREMOVED$Data <- as.character("Pelagics Removed")

# PELAGICS CARTILAGINOUS SINGLETONS Removed
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/02_PelagicsSharksSingletonsRemoved/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED <- read.table("01_VariableImportance_SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED$Data <- as.character("PELAG.CART.SINGL.REM")

# Reef
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/03_Reef/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.REEF <- read.table("01_VariableImportance_ENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.REEF$Data <- as.character("All Reef")

# Sediment
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/04_Sediment/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.SEDIMENT <- read.table("01_VariableImportance_SEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.SEDIMENT$Data <- as.character("Sediment")

# Sponge
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/05_Sponge/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.SPONGE <- read.table("01_VariableImportance_SPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.SPONGE$Data <- as.character("Circalittoral Reef (Invertebrate Dominated)")

# Macroalgae
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/06_Macroalgae/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.MACROALGAE <- read.table("01_VariableImportance_MACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.MACROALGAE$Data <- as.character("Infralittoral Reef (Macroalgae Dominated)")

# Carnivores
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/09_Carnivores/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.CARNIVORES <- read.table("01_VariableImportance_CARNIVORES.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.CARNIVORES$Data <- as.character("Carnivores")


# Herbivores
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/10_Herbivores/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.HERBIVORES <- read.table("01_VariableImportance_HERBIVORES.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.HERBIVORES$Data <- as.character("Herbivores")

# Invertivores
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_Invertivores/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.INVERTIVORES <- read.table("01_VariableImportance_INVERTIVORES.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.INVERTIVORES$Data <- as.character("Invertivores")

# C.auratus
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/01_C.auratus/Abundance_SiteExc/")
getwd()
dir()

BestModelsSelection.C.auratus <- read.table("01_VariableImportance_C.auratus.Abundance.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.C.auratus$Data <- as.character("C.auratus")


# N.tetricus
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/02_N.tetricus/Abundance_SiteExc/")
getwd()
dir()

BestModelsSelection.N.tetricus <- read.table("01_VariableImportance_N.tetricus.Abundance.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.N.tetricus$Data <- as.character("N.tetricus")


# M.hippocrepis
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/03_M.hippocrepis/Abundance_SiteExc/")
getwd()
dir()

BestModelsSelection.M.hippocrepis <- read.table("01_VariableImportance_M.hippocrepis.Abundance.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.M.hippocrepis$Data <- as.character("M.hippocrepis")


# M.flavolineata
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/04_M.flavolineata/Abundance_SiteExc/")
getwd()
dir()

BestModelsSelection.M.flavolineata <- read.table("01_VariableImportance_M.flavolineata.Abundance.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.M.flavolineata$Data <- as.character("M.flavolineata")


#### Join Best Model Selections ####

BestModelSelection <- rbind(BestModelsSelection.ENTIRE, 
                             BestModelsSelection.REEF,
                             BestModelsSelection.SEDIMENT,
                             BestModelsSelection.SPONGE,
                             BestModelsSelection.MACROALGAE,
                             BestModelsSelection.CARNIVORES,
                             BestModelsSelection.HERBIVORES,
                            BestModelsSelection.INVERTIVORES,
                            BestModelsSelection.C.auratus,
                            BestModelsSelection.N.tetricus,
                            BestModelsSelection.M.hippocrepis
                            )


# Make Variables Consistent
BestModelSelection$Variable <- ifelse(grepl("SST", BestModelSelection$BestModel.Variables), "SST",
                                      ifelse(grepl("rugosity", BestModelSelection$BestModel.Variables), "Rugosity",
                                             ifelse(grepl("dist2reef", BestModelSelection$BestModel.Variables), "Distance to Reef",
                                                    ifelse(grepl("bathy_mean", BestModelSelection$BestModel.Variables), "Bathymetry",
                                                           ifelse(grepl("Status", BestModelSelection$BestModel.Variables), "MPA Status",
                                                                  ifelse(grepl("Year", BestModelSelection$BestModel.Variables), "Year",
                                                                         ifelse(grepl("fbpi10_30", BestModelSelection$BestModel.Variables), "FineBPI",
                                                                                ifelse(grepl("curvature", BestModelSelection$BestModel.Variables), "Curvature",
                                                                                       ifelse(grepl("northness", BestModelSelection$BestModel.Variables), "Northness",
                                                                                              ifelse(grepl("eastness", BestModelSelection$BestModel.Variables), "Eastness",
                                             NA))))))))))

BestModelSelection <- BestModelSelection %>%
  dplyr::select(c(Data, Variable, VariableImportance))


#### Plot Change Across Scales ####

# Set working directory
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/15_Figures/03_VariableImportance/")
getwd()
dir()

mypalette <- colorRampPalette((brewer.pal(7, "YlOrRd")), space = "Lab")

BestModelSelection$Data <- factor(BestModelSelection$Data, levels = rev(c("Entire Dataset", 
                                                                      "All Reef",
                                                                      "Sediment",
                                                                      "Infralittoral Reef (Macroalgae Dominated)",
                                                                      "Circalittoral Reef (Invertebrate Dominated)",
                                                                      "Carnivores",
                                                                      "Herbivores",
                                                                      "Invertivores",
                                                                      "C.auratus",
                                                                      "N.tetricus",
                                                                      "M.hippocrepis"
                                                                      )))

BestModelSelection$Variable <- factor(BestModelSelection$Variable, levels = c("Year", 
                                                                      "Bathymetry", 
                                                                      "Distance to Reef", 
                                                                      "Rugosity",
                                                                      "MPA Status",
                                                                      "Curvature",
                                                                      "SST",
                                                                      "FineBPI",
                                                                      "Eastness",
                                                                      "Northness"
))


zp1 <- ggplot(BestModelSelection, aes(x = Variable, y = Data, fill = VariableImportance)) +
  geom_tile() +
  scale_fill_gradientn(colours = mypalette(100), name = "Variable Importance") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 10),
          axis.title.x = element_text(face = "bold"),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        legend.title = element_blank(),
        legend.text = element_text(size = 10, vjust = 6, face = "bold"),
        legend.direction = "horizontal",
        legend.position = c(0.5, 1.05),
        legend.justification = "centre",
        legend.background = element_blank(),
        plot.margin = unit(c(10,2,2,2),"mm")
        )
  
plot(zp1)

ggsave("01_VariableImportance.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)

