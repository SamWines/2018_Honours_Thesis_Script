# Script for producing line chart figures of deviance explained (%) across all scales tested (with only constant variables included)

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

#### Import Files ####

# Entire Dataset
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/01_EntireDataset/SpeciesRichness_ConsVars/")
getwd()
dir()

BestModelsSelection.ENTIRE <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.ENTIRE$Data <- as.character("Entire Assemblage")

# Pelagics Removed
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/02_PelagicsRemoved/SpeciesRichness_ConsVars/")
getwd()
dir()

BestModelsSelection.PELAGICSREMOVED <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.PELAGICSREMOVED$Data <- as.character("Pelagics Removed")

# PELAGICS CARTILAGINOUS SINGLETONS Removed
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/02_PelagicsSharksSingletonsRemoved/SpeciesRichness_ConsVars/")
getwd()
dir()

BestModelsSelection.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED$Data <- as.character("Mobile Species Removed")

# Reef
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/03_Reef/SpeciesRichness_ConsVars/")
getwd()
dir()

BestModelsSelection.REEF <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.REEF$Data <- as.character("Reef")

# Sediment
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/04_Sediment/SpeciesRichness_ConsVars/")
getwd()
dir()

BestModelsSelection.SEDIMENT <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.SEDIMENT$Data <- as.character("Sediment")

# Sponge
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/05_Sponge/SpeciesRichness_ConsVars/")
getwd()
dir()

BestModelsSelection.SPONGE <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.SPONGE$Data <- as.character("Circalittoral Reef")

# Macroalgae
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/06_Macroalgae/SpeciesRichness_ConsVars/")
getwd()
dir()

BestModelsSelection.MACROALGAE <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.MACROALGAE$Data <- as.character("Infralittoral Reef")

# Carnivores
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/09_Carnivores/SpeciesRichness_ConsVars/")
getwd()
dir()

BestModelsSelection.CARNIVORES <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.CARNIVORES$Data <- as.character("Carnivores")


# Herbivores
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/10_Herbivores/SpeciesRichness_ConsVars/")
getwd()
dir()

BestModelsSelection.HERBIVORES <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.HERBIVORES$Data <- as.character("Herbivores")

# Invertivores
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_Invertivores/SpeciesRichness_ConsVars/")
getwd()
dir()

BestModelsSelection.INVERTIVORES <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.INVERTIVORES$Data <- as.character("Invertivores")

# C.auratus
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/01_C.auratus/Abundance_ConsVars/")
getwd()
dir()

BestModelsSelection.C.auratus <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.C.auratus$Data <- as.character("C.auratus")


# N.tetricus
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/02_N.tetricus/Abundance_ConsVars/")
getwd()
dir()

BestModelsSelection.N.tetricus <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.N.tetricus$Data <- as.character("N.tetricus")


# M.hippocrepis
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/03_M.hippocrepis/Abundance_ConsVars/")
getwd()
dir()

BestModelsSelection.M.hippocrepis <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.M.hippocrepis$Data <- as.character("M.hippocrepis")


# M.flavolineata
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/04_M.flavolineata/Abundance_ConsVars/")
getwd()
dir()

BestModelsSelection.M.flavolineata <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.M.flavolineata$Data <- as.character("M.flavolineata")


#### Join Best Model Selections ####

BestModelSelection <- rbind(BestModelsSelection.ENTIRE, 
                             BestModelsSelection.PELAGICSREMOVED,
                            BestModelsSelection.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED,
                             BestModelsSelection.REEF,
                             BestModelsSelection.SEDIMENT,
                             BestModelsSelection.SPONGE,
                             BestModelsSelection.MACROALGAE,
                             BestModelsSelection.CARNIVORES,
                             BestModelsSelection.HERBIVORES,
                            BestModelsSelection.INVERTIVORES,
                            BestModelsSelection.C.auratus,
                            BestModelsSelection.N.tetricus,
                            BestModelsSelection.M.hippocrepis,
                            BestModelsSelection.M.flavolineata)



#### Plot Change Across Scales ####

# Set working directory
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/15_Figures/02_LineCharts_Cons/")
getwd()
dir()


# Carnivores Vs Herbivores

Carnivores.Herbivores.Invertivores <- BestModelSelection %>%
  filter(Data == "Carnivores" | Data == "Herbivores" | Data == "Invertivores")

Carnivores.Herbivores.Invertivores_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Carnivores.Herbivores.Invertivores, aes(x=Scale, y=Deviance.Explained, colour=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.75, 0.75),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
Carnivores.Herbivores.Invertivores_SpeciesRichness_DevianceExplained_01
ggsave("Carnivores_Herbivores_Invertivores_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm", dpi = 300, limitsize = TRUE)


# Carnivores Vs Herbivores

Carnivores.Herbivores <- BestModelSelection %>%
  filter(Data == "Carnivores" | Data == "Herbivores")

Carnivores.Herbivores_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Carnivores.Herbivores, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.75, 0.75),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
Carnivores.Herbivores_SpeciesRichness_DevianceExplained_01
ggsave("Carnivores_Herbivores_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm", dpi = 300, limitsize = TRUE)


# Reef Vs Sediment

Reef.Sediment <- BestModelSelection %>%
  filter(Data == "Reef" | Data == "Sediment")

Reef.Sediment_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Reef.Sediment, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.75, 0.75),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
Reef.Sediment_SpeciesRichness_DevianceExplained_01
ggsave("Reef_Sediment_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm", dpi = 300, limitsize = TRUE)


# Macroalgae Vs Sponge

Macroalgae.Sponge <- BestModelSelection %>%
  filter(Data == "Infralittoral Reef" | Data == "Circalittoral Reef")

Macroalgae.Sponge_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Macroalgae.Sponge, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.75, 0.75),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
Macroalgae.Sponge_SpeciesRichness_DevianceExplained_01
ggsave("Macroalgae_Sponge_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm", dpi = 300, limitsize = TRUE)


# Macroalgae Vs Sponge Vs Sediment

Macroalgae.Sponge.Sediment <- BestModelSelection %>%
  filter(Data == "Infralittoral Reef" | Data == "Circalittoral Reef" | Data == "Sediment")

Macroalgae.Sponge.Sediment_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Macroalgae.Sponge.Sediment, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.75, 0.75),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
Macroalgae.Sponge.Sediment_SpeciesRichness_DevianceExplained_01
ggsave("Macroalgae.Sponge.Sediment_SpeciesRichness_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm", dpi = 300, limitsize = TRUE)


# Macroalgae Vs Sponge Vs Sediment Vs Enitre Assemblage

Macroalgae.Sponge.Sediment.Entire <- BestModelSelection %>%
  filter(Data == "Infralittoral Reef" | Data == "Circalittoral Reef" | Data == "Sediment" | Data == "Entire Assemblage")

Macroalgae.Sponge.Sediment.Entire$Data <- factor(Macroalgae.Sponge.Sediment.Entire$Data, levels = c("Entire Assemblage", "Infralittoral Reef", "Circalittoral Reef", "Sediment"))

Macroalgae.Sponge.Sediment.Entire_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Macroalgae.Sponge.Sediment.Entire, aes(x=Scale, y=Deviance.Explained, colour=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.75, 0.75),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
Macroalgae.Sponge.Sediment.Entire_SpeciesRichness_DevianceExplained_01
ggsave("Macroalgae.Sponge.Sediment.Entire_SpeciesRichness_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm", dpi = 300, limitsize = TRUE)


# Entire Dataset Vs Pelagics Removed

Entire.PelagicsRemoved <- BestModelSelection %>%
  filter(Data == "Entire Assemblage" | Data == "Pelagics Removed")

Entire.PelagicsRemoved_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Entire.PelagicsRemoved, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.75, 0.75),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
Entire.PelagicsRemoved_SpeciesRichness_DevianceExplained_01
ggsave("Entire_PelagicsRemoved_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


# Entire Dataset Vs PelagicCartilagiousSingletonsRemoved

Entire.PelagicCartilagiousSingletonsRemoved <- BestModelSelection %>%
  filter(Data == "Entire Assemblage" | Data == "Mobile Species Removed")

Entire.PelagicCartilagiousSingletonsRemoved_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Entire.PelagicCartilagiousSingletonsRemoved, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.75, 0.75),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
Entire.PelagicCartilagiousSingletonsRemoved_SpeciesRichness_DevianceExplained_01
ggsave("Entire_PelagicCartilagiousSingletonsRemoved_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


# Key Individual Species

KeyIndividualSpecies <- BestModelSelection %>%
  filter(Data == "C.auratus" | Data == "N.tetricus" | Data == "M.hippocrepis" | Data == "M.flavolineata")

KeyIndividualSpecies_Abundance_DevianceExplained_01 <- ggplot(data=KeyIndividualSpecies, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.75, 0.75),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
KeyIndividualSpecies_Abundance_DevianceExplained_01
ggsave("KeyIndividualSpecies_Abundance_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


# Key Individual Species

KeyIndividualSpeciesMinus <- BestModelSelection %>%
  filter(Data == "C.auratus" | Data == "N.tetricus" | Data == "M.hippocrepis")

KeyIndividualSpeciesMinus_Abundance_DevianceExplained_01 <- ggplot(data=KeyIndividualSpeciesMinus, aes(x=Scale, y=Deviance.Explained, colour=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  scale_y_continuous(limits = c(28,64)) +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.87, 0.88),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
KeyIndividualSpeciesMinus_Abundance_DevianceExplained_01
ggsave("KeyIndividualSpeciesMinus_Abundance_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


C.auratus <- BestModelSelection %>%
  filter(Data == "C.auratus")

C.auratus_Abundance_DevianceExplained_01 <- ggplot(data=C.auratus, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.75, 0.25),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
C.auratus_Abundance_DevianceExplained_01
ggsave("C.auratus_Abundance_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


N.tetricus <- BestModelSelection %>%
  filter(Data == "N.tetricus")

N.tetricus_Abundance_DevianceExplained_01 <- ggplot(data=N.tetricus, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.85, 0.85),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
N.tetricus_Abundance_DevianceExplained_01
ggsave("N.tetricus_Abundance_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


M.hippocrepis <- BestModelSelection %>%
  filter(Data == "M.hippocrepis")

M.hippocrepis_Abundance_DevianceExplained_01 <- ggplot(data=M.hippocrepis, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.85, 0.1),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
M.hippocrepis_Abundance_DevianceExplained_01
ggsave("M.hippocrepis_Abundance_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


M.flavolineata <- BestModelSelection %>%
  filter(Data == "M.flavolineata")

M.flavolineata_Abundance_DevianceExplained_01 <- ggplot(data=M.flavolineata, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale (m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.position= c(0.85, 0.85),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold"),
          axis.title.x = element_text(vjust = -3),
          axis.title.y = element_text(vjust = 4),
          plot.margin = margin(10, 10, 15, 15)
  )
M.flavolineata_Abundance_DevianceExplained_01
ggsave("M.flavolineata_Abundance_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


write.table(BestModelSelection, file = "01_SummaryTable.txt" , sep = "\t", row.names = FALSE)

