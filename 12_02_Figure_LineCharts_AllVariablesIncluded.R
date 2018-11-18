# Script for producing line chart figures of deviance explained (%) across all scales tested (with all variables included)

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
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/01_EntireDataset/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.ENTIRE <- read.table("01_BestModelSelection_SpeciesRichness.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.ENTIRE$Data <- as.character("ENTIRE")

# Pelagics Removed
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/02_PelagicsRemoved/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.PELAGICSREMOVED <- read.table("01_BestModelSelection_SpeciesRichness.PELAGICSREMOVED.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.PELAGICSREMOVED$Data <- as.character("PELAGICSREMOVED")

# PELAGICS CARTILAGINOUS SINGLETONS Removed
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/02_PelagicsSharksSingletonsRemoved/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED <- read.table("01_BestModelSelection_SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED$Data <- as.character("PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED")

# Reef
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/03_Reef/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.REEF <- read.table("01_BestModelSelection_SpeciesRichness.REEF.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.REEF$Data <- as.character("REEF")

# Sediment
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/04_Sediment/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.SEDIMENT <- read.table("01_BestModelSelection_SpeciesRichness.SEDIMENT.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.SEDIMENT$Data <- as.character("SEDIMENT")

# Sponge
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/05_Sponge/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.SPONGE <- read.table("01_BestModelSelection_SpeciesRichness.SPONGE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.SPONGE$Data <- as.character("SPONGE")

# Macroalgae
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/06_Macroalgae/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.MACROALGAE <- read.table("01_BestModelSelection_SpeciesRichness.MACROALGAE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.MACROALGAE$Data <- as.character("MACROALGAE")

# Carnivores
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/09_Carnivores/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.CARNIVORES <- read.table("01_BestModelSelection_SpeciesRichness.CARNIVORES.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.CARNIVORES$Data <- as.character("CARNIVORES")


# Herbivores
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/10_Herbivores/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.HERBIVORES <- read.table("01_BestModelSelection_SpeciesRichness.HERBIVORES.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.HERBIVORES$Data <- as.character("HERBIVORES")

# Invertivores
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_Invertivores/SpeciesRichness_SiteExc/")
getwd()
dir()

BestModelsSelection.INVERTIVORES <- read.table("01_BestModelSelection_SpeciesRichness.INVERTIVORES.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.INVERTIVORES$Data <- as.character("INVERTIVORES")

# C.auratus
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/01_C.auratus/Abundance_SiteExc/")
getwd()
dir()

BestModelsSelection.C.auratus <- read.table("01_BestModelSelection_C.auratus.Abundance.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.C.auratus$Data <- as.character("C.auratus")


# N.tetricus
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/02_N.tetricus/Abundance_SiteExc/")
getwd()
dir()

BestModelsSelection.N.tetricus <- read.table("01_BestModelSelection_N.tetricus.Abundance.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.N.tetricus$Data <- as.character("N.tetricus")


# M.hippocrepis
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/03_M.hippocrepis/Abundance_SiteExc/")
getwd()
dir()

BestModelsSelection.M.hippocrepis <- read.table("01_BestModelSelection_M.hippocrepis.Abundance.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
BestModelsSelection.M.hippocrepis$Data <- as.character("M.hippocrepis")


# M.flavolineata
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/12_Modelling/11_KeyIndividualSpecies/04_M.flavolineata/Abundance_SiteExc/")
getwd()
dir()

BestModelsSelection.M.flavolineata <- read.table("01_BestModelSelection_M.flavolineata.Abundance.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 
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
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/15_Figures/02_LineCharts/")
getwd()
dir()


# Carnivores Vs Herbivores

Carnivores.Herbivores.Invertivores <- BestModelSelection %>%
  filter(Data == "CARNIVORES" | Data == "HERBIVORES" | Data == "INVERTIVORES")

Carnivores.Herbivores.Invertivores_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Carnivores.Herbivores.Invertivores, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale(m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          #legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
Carnivores.Herbivores.Invertivores_SpeciesRichness_DevianceExplained_01
ggsave("Carnivores_Herbivores_Invertivores_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm", dpi = 300, limitsize = TRUE)


# Carnivores Vs Herbivores

Carnivores.Herbivores <- BestModelSelection %>%
  filter(Data == "CARNIVORES" | Data == "HERBIVORES")

Carnivores.Herbivores_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Carnivores.Herbivores, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale(m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          #legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
Carnivores.Herbivores_SpeciesRichness_DevianceExplained_01
ggsave("Carnivores_Herbivores_Invertivores_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm", dpi = 300, limitsize = TRUE)


# Reef Vs Sediment

Reef.Sediment <- BestModelSelection %>%
  filter(Data == "REEF" | Data == "SEDIMENT")

Reef.Sediment_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Reef.Sediment, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale(m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          #legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
Reef.Sediment_SpeciesRichness_DevianceExplained_01
ggsave("Reef_Sediment_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm", dpi = 300, limitsize = TRUE)


# Macroalgae Vs Sponge

Macroalgae.Sponge <- BestModelSelection %>%
  filter(Data == "MACROALGAE" | Data == "SPONGE")

Macroalgae.Sponge_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Macroalgae.Sponge, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale(m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          #legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
Macroalgae.Sponge_SpeciesRichness_DevianceExplained_01
ggsave("Macroalgae_Sponge_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm", dpi = 300, limitsize = TRUE)


# Entire Dataset Vs Pelagics Removed

Entire.PelagicsRemoved <- BestModelSelection %>%
  filter(Data == "ENTIRE" | Data == "PELAGICSREMOVED")

Entire.PelagicsRemoved_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Entire.PelagicsRemoved, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale(m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          #legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
Entire.PelagicsRemoved_SpeciesRichness_DevianceExplained_01
ggsave("Entire_PelagicsRemoved_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


# Entire Dataset Vs PelagicCartilagiousSingletonsRemoved

Entire.PelagicCartilagiousSingletonsRemoved <- BestModelSelection %>%
  filter(Data == "ENTIRE" | Data == "PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED")

Entire.PelagicCartilagiousSingletonsRemoved_SpeciesRichness_DevianceExplained_01 <- ggplot(data=Entire.PelagicCartilagiousSingletonsRemoved, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale(m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          #legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
Entire.PelagicCartilagiousSingletonsRemoved_SpeciesRichness_DevianceExplained_01
ggsave("Entire_PelagicCartilagiousSingletonsRemoved_SpeciesRichness_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 300, height = 100, units = "mm" ,limitsize = TRUE)


# Key Individual Species

KeyIndividualSpecies <- BestModelSelection %>%
  filter(Data == "C.auratus" | Data == "N.tetricus" | Data == "M.hippocrepis" | Data == "M.flavolineata")

KeyIndividualSpecies_Abundance_DevianceExplained_01 <- ggplot(data=KeyIndividualSpecies, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale(m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          #legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
KeyIndividualSpecies_Abundance_DevianceExplained_01
ggsave("KeyIndividualSpecies_Abundance_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


C.auratus <- BestModelSelection %>%
  filter(Data == "C.auratus")

C.auratus_Abundance_DevianceExplained_01 <- ggplot(data=C.auratus, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale(m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          #legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
C.auratus_Abundance_DevianceExplained_01
ggsave("C.auratus_Abundance_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


N.tetricus <- BestModelSelection %>%
  filter(Data == "N.tetricus")

N.tetricus_Abundance_DevianceExplained_01 <- ggplot(data=N.tetricus, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale(m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          #legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
N.tetricus_Abundance_DevianceExplained_01
ggsave("N.tetricus_Abundance_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


M.hippocrepis <- BestModelSelection %>%
  filter(Data == "M.hippocrepis")

M.hippocrepis_Abundance_DevianceExplained_01 <- ggplot(data=M.hippocrepis, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale(m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          #legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
M.hippocrepis_Abundance_DevianceExplained_01
ggsave("M.hippocrepis_Abundance_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)


M.flavolineata <- BestModelSelection %>%
  filter(Data == "M.flavolineata")

M.flavolineata_Abundance_DevianceExplained_01 <- ggplot(data=M.flavolineata, aes(x=Scale, y=Deviance.Explained, group=Data)) +
  geom_line(mapping = aes(linetype = Data))+
  geom_point()+
  xlab("Scale(m)") + 
  ylab("Deviance Explained (%)") +
  theme_classic() +
  theme(  legend.background = element_rect(fill = "transparent"), # get rid of legend bg
          legend.box.background = element_rect(fill = "transparent"),
          #legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(size = 15),
          axis.title = element_text(face = "bold")
  )
M.flavolineata_Abundance_DevianceExplained_01
ggsave("M.flavolineata_Abundance_DevianceExplained_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 150, height = 100, units = "mm" ,limitsize = TRUE)

write.table(BestModelSelection, file = "01_SummaryTable.txt" , sep = "\t", row.names = FALSE)