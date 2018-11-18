# Script for producing bar plots showing species and families with highest abundances in this study

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 14 September 2018

# From http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/

# Set working directory
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/09_FunctionalInformation/")
getwd()
dir()

# Libraries Required
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

# Input Data
Latest.MaxN <- read.table("Analysis_MaxN_SpUpdated_HabitatInc_EnviroInc_FunctionalInc.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

Latest.Lengths <- read.table("Analysis_3D_Point_Length_Measurements_SpUpdated_BiomassIncluded_HabitatInc_EnviroInc_FunctionalInc.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)


# Reset Working Directory For Output Folder
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/15_Figures/05_SiteSummaryMetrics/")
getwd()
dir()

# Set Up Data

TotalAbundance <- Latest.MaxN %>%
  dplyr::select(c(Genus_species, Count))%>%
  group_by(Genus_species)%>%
  summarise_each(funs(sum(Count)))
head(TotalAbundance,2)

TotalCount <- sum(TotalAbundance$Count, na.rm = TRUE)
TotalAbundance$SpeciesPercentage <- ((TotalAbundance$Count) / TotalCount)

TotalAbundance$Genus_species <- factor(TotalAbundance$Genus_species, levels = TotalAbundance$Genus_species)

TotalAbundance <- TotalAbundance[order(-TotalAbundance$SpeciesPercentage),] 

TotalAbundanceRemainder <- TotalAbundance %>%
  filter(SpeciesPercentage < 0.02)

TotalAbundanceRemainder <- sum(TotalAbundanceRemainder$SpeciesPercentage)

TotalAbundanceRemainder2 <- data.frame("Remainder of Species", NA, TotalAbundanceRemainder)
colnames(TotalAbundanceRemainder2) <- c("Genus_species", "Count", "SpeciesPercentage")

TotalAbundance <- rbind(TotalAbundance, TotalAbundanceRemainder2)

# Stacked Bar Chart

TotalAbundance$Genus_species <- factor(TotalAbundance$Genus_species, levels = TotalAbundance$Genus_species)

species_plot <- ggplot(data = subset(TotalAbundance, SpeciesPercentage > 0.02)) +
  geom_bar(stat = "identity", width = 100, aes(1, SpeciesPercentage, fill=Genus_species), position="fill", color="black") +
  ylab("Percentage") +
  scale_fill_brewer("Species", palette="Paired") +
  scale_x_discrete(breaks = NULL) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size = 15),
        axis.title = element_text(face = "bold")
  )
plot(species_plot)

ggsave("BarPlot_TotalAbundance_Species_ENTIRE_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 100, height = 100, units = "mm" ,limitsize = TRUE)



#### FAMILIES PLOT ####
# Set Up Data

TotalAbundance <- Latest.MaxN %>%
  dplyr::select(c(Family, Count))%>%
  group_by(Family)%>%
  summarise_each(funs(sum(Count)))
head(TotalAbundance,2)

TotalCount <- sum(TotalAbundance$Count, na.rm = TRUE)
TotalAbundance$FamilyPercentage <- ((TotalAbundance$Count) / TotalCount)

TotalAbundance$Family <- factor(TotalAbundance$Family, levels = TotalAbundance$Family)

TotalAbundance <- TotalAbundance[order(-TotalAbundance$FamilyPercentage),] 

TotalAbundanceRemainder <- TotalAbundance %>%
  filter(FamilyPercentage < 0.02)

TotalAbundanceRemainder <- sum(TotalAbundanceRemainder$FamilyPercentage)

TotalAbundanceRemainder2 <- data.frame("Remainder of Families", NA, TotalAbundanceRemainder)
colnames(TotalAbundanceRemainder2) <- c("Family", "Count", "FamilyPercentage")

TotalAbundance <- rbind(TotalAbundance, TotalAbundanceRemainder2)

# Stacked Bar Chart

TotalAbundance$Family <- factor(TotalAbundance$Family, levels = TotalAbundance$Family)

species_plot <- ggplot(data = subset(TotalAbundance, FamilyPercentage > 0.02)) +
  geom_bar(stat = "identity", width = 100, aes(1, FamilyPercentage, fill=Family), position="fill", color="black") +
  ylab("Percentage") +
  scale_fill_brewer("Family", palette="Paired") +
  scale_x_discrete(breaks = NULL) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size = 15),
        axis.title = element_text(face = "bold")
  )
plot(species_plot)

ggsave("BarPlot_TotalAbundance_Family_ENTIRE_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 100, height = 100, units = "mm" ,limitsize = TRUE)


#### PELAGICS REMOVED PLOT ####

# Set Up Data

Latest.MaxN.PELAGICSREMOVED <- Latest.MaxN %>%
  filter(Water.column != "pelagic non-site attached")

TotalAbundance <- Latest.MaxN.PELAGICSREMOVED %>%
  dplyr::select(c(Genus_species, Count))%>%
  group_by(Genus_species)%>%
  summarise_each(funs(sum(Count)))
head(TotalAbundance,2)

TotalCount <- sum(TotalAbundance$Count, na.rm = TRUE)
TotalAbundance$SpeciesPercentage <- ((TotalAbundance$Count) / TotalCount)

TotalAbundance$Genus_species <- factor(TotalAbundance$Genus_species, levels = TotalAbundance$Genus_species)

TotalAbundance <- TotalAbundance[order(-TotalAbundance$SpeciesPercentage),] 

TotalAbundanceRemainder <- TotalAbundance %>%
  filter(SpeciesPercentage < 0.02)

TotalAbundanceRemainder <- sum(TotalAbundanceRemainder$SpeciesPercentage)

TotalAbundanceRemainder2 <- data.frame("Remainder of Species", NA, TotalAbundanceRemainder)
colnames(TotalAbundanceRemainder2) <- c("Genus_species", "Count", "SpeciesPercentage")

TotalAbundance <- rbind(TotalAbundance, TotalAbundanceRemainder2)

# Stacked Bar Chart

TotalAbundance$Genus_species <- factor(TotalAbundance$Genus_species, levels = TotalAbundance$Genus_species)

species_plot <- ggplot(data = subset(TotalAbundance, SpeciesPercentage > 0.02)) +
  geom_bar(stat = "identity", width = 100,aes(1, SpeciesPercentage, fill=Genus_species), position="fill", color="black") +
  ylab("Percentage") +
  scale_fill_brewer("Species", palette="Paired") +
  scale_x_discrete(breaks = NULL) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        text = element_text(size = 15),
        axis.title = element_text(face = "bold")
  )
plot(species_plot)

ggsave("BarPlot_TotalAbundance_Species_PELAGICSREMOVED_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 100, height = 100, units = "mm" ,limitsize = TRUE)


