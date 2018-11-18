# Script for calculating summary statistics for each BRUV deployment in the study

# This script is used to:
  # 1. Change MPA Status to "0" for fished and "1" for no-take, and assign test and training data
  # 2. Calculate species richness for each site
  # 3. Calculate family richness for each site
  # 4. Calculate total abundance for each site
  # 5. Calculate Shannon-Wiener Index (H') for each site
  # 6. Calculate mean trophic level for each site
  # 7. Calculate species richness for all sites with various pelagic or mobile species removed
  # 8. Calculate species richness of higher carnivores for each site
  # 9. Calculate species richness of browsing herbivores for each site
  # 10. Calculate species richness of benthic invertivores for each site
  # 11. Calculate species richness of planktivores for each site
  # 12. Calculate abundance of M.hippocrepis, C.auratus and N.tetricus for each site


# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 9 September 2018

# Libraries required
detach("package:plyr", unload=TRUE)#will error - no worries
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(ggplot2)
library(stringr)
library(vegan)
library(reshape)


# Load input files from multiple directories

rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/09_FunctionalInformation/")
getwd()
dir()

Latest.MaxN <- read.table("Analysis_MaxN_SpUpdated_HabitatInc_EnviroInc_FunctionalInc.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
Latest.Lengths <- read.table("Analysis_3D_Point_Length_Measurements_SpUpdated_BiomassIncluded_HabitatInc_EnviroInc_FunctionalInc.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)


getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/08_AddEnvironmentalVariables/")
getwd()
dir()

EnvironmentalVariables <- read.table("Updated.EnvionmentalVariables.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)


getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/06_Biomass/")
getwd()
dir()

TotalBiomass <- read.table("AnalysisTotalBiomass.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 


# Reset working directory for outputs
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/11_Modelling_Subsets/01_SiteSummaries")
getwd()
dir()

Training.Test.Split <- read.table("01_Training_Test_Split.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)



#### Adjust columns in environmental variables ####
EnvironmentalVariables <- EnvironmentalVariables %>%
  dplyr::select(-c(Date, Time, Location, Status, Site, Depth, Observer, Successful, CampaignID, ID, Latitude.y, Longitude.y, Easting, Northing)) %>%
  distinct(HonoursID, .keep_all = TRUE)



#### Attach metadata to each site ####
SiteMetadata <- Latest.MaxN %>%
  dplyr::select(HonoursID, Sample.No, Easting, Northing, Comment, Cams, Filename, Date, Time.x, Location, Status, Site, Observer, CampaignID) %>%
  distinct(HonoursID, .keep_all = TRUE)



#### Assign test/training data ####
    #SiteMetadata$Training0.Test1 <- rbinom((NROW(SiteMetadata)), 1, 0.25)
    #Training0.Test1 <- SiteMetadata %>%
      #dplyr::select(HonoursID, Training0.Test1)
    #write.table(Training0.Test1, file = "01_Training_Test_Split.txt" , sep = "\t", row.names = FALSE)



#### Change Site Status to 1 (NoTake) and 0 (Fished) ####
SiteMetadata$Status <- gsub("NoTake", "1", SiteMetadata$Status)
SiteMetadata$Status <- gsub("Fished", "0", SiteMetadata$Status)
SiteMetadata$Status <- as.numeric(SiteMetadata$Status)



#### Create individual species summary ####
df <- Latest.MaxN
df = df[!duplicated(df$Genus_species),]
df <- df %>%
  select(c(Genus_species, Family, Trophic.Group, Water.column))
write.table(df, file = "01_SpeciesSummary.txt" , sep = "\t", row.names = FALSE)



#### Calculate species richness of the entire assemblage ####
SpeciesRichness <- Latest.MaxN %>%
  dplyr::select(c(HonoursID, Genus_species))%>%
  group_by(HonoursID)%>%
  summarise(SpeciesRichness = n_distinct(Genus_species))
head(SpeciesRichness,2)



#### Calculate familty richness of the entire assemblage ####
FamilyRichness <- Latest.MaxN %>%
  dplyr::select(c(HonoursID, Family))%>%
  group_by(HonoursID)%>%
  summarise(FamilyRichness = n_distinct(Family))
head(FamilyRichness,2)



#### Calculate the total abundance at each site ####

TotalAbundance <- Latest.MaxN %>%
  dplyr::select(c(HonoursID, Count))%>%
  group_by(HonoursID)%>%
  summarise_each(funs(sum(Count)))
head(TotalAbundance,2)



#### Calculate the Shannon-Wiener Index (H') of each site ####

Shannon.Wiener.Diversity.Index <- Latest.MaxN %>%
  dplyr::select(c(HonoursID,Genus_species,Count))

Shannon.Wiener.Diversity.Index.Pivot<- cast(Shannon.Wiener.Diversity.Index, HonoursID ~ Genus_species, sum, value = "Count")

Shannon.Wiener.Diversity.Index.Pivot$Shannon.Wiener.Diversity.Index <- diversity(Shannon.Wiener.Diversity.Index.Pivot, index="shannon")

Shannon.Wiener.Diversity.Index.Output <- Shannon.Wiener.Diversity.Index.Pivot %>%
  dplyr::select(c(HonoursID, Shannon.Wiener.Diversity.Index))



#### Calculate the mean trophic level of each site ####

Mean.Trophic.Level <- Latest.MaxN %>%
  dplyr::select(c(HonoursID, Trophic.Level))%>%
  group_by(HonoursID)%>%
  summarise_each(funs(mean(Trophic.Level, na.rm = TRUE)))
head(Mean.Trophic.Level,2)



#### Calculate species richness for all sites with various pelagic or mobile species removed ####

# Species richness when "pelagic non-site attached" species removed

Latest.MaxN.PELAGICSREMOVED <- Latest.MaxN %>%
  filter(Water.column != "pelagic non-site attached")
SpeciesRichness.PELAGICSREMOVED <- Latest.MaxN.PELAGICSREMOVED %>%
  dplyr::select(c(HonoursID, Genus_species))%>%
  group_by(HonoursID)%>%
  summarise(SpeciesRichness.PELAGICSREMOVED = n_distinct(Genus_species))
head(SpeciesRichness.PELAGICSREMOVED,2)


# Species richness when "pelagic non-site attached" species and cartilaginous species removed

Latest.MaxN.PELAGICSREMOVED.CARTILAGINOUSREMOVED <- Latest.MaxN.PELAGICSREMOVED %>%
  filter(Genus_species != "Mustelus antarcticus" |
         Genus_species != "Cephaloscyllium laticeps" |
         Genus_species != "Notorynchus cepedianus" |
         Genus_species != "Dasyatis brevicaudata" |
         Genus_species != "Heterodontus portusjacksoni" |
         Genus_species != "Trygonorrhina fasciata" |
         Genus_species != "Pristiophorus nudipinnis" |
         Genus_species != "Urolophus paucimaculatus" |
         Genus_species != "Parascyllium variolatum" |
         Genus_species != "Galeorhinus galeus" |
         Genus_species != "Carcharhinus brachyurus" |
         Genus_species != "Callorhinchus milii" |
         Genus_species != "Pristiophorus cirratus" |
         Genus_species != "Parascyllium ferrugineum" |
         Genus_species != "Orectolobus maculatus" |
         Genus_species != "Parascyllium sp" |
         Genus_species != "Dipturus whitleyi" |
         Genus_species != "Urolophus sp"
         )

SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED <- Latest.MaxN.PELAGICSREMOVED.CARTILAGINOUSREMOVED %>%
  dplyr::select(c(HonoursID, Genus_species))%>%
  group_by(HonoursID)%>%
  summarise(SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED = n_distinct(Genus_species))
head(SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED,2)


# Species richness of cartilaginous species at all sites

Latest.MaxN.SHARKSONLY <- Latest.MaxN.PELAGICSREMOVED %>%
  filter(Genus_species == "Mustelus antarcticus" |
         Genus_species == "Cephaloscyllium laticeps" |
         Genus_species == "Notorynchus cepedianus" |
         Genus_species == "Dasyatis brevicaudata" |
         Genus_species == "Heterodontus portusjacksoni" |
         Genus_species == "Trygonorrhina fasciata" |
         Genus_species == "Pristiophorus nudipinnis" |
         Genus_species == "Urolophus paucimaculatus" |
         Genus_species == "Parascyllium variolatum" |
         Genus_species == "Galeorhinus galeus" |
         Genus_species == "Carcharhinus brachyurus" |
         Genus_species == "Callorhinchus milii" |
         Genus_species == "Pristiophorus cirratus" |
         Genus_species == "Parascyllium ferrugineum" |
         Genus_species == "Orectolobus maculatus" |
         Genus_species == "Parascyllium sp" |
         Genus_species == "Dipturus whitleyi" |
         Genus_species == "Urolophus sp"
  )

SpeciesRichness.SHARKSONLY <- Latest.MaxN.SHARKSONLY %>%
  dplyr::select(c(HonoursID, Genus_species))%>%
  group_by(HonoursID)%>%
  summarise(SpeciesRichness.SHARKSONLY = n_distinct(Genus_species))
head(SpeciesRichness.SHARKSONLY,2)


# Species richness when "pelagic non-site attached" species, cartilaginous species and singleton (only seen once) speciesremoved

Species.Occurence <- Latest.MaxN %>%
  dplyr::select(c(HonoursID,Genus_species,Count))

Species.Occurence.Pivot<- cast(Species.Occurence, Genus_species ~ HonoursID, value = "Count")

Species.Occurence.Pivot$TotalTimesOccured <- rowSums(Species.Occurence.Pivot)

Singleton.Species <- Species.Occurence.Pivot %>%
  filter(TotalTimesOccured == "0" |
           TotalTimesOccured == "1") %>%
  dplyr::select(c(Genus_species))
Singleton.Species <- as.vector(Singleton.Species$Genus_species)
class(Singleton.Species)

Latest.MaxN.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED <- Latest.MaxN.PELAGICSREMOVED %>%
  filter(!(Genus_species %in% Singleton.Species))

SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED <- Latest.MaxN.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED %>%
  dplyr::select(c(HonoursID, Genus_species))%>%
  group_by(HonoursID)%>%
  summarise(SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED = n_distinct(Genus_species))
head(SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED,2)



#### Make a list of all pelagic, cartilaginous and singleton species ####

Species.PELAGICS <- Latest.MaxN %>%
  filter(Water.column == "pelagic non-site attached")
Species.PELAGICS <- as.data.frame(unique(Species.PELAGICS$Genus_species))
Species.PELAGICS$Label <- paste("Pelagic Species")
colnames(Species.PELAGICS)[1] <- "Genus_species"

Species.CARTILAGINOUS <- as.data.frame(c("Mustelus antarcticus",
                                           "Cephaloscyllium laticeps",
                                           "Notorynchus cepedianus",
                                           "Dasyatis brevicaudata",
                                           "Heterodontus portusjacksoni",
                                           "Trygonorrhina fasciata",
                                           "Pristiophorus nudipinnis",
                                           "Urolophus paucimaculatus",
                                           "Parascyllium variolatum",
                                           "Galeorhinus galeus",
                                           "Carcharhinus brachyurus",
                                           "Callorhinchus milii",
                                           "Pristiophorus cirratus",
                                           "Parascyllium ferrugineum",
                                           "Orectolobus maculatus",
                                           "Parascyllium sp",
                                           "Dipturus whitleyi",
                                           "Urolophus sp"))
Species.CARTILAGINOUS$Label <- paste("Cartilaginous Species")
colnames(Species.CARTILAGINOUS)[1] <- "Genus_species"

Species.SINGLETONS <- as.data.frame(Singleton.Species)
Species.SINGLETONS$Label <- paste("Singleton Species")
colnames(Species.SINGLETONS)[1] <- "Genus_species"

Species.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONS <- rbind(Species.PELAGICS, Species.CARTILAGINOUS, Species.SINGLETONS)

write.table(Species.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONS, file = "01_Species.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONS.txt" , sep = "\t", row.names = FALSE)



#### Calculate species richness of higher carnivores for each site ####

Latest.MaxN.CARNIVORES <- Latest.MaxN %>%
  filter(Trophic.Group == "higher carnivore")

SpeciesRichness.CARNIVORES <- Latest.MaxN.CARNIVORES %>%
  dplyr::select(c(HonoursID, Genus_species))%>%
  group_by(HonoursID)%>%
  summarise(SpeciesRichness.CARNIVORES = n_distinct(Genus_species))
head(SpeciesRichness.CARNIVORES,2)



#### Calculate species richness of browsing herbivores for each site ####

Latest.MaxN.HERBIVORES <- Latest.MaxN %>%
  filter(Trophic.Group == "Browsing herbivore")

SpeciesRichness.HERBIVORES <- Latest.MaxN.HERBIVORES %>%
  dplyr::select(c(HonoursID, Genus_species))%>%
  group_by(HonoursID)%>%
  summarise(SpeciesRichness.HERBIVORES = n_distinct(Genus_species))
head(SpeciesRichness.HERBIVORES,2)



#### Calculate species richness of benthic invertivores for each site ####

Latest.MaxN.INVERTIVORES <- Latest.MaxN %>%
  filter(Trophic.Group == "benthic invertivore")

SpeciesRichness.INVERTIVORES <- Latest.MaxN.INVERTIVORES %>%
  dplyr::select(c(HonoursID, Genus_species))%>%
  group_by(HonoursID)%>%
  summarise(SpeciesRichness.INVERTIVORES = n_distinct(Genus_species))
head(SpeciesRichness.INVERTIVORES,2)



#### Calculate species richness of planktivores for each site ####

Latest.MaxN.PLANKTIVORES <- Latest.MaxN %>%
  filter(Trophic.Group == "planktivore")

SpeciesRichness.PLANKTIVORES <- Latest.MaxN.PLANKTIVORES %>%
  dplyr::select(c(HonoursID, Genus_species))%>%
  group_by(HonoursID)%>%
  summarise(SpeciesRichness.PLANKTIVORES = n_distinct(Genus_species))
head(SpeciesRichness.PLANKTIVORES,2)



#### Calculate abundance of M.hippocrepis, C.auratus and N.tetricus for each site ####

Abundance.Species <- Latest.MaxN %>%
  dplyr::select(c(HonoursID,Genus_species,Count))

Abundance.Species.Pivot <- cast(Abundance.Species, HonoursID ~ Genus_species, sum, value = "Count")

C.auratus.Abundance <- Abundance.Species.Pivot %>%
  dplyr::select(c(HonoursID,`Chrysophrys auratus`))
colnames(C.auratus.Abundance) <- c("HonoursID", "C.auratus.Abundance")

N.tetricus.Abundance <- Abundance.Species.Pivot %>%
  dplyr::select(c(HonoursID,`Notolabrus tetricus`))
colnames(N.tetricus.Abundance) <- c("HonoursID", "N.tetricus.Abundance")

M.hippocrepis.Abundance <- Abundance.Species.Pivot %>%
  dplyr::select(c(HonoursID,`Meuschenia hippocrepis`))
colnames(M.hippocrepis.Abundance) <- c("HonoursID", "M.hippocrepis.Abundance")

M.flavolineata.Abundance <- Abundance.Species.Pivot %>%
  dplyr::select(c(HonoursID,`Meuschenia flavolineata`))
colnames(M.flavolineata.Abundance) <- c("HonoursID", "M.flavolineata.Abundance")



#### Merge all summaries together to make output site summaries table ####

temp.Master1 <- (merge(SpeciesRichness, FamilyRichness, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master2 <- (merge(temp.Master1, TotalAbundance, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master3 <- (merge(temp.Master2, TotalBiomass, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master4 <- (merge(temp.Master3, Shannon.Wiener.Diversity.Index.Output, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master5 <- (merge(temp.Master4, Mean.Trophic.Level, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master6 <- (merge(temp.Master5, SpeciesRichness.PELAGICSREMOVED, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master7 <- (merge(temp.Master6, SpeciesRichness.CARNIVORES, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master8 <- (merge(temp.Master7, SpeciesRichness.HERBIVORES, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master9 <- (merge(temp.Master8, SpeciesRichness.INVERTIVORES, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master10 <- (merge(temp.Master9, SpeciesRichness.PLANKTIVORES, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master11 <- (merge(temp.Master10, SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master12 <- (merge(temp.Master11, SpeciesRichness.SHARKSONLY, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master13 <- (merge(temp.Master12, SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master14 <- (merge(temp.Master13, C.auratus.Abundance, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master15 <- (merge(temp.Master14, N.tetricus.Abundance, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master16 <- (merge(temp.Master15, M.hippocrepis.Abundance, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master17 <- (merge(temp.Master16, M.flavolineata.Abundance, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master18 <- (merge(temp.Master17, SiteMetadata, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master19 <- (merge(temp.Master18, Training.Test.Split, by = "HonoursID", all.x = TRUE, sort = TRUE))
temp.Master20 <- (merge(temp.Master19, EnvironmentalVariables, by = "HonoursID", all.x = TRUE, sort = TRUE))

# Export site summaries table
write.table(temp.Master20, file = "01_SiteSummaries.txt" , sep = "\t", row.names = FALSE)


