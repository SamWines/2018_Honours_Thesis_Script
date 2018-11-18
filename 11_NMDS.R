# Script for completing Non-Metric Dimensional Scaling to understand species driving each model

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 9 September 2018

# Libraries required
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(reshape)
library(vegan)
library(MASS)
library(grid)


# Set working directory
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/09_FunctionalInformation/")
getwd()
dir()


# Import Files

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



getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/11_Modelling_Subsets/01_SiteSummaries")
getwd()
dir()

Training.Test.Split <- read.table("01_Training_Test_Split.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

Species.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONS <- read.table("01_Species.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONS.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)



# Reset Working Directory for Outputs
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/17_NMDS/")
getwd()
dir()



# Adjust Columns in Environmental Variables
EnvironmentalVariables <- EnvironmentalVariables %>%
  dplyr::select(-c(Date, Time, Location, Status, Site, Depth, Observer, Successful, CampaignID, ID, Latitude.y, Longitude.y, Easting, Northing)) %>%
  distinct(HonoursID, .keep_all = TRUE)


# Attach Metadata to each site
SiteMetadata <- Latest.MaxN %>%
  dplyr::select(HonoursID, Sample.No, Easting, Northing, Comment, Cams, Filename, Date, Time.x, Location, Status, Site, Observer, CampaignID) %>%
  distinct(HonoursID, .keep_all = TRUE)

# Assign test/training data
    #SiteMetadata$Training0.Test1 <- rbinom((NROW(SiteMetadata)), 1, 0.25)
    #Training0.Test1 <- SiteMetadata %>%
      #dplyr::select(HonoursID, Training0.Test1)
    #write.table(Training0.Test1, file = "01_Training_Test_Split.txt" , sep = "\t", row.names = FALSE)

# Change Site Status to 1 (NoTake) and 0 (Fished)
SiteMetadata$Status <- gsub("NoTake", "1", SiteMetadata$Status)
SiteMetadata$Status <- gsub("Fished", "0", SiteMetadata$Status)
SiteMetadata$Status <- as.numeric(SiteMetadata$Status)



#### ENTIRE ####

Site.Summaries.ENTIRE <- Latest.MaxN %>%
  dplyr::select(c(HonoursID,Genus_species,Count,Trophic.Group))

Species.Site.Abundance.Pivot.ENTIRE<- cast(Site.Summaries.ENTIRE, Genus_species ~ HonoursID, sum, value = "Count")

Species.Site.Abundance.Pivot.ENTIRE_2 <- Species.Site.Abundance.Pivot.ENTIRE[,-1]
rownames(Species.Site.Abundance.Pivot.ENTIRE_2) <- Species.Site.Abundance.Pivot.ENTIRE[,1]

write.csv(Species.Site.Abundance.Pivot.ENTIRE_2, file = "temp.csv")

abund_table<-read.csv("temp.csv",row.names=1,check.names=FALSE)

abund_table<-t(abund_table)

example_NMDS<-metaMDS(abund_table,distance = "bray", trymax = 50)

fit <- envfit(example_NMDS, abund_table, perm = 999)

plot(example_NMDS)

# create a grouping variable that has a length of 24, the same # of rows of
# varespec using the rep function
Site.Summaries.ENTIRE <- Site.Summaries.ENTIRE[!duplicated(Site.Summaries.ENTIRE$Genus_species),]
Site.Summaries.ENTIRE <- Site.Summaries.ENTIRE %>%
  dplyr::select(c(Genus_species, Trophic.Group))

Species.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONS <- Species.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONS[!duplicated(Species.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONS$Genus_species),]

merger <- data.frame(colnames(abund_table))
merger <- merge(merger, Site.Summaries.ENTIRE,  by.x = "colnames.abund_table.", by.y = "Genus_species", sort = FALSE)
merger <- merge(merger, Species.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONS, by.x = "colnames.abund_table.", by.y = "Genus_species", sort = FALSE, all.x = TRUE)

grp.Trophic <- merger$Trophic.Group
grp.PelagicEtc <- merger$Label

# Then take a look at the results
grp.Trophic
grp.PelagicEtc


data.scores <- as.data.frame(scores(example_NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
head(data.scores)  #look at the data

scores(fit, "vectors")
species.scores <- as.data.frame((fit[["vectors"]]$arrows)*1)
species.scores$r <- fit[["vectors"]]$r
species.scores$species <- rownames(species.scores)
species.scores <- merge(species.scores, merger, by.x = "species", by.y = "colnames.abund_table.", sort = FALSE)
species.scores <- species.scores %>%
  filter(`r` > 0.1)


vec.sp.df<-as.data.frame((fit$vectors$arrows*sqrt(fit$vectors$r))*3)
vec.sp.df$r <- fit[["vectors"]]$r
vec.sp.df$species <- rownames(vec.sp.df)
vec.sp.df <- merge(vec.sp.df, merger, by.x = "species", by.y = "colnames.abund_table.", sort = FALSE)
vec.sp.df <- vec.sp.df %>%
  filter(`r` > 0.1)



ggplot() + 
  geom_label_repel(data=vec.sp.df,aes(x=NMDS1,y=NMDS2,label=species,colour=Trophic.Group),alpha=1) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2),size=2) + # add the point markers
  geom_segment(data=vec.sp.df,aes(x=0,xend=NMDS1,y=0,yend=NMDS2), arrow = grid::arrow(length = unit(0.5, "cm")),colour="grey",inherit.aes=FALSE) +
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  #scale_colour_manual(values=c("MACROALGAE" = "red", "SPONGE" = "blue")) +
  scale_x_continuous(limits = c(-3, 3)) +
  coord_equal() +
  theme_bw()

ggsave("TrophicGroups_NMDS_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 300, height = 150, units = "mm" ,limitsize = TRUE)


ggplot() + 
  geom_label_repel(data=vec.sp.df,aes(x=NMDS1,y=NMDS2,label=species,colour=Label),alpha=1) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2),size=2) + # add the point markers
  geom_segment(data=vec.sp.df,aes(x=0,xend=NMDS1,y=0,yend=NMDS2), arrow = grid::arrow(length = unit(0.5, "cm")),colour="grey",inherit.aes=FALSE) +
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  #scale_colour_manual(values=c("MACROALGAE" = "red", "SPONGE" = "blue")) +
  scale_x_continuous(limits = c(-3, 3)) +
  coord_equal() +
  theme_bw()

ggsave("PelagicsCartilaginousSingletons_NMDS_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 300, height = 150, units = "mm" ,limitsize = TRUE)



#### REEF.SEDIMENT ####

Site.Summaries.REEF <- Latest.MaxN %>%
  filter(Reef == "Reef") %>% 
  dplyr::select(c(HonoursID,Genus_species,Count))

Site.Summaries.REEF$REEF.SEDIMENT <- paste("REEF")


Site.Summaries.SEDIMENT <- Latest.MaxN %>%
  filter(Sediment == "Sediment") %>% 
  dplyr::select(c(HonoursID,Genus_species,Count))

Site.Summaries.SEDIMENT$REEF.SEDIMENT <- paste("SEDIMENT")

REEF.SEDIMENT <- rbind(Site.Summaries.REEF, Site.Summaries.SEDIMENT)


Species.Site.Abundance.ENTIRE <- REEF.SEDIMENT %>%
  dplyr::select(c(HonoursID,Genus_species,Count))

Species.Site.Abundance.Pivot.ENTIRE<- cast(Species.Site.Abundance.ENTIRE, Genus_species ~ HonoursID, sum, value = "Count")

Species.Site.Abundance.Pivot.ENTIRE_2 <- Species.Site.Abundance.Pivot.ENTIRE[,-1]
rownames(Species.Site.Abundance.Pivot.ENTIRE_2) <- Species.Site.Abundance.Pivot.ENTIRE[,1]

write.csv(Species.Site.Abundance.Pivot.ENTIRE_2, file = "temp.csv")

abund_table<-read.csv("temp.csv",row.names=1,check.names=FALSE)

abund_table<-t(abund_table)

example_NMDS<-metaMDS(abund_table,distance = "bray", trymax = 50)

fit <- envfit(example_NMDS, abund_table, perm = 999)

plot(example_NMDS)

# create a grouping variable that has a length of 24, the same # of rows of
# varespec using the rep function
REEF.SEDIMENT <- REEF.SEDIMENT[!duplicated(REEF.SEDIMENT$HonoursID),]
REEF.SEDIMENT <- REEF.SEDIMENT %>%
  dplyr::select(c(HonoursID, REEF.SEDIMENT))

merger <- data.frame(row.names(abund_table))
merger <- merge(merger, REEF.SEDIMENT,  by.x = "row.names.abund_table.", by.y = "HonoursID", sort = FALSE)

grp <- merger$REEF.SEDIMENT

# Then take a look at the results
grp

data.scores <- as.data.frame(scores(example_NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- grp  #  add the grp variable created earlier
head(data.scores)  #look at the data

scores(fit, "vectors")
species.scores <- as.data.frame((fit[["vectors"]]$arrows)*1)
species.scores$r <- fit[["vectors"]]$r
species.scores$species <- rownames(species.scores)
species.scores <- species.scores %>%
  filter(`r` > 0.01)


vec.sp.df<-as.data.frame((fit$vectors$arrows*sqrt(fit$vectors$r))*3)
vec.sp.df$r <- fit[["vectors"]]$r
vec.sp.df$species <- rownames(vec.sp.df)
vec.sp.df <- vec.sp.df %>%
  filter(`r` > 0.01)





ggplot() + 
  geom_label_repel(data=vec.sp.df,aes(x=NMDS1,y=NMDS2,label=species),alpha=1) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=2) + # add the point markers
  geom_segment(data=vec.sp.df,aes(x=0,xend=NMDS1,y=0,yend=NMDS2), arrow = grid::arrow(length = unit(0.5, "cm")),colour="grey",inherit.aes=FALSE) +
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("REEF" = "red", "SEDIMENT" = "blue")) +
  scale_x_continuous(limits = c(-3, 3)) +
  coord_equal() +
  theme_bw()

ggsave("REEF_SEDIMENT_NMDS_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 300, height = 150, units = "mm" ,limitsize = TRUE)






#### MACROALGAE.SPONGE ####

Site.Summaries.MACROALGAE <- Latest.MaxN %>%
  filter(Macroalgae == "Macroalgae") %>% 
  dplyr::select(c(HonoursID,Genus_species,Count))

Site.Summaries.MACROALGAE$MACROALGAE.SPONGE <- paste("MACROALGAE")


Site.Summaries.SPONGE <- Latest.MaxN %>%
  filter(Sponge == "Sponge") %>% 
  dplyr::select(c(HonoursID,Genus_species,Count))

Site.Summaries.SPONGE$MACROALGAE.SPONGE <- paste("SPONGE")

MACROALGAE.SPONGE <- rbind(Site.Summaries.MACROALGAE, Site.Summaries.SPONGE)


Species.Site.Abundance.ENTIRE <- MACROALGAE.SPONGE %>%
  dplyr::select(c(HonoursID,Genus_species,Count))

Species.Site.Abundance.Pivot.ENTIRE<- cast(Species.Site.Abundance.ENTIRE, Genus_species ~ HonoursID, sum, value = "Count")

Species.Site.Abundance.Pivot.ENTIRE_2 <- Species.Site.Abundance.Pivot.ENTIRE[,-1]
rownames(Species.Site.Abundance.Pivot.ENTIRE_2) <- Species.Site.Abundance.Pivot.ENTIRE[,1]

write.csv(Species.Site.Abundance.Pivot.ENTIRE_2, file = "temp.csv")

abund_table<-read.csv("temp.csv",row.names=1,check.names=FALSE)

abund_table<-t(abund_table)

example_NMDS<-metaMDS(abund_table,distance = "bray", trymax = 50)

fit <- envfit(example_NMDS, abund_table, perm = 999)

plot(example_NMDS)

# create a grouping variable that has a length of 24, the same # of rows of
# varespec using the rep function
MACROALGAE.SPONGE <- MACROALGAE.SPONGE[!duplicated(MACROALGAE.SPONGE$HonoursID),]
MACROALGAE.SPONGE <- MACROALGAE.SPONGE %>%
  dplyr::select(c(HonoursID, MACROALGAE.SPONGE))

merger <- data.frame(row.names(abund_table))
merger <- merge(merger, MACROALGAE.SPONGE,  by.x = "row.names.abund_table.", by.y = "HonoursID", sort = FALSE)

grp <- merger$MACROALGAE.SPONGE

# Then take a look at the results
grp

data.scores <- as.data.frame(scores(example_NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp <- grp  #  add the grp variable created earlier
head(data.scores)  #look at the data

scores(fit, "vectors")
species.scores <- as.data.frame((fit[["vectors"]]$arrows)*1)
species.scores$r <- fit[["vectors"]]$r
species.scores$species <- rownames(species.scores)
species.scores <- species.scores %>%
  filter(`r` > 0.01)


vec.sp.df<-as.data.frame((fit$vectors$arrows*sqrt(fit$vectors$r))*3)
vec.sp.df$r <- fit[["vectors"]]$r
vec.sp.df$species <- rownames(vec.sp.df)
vec.sp.df <- vec.sp.df %>%
  filter(`r` > 0.01)





ggplot() + 
  geom_label_repel(data=vec.sp.df,aes(x=NMDS1,y=NMDS2,label=species),alpha=1) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=2) + # add the point markers
  geom_segment(data=vec.sp.df,aes(x=0,xend=NMDS1,y=0,yend=NMDS2), arrow = grid::arrow(length = unit(0.5, "cm")),colour="grey",inherit.aes=FALSE) +
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("MACROALGAE" = "red", "SPONGE" = "blue")) +
  scale_x_continuous(limits = c(-3, 3)) +
  coord_equal() +
  theme_bw()

ggsave("MACROALGAE_SPONGE_NMDS_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 300, height = 150, units = "mm" ,limitsize = TRUE)




#### ALL HABITATS COMBINED ####

Site.Summaries.SEDIMENT <- Latest.MaxN %>%
  filter(Sediment == "Sediment") %>% 
  dplyr::select(c(HonoursID,Genus_species,Count))

Site.Summaries.SEDIMENT$MACROALGAE.SPONGE.SEDIMENT <- paste("Sediment")

Site.Summaries.MACROALGAE <- Latest.MaxN %>%
  filter(Macroalgae == "Macroalgae") %>% 
  dplyr::select(c(HonoursID,Genus_species,Count))

Site.Summaries.MACROALGAE$MACROALGAE.SPONGE.SEDIMENT <- paste("Infralittoral Reef")


Site.Summaries.SPONGE <- Latest.MaxN %>%
  filter(Sponge == "Sponge") %>% 
  dplyr::select(c(HonoursID,Genus_species,Count))

Site.Summaries.SPONGE$MACROALGAE.SPONGE.SEDIMENT <- paste("Circalittoral Reef")

MACROALGAE.SPONGE.SEDIMENT <- rbind(Site.Summaries.SEDIMENT, Site.Summaries.MACROALGAE, Site.Summaries.SPONGE)


Species.Site.Abundance.ENTIRE <- MACROALGAE.SPONGE.SEDIMENT %>%
  dplyr::select(c(HonoursID,Genus_species,Count))

Species.Site.Abundance.Pivot.ENTIRE<- cast(Species.Site.Abundance.ENTIRE, Genus_species ~ HonoursID, sum, value = "Count")

Species.Site.Abundance.Pivot.ENTIRE_2 <- Species.Site.Abundance.Pivot.ENTIRE[,-1]
rownames(Species.Site.Abundance.Pivot.ENTIRE_2) <- Species.Site.Abundance.Pivot.ENTIRE[,1]

write.csv(Species.Site.Abundance.Pivot.ENTIRE_2, file = "temp.csv")

abund_table<-read.csv("temp.csv",row.names=1,check.names=FALSE)

abund_table<-t(abund_table)

example_NMDS<-metaMDS(abund_table,distance = "bray", trymax = 50)

fit <- envfit(example_NMDS, abund_table, perm = 999)

plot(example_NMDS)

# create a grouping variable that has a length of 24, the same # of rows of
# varespec using the rep function
MACROALGAE.SPONGE.SEDIMENT <- MACROALGAE.SPONGE.SEDIMENT[!duplicated(MACROALGAE.SPONGE.SEDIMENT$HonoursID),]
MACROALGAE.SPONGE.SEDIMENT <- MACROALGAE.SPONGE.SEDIMENT %>%
  dplyr::select(c(HonoursID, Genus_species, MACROALGAE.SPONGE.SEDIMENT))

merger <- data.frame(colnames(abund_table))
merger <- merge(merger, MACROALGAE.SPONGE.SEDIMENT,  by.x = "colnames.abund_table.", by.y = "Genus_species", sort = FALSE)
merger <- merge(merger, Species.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONS, by.x = "colnames.abund_table.", by.y = "Genus_species", sort = FALSE, all.x = TRUE)

grp.Habitat <- merger$MACROALGAE.SPONGE.SEDIMENT
grp.Trophic <- merger$Trophic.Group
grp.PelagicEtc <- merger$Label

# Then take a look at the results
grp.Habitat
grp.Trophic
grp.PelagicEtc

data.scores <- as.data.frame(scores(example_NMDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$grp.Habitat <- grp.Habitat  #  add the grp variable created earlier
head(data.scores)  #look at the data

scores(fit, "vectors")
species.scores <- as.data.frame((fit[["vectors"]]$arrows)*1)
species.scores$r <- fit[["vectors"]]$r
species.scores$species <- rownames(species.scores)
species.scores <- merge(species.scores, merger, by.x = "species", by.y = "colnames.abund_table.", sort = FALSE)
species.scores <- species.scores %>%
  filter(`r` > 0.1)


vec.sp.df<-as.data.frame((fit$vectors$arrows*sqrt(fit$vectors$r))*3)
vec.sp.df$r <- fit[["vectors"]]$r
vec.sp.df$species <- rownames(vec.sp.df)
vec.sp.df <- merge(vec.sp.df, merger, by.x = "species", by.y = "colnames.abund_table.", sort = FALSE)
vec.sp.df <- vec.sp.df %>%
  filter(`r` > 0.1)





ggplot() + 
  geom_label_repel(data=vec.sp.df,aes(x=NMDS1,y=NMDS2,label=species,colour= grp.PelagicEtc),alpha=1) +  # add the species labels
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp.Habitat,colour=grp.Habitat),size=2) + # add the point markers
  geom_segment(data=vec.sp.df,aes(x=0,xend=NMDS1,y=0,yend=NMDS2), arrow = grid::arrow(length = unit(0.5, "cm")),colour="grey",inherit.aes=FALSE) +
  #geom_text(data=data.scores,aes(x=NMDS1,y=NMDS2,label=site),size=6,vjust=0) +  # add the site labels
  scale_colour_manual(values=c("Infralittoral Reef" = "green", "Circalittoral Reef" = "blue", "Sediment" = "red")) +
  scale_x_continuous(limits = c(-3, 3)) +
  coord_equal() +
  theme_bw()

ggsave("MACROALGAE.SPONGE.SEDIMENT_NMDS_01.png", plot = last_plot(), device = "png",
       scale = 1, width = 300, height = 150, units = "mm" ,limitsize = TRUE)





