# Script to make calculate biomass from lengths using fish-base length weight relationships

# This script is used to:
# 1. 

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 6 September 2018

# Set working directory for input files
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/05_UpdatingSpeciesNames_RemovingNonFish")
getwd()
dir()

# Load and format Global Archive output files
# Load MaxN
dir()
counts <- read.table("Analysis_MaxN_SpUpdated.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
counts2 <- counts[,c("HonoursID","Family","Genus_species","Count")]

# Load Length measurements
dir()
lengths <- read.table("Analysis_3D_Point_Length_Measurements_SpUpdated.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
lengths2 <- lengths[,c("HonoursID","Family","Genus_species","Length..mm.")]


# Reset working directory for output files
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/06_Biomass/")
getwd()


# Import length-weight relationships (all length-weigth relationships sourced from http://www.fishbase.se/)
dir()
lw_relation <- read.table("PointAddis_LengthWeights.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)


#### Calculate biomass of measured individuals ####

# Merge length weight relationships with individual observations
individual_lengths <- merge(lengths,lw_relation,by.x = "Genus_species",by.y = "Species_Name", all.x = TRUE)

# Change a and b from character to numeric so that formulas can be run
sapply(individual_lengths, class)
individual_lengths$a <- as.numeric(as.character(individual_lengths$a))
individual_lengths$b <- as.numeric(as.character(individual_lengths$b))
sapply(individual_lengths, class)

# Change units from mm to cm
individual_lengths$length.cm <- (individual_lengths$Length..mm.)/10

# Calculate mass of each individual
individual_lengths$Individual_Mass.g <- (individual_lengths$a)*(individual_lengths$length.cm)^(individual_lengths$b)


#### Calculate the average biomass for all non measured individuals ####

# Calculate mean lengths of each species
average_lengths <- aggregate(lengths2$Length..mm.,by=list(Genus_species=lengths2$Genus_species),data=lengths,FUN=mean)

# Find number of length measurements for each species in each drop and add column
library(plyr)
freq <- ddply(lengths2, .(HonoursID, Genus_species), nrow)
names(freq) <- c("HonoursID", "Genus_species", "Freq")

# Merge frequency of length measurements and actual MaxN for each species in each drop
merged <- merge(counts2,freq,
                by = c("HonoursID","Genus_species")
                ,all = TRUE)
merged[ , "Freq"][is.na(merged[ , "Freq"] ) ] <- 0

# Calculate numbers of individuals not able to be measured
merged$individuals_not_counted <- (merged$Count) - (merged$Freq)

# Add average lengths and length weight relationships to "merged" dataframe
  # Average lengths have the column name "x"
merged <- merge(merged,average_lengths,by.x = "Genus_species", by.y = "Genus_species", all = TRUE)
merged <- merge(merged,lw_relation,by.x = "Genus_species",by.y = "Species_Name", all.x = TRUE)


# Check for NAs (QA Step)
#   merged$x = average length measurements for each species
#     => NAs are species that have no lengths at all
#         => No average length measurement

merged <- merged[!grepl("na", merged$a),]

library(dplyr)

NAs <- merged %>%
  select( Genus_species, HonoursID, Family, Count, Freq, individuals_not_counted, x, a, b) %>%
  filter(is.na(x) == TRUE)

## Temporarily make NAs = 0
merged$x[is.na(merged$x)] <- 0


# Calculate estimated biomass of unmeasured individuals (average individual mass of species * number of individuals not counted)

# Equation for working out mass from length a*(length(cm)^b)
# Change a and b from character to numeric so that formulas can be run
sapply(merged, class)
merged$a <- as.numeric(as.character(merged$a))
merged$b <- as.numeric(as.character(merged$b))
sapply(merged, class)

merged$length.cm <- (merged$x)/10

merged$Individual_Mass <- (merged$a)*(merged$length.cm)^(merged$b)

merged$total_average_biomass <- (merged$Count)*(merged$Individual_Mass)


## Merge original individual measurements with total biomasses for each drop

individual_lengths2 <- individual_lengths[,c("HonoursID","Family","Genus_species","Count","Length..mm.","length.cm","a","b","Individual_Mass.g")]

# Make common column format for merged and individual_measured_weights
#       Sample, Family, Genus_species, Count, Length..mm., Length..cm., a, b, Weight..g.
total <- merged[,c("HonoursID", "Family", "Genus_species", "individuals_not_counted", "x","length.cm", "a", "b", "Individual_Mass")]
colnames(total) <-  c("HonoursID","Family","Genus_species","Count","Length..mm.","length.cm","a","b","Individual_Mass.g")

# Combine with individual biomass for each site
total2 <- rbind(total, individual_lengths2)

total2$total_average_biomass <- (total2$Count)*(total2$Individual_Mass.g)

# Make NA = 0
total2$Count[is.na(total2$Count)] <- 0
total2$length.cm[is.na(total2$length.cm)] <- 0
total2$Individual_Mass.g[is.na(total2$Individual_Mass.g)] <- 0
total2$total_average_biomass[is.na(total2$total_average_biomass)] <- 0

total_biomass_by_site <- aggregate(total2$total_average_biomass,by=list(ID = total2$HonoursID),FUN=sum)


#### Export individual and total biomass ####

# Total Biomass
colnames(total_biomass_by_site) <- c("HonoursID", "Total.Biomass..g.")

write.table(total_biomass_by_site, file = "AnalysisTotalBiomass.txt" , sep = "\t", row.names = FALSE)


# Individual Biomass
colnames(individual_lengths)[39] <- "Length..cm."
colnames(individual_lengths)[40] <- "Biomass..g."

write.table(individual_lengths, file = "Analysis_3D_Point_Length_Measurements_SpUpdated_BiomassIncluded.txt" , sep = "\t", row.names = FALSE)

