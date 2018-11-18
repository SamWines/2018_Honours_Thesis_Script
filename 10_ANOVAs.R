# Script for producing ANOVAs to understand change between years of sampling and protection status

# Author: Sam Wines (slwines@deakin.edu.au)

# Updated 14 September 2018


# Libraries required
library(dplyr)
library(ggpubr)
library(multcomp)
library(car)

# Set import directory
rm(list=ls())
getwd()
setwd("/Users/samwines/Desktop/Uni-Honours/02_Data/16_ANOVA/")
getwd()
dir()


# Import Files
Site.Summaries.ENTIRE <- read.table("01_5m_SiteSummariesENTIRE.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) 

# Prepare Data

ENTIRE.Stats <- Site.Summaries.ENTIRE %>%
  dplyr::select(c(Year, Status, SpeciesRichness, FamilyRichness, Total.Biomass..g., Shannon.Wiener.Diversity.Index, SpeciesRichness.PELAGICSREMOVED.CARTILAGINOUSREMOVED.SINGLETONSREMOVED, SpeciesRichness.CARNIVORES, SpeciesRichness.HERBIVORES, C.auratus.Abundance, N.tetricus.Abundance, M.hippocrepis.Abundance, M.flavolineata.Abundance))
ENTIRE.Stats$Year <- as.factor(ENTIRE.Stats$Year)
ENTIRE.Stats$Status <- as.factor(ENTIRE.Stats$Status)

# Show the levels
levels(ENTIRE.Stats$Year)
levels(ENTIRE.Stats$Status)


#### ANOVA Process (ENTIRE, Species Richness for Year) ####


group_by(ENTIRE.Stats, Year) %>%
           summarise(
             count = n(),
             mean = mean(SpeciesRichness, na.rm = TRUE),
             sd = sd(SpeciesRichness, na.rm = TRUE)
           )


# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
ggboxplot(ENTIRE.Stats, x = "Year", y = "SpeciesRichness", 
          color = "Year", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("2013", "2017", "2018"),
          ylab = "Species Richness", xlab = "Year")


# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(ENTIRE.Stats, x = "Year", y = "SpeciesRichness", 
       add = c("mean_se", "jitter"), 
       order = c("2013", "2017", "2018"),
       ylab = "Species Richness", xlab = "Year")


# Compute the analysis of variance
aov.SpeciesRichness.ENTIRE <- aov(SpeciesRichness ~ Year, data = ENTIRE.Stats)
# Summary of the analysis
summary(aov.SpeciesRichness.ENTIRE)

TukeyHSD(aov.SpeciesRichness.ENTIRE)

summary(glht(aov.SpeciesRichness.ENTIRE, linfct = mcp(Year = "Tukey")))


# Check Assumptions

# 1. Homogeneity of variances
plot(aov.SpeciesRichness.ENTIRE, 1)

# Levenes Test
leveneTest(SpeciesRichness ~ Year, data = ENTIRE.Stats)

# 2. Normality
plot(aov.SpeciesRichness.ENTIRE, 2)

# Extract the residuals
aov_residuals.SpeciesRichness.ENTIRE <- residuals(object = aov.SpeciesRichness.ENTIRE )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals.SpeciesRichness.ENTIRE )


#### ANOVA Process (ENTIRE, Species Richness Herbivores for Year) ####

group_by(ENTIRE.Stats, Year) %>%
           summarise(
             count = n(),
             mean = mean(SpeciesRichness.HERBIVORES, na.rm = TRUE),
             sd = sd(SpeciesRichness.HERBIVORES, na.rm = TRUE)
           )


# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
ggboxplot(ENTIRE.Stats, x = "Year", y = "SpeciesRichness.HERBIVORES", 
          color = "Year", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("2013", "2017", "2018"),
          ylab = "Herbivore Species Richness", xlab = "Year")


# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(ENTIRE.Stats, x = "Year", y = "SpeciesRichness.HERBIVORES", 
       add = c("mean_se", "jitter"), 
       order = c("2013", "2017", "2018"),
       ylab = "Herbivore Species Richness", xlab = "Year")


# Compute the analysis of variance
aov.SpeciesRichness.HERBIVORES.ENTIRE <- aov(SpeciesRichness.HERBIVORES ~ Year, data = ENTIRE.Stats)
# Summary of the analysis
summary(aov.SpeciesRichness.HERBIVORES.ENTIRE)

TukeyHSD(aov.SpeciesRichness.HERBIVORES.ENTIRE)

summary(glht(aov.SpeciesRichness.HERBIVORES.ENTIRE, linfct = mcp(Year = "Tukey")))


# Check Assumptions

# 1. Homogeneity of variances
plot(aov.SpeciesRichness.HERBIVORES.ENTIRE, 1)

# Levenes Test
leveneTest(SpeciesRichness.HERBIVORES ~ Year, data = ENTIRE.Stats)

# 2. Normality
plot(aov.SpeciesRichness.HERBIVORES.ENTIRE, 2)

# Extract the residuals
aov_residuals.SpeciesRichness.HERBIVORES.ENTIRE <- residuals(object = aov.SpeciesRichness.HERBIVORES.ENTIRE )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals.SpeciesRichness.HERBIVORES.ENTIRE )


#### ANOVA Process (ENTIRE, Species Richness Carnivores for Year) ####

group_by(ENTIRE.Stats, Year) %>%
           summarise(
             count = n(),
             mean = mean(SpeciesRichness.CARNIVORES, na.rm = TRUE),
             sd = sd(SpeciesRichness.CARNIVORES, na.rm = TRUE)
           )


# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
ggboxplot(ENTIRE.Stats, x = "Year", y = "SpeciesRichness.CARNIVORES", 
          color = "Year", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("2013", "2017", "2018"),
          ylab = "Carnivore Species Richness", xlab = "Year")


# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(ENTIRE.Stats, x = "Year", y = "SpeciesRichness.CARNIVORES", 
       add = c("mean_se", "jitter"), 
       order = c("2013", "2017", "2018"),
       ylab = "Carnivore Species Richness", xlab = "Year")


# Compute the analysis of variance
aov.SpeciesRichness.CARNIVORES.ENTIRE <- aov(SpeciesRichness.CARNIVORES ~ Year, data = ENTIRE.Stats)
# Summary of the analysis
summary(aov.SpeciesRichness.CARNIVORES.ENTIRE)

TukeyHSD(aov.SpeciesRichness.CARNIVORES.ENTIRE)

summary(glht(aov.SpeciesRichness.CARNIVORES.ENTIRE, linfct = mcp(Year = "Tukey")))


# Check Assumptions

# 1. Homogeneity of variances
plot(aov.SpeciesRichness.CARNIVORES.ENTIRE, 1)

# Levenes Test
leveneTest(SpeciesRichness.CARNIVORES ~ Year, data = ENTIRE.Stats)

# 2. Normality
plot(aov.SpeciesRichness.CARNIVORES.ENTIRE, 2)

# Extract the residuals
aov_residuals.SpeciesRichness.CARNIVORES.ENTIRE <- residuals(object = aov.SpeciesRichness.CARNIVORES.ENTIRE )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals.SpeciesRichness.CARNIVORES.ENTIRE )


#### ANOVA Process (ENTIRE, Species Richness for Status) ####

group_by(ENTIRE.Stats, Status) %>%
           summarise(
             count = n(),
             mean = mean(SpeciesRichness, na.rm = TRUE),
             sd = sd(SpeciesRichness, na.rm = TRUE)
           )


# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
ggboxplot(ENTIRE.Stats, x = "Status", y = "SpeciesRichness", 
          color = "Status", palette = c("#00AFBB", "#E7B800"),
          order = c("0", "1"),
          ylab = "Species Richness", xlab = "Status")


# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(ENTIRE.Stats, x = "Status", y = "SpeciesRichness", 
       add = c("mean_se", "jitter"), 
       order = c("0", "1"),
       ylab = "Species Richness", xlab = "Status")


# Compute the analysis of variance
aov.SpeciesRichness.ENTIRE <- aov(SpeciesRichness ~ Status, data = ENTIRE.Stats)
# Summary of the analysis
summary(aov.SpeciesRichness.ENTIRE)

TukeyHSD(aov.SpeciesRichness.ENTIRE)

summary(glht(aov.SpeciesRichness.ENTIRE, linfct = mcp(Status = "Tukey")))


# Check Assumptions

# 1. Homogeneity of variances
plot(aov.SpeciesRichness.ENTIRE, 1)

# Levenes Test
leveneTest(SpeciesRichness ~ Status, data = ENTIRE.Stats)

# 2. Normality
plot(aov.SpeciesRichness.ENTIRE, 2)

# Extract the residuals
aov_residuals.SpeciesRichness.ENTIRE <- residuals(object = aov.SpeciesRichness.ENTIRE )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals.SpeciesRichness.ENTIRE )


#### ANOVA Process (ENTIRE, Species Richness Herbivores for Status) ####

group_by(ENTIRE.Stats, Status) %>%
           summarise(
             count = n(),
             mean = mean(SpeciesRichness.HERBIVORES, na.rm = TRUE),
             sd = sd(SpeciesRichness.HERBIVORES, na.rm = TRUE)
           )


# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
ggboxplot(ENTIRE.Stats, x = "Status", y = "SpeciesRichness.HERBIVORES", 
          color = "Status", palette = c("#00AFBB", "#E7B800"),
          order = c("0", "1"),
          ylab = "Herbivore Species Richness", xlab = "Status")


# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(ENTIRE.Stats, x = "Status", y = "SpeciesRichness.HERBIVORES", 
       add = c("mean_se", "jitter"), 
       order = c("0", "1"),
       ylab = "Herbivore Species Richness", xlab = "Status")


# Compute the analysis of variance
aov.SpeciesRichness.HERBIVORES.ENTIRE <- aov(SpeciesRichness.HERBIVORES ~ Status, data = ENTIRE.Stats)
# Summary of the analysis
summary(aov.SpeciesRichness.HERBIVORES.ENTIRE)

TukeyHSD(aov.SpeciesRichness.HERBIVORES.ENTIRE)

summary(glht(aov.SpeciesRichness.HERBIVORES.ENTIRE, linfct = mcp(Status = "Tukey")))


# Check Assumptions

# 1. Homogeneity of variances
plot(aov.SpeciesRichness.HERBIVORES.ENTIRE, 1)

# Levenes Test
leveneTest(SpeciesRichness.HERBIVORES ~ Status, data = ENTIRE.Stats)

# 2. Normality
plot(aov.SpeciesRichness.HERBIVORES.ENTIRE, 2)

# Extract the residuals
aov_residuals.SpeciesRichness.HERBIVORES.ENTIRE <- residuals(object = aov.SpeciesRichness.HERBIVORES.ENTIRE )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals.SpeciesRichness.HERBIVORES.ENTIRE )


#### ANOVA Process (ENTIRE, Species Richness Carnivores for Status) ####

group_by(ENTIRE.Stats, Status) %>%
           summarise(
             count = n(),
             mean = mean(SpeciesRichness.CARNIVORES, na.rm = TRUE),
             sd = sd(SpeciesRichness.CARNIVORES, na.rm = TRUE)
           )


# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
ggboxplot(ENTIRE.Stats, x = "Status", y = "SpeciesRichness.CARNIVORES", 
          color = "Status", palette = c("#00AFBB", "#E7B800"),
          order = c("0", "1"),
          ylab = "Carnivore Species Richness", xlab = "Status")


# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(ENTIRE.Stats, x = "Status", y = "SpeciesRichness.CARNIVORES", 
       add = c("mean_se", "jitter"), 
       order = c("0", "1"),
       ylab = "Carnivore Species Richness", xlab = "Status")


# Compute the analysis of variance
aov.SpeciesRichness.CARNIVORES.ENTIRE <- aov(SpeciesRichness.CARNIVORES ~ Status, data = ENTIRE.Stats)
# Summary of the analysis
summary(aov.SpeciesRichness.CARNIVORES.ENTIRE)

TukeyHSD(aov.SpeciesRichness.CARNIVORES.ENTIRE)

summary(glht(aov.SpeciesRichness.CARNIVORES.ENTIRE, linfct = mcp(Status = "Tukey")))


# Check Assumptions

# 1. Homogeneity of variances
plot(aov.SpeciesRichness.CARNIVORES.ENTIRE, 1)

# Levenes Test
leveneTest(SpeciesRichness.CARNIVORES ~ Status, data = ENTIRE.Stats)

# 2. Normality
plot(aov.SpeciesRichness.CARNIVORES.ENTIRE, 2)

# Extract the residuals
aov_residuals.SpeciesRichness.CARNIVORES.ENTIRE <- residuals(object = aov.SpeciesRichness.CARNIVORES.ENTIRE )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals.SpeciesRichness.CARNIVORES.ENTIRE )
