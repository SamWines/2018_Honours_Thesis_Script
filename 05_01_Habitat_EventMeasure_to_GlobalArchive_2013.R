###### Importing Habitat Annotation points from TransectMeasure (Stereo)----


### Written by Tim Langlois 
### Any errors are due to Tim Langlois
### Please forward any updates and improvements to timothy.langlois@uwa.edu.au or make a pull request on the GitHub

### This version has been adapted for use by Sam Wines (slwines@deakin.edu.au)

### Designed to take direct output from an TransectMeasure project. 

### objective is to 
# 1 Import and combine data from .txt file data collected in a 4 x 5 grid of CATAMI and relief codes
# 2 Summarise cover of benthos and score of complexity as % and mean and SD values
# 3 Summarise cover of benthos and score of complexity as point scores


# Set directories----
rm(list=ls())
study<-"PointAddis2013"

data.dir=("/Users/samwines/Desktop/Uni-Honours/02_Data/07_HabitatData/Langlois_Format/2013")

habitat.data=paste(data.dir,"Data/Habitat",sep="/")
tidy.data=paste(data.dir,"Data/Tidy data",sep="/")
plots=paste(data.dir,"Plots",sep="/")


# Libraries required:
detach("package:plyr", unload=TRUE)#will error - no worries
library(tidyr)
citation("tidyr")
library(dplyr)
citation("dplyr")
options(dplyr.width = Inf) #enables head() to display all coloums
library(ggplot2)
library(stringr)


# Load and format habitat annotation data from TransectMeasure----
setwd(habitat.data)
dir()
hab<-read.delim('PA_2013_Dot Point Measurements.txt',header=T,skip=4,stringsAsFactors=FALSE)%>%
  setNames(tolower(names(.)))%>%
  mutate(OpCode= str_replace_all(.$filename,c(".avi" = "","_" = "", ".AVI" = "")))%>%
  select(-c(filename,x,x.1,frame,time..mins.,date,location,site..,transect..,latitude,longitude,rugosity,depth,collector,fishing.status,spare,spare.1,code,radius..))
head(hab,2)
unique(hab$OpCode)


# Create %fov----
fov.point.score<-hab%>%
  select(-c(broad,morphology,type,relief))%>%
  filter(!fieldofview=="")%>%
  filter(!is.na(fieldofview))%>%
  mutate(fieldofview=paste("fieldofview",fieldofview,sep = "."))%>%
  mutate(count=1)%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  select(-c(image.row,image.col))%>%
  group_by(OpCode)%>%
  summarise_each(funs(sum))
head(fov.point.score,2)

fov.percent.cover<-fov.point.score%>%
  mutate_each(funs(replace(.,is.na(.),0)))%>%
  mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  group_by(OpCode)%>%
  mutate_each(funs(./Total.Sum), matches("fieldofview."))%>%
  select(-Total.Sum)
head(fov.percent.cover,2)

NoFOV <- fov.percent.cover %>%
  filter(fieldofview.Open==0)%>%
  select(OpCode, fieldofview.Open)



# Create relief----
relief.point.score<-hab%>%
  filter(!broad%in%c("Unknown","Open Water"))%>%
  filter(!relief%in%c("Unknown",""))%>%
  select(-c(broad,morphology,type,fieldofview))%>%
  mutate(count=1)%>%
  mutate(relief=ifelse(relief==".0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.","Relief: 0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.",ifelse(relief==".1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.","Relief: 1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.",ifelse(relief==".2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.","Relief: 2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.",ifelse(relief==".3. Good relief structure with some overhangs. >45 substrate slope.","Relief: 3. Good relief structure with some overhangs. >45 substrate slope.",ifelse(relief==".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.","Relief: 4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.",ifelse(relief==".5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.","Relief: 5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.",relief)))))))%>%
  spread(key=relief,value=count, fill=0)%>%
  select(-c(image.row,image.col))%>%
  group_by(OpCode)%>%
  summarise_each(funs(sum))
head(relief.point.score,2)

relief.mean.and.sd<-hab%>%
  filter(!broad%in%c("Unknown","Open Water"))%>%
  filter(!relief%in%c("Unknown",""))%>%
  select(-c(broad,morphology,type,fieldofview,image.row,image.col))%>%
  mutate(relief.rank=ifelse(relief==".0. Flat substrate, sandy, rubble with few features. ~0 substrate slope.",0,ifelse(relief==".1. Some relief features amongst mostly flat substrate/sand/rubble. <45 degree substrate slope.",1,ifelse(relief==".2. Mostly relief features amongst some flat substrate or rubble. ~45 substrate slope.",2,ifelse(relief==".3. Good relief structure with some overhangs. >45 substrate slope.",3,ifelse(relief==".4. High structural complexity, fissures and caves. Vertical wall. ~90 substrate slope.",4,ifelse(relief==".5. Exceptional structural complexity, numerous large holes and caves. Vertical wall. ~90 substrate slope.",5,relief)))))))%>%
  select(-c(relief))%>%
  mutate(relief.rank=as.numeric(relief.rank))%>%
  group_by(OpCode)%>%
  summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))
head(relief.mean.and.sd,2)


# CREATE catami_broad------
broad.point.score<-hab%>%
  select(-c(fieldofview,morphology,type,relief))%>%
  mutate(broad=ifelse(broad=="Octocoral/Black","Octocoral.Black",ifelse(broad=="Invertebrate complex","Invertebrate.complex",ifelse(broad=="Open Water","Open.Water",broad))))%>% #correct bad names
  filter(!broad=="")%>%
  filter(!is.na(broad))%>%
  filter(!broad=="Unknown")%>%
  filter(!broad=="Open.Water")%>%
  mutate(broad=paste("Biota:",broad,sep = " "))%>%
  mutate(count=1)%>%
  group_by(OpCode)%>%
  spread(key=broad,value=count,fill=0)%>%
  select(-c(image.row,image.col))%>%
  group_by(OpCode)%>%
  summarise_each(funs(sum))
head(broad.point.score)

broad.percent.cover<-broad.point.score%>%
  mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  group_by(OpCode)%>%
  mutate_each(funs(./Total.Sum), matches("Biota:"))%>%
  select(-Total.Sum)%>%
  mutate(`Biota: Reef`=`Biota: Consolidated`+`Biota: Macroalgae`+`Biota: Invertebrate.complex`) #Add in a Reef classification 
head(broad.percent.cover)


# CREATE catami_broad_morphology_type-----
#Have to do this in two stages to allow points with morphology only
broad_morphology_only<-hab%>%
  select(-c(fieldofview,relief))%>%
  filter(!morphology=="")%>%
  filter(!is.na(morphology))%>%
  filter(!morphology=="Unknown")%>%
  filter(is.na(type)|type=="")%>%
  filter(!type=="No biota")%>%
  mutate(catami_broad_morphology_type=paste("Biota: ",broad,": ",morphology,sep = ""))
head(broad_morphology_only,2)
unique(broad_morphology_only$type)

broad_morphology_with_type<-hab%>%
  select(-c(fieldofview,relief))%>%
  filter(!morphology=="")%>%
  filter(!is.na(morphology))%>%
  filter(!morphology=="Unknown")%>%
  filter(!type=="")%>%
  filter(!is.na(type))%>%
  filter(!type=="No biota")%>%
  mutate(catami_broad_morphology_type=paste("Biota: ",broad,": ",morphology,": ",type,sep = ""))
head(broad_morphology_with_type,2)

broad_morphology_type.point.score<-bind_rows(broad_morphology_only,broad_morphology_with_type)%>%
  mutate(count=1)%>%
  group_by(OpCode)%>%
  spread(key=catami_broad_morphology_type,value=count,fill=0)%>%
  select(-c(image.row,image.col,broad,morphology,type))%>%
  group_by(OpCode)%>%
  summarise_each(funs(sum))
head(broad_morphology_type.point.score,2)

broad_morphology_type.percent.cover<-broad_morphology_type.point.score%>%
  mutate(Total.Sum=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  group_by(OpCode)%>%
  mutate_each(funs(./Total.Sum), matches("Biota:"))%>%
  select(-Total.Sum)
head(broad_morphology_type.percent.cover,2)


# Write final habitat data----
# join starting with relief - as this is most likely to have the most samples with habitat data
setwd(tidy.data)
dir()

habitat.point.score<-relief.point.score%>%
  left_join(fov.point.score,by="OpCode")%>%
  left_join(broad.point.score,by="OpCode")
# %>%
#   left_join(broad_morphology_type.point.score,by="OpCode") #use this line if you have morphology_type data
head(habitat.point.score,2)
write.table(habitat.point.score,file=paste("x",study,"R_habitat.point.score.txt",sep = "_"), sep = "\t", row.names = FALSE)


habitat.percent.cover.and.mean.and.sd.of.relief<-relief.mean.and.sd%>%
  left_join(fov.percent.cover,by="OpCode")%>%
  left_join(broad.percent.cover,by="OpCode")
# %>%
#   left_join(broad_morphology_type.percent.cover,by="OpCode") #use this line if you have morphology_type data
head(habitat.percent.cover.and.mean.and.sd.of.relief,2)
write.table(habitat.percent.cover.and.mean.and.sd.of.relief,file=paste("x",study,"R_habitat.percent.cover.and.mean.and.sd.of.relief.txt",sep = "_"), sep = "\t", row.names = FALSE)




# Habitat.plot----
setwd(plots)
habitat.plot<-habitat.percent.cover.and.mean.and.sd.of.relief%>%
  gather(key=habitat, value = value, 2:ncol(.))%>%
  filter(habitat%in%c("mean.relief","sd.relief","Biota: Macroalgae", "Biota: Invertebrate.complex","Biota: Octocoral.Black","Biota: Sponges","Biota: Stony.corals","Biota: Unconsolidated","Biota: Reef"))
tail(habitat.plot)

# habitat.ggplot<-ggplot(habitat.plot,aes(x=value))+
habitat.ggplot<-ggplot(habitat.plot,aes(x=habitat,y=value))+
  #   geom_histogram()+
  geom_boxplot()+
  #   facet_grid(habitat~.,scales="free")+
  ylab("Percent cover or Value")+
  theme(strip.text.y = element_text(angle=0))+
  #   facet_grid(habitat~.,scales="free")+
  coord_flip()
habitat.ggplot
ggsave(habitat.ggplot,file=paste("x",study,"R_habitat.horizontal.boxplot.png",sep = "_"), width = 10, height = 10,units = "cm")