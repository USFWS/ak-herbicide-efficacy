library(sf)
library(tidyverse)
#library(sp)
library("stringr")


# Ugashik OH --------------------------------------------------------------
setwd ("C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/SouthernAlaska/2023_Sampling/UgashikOrangeHawkweedJul2021")
Ugashik_spatial<-st_read(dsn=".",layer="UgashikOrangeHawkweedJul2021")
names(Ugashik_spatial)


Ugashik_spatial$geometry
st_area(Ugashik_spatial)
plot(Ugashik_spatial)

set.seed(2025)
SRS_UGOH<-st_sample(Ugashik_spatial,size=120)

#Alaska Albers, EPSG:3338 to wgs84 EPSG 4326
SRS_UGOH<-st_transform(SRS_UGOH, 4326)

ggplot() +
  geom_sf(data = Ugashik_spatial,col="grey") +
  geom_sf(data = SRS_UGOH, col ="black")+
  theme_bw() +
  ggtitle("")

#SRS_UGOH is a spatial object, covert to data.frame
SRS_UGOH<-as.data.frame(SRS_UGOH)
#create two columns
SRS_UGOH<-SRS_UGOH %>%
  separate(geometry, c("Longitude", "Latitude"), ", ")
  
#Clean it up, Export random sample
SRS_UGOH$Longitude<-substr(SRS_UGOH$Longitude, start=3, stop=nchar(SRS_UGOH$Longitude)-1)
SRS_UGOH$Latitude<-substr(SRS_UGOH$Latitude, start=1, stop=nchar(SRS_UGOH$Latitude)-1)

SRS_UGOH$Site<-rep("UGOH",length(SRS_UGOH$Latitude))
SRS_UGOH$Year<-rep(2025,length(SRS_UGOH$Latitude))
SRS_UGOH$Sample<-rep(1:length(SRS_UGOH$Latitude))
SRS_UGOH$Site<-paste(SRS_UGOH$Site, "_", SRS_UGOH$Sample,"_",SRS_UGOH$Year)
  
SRS_UGOH<-SRS_UGOH |> 
  select(Site,Latitude,Longitude)

#write.csv(SRS_UGOH,"C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/SouthernAlaska/2025_Sampling/Ugashik_OH_SRS_20250207.csv", row.names=FALSE)



# CB_Thistle --------------------------------------------------------------
setwd ("C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/SouthernAlaska/2023_Sampling/IZMInvasives2022")
CB_Thistle_spatial<-st_read(dsn=".",layer="2022ThistleCombinedPolygon")
names(CB_Thistle_spatial)


CB_Thistle_spatial$geometry
st_area(CB_Thistle_spatial)
plot(CB_Thistle_spatial)

set.seed(2025)
SRS_CBTH<-st_sample(CB_Thistle_spatial,size=120)

#Alaska Albers, EPSG:3338 to wgs84 EPSG 4326
SRS_CBTH<-st_transform(SRS_CBTH, 4326)

ggplot() +
  geom_sf(data = CB_Thistle_spatial,col="grey") +
  geom_sf(data = SRS_CBTH, col ="black")+
  theme_bw() +
  ggtitle("")

#SRS_CBTH is a spatial object, covert to data.frame
SRS_CBTH<-as.data.frame(SRS_CBTH)
#create two columns
SRS_CBTH<-SRS_CBTH %>%
  separate(geometry, c("Longitude", "Latitude"), ", ")

#Clean it up, Export random sample
SRS_CBTH$Longitude<-substr(SRS_CBTH$Longitude, start=3, stop=nchar(SRS_CBTH$Longitude)-1)
SRS_CBTH$Latitude<-substr(SRS_CBTH$Latitude, start=1, stop=nchar(SRS_CBTH$Latitude)-1)

SRS_CBTH$Site<-rep("CBTH",length(SRS_CBTH$Latitude))
SRS_CBTH$Year<-rep(2025,length(SRS_CBTH$Latitude))
SRS_CBTH$Sample<-rep(1:length(SRS_CBTH$Latitude))
SRS_CBTH$Site<-paste(SRS_CBTH$Site, "_",  SRS_CBTH$Sample,"_",SRS_CBTH$Year )



SRS_CBTH<-SRS_CBTH |> 
  select(Site,Latitude,Longitude)



#write.csv(SRS_CBTH,"C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/SouthernAlaska/2025_Sampling/CB_Thistle_SRS_20250207.csv", row.names=FALSE)

# CB_oxeye --------------------------------------------------------------
setwd ("C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/SouthernAlaska/2023_Sampling/IZMInvasives2022")
CB_Oxeye_spatial<-st_read(dsn=".",layer="2022OxeyeCombinedPolygon")
names(CB_Oxeye_spatial)


CB_Oxeye_spatial$geometry
st_area(CB_Oxeye_spatial)
plot(CB_Oxeye_spatial)

set.seed(2025)
SRS_CBOX<-st_sample(CB_Oxeye_spatial,size=40)

#Alaska Albers, EPSG:3338 to wgs84 EPSG 4326
SRS_CBOX<-st_transform(SRS_CBOX, 4326)

ggplot() +
  geom_sf(data = CB_Oxeye_spatial,col="grey") +
  geom_sf(data = SRS_CBOX, col ="black")+
  theme_bw() +
  ggtitle("")

#SRS_CBOX is a spatial object, covert to data.frame
SRS_CBOX<-as.data.frame(SRS_CBOX)
#create two columns
SRS_CBOX<-SRS_CBOX %>%
  separate(geometry, c("Longitude", "Latitude"), ", ")

#Clean it up, Export random sample
SRS_CBOX$Longitude<-substr(SRS_CBOX$Longitude, start=3, stop=nchar(SRS_CBOX$Longitude)-1)
SRS_CBOX$Latitude<-substr(SRS_CBOX$Latitude, start=1, stop=nchar(SRS_CBOX$Latitude)-1)

SRS_CBOX$Site<-rep("CBOX",length(SRS_CBOX$Latitude))
SRS_CBOX$Year<-rep(2025,length(SRS_CBOX$Latitude))
SRS_CBOX$Sample<-rep(1:length(SRS_CBOX$Latitude))
SRS_CBOX$Site<-paste(SRS_CBOX$Site, "_", SRS_CBOX$Sample, "_", SRS_CBOX$Year)

SRS_CBOX<-SRS_CBOX |> 
  select(Site,Latitude,Longitude)


# CB_Hawkweed--------------------------------------------------------------
setwd ("C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/SouthernAlaska/2023_Sampling/IZMInvasives2022")
CB_Hawkweed_spatial<-st_read(dsn=".",layer="2022HawkweedCombinedPolygon")
names(CB_Hawkweed_spatial)


CB_Hawkweed_spatial$geometry
st_area(CB_Hawkweed_spatial)
plot(CB_Hawkweed_spatial)

set.seed(2025)
SRS_CBHW<-st_sample(CB_Hawkweed_spatial,size=40)

#Alaska Albers, EPSG:3338 to wgs84 EPSG 4326
SRS_CBHW<-st_transform(SRS_CBHW, 4326)

ggplot() +
  geom_sf(data = CB_Hawkweed_spatial,col="grey") +
  geom_sf(data = SRS_CBHW, col ="black")+
  theme_bw() +
  ggtitle("")

#SRS_CBHW is a spatial object, covert to data.frame
SRS_CBHW<-as.data.frame(SRS_CBHW)
#create two columns
SRS_CBHW<-SRS_CBHW %>%
  separate(geometry, c("Longitude", "Latitude"), ", ")

#Clean it up, Export random sample
SRS_CBHW$Longitude<-substr(SRS_CBHW$Longitude, start=3, stop=nchar(SRS_CBHW$Longitude)-1)
SRS_CBHW$Latitude<-substr(SRS_CBHW$Latitude, start=1, stop=nchar(SRS_CBHW$Latitude)-1)

SRS_CBHW$Site<-rep("CBHW",length(SRS_CBHW$Latitude))
SRS_CBHW$Year<-rep(2025,length(SRS_CBHW$Latitude))
SRS_CBHW$Sample<-rep(1:length(SRS_CBHW$Latitude))
SRS_CBHW$Site<-paste(SRS_CBHW$Site, "_", SRS_CBHW$Sample, "_", SRS_CBHW$Year)

SRS_CBHW<-SRS_CBHW |> 
  select(Site,Latitude,Longitude)

#write.csv(SRS_CBHW,"C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/SouthernAlaska/2025_Sampling/CB_Hawkweed_SRS_20250207.csv", row.names=FALSE)

# CB_Buttercup--------------------------------------------------------------
setwd ("C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/SouthernAlaska/2023_Sampling/IZMInvasives2022")
CB_Buttercup_spatial<-st_read(dsn=".",layer="USFWSCreepingButtercup2022")
names(CB_Buttercup_spatial)


CB_Buttercup_spatial$geometry
st_area(CB_Buttercup_spatial)
plot(CB_Buttercup_spatial)

set.seed(2025)
SRS_CBBC<-st_sample(CB_Buttercup_spatial,size=120)

#Alaska Albers, EPSG:3338 to wgs84 EPSG 4326
SRS_CBBC<-st_transform(SRS_CBBC, 4326)

ggplot() +
  geom_sf(data = CB_Buttercup_spatial,col="grey") +
  geom_sf(data = SRS_CBBC, col ="black")+
  theme_bw() +
  ggtitle("")

#SRS_CBBC is a spatial object, covert to data.frame
SRS_CBBC<-as.data.frame(SRS_CBBC)
#create two columns
SRS_CBBC<-SRS_CBBC %>%
  separate(geometry, c("Longitude", "Latitude"), ", ")

#Clean it up, Export random sample
SRS_CBBC$Longitude<-substr(SRS_CBBC$Longitude, start=3, stop=nchar(SRS_CBBC$Longitude)-1)
SRS_CBBC$Latitude<-substr(SRS_CBBC$Latitude, start=1, stop=nchar(SRS_CBBC$Latitude)-1)

SRS_CBBC$Site<-rep("CBBC",length(SRS_CBBC$Latitude))
SRS_CBBC$Year<-rep(2025,length(SRS_CBBC$Latitude))
SRS_CBBC$Sample<-rep(1:length(SRS_CBBC$Latitude))
SRS_CBBC$Site<-paste(SRS_CBBC$Site, "_", SRS_CBBC$Sample, "_", SRS_CBBC$Year)

SRS_CBBC<-SRS_CBBC |> 
  select(Site,Latitude,Longitude)

SRS_Invasive_SouthernAlaska<-rbind(SRS_UGOH,SRS_CBBC,SRS_CBHW,SRS_CBOX,SRS_CBTH)
write.csv(SRS_Invasive_SouthernAlaska,"C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/SouthernAlaska/2025_Sampling/SRS_Invasive_SouthernAlaska_20250213.csv", row.names=FALSE)
