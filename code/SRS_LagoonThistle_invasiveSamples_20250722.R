library(sf)
library(tidyverse)
library(sp)
library("stringr")


# Cove Island --------------------------------------------------------------
Lagoon_Thistle <- st_read("C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/wishnek_herbicideEfficacy/ak-herbicide-efficiency/data/LagoonThistleUpdatedPolygons2025.shp")

names(Lagoon_Thistle)

CoveIsland<-Lagoon_Thistle[1,1]
BirdsallIsland<-Lagoon_Thistle[2,1]


set.seed(2025)
SRS_CoveIsland <- st_sample(CoveIsland, size = 40)

#Alaska Albers, EPSG:3338 to wgs84 EPSG 4326
SRS_CoveIsland <- st_transform(SRS_CoveIsland, 4326)

ggplot() +
  #geom_sf(data = Lagoon_Thistle, col = "grey") +
  geom_sf(data = CoveIsland, col = "grey") +
  geom_sf(data = SRS_CoveIsland, col = "black") +
  theme_bw() +
  ggtitle("Cove Island Simple Random Sample (n=40)")
 

#SRS_CoveIsland is a spatial object, covert to data.frame
SRS_CoveIsland <- as.data.frame(SRS_CoveIsland)
#create two columns
SRS_CoveIsland <- SRS_CoveIsland %>%
  separate(geometry, c("Longitude", "Latitude"), ", ")

#Clean it up, Export random sample
SRS_CoveIsland$Longitude <- substr(SRS_CoveIsland$Longitude,
                             start = 3,
                             stop = nchar(SRS_CoveIsland$Longitude) - 1)
SRS_CoveIsland$Latitude <- substr(SRS_CoveIsland$Latitude,
                            start = 1,
                            stop = nchar(SRS_CoveIsland$Latitude) - 1)

SRS_CoveIsland$Site <- rep("CoveIsland", length(SRS_CoveIsland$Latitude))
SRS_CoveIsland$Year <- rep(2025, length(SRS_CoveIsland$Latitude))
SRS_CoveIsland$Sample <- rep(1:length(SRS_CoveIsland$Latitude))
SRS_CoveIsland$Sample <- paste(SRS_CoveIsland$Site, "_", SRS_CoveIsland$Sample, "_", SRS_CoveIsland$Year)

SRS_CoveIsland <- SRS_CoveIsland |> 
  dplyr::select(Site, Year, Latitude, Longitude, Sample)


# Birdsall Island ---------------------------------------------------------

BirdsallIsland<-Lagoon_Thistle[2,1]


set.seed(2025)
SRS_BirdsallIsland <- st_sample(BirdsallIsland, size = 40)

#Alaska Albers, EPSG:3338 to wgs84 EPSG 4326
SRS_BirdsallIsland <- st_transform(SRS_BirdsallIsland, 4326)

ggplot() +
  geom_sf(data = BirdsallIsland, col = "grey") +
  geom_sf(data = SRS_BirdsallIsland, col = "black") +
  theme_bw() +
  ggtitle("Birdsall Island Simple Random Sample (n=40)")


#SRS_BirdsallIsland is a spatial object, covert to data.frame
SRS_BirdsallIsland <- as.data.frame(SRS_BirdsallIsland)
#create two columns
SRS_BirdsallIsland <- SRS_BirdsallIsland %>%
  separate(geometry, c("Longitude", "Latitude"), ", ")

#Clean it up, Export random sample
SRS_BirdsallIsland$Longitude <- substr(SRS_BirdsallIsland$Longitude,
                                       start = 3,
                                       stop = nchar(SRS_BirdsallIsland$Longitude) - 1)
SRS_BirdsallIsland$Latitude <- substr(SRS_BirdsallIsland$Latitude,
                                      start = 1,
                                      stop = nchar(SRS_BirdsallIsland$Latitude) - 1)

SRS_BirdsallIsland$Site <- rep("BirdsallIsland", length(SRS_BirdsallIsland$Latitude))
SRS_BirdsallIsland$Year <- rep(2025, length(SRS_BirdsallIsland$Latitude))
SRS_BirdsallIsland$Sample <- rep(1:length(SRS_BirdsallIsland$Latitude))
SRS_BirdsallIsland$Sample <- paste(SRS_BirdsallIsland$Site, "_", SRS_BirdsallIsland$Sample, "_", SRS_BirdsallIsland$Year)

SRS_BirdsallIsland <- SRS_BirdsallIsland |> 
  dplyr::select(Site, Year, Latitude, Longitude, Sample)




SRS_IzembekThistle<- rbind(SRS_CoveIsland, SRS_BirdsallIsland)
write.csv(
  SRS_IzembekThistle,
  "C:/Users/AMBenson/OneDrive - DOI/Documents/InvasiveSpecies/wishnek_herbicideEfficacy/ak-herbicide-efficiency/data/SRS_IzembekThistle_20250722.csv",
  row.names = FALSE
)
