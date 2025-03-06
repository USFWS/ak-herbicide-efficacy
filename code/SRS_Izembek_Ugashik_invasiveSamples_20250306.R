library(sf)
library(tidyverse)
library(sp)
library("stringr")


# Ugashik OH --------------------------------------------------------------
Ugashik_spatial <- st_read("./data/UgashikOrangeHawkweedJul2021/UgashikOrangeHawkweedJul2021.shp")

names(Ugashik_spatial)


Ugashik_spatial$geometry
st_area(Ugashik_spatial)
plot(Ugashik_spatial)

set.seed(2025)
SRS_UGOH <- st_sample(Ugashik_spatial, size = 120)

#Alaska Albers, EPSG:3338 to wgs84 EPSG 4326
SRS_UGOH <- st_transform(SRS_UGOH, 4326)

ggplot() +
  geom_sf(data = Ugashik_spatial, col = "grey") +
  geom_sf(data = SRS_UGOH, col = "black") +
  theme_bw() +
  ggtitle("")

#SRS_UGOH is a spatial object, covert to data.frame
SRS_UGOH <- as.data.frame(SRS_UGOH)
#create two columns
SRS_UGOH <- SRS_UGOH %>%
  separate(geometry, c("Longitude", "Latitude"), ", ")

#Clean it up, Export random sample
SRS_UGOH$Longitude <- substr(SRS_UGOH$Longitude,
                             start = 3,
                             stop = nchar(SRS_UGOH$Longitude) - 1)
SRS_UGOH$Latitude <- substr(SRS_UGOH$Latitude,
                            start = 1,
                            stop = nchar(SRS_UGOH$Latitude) - 1)

SRS_UGOH$Site <- rep("UGOH", length(SRS_UGOH$Latitude))
SRS_UGOH$Year <- rep(2025, length(SRS_UGOH$Latitude))
SRS_UGOH$Sample <- rep(1:length(SRS_UGOH$Latitude))
SRS_UGOH$Site <- paste(SRS_UGOH$Site, "_", SRS_UGOH$Sample, "_", SRS_UGOH$Year)

SRS_UGOH <- SRS_UGOH |>
  select(Site, Year, Latitude, Longitude)



# CB_Thistle --------------------------------------------------------------

CB_Thistle_spatial <- st_read("./data/IZMInvasives2022/2022ThistleCombinedPolygon.shp")
names(CB_Thistle_spatial)


CB_Thistle_spatial$geometry
st_area(CB_Thistle_spatial)
plot(CB_Thistle_spatial)

set.seed(2025)
SRS_CBCT <- st_sample(CB_Thistle_spatial, size = 120)

#Alaska Albers, EPSG:3338 to wgs84 EPSG 4326
SRS_CBCT <- st_transform(SRS_CBCT, 4326)

ggplot() +
  geom_sf(data = CB_Thistle_spatial, col = "grey") +
  geom_sf(data = SRS_CBCT, col = "black") +
  theme_bw() +
  ggtitle("")

#SRS_CBCT is a spatial object, covert to data.frame
SRS_CBCT <- as.data.frame(SRS_CBCT)
#create two columns
SRS_CBCT <- SRS_CBCT %>%
  separate(geometry, c("Longitude", "Latitude"), ", ")

#Clean it up, Export random sample
SRS_CBCT$Longitude <- substr(SRS_CBCT$Longitude,
                             start = 3,
                             stop = nchar(SRS_CBCT$Longitude) - 1)
SRS_CBCT$Latitude <- substr(SRS_CBCT$Latitude,
                            start = 1,
                            stop = nchar(SRS_CBCT$Latitude) - 1)

SRS_CBCT$Site <- rep("CBCT", length(SRS_CBCT$Latitude))
SRS_CBCT$Year <- rep(2025, length(SRS_CBCT$Latitude))
SRS_CBCT$Sample <- rep(1:length(SRS_CBCT$Latitude))
SRS_CBCT$Site <- paste(SRS_CBCT$Site, "_", SRS_CBCT$Sample, "_", SRS_CBCT$Year)



SRS_CBCT <- SRS_CBCT |>
  select(Site, Year, Latitude, Longitude)

# CB_oxeye --------------------------------------------------------------

CB_Oxeye_spatial <- st_read("./data/IZMInvasives2022/2022OxeyeCombinedPolygon.shp")
names(CB_Oxeye_spatial)


CB_Oxeye_spatial$geometry
st_area(CB_Oxeye_spatial)
plot(CB_Oxeye_spatial)

set.seed(2025)
SRS_CBOD <- st_sample(CB_Oxeye_spatial, size = 40)

#Alaska Albers, EPSG:3338 to wgs84 EPSG 4326
SRS_CBOD <- st_transform(SRS_CBOD, 4326)

ggplot() +
  geom_sf(data = CB_Oxeye_spatial, col = "grey") +
  geom_sf(data = SRS_CBOD, col = "black") +
  theme_bw() +
  ggtitle("")

#SRS_CBOD is a spatial object, covert to data.frame
SRS_CBOD <- as.data.frame(SRS_CBOD)
#create two columns
SRS_CBOD <- SRS_CBOD %>%
  separate(geometry, c("Longitude", "Latitude"), ", ")

#Clean it up, Export random sample
SRS_CBOD$Longitude <- substr(SRS_CBOD$Longitude,
                             start = 3,
                             stop = nchar(SRS_CBOD$Longitude) - 1)
SRS_CBOD$Latitude <- substr(SRS_CBOD$Latitude,
                            start = 1,
                            stop = nchar(SRS_CBOD$Latitude) - 1)

SRS_CBOD$Site <- rep("CBOD", length(SRS_CBOD$Latitude))
SRS_CBOD$Year <- rep(2025, length(SRS_CBOD$Latitude))
SRS_CBOD$Sample <- rep(1:length(SRS_CBOD$Latitude))
SRS_CBOD$Site <- paste(SRS_CBOD$Site, "_", SRS_CBOD$Sample, "_", SRS_CBOD$Year)

SRS_CBOD <- SRS_CBOD |>
  select(Site, Year, Latitude, Longitude)


# CB_Hawkweed--------------------------------------------------------------

CB_Hawkweed_spatial <- st_read("./data/IZMInvasives2022/2022HawkweedCombinedPolygon.shp")
names(CB_Hawkweed_spatial)


CB_Hawkweed_spatial$geometry
st_area(CB_Hawkweed_spatial)
plot(CB_Hawkweed_spatial)

set.seed(2025)
SRS_CBOH <- st_sample(CB_Hawkweed_spatial, size = 40)

#Alaska Albers, EPSG:3338 to wgs84 EPSG 4326
SRS_CBOH <- st_transform(SRS_CBOH, 4326)

ggplot() +
  geom_sf(data = CB_Hawkweed_spatial, col = "grey") +
  geom_sf(data = SRS_CBOH, col = "black") +
  theme_bw() +
  ggtitle("")

#SRS_CBOH is a spatial object, covert to data.frame
SRS_CBOH <- as.data.frame(SRS_CBOH)
#create two columns
SRS_CBOH <- SRS_CBOH %>%
  separate(geometry, c("Longitude", "Latitude"), ", ")

#Clean it up, Export random sample
SRS_CBOH$Longitude <- substr(SRS_CBOH$Longitude,
                             start = 3,
                             stop = nchar(SRS_CBOH$Longitude) - 1)
SRS_CBOH$Latitude <- substr(SRS_CBOH$Latitude,
                            start = 1,
                            stop = nchar(SRS_CBOH$Latitude) - 1)

SRS_CBOH$Site <- rep("CBOH", length(SRS_CBOH$Latitude))
SRS_CBOH$Year <- rep(2025, length(SRS_CBOH$Latitude))
SRS_CBOH$Sample <- rep(1:length(SRS_CBOH$Latitude))
SRS_CBOH$Site <- paste(SRS_CBOH$Site, "_", SRS_CBOH$Sample, "_", SRS_CBOH$Year)

SRS_CBOH <- SRS_CBOH |>
  select(Site, Year, Latitude, Longitude)


SRS_Invasive_SouthernAlaska <- rbind(SRS_UGOH, SRS_CBOH, SRS_CBOD, SRS_CBCT)
write.csv(
  SRS_Invasive_SouthernAlaska,
  "./data/Sampling/SRS_Invasive_SouthernAlaska_20250306.csv",
  row.names = FALSE
)
