# iDiv page to download data
https://idata.idiv.de/ddm/Data/ShowData/3474?version=76

library(sf) # version ‘1.0.16’
library(terra) # version ‘1.7.71’
library(tidyverse) # version ‘2.0.0’

# download sPlotOpen dataset directly from R
devtools::install_github("andrewsiefert/sPlotOpenR", dependencies = T, force = T)

library(sPlotOpenR)

# download sPlotOpen dataset and load into R
data <- get_sPlot(load = T) 

DT <- data[["DT"]]
DT$Species <- gsub(DT$Species, pattern = ' ', replacement = '_')

# Separate data into species and non-species levels
species_level <- DT[grepl("_", DT$Species),]
NOspecies_level <- DT[!grepl("_", DT$Species),]

# Merge data
d <- merge(species_level, data[["header"]], by = 'PlotObservationID')

rm(DT2.oa)
rm(header.oa)

# Filter for Europe, Year >= 1992, and Location uncertainty < 250
d <- d[d$Continent == 'Europe' & d$Location_uncertainty < 250,]
d$Year <- substr(d$Date_of_recording, 1, 4)
d <- d[d$Year >= 1992,]

# European grid: 1) raster layer of 0.5 degree 2) projection in LAEA crs(39.5km of spatial resolution)
r <- rast(resolution=0.5)
e <- ext(-16.1,40.18, 32.88, 84.73)
r<- crop(r, e)
r <- project(r, "EPSG:3035")
values(r) <- 1:ncell(r)

# transformation of the raster layer in polygons
r <- as.polygons(r)
r.sf <- st_as_sf(r)
st_crs(r.sf) <- st_crs(3035)
r.sf <- st_transform(r.sf, crs = st_crs(3035))
r.sf <- st_make_valid(r.sf) 

# dataframe with plots and relative location then, transformation in sf points
dCoord <- d %>% dplyr::select(Longitude, Latitude, PlotObservationID)%>%
  unique()%>%
  drop_na() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  st_crop(., c(xmin = -16.1, ymin = 32.88, xmax = 40.18  , ymax =84.73))%>%
  st_transform(crs = st_crs(3035))

# intersection of sf points with the european grid polygons
OV <- st_intersects(dCoord, r.sf)
OV_l <- unlist(OV) 

# dataframe of the plots within each grid cell
d_id <- d %>% dplyr::select(Longitude, Latitude, PlotObservationID)%>%
  unique()%>%
  drop_na() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  st_crop(., c(xmin = -16.1, ymin = 32.88, xmax = 40.18 , ymax =84.73))%>%
  st_transform(crs = st_crs(3035))%>%
  data.frame(st_coordinates(st_cast(.,"MULTIPOINT")))%>%
  dplyr::select(-geometry)%>%
  cbind(., id=OV_l) %>%
  dplyr::select(id, PlotObservationID) #plot: 9481

rm(dCoord)

# final dataframe
d <- d %>% 
  dplyr::select(Longitude, Latitude, PlotObservationID, Species, Year)%>%
  unique()%>%
  drop_na() %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  st_crop(., c(xmin = -16.1, ymin = 32.88, xmax = 40.18 , ymax =84.73))%>%
  st_transform(crs = st_crs(3035))%>%
  data.frame(st_coordinates(st_cast(.,"MULTIPOINT")))%>%
  dplyr::select(-geometry) %>% left_join(., d_id, by="PlotObservationID")


saveRDS(d, "d.rds")
