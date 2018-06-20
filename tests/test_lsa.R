#+ preparation, echo=FALSE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# Avoid interference with old variables by cleaning the Global Environment
rm(list=ls(all=TRUE))

# Automagically set the wd and the root of all projects 
if (!require("here")) install.packages("here")
library(here)
wd.chr <- here::here()

# loading the library to manage all the other libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
requiredPackages <- read.csv("./settings/requiredPackages.csv", quote = "", sep = ",", header=TRUE, stringsAsFactors=FALSE)
p_load(char=requiredPackages$packageName, character.only=TRUE )
p_loaded()

# Dynamic Sourcing of all the required functions
source(paste0("../R-utilities/R-utilities.R"))
source_files_recursively.fun("./R")
source_files_recursively.fun("../agrometeor-public/R/")

# install.packages("rjson")

# load JSON file
lsa.json <- jsonlite::fromJSON("~/Documents/code/agromet-tests/data-raw/lsasaf.json")

# get data (date and ens) in a list
lsa.list <- lsa.json$features$properties

# get coordinates in a list
lsa.coordinates.list <- lsa.json$features$geometry$coordinates

# add coordinates of each measure
for(i in 1:length(lsa.coordinates.list)){
lsa.list[[i]][,'long'] <- NA
lsa.list[[i]][,'lat'] <- NA
lsa.list[[i]][,'sid'] <- NA
}
for(i in 1:length(lsa.coordinates.list)){
  for(j in 1:nrow(lsa.list[[i]])){
    lsa.list[[i]][j, 'long'] <- lsa.coordinates.list[[i]][1]
    lsa.list[[i]][j, 'lat'] <- lsa.coordinates.list[[i]][2]
    lsa.list[[i]][j, 'sid'] <- paste0("st_",i)
  }
}

# create a data frame with every data
lsa.df <- bind_rows(lsa.list)
lsa.df$ens <- as.numeric(lsa.df$ens)
lsa.df$mhour <- as_datetime(lsa.df$mhour)

# convert to sf
lsa.sf <- st_as_sf(lsa.df, coords = c('long', 'lat'))
st_crs(lsa.sf) <- "+proj=longlat +datum=WGS84 +no_defs"

# convert to Lambert 2008 CRS
lambert2008.crs <- "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
lsa.3812.sf <- st_transform(lsa.sf, crs = lambert2008.crs)

# observe ens for one station for period
lsa.st_450.df <- subset(lsa.df, sid == "st_450")
plot(x = lsa.st_450.df$mhour, y = lsa.st_450.df$ens)

# observe stations positions
    # stations.sp <- build_agromet_stations_points.sp.fun()
    # stations.sp <- sp::spTransform(stations.sp, CRSobj = "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
    # stations.sf <- sf::st_as_sf(stations.sp)
lsa_st_visu.sf <- lsa.sf %>%
  select(geometry, sid) %>%
  group_by(sid) %>%
  summarise(first(sid))
mapview(lsa_st_visu.sf) + mapview(stations.sf, col.regions = "red")


#' It returns a data frame with every point of solar irradiance measure around a station
#' @param lsa.3812.sf A simple feature of solar irradiance in Wallonia
#' @param radius.num A numeric corresponding to the radius of the buffer that you want
#' @param stations.sf A simple feature which has coordinates of points
#' @return a data frame presenting percentage of cover of each land cover around a station
extract_stations_lsa_buffer <- function(lsa.3812.sf, radius.num, stations.sf) {
  
  # Make a buffer around stations
  # https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a
  # https://stackoverflow.com/questions/46704878/circle-around-a-geographic-point-with-st-buffer
  stations.buff.sf <- sf::st_buffer(x = stations.sf, dist = radius.num,nQuadSegs = 100)
  
  # Cross-reference data to find the different land covers in the buffer
  # http://inspire.ngi.be/download-free/atomfeeds/AtomFeed-CLC2012-en.xml - CRS provided in the link
  class.buff.stations.sf <- sf::st_intersection(lsa.3812.sf, stations.buff.sf) 

  base::return(class.buff.stations.sf)
  
}
# buffers on physical stations
buff_lsa.sf <- extract_stations_lsa_buffer(lsa.3812.sf, radius.num = 5000, stations.sf)

buff_lsa_visu.sf <- buff_lsa.sf %>%
  select(geometry, sid) %>%
  group_by(sid) %>%
  summarise(first(sid))
mapview(buff_lsa_visu.sf) + mapview(stations.sf, col.regions = "red")

# mean of lsa for one station inside buffer
buff_mean_lsa.sf <- buff_lsa.sf %>%
  filter(sid.1 == 1) %>%
  group_by(mhour) %>%
  summarise(mean_ens =mean(ens))
plot(y = buff_mean_lsa.sf$mean_ens, x = buff_mean_lsa.sf$mhour)

load("~/Documents/code/agromet-tests/data-raw/wal_grid_1km.sf.rda")
# grid of virtual stations crossed with solar irradiance spots
grid_lsa.sf <- st_intersection(lsa.3812.sf, wal_grid_1km.sf)

grid_lsa_visu.sf <- grid_lsa.sf %>%
  select(geometry, sid) %>%
  group_by(sid) %>%
  summarise(first(sid))
mapview(grid_lsa_visu.sf) + mapview(wal_grid_1km.sf[2000:6000,], col.regions = "red") # env 5% covered by st lsa














