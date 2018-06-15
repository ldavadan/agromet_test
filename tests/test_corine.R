#+ ---------------------------------
#' ## Script preparation
#' 
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

# https://www.geo.be/#!/catalog/details/bcd19aa9-c320-4116-971b-6e4376137f13?l=en

# Download shapefile
# Warning : go to https://ac.ngi.be/client-open/vndl5zdmH4wgScjyTVdS?language=en&openpath=ngi-standard-open%2FVectordata%2FCLC2012%2FCLC12_BE_shp_L08.zip&tab=dataaccess&auth=false&open=true&accesscode=vndl5zdmH4wgScjyTVdS
# and accept the conditions and then, download the file CLC12_BE.zip and unzip it a a folder
# download.file("https://ac.ngi.be/client-open/2RxqJzG8p4ezxcE926oF/ngi-standard-open/Vectordata/CLC2012/CLC12_BE_shp_L08.zip",
#              destfile = "./data-raw/CLC12_BE.zip")

#unzip("./data-raw/CLC12_BE.zip", exdir = "./data-raw")

# Lambert 2008
lambert2008.crs <- "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"


# Load spatial data from Wallonia limits
be.sp <- getData('GADM', country = 'BE', level = 1, download = TRUE)
wallonie.sp <- be.sp[be.sp$NAME_1 == "Wallonie",]
wallonie.3812.poly.sp <- spTransform(wallonie.sp, CRS(projargs = dplyr::filter(rgdal::make_EPSG(), code == "3812")$prj4))

# CORINE land cover for Belgium
corine.sp <- readShapePoly("./data-raw/CLC12_BE.shp")
crs(corine.sp) <- lambert2008.crs
# CORINE land cover for Wallonia
# https://stackoverflow.com/questions/13982773/crop-for-spatialpolygonsdataframe
corine.wal.sp <- crop(corine.sp, wallonie.3812.poly.sp)


# Legend for CORINE land cover
# Download legend
#download.file("http://www.eea.europa.eu/data-and-maps/data/corine-land-cover-2006-raster-1/corine-land-cover-classes-and/clc_legend.csv/at_download/file",
#              destfile = "./data-raw/clc_legend.csv")
legend <- read.csv(file = "./data-raw/clc_legend.csv", header = TRUE, sep = ",")

# Legend codes for Wallonia
legend.code.wal <- data.frame(unique(corine.wal.sp$code_12))

# Legend for CORINE land cover in Wallonia
# https://stackoverflow.com/questions/38850629/subset-a-column-in-data-frame-based-on-another-data-frame-list
legend.wal <- subset(legend, CLC_CODE %in% legend.code.wal$unique.corine.wal.sp.code_12.)

# CLC_CODE class from integer to numeric
legend.wal$CLC_CODE <- as.numeric(legend.wal$CLC_CODE)

corine.wal.sf <- st_as_sf(corine.wal.sp, crs = 3812)
corine.wal.sf$code_12 <- as.numeric(paste(corine.wal.sf$code_12))


# Description of the function to reclass CLC
reclass_CLC <- function(col.name){
  
  case_when(col.name <= 142 ~ "Artificials surfaces",
            col.name == 211 ~ "Agricultural areas",
            col.name == 222 ~ "Agricultural areas",
            col.name == 231 ~ "Herbaceous vegetation",
            col.name == 242 ~ "Agricultural areas",
            col.name == 243 ~ "Agricultural areas",
            col.name == 311 ~ "Forest",
            col.name == 312 ~ "Forest",
            col.name == 313 ~ "Forest",
            col.name == 321 ~ "Herbaceous vegetation",
            col.name == 322 ~ "Herbaceous vegetation",
            col.name == 324 ~ "Forest",
            col.name > 400 ~ "Water")
}

# Reclass all types of CLC to create 6 groups
corine.wal.simple.sf <- corine.wal.sf %>%
  mutate(CLASS = reclass_CLC(code_12))

#corine.wal.simple.sp <- as(corine.wal.simple.sf, "Spatial")



### Create a grid with class of each point
# crs(wallonie.3812.sp)
# crs(corine.wal.simple.sp) <- crs(wallonie.3812.sp)
# identical(crs(wallonie.3812.sp), crs(corine.wal.simple.sp))
# test <- over(x = wallonie.3812.sp, y = corine.wal.simple.sp)
# 
# class.grid <- bind_cols(data.frame(wallonie.3812.sp), class = test$CLASS)
# coordinates(class.grid) <- c("x1", "x2")
# library(mapview)
# mapview(class.grid, col.region = c("yellow3", "red", "forestgreen", "green", "grey10", "cyan"))



# Load AGROMET stations from API and project in EPSG:3812
stations.sp <- build_agromet_stations_points.sp.fun()
stations.sp <- spTransform(stations.sp, CRSobj = "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
stations.sf <- st_as_sf(stations.sp)

# Make a buffer around stations
# https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a
# https://stackoverflow.com/questions/46704878/circle-around-a-geographic-point-with-st-buffer
stations.buff.sf <- st_buffer(x = stations.sf, dist = 100)

# Cross-reference data to find the different land covers in the buffer
# http://inspire.ngi.be/download-free/atomfeeds/AtomFeed-CLC2012-en.xml - CRS provided in the link
class.buff.stations.sf <- st_intersection(corine.wal.simple.sf, stations.buff.sf) 
class.buff.stations.sf <- mutate(class.buff.stations.sf, customID = paste0("poly_",seq_along(1:nrow(class.buff.stations.sf))))

# Verification
identical(nrow(class.buff.stations.sf), length(unique(class.buff.stations.sf$customID)))

# Extract area of land covers in the buffer
# https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a
class.buff.stations.sf.summary <- group_by(class.buff.stations.sf, customID) %>% 
  dplyr::summarise() %>%
  mutate(common_area = st_area(.))

# Make a column with percentage of occupation of each land cover
# class.buff.stations.gr.sf <- inner_join(as.data.frame(class.buff.stations.sf), 
#                                         as.data.frame(class.buff.stations.sf.summary), "ID", copy = TRUE) %>%
#   select(sid, CLASS, common_area) %>%
#   mutate(rate_cover = as.numeric(common_area/(pi*100^2) * 100))

# Make a column with percentage of occupation of each land cover
class.buff <- st_join(x = class.buff.stations.sf, y = class.buff.stations.sf.summary, join = st_covered_by) %>%
  select(sid, CLASS, common_area) %>%
  mutate(rate_cover = as.numeric(common_area/(pi*100^2) * 100))

## Identification of the polygon which was problematic
guilty <- dplyr::filter(corine.wal.simple.sf, ID == "BE-6930")
map <- mapview(class.buff)

mapview(guilty) + stations.buff.sf

