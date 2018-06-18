
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

# wallonie.3812.sf <- build_wal_grid.fun(1000, TRUE, TRUE)
# wall.df <- do.call(rbind, st_geometry(wallonie.3812.sf)) %>% 
#   as_tibble() %>% setNames(c("lon","lat"))
# wallonie.3812.df$geometry <- as.double(unlist(wallonie.3812.df$geometry))
# # inspired by https://stackoverflow.com/questions/19627344/how-to-create-a-raster-from-a-data-frame-in-r
# wallonie.3812.ras <- rasterFromXYZ(wall.df)



# Raster of DTM from Wallonia - Resolution 10m
wal.ele.ras <- raster("./data-raw/DTM20_WA_ascii_L08.txt")
proj4string(wal.ele.ras) <- CRS("+init=epsg:3812")
# The data are not projected but are in longlat so we need to project it to get the distance units
# wal.ele.ras <- raster::projectRaster(wal.ele.ras, crs = toString((dplyr::filter(rgdal::make_EPSG(), code=="3812"))$prj4))

# compute the slope from the elevation
wal.slope.ras <- raster::terrain(wal.ele.ras, opt="slope", unit="degrees")

# compute the aspect from the elevation
wal.aspect.ras <- raster::terrain(wal.ele.ras, opt="aspect", unit="degrees")

# create the stack of rasters
wal_topo_highRes.ras <- stack(wal.ele.ras, wal.slope.ras, wal.aspect.ras)

# wal_topo_1km.ras <- aggregate(wal_topo_highRes.ras, fact=100)



