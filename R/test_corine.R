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
#download.file("https://ac.ngi.be/client-open/4JLSG7JnsKlqHwlGdiQh/ngi-standard-open/Vectordata/CLC2012/CLC12_BE_shp_L08.zip",
#              destfile = "./data-raw/CLC12_BE.zip")

#unzip("./data-raw/CLC12_BE.zip", exdir = "./data-raw")



# CORINE land cover for Belgium
corine.sp <- readShapePoly("./data-raw/CLC12_BE.shp")
# CORINE land cover for Wallonia
# https://stackoverflow.com/questions/13982773/crop-for-spatialpolygonsdataframe
load("~/Documents/code/agromet-tests/data-raw/wallonie.3812.sp")
corine.wal.sp <- crop(corine.sp, wallonie.3812.sp)

# Legend for CORINE land cover
# Download legend
download.file("http://www.eea.europa.eu/data-and-maps/data/corine-land-cover-2006-raster-1/corine-land-cover-classes-and/clc_legend.csv/at_download/file",
              destfile = "./data-raw/clc_legend.csv")
legend <- read.csv(file = "./data-raw/clc_legend.csv", header = TRUE, sep = ",")

# Legend codes for Wallonia
legend.code.wal <- data.frame(unique(corine.wal.sp$code_12))

# Legend for CORINE land cover in Wallonia
# https://stackoverflow.com/questions/38850629/subset-a-column-in-data-frame-based-on-another-data-frame-list
legend.wal <- subset(legend, CLC_CODE %in% legend.code.wal$unique.corine.wal.sp.code_12.)

# CLC_CODE class from integer to numeric
legend.wal$CLC_CODE <- as.numeric(legend.wal$CLC_CODE)


corine.wal.sf <- st_as_sf(corine.wal.sp)
corine.wal.sf$code_12 <- as.numeric(paste(corine.wal.sf$code_12))


# Description of the function to reclass CLC
reclass_CLC <- function(col.name){
  
  case_when(col.name <= 121 ~ "Artificials surfaces",
            col.name == 122 ~ "Road/Rail",
            col.name == 123 ~ "Artificials surfaces",
            col.name == 124 ~ "Artificials surfaces",
            col.name == 131 ~ "Artificials surfaces",
            col.name == 132 ~ "Artificials surfaces",
            col.name == 133 ~ "Artificials surfaces",
            col.name == 141 ~ "Artificials surfaces",
            col.name == 142 ~ "Artificials surfaces",
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


corine.wal.simple.sf <- corine.wal.sf %>%
  mutate(CLASS = reclass_CLC(code_12))

corine.wal.simple.sp <- as(corine.wal.simple.sf, "Spatial")
# https://gis.stackexchange.com/questions/17798/converting-a-polygon-into-a-raster-using-r
corine.wal.simple.df <- data.frame(corine.wal.simple.sf)
rast <- raster()
crs(rast) <- '+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333
+x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'
extent(rast) <- extent(corine.wal.simple.sp)
corine.rast <- rasterize(corine.wal.simple.sp, rast)




levels(corine.rast)






grid.sp <- build_wal_grid.sp.fun(1000)
# core the topo rasters stack at the positions of the interpolation grid

corine.df <- data.frame(
  raster::extract(
    projectRaster(
      corine.rast,
      crs=crs(wallonie.3812.sp)),
    wallonie.3812.sp,
    weights=FALSE,
    fun=mean
  )
)






