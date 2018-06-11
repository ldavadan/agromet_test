#+ ---------------------------------
#' ## Script preparation
#' 
#+ preparation, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

# Avoid interference with old variables by cleaning the Global Environment
rm(list=ls(all=TRUE))

# Automagically set the wd and the root of all projects 
if (!require("here")) install.packages("here")
library(here)
wd.chr <- here::here()

# loading the library to manage all the other libraries
if (!require("pacman")) install.packages("pacman")
library(pacman)
requiredPackages <- read.csv("./requiredPackages.csv", quote = "", sep = ",", header=TRUE, stringsAsFactors=FALSE)
p_load(char=requiredPackages$packageName, character.only=TRUE )
p_loaded()

# Dynamic Sourcing of all the required functions
source(paste0("../../R-utilities/R-utilities.R"))
source_files_recursively.fun("./R")
source_files_recursively.fun("../../agrometeor-public/R/")


# Loading data from API : list of all the stations
stations.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_TOKEN"),
    table_name.chr = "station",
    sensors.chr=NULL,
    stations_ids.chr = "all",
    api_v.chr="v1",
    test.bool=FALSE
  )
)
  
# Load a recordset : tsa from station 61 from 3-2-16 to 5-4-16
records.df <-prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_TOKEN"),
    table_name.chr = "cleandata",
    sensors.chr="tsa",
    stations_ids.chr = "61",
    api_v.chr="v2",
    test.bool=FALSE,
    dfrom="2016-02-03",
    dto="2016-04-05"
  )
)
# app.pameseb.be

# test of clipping map
wall.ras <- build_topo_rasters.fun()
wall.pol <- rasterToPolygons(wall.ras)


# install.packages("rgeos")
library(rgeos)
library(sp)

data("wrld_simpl")
map <- crop(wrld_simpl, wall.pol)
ggplot() + geom_polygon(data = map, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)



###



records.sf <- st_as_sf(x = records.df, coords = c("longitude", "latitude") )

map <- build_leaflet_template.fun(records.sf = records.sf)
map


