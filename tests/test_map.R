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




# get data from Wallonia limits
#wallonia.sf <- dplyr::filter(st_as_sf(getData('GADM', country="BE", level=1)), NAME_1=="Wallonie")
wallonie.grid <- build_wal_grid.fun(res.num = 2000, geom.chr = "polygons")

# loading data from all the AGROMET stations
stations.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "station",
    sensors.chr=NULL,
    stations_ids.chr = "all",
    api_v.chr="v2",
    test.bool=FALSE
  )
)


# deleting stations without data
stations.df <- stations.df%>%
  filter(poste != "China") %>%
  filter(network_name == "pameseb") %>% 
  filter(type_name != "Sencrop") %>% 
  filter(!is.na(to)) %>% 
  filter(state == "Ok") %>%
  select(sid, poste, longitude, latitude)

stations.df$longitude <- as.numeric(stations.df$longitude)
stations.df$latitude <- as.numeric(stations.df$latitude)

#stations.sf <- st_as_sf(x = stations.df, coords = c("longitude", "latitude"), crs = 4326)



# load examples
load("~/Documents/code/test/api_test/example.df.Rdata")
load("~/Documents/code/test/api_test/example.sf.Rdata")


# building the map with ggplot2

# ggmap <- ggplot() + 
#   geom_sf(data = wallonia.sf) +
#   geom_sf(data = stations.sf, colour = "red") +
#   north(wallonia.sf, scale = 0.1, location = "bottomleft", anchor = c(x = 2.7, y = 49.7), symbol = 12) +
#   ggsn::scalebar(wallonia.sf, dist = 50, dd2km = TRUE, model = 'WGS84', 
#            location = "bottomleft", st.dist = 0.03, st.size = 3,
#            box.fill = c("grey", "white"), box.color = "grey") +
#   labs(y= "Latitude", x = "Longitude") #+
#   #scale_fill_gradientn(aesthetics = example.sf$response, colours = terrain.colors(10))
# ggmap


################################################

load("~/Documents/code/test/api_test/example.r.Rdata")

projection(example.r) <- '+init=epsg:4326 +ellps=WGS84 +datum=WGS84'
example.r <- projectRaster(example.r, crs = '+init=EPSG:4326 +ellps=WGS84 +datum=WGS84')

library(tmap)
file <- "./craw.png"

data(land)


# Declaration of the function to do breaks for the legend
breaks_legend <- function(spatial_data = NULL){
  
  # get values of summary and stock them in a list
  breaks <- as.list(summary(spatial_data$response))
  breaks <- breaks[1:6]
  
  return(breaks)
}

# Declaration of the function to create the map of spatialized temperature
create_map_tsa <- function(
  background_map = NULL,
  spatial_data.ras = NULL,
  proj_syst = NULL){
  
  library(tmap)
  # create the background of the map with scalebar, compass and CRA-W logo
  tm_shape(background_map, projection = 4326) +
    tm_borders("grey20") +
    tm_compass(position = c(0.2,0.2), color.light = "grey20") +
    tm_scale_bar(breaks = NULL, width = NA, size = 0.5, text.color = "grey20",
                 color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.2,0.01),
                 just = NA) +
    tm_logo(file, height = 3, halign = "center", margin = 0.2,
            position = c(0,0), just = NA) 
  
  # add the layer with data and legend
  tm_shape(spatial_data.ras, projection = proj_syst) +
    tm_raster("response", palette = "OrRd", auto.palette.mapping = FALSE,
              title = "Temperature (°C)", breaks = breaks_legend(spatial_data.ras)) +
    tm_layout(legend.position = c(0.01,0.1))
}

example.sp <- as(example.sf, "Spatial")
# example.sppix <- as(example.sp, "SpatialPixelsDataFrame")
# example.grid <- as(example.sppix, "SpatialGridDataFrame")
load("~/Documents/code/test/api_test/example.sp")
# projection(example.sp) <- '+init=epsg:4326 +ellps=WGS84 +datum=WGS84'

load("~/Documents/code/test/api_test/gridded.3812.sp")



map <- tm_shape(shp = gridded.3812.sp, projection = "EPSG:3812") +
  tm_raster("response", palette = "OrRd", title="Temperature", auto.palette.mapping=FALSE, 
            breaks = seq(as.numeric(breaks_legend(gridded.3812.sp)[1]), 
                         as.numeric(breaks_legend(gridded.3812.sp)[6]),
                         length = 11)) +
  tm_layout(legend.position = c(0.01,0.2), legend.text.size = 0.7) +
  tm_shape(wallonie.grid, projection = 3812) +
  tm_borders("black", lwd = 2) +
  tm_fill(alpha = 0) +
  tm_compass(position = c(0.2,0.2), color.light = "grey20") +
  tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",
               color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.2,0.01),
               just = NA) +
  tm_logo(file, height = 2, halign = "center", margin = 0.2,
          position = c(0,0), just = NA) 
map

###

breaks_legend(example.grid)

create_map_tsa(background_map = wallonia.sf, spatial_data.ras = example.r, proj_syst = 4326)

