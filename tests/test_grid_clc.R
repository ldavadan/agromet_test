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


# Get CLC from Wallonia
corine.wal.simple.sf <- get_clc_wal()

# load("~/Documents/code/agromet-tests/data-raw/wal_grid_1km.sf.rda")
# Create column area of each cell
wal_grid_1km.sf <- wal_grid_1km.sf %>%
  dplyr::mutate(AREA = sf::st_area(.))

# st_crs(corine.wal.simple.sf) <- lambert2008.crs
# Extract CLC for each cell from the grid
# https://github.com/r-spatial/sf/issues/347
# install.packages("lwgeom")
library(lwgeom)
class.clip.sf <- sf::st_intersection(lwgeom::st_make_valid(corine.wal.simple.sf), wal_grid_1km.sf)

# create a new column with custom id and select columns
class.clip.sf <- dplyr::mutate(class.clip.sf,
                                        customID = base::paste0("poly_",
                                                                base::seq_along(1:base::nrow(class.clip.sf)))) %>%
  dplyr::select(ID, Area_ha, Shape_Leng, Shape_Area, CLASS, sid, customID, AREA, geometry)

# create new column with area of each polygon for every cell
class.clip.sf.summary <- dplyr::group_by(class.clip.sf, customID) %>% 
  dplyr::summarise() %>%
  dplyr::mutate(common_area = sf::st_area(.))

# create a column with percentage of cover of every CLC in each cell
class.clip.final.sf <- sf::st_join(x = class.clip.sf, y = class.clip.sf.summary, join = sf::st_covered_by) %>%
  dplyr::mutate(rate_cover = base::as.numeric(common_area/AREA * 100)) %>%
  dplyr::select(sid, CLASS, common_area, rate_cover)


##### extract
class.clip.final.sp <- as(class.clip.final.sf, "Spatial")
class.clip.final.sp$common_area <- base::as.numeric(class.clip.final.sp$common_area)
library(rgdal)
# write shapefile
# https://stackoverflow.com/questions/13926811/export-a-polygon-from-an-r-plot-as-a-shapefile
rgdal::writeOGR(class.clip.final.sp, dsn = './data-output/test', layer = 'class_grid_clc', driver = "ESRI Shapefile")


# create table more lisible
class.clip.clean.df <- base::data.frame(class.clip.final.sf) %>%
  dplyr::select(sid, CLASS, rate_cover) %>%
  reshape2::dcast(sid ~ CLASS, fun = base::sum)




