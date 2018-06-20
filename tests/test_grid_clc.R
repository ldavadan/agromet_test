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
  dplyr::mutate(area_cell = sf::st_area(.))

# st_crs(corine.wal.simple.sf) <- lambert2008.crs
# Extract CLC for each cell from the grid
# https://github.com/r-spatial/sf/issues/347
# install.packages("lwgeom")
library(lwgeom)
vs.grid.1km.clc.wallonia.polygons_sf <- sf::st_intersection(lwgeom::st_make_valid(corine.wal.simple.sf), wal_grid_1km.sf)

# create a new column with custom id and select columns
vs.grid.1km.clc.wallonia.polygons_sf <- dplyr::mutate(vs.grid.1km.clc.wallonia.polygons_sf,
                                        customID = base::paste0("poly_",
                                                                base::seq_along(1:base::nrow(vs.grid.1km.clc.wallonia.polygons_sf)))) %>%
  dplyr::select(ID, Area_ha, Shape_Leng, Shape_Area, CLASS, sid, customID, area_cell, geometry)

# create new column with area of each polygon for every cell
vs.grid.1km.poly_area_per_cell.clc.wallonia.polygons_sf <- dplyr::group_by(vs.grid.1km.clc.wallonia.polygons_sf, customID) %>% 
  dplyr::summarise() %>%
  dplyr::mutate(common_area = sf::st_area(.))

# create a column with percentage of cover of every CLC in each cell
vs.grid.1km.intersect.clc.wallonia.polygons_sf <- sf::st_join(x = vs.grid.1km.clc.wallonia.polygons_sf, y = vs.grid.1km.poly_area_per_cell.clc.wallonia.polygons_sf, join = sf::st_covered_by) %>%
  dplyr::mutate(rate_cover = base::as.numeric(common_area/AREA * 100)) %>%
  dplyr::select(sid, CLASS, common_area, rate_cover)



##### extract
vs.grid.1km.intersect.clc_wallonia.polygons_sp <- as(vs.grid.1km.intersect.clc.wallonia.polygons_sf, "Spatial")
vs.grid.1km.intersect.clc_wallonia.polygons_sp$common_area <- base::as.numeric(vs.grid.1km.intersect.clc_wallonia.polygons_sp$common_area)
library(rgdal)
# write shapefile
# https://stackoverflow.com/questions/13926811/export-a-polygon-from-an-r-plot-as-a-shapefile
rgdal::writeOGR(vs.grid.1km.intersect.clc_wallonia.polygons_sp, dsn = './data-output/test', layer = 'vs.grid.1km.intersect.clc_wallonia.polygons', driver = "ESRI Shapefile")


# create table more lisible
vs.grid.1km.intersect.clc_wallonia.df <- base::data.frame(vs.grid.1km.intersect.clc_wallonia.polygons_sf) %>%
  dplyr::select(sid, CLASS, rate_cover) %>%
  reshape2::dcast(sid ~ CLASS, fun = base::sum)




