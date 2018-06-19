# create a grid of points (center of each cell) for Wallonia
build_wal_grid.fun <- function(res.num, sf.bool, CRS.proj.bool){
  
  # load some spatial data. Administrative Boundary
  be.sp <- raster::getData('GADM', country = 'BE', level = 1, download = TRUE)
  be.sp$NAME_1
  wallonie.sp <- be.sp[be.sp$NAME_1 == "Wallonie",]
  
  # set CRS to "lambert 2008" which EPSG is 3812
  wallonie.3812.sp <- sp::spTransform(wallonie.sp, sp::CRS(projargs = dplyr::filter(rgdal::make_EPSG(), code == "3812")$prj4))
  
  # Create a grid of points within the bbox of the SpatialPolygonsDataFrame (wallonie with meters as map units)
  grid.sp <- sp::makegrid(wallonie.3812.sp, cellsize = res.num) # cellsize in map units!
  
  # grid is a data.frame. To change it to a spatial data set we have to :
  grid.sp <- sp::SpatialPoints(grid.sp, proj4string = sp::CRS(sp::proj4string(wallonie.3812.sp)))
  
  # subset to wallonie polygon
  grid.sp <- grid.sp[wallonie.3812.sp, ]
  
  # make it a sf to easily append an id
  grid.sf <- st_as_sf(grid.sp)
  grid.sf$sid <- paste0("vs_", seq_along(1:nrow(grid.sf)))
  
  # if CRS.proj.bool = TRUE, we want a projected CRS (Lamber 2008 - EPSG 3812)
  if(CRS.proj.bool == TRUE){
    grid.sf
  }
  # else put in a geographic CRS WGS84
  else{
    #grid.s <- sp::spTransform(grid.sp, sp::CRS(sp::proj4string(wallonie.sp)))
    grid.sf <- sf::st_transform(grid.sp, sp::CRS(sp::proj4string(wallonie.sp)))
  }
  
  if(sf.bool == TRUE){
    grid.sf
  }
  # else transform to sf
  else{
    grid.sp <- sf::as(grid.sf, "spatial")
  }
  
}

# Create grid wallonia
wallonie.3812.sf <- build_wal_grid.fun(1000, TRUE, TRUE)
# Separate columns geometry
# https://github.com/r-spatial/sf/issues/231
wallonie.3812.df <- do.call(rbind, st_geometry(wallonie.3812.sf)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
# inspired by https://stackoverflow.com/questions/19627344/how-to-create-a-raster-from-a-data-frame-in-r
wallonie.3812.ras <- rasterFromXYZ(wallonie.3812.df)

corine.wal.simple.sf <- get_clc_wal()
# Rasterize
corine.wal.simple.sp <- as(corine.wal.simple.sf, "Spatial")
corine.wal.simple.ras <- rasterize(corine.wal.simple.sp, wallonie.3812.ras)
###########
# clc <- as.data.frame(corine.wal.simple.ras, xy=TRUE)

# core the topo rasters stack at the positions of the interpolation grid
clc.df <- data.frame(
  raster::extract(
    corine.wal.simple.ras,
    as(wallonie.3812.sf, "Spatial"), weights = TRUE, fun = max)
  )
###########
# build grid wallonia
wallonie.3812.sf <- build_wal_grid.fun(1000, TRUE, TRUE)
# wallonie.3812.sp <- spTransform(wallonie.3812.sp, CRSobj = lambert2008.crs)
# wallonie.3812.sf <- st_as_sf(wallonie.3812.sp)

# extract clc on buffers
class.grid.wal.sf <- extract_stations_clc_buffer(corine.wal.simple.sf, radius.num = 500, stations.sf = wallonie.3812.sf)
#mapview(class.grid.wal.sf)
class.buff.clean.df <- convert_stations_clc_buffer(class.grid.wal.sf)


### Export shapefile
# sf to sp + conversion common_area to numeric because units not accepted
class.grid.wal.sp <- as(class.grid.wal.sf, "Spatial")
class.grid.wal.sp$common_area <- as.numeric(class.grid.wal.sp$common_area)
library(rgdal)
# write shapefile
# https://stackoverflow.com/questions/13926811/export-a-polygon-from-an-r-plot-as-a-shapefile
writeOGR(class.grid.wal.sp, dsn = './data-output/test', layer = 'sid', driver = "ESRI Shapefile")
