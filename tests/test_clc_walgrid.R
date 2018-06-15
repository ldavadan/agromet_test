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



corine.wal.simple.sf <- get_clc_wal()

wallonie.3812.sf <- build_wal_grid.fun(10000, TRUE, TRUE)
# wallonie.3812.sp <- spTransform(wallonie.3812.sp, CRSobj = lambert2008.crs)
# wallonie.3812.sf <- st_as_sf(wallonie.3812.sp)

class.grid.wal.sf <- extract_stations_clc_buffer(corine.wal.simple.sf, radius.num = 500, stations.sf = wallonie.3812.sf)
mapview(class.grid.wal.sf)
result <- convert_stations_clc_buffer(class.grid.wal.sf)



