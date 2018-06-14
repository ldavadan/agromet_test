### Script of functions to get CORINE land cover from Wallonia and identify land covers near to PAMESEB stations


#' It creates 6 groups from the 26 different land cover labels
#' @author Loïc Davadan - ldavadan.github.io
#' @param col.name A name of a column you want to reclass
reclass_CLC <- function(col.name){
  
  dplyr::case_when(col.name <= 121 ~ "Artificials surfaces",
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


#' It gets CLC data from Wallonia and reclass them
get_clc_wal <- function() {
  
  # Lambert 2008
  lambert2008.crs <- "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
  
  # Load spatial data from Wallonia limits
  be.sp <- raster::getData('GADM', country = 'BE', level = 1, download = TRUE)
  be.sp$NAME_1
  wallonie.sp <- be.sp[be.sp$NAME_1 == "Wallonie",]
  wallonie.3812.poly.sp <- sp::spTransform(wallonie.sp, CRS(projargs = dplyr::filter(rgdal::make_EPSG(), code == "3812")$prj4))
  
  # CORINE land cover for Belgium
  corine.sp <- maptools::readShapePoly("./data-raw/CLC12_BE.shp")
  crs(corine.sp) <- lambert2008.crs
  # CORINE land cover for Wallonia
  # https://stackoverflow.com/questions/13982773/crop-for-spatialpolygonsdataframe
  corine.wal.sp <- raster::crop(corine.sp, wallonie.3812.poly.sp)
  
  # Legend for CORINE land cover
  # Download legend
  utils::download.file("http://www.eea.europa.eu/data-and-maps/data/corine-land-cover-2006-raster-1/corine-land-cover-classes-and/clc_legend.csv/at_download/file",
                destfile = "./data-raw/clc_legend.csv")
  legend <- utils::read.csv(file = "./data-raw/clc_legend.csv", header = TRUE, sep = ",")
  
  # Legend codes present in Wallonia
  legend.code.wal <- base::data.frame(base::unique(corine.wal.sp$code_12))
  
  # Legend for CORINE land cover in Wallonia
  # https://stackoverflow.com/questions/38850629/subset-a-column-in-data-frame-based-on-another-data-frame-list
  legend.wal <- base::subset(legend, CLC_CODE %in% legend.code.wal$unique.corine.wal.sp.code_12.)
  
  # CLC_CODE class from integer to numeric
  legend.wal$CLC_CODE <- base::as.numeric(legend.wal$CLC_CODE)
  
  corine.wal.sf <- sf::st_as_sf(corine.wal.sp)
  corine.wal.sf$code_12 <- base::as.numeric(base::paste(corine.wal.sf$code_12))
  
  # Reclass all types of CLC to create 6 groups
  corine.wal.simple.sf <- corine.wal.sf %>%
    dplyr::mutate(CLASS = reclass_CLC(code_12))
  
}


#' It returns a data frame with percentage of cover of each land cover around a station
#' @param corine.wal.simple.sf A simple feature of land covers in Wallonia
#' @param radius.num A numeric corresponding to the radius of the buffer that you want
#' @param stations.sf A simple feature which has coordinates of points
#' @return a data frame presenting percentage of cover of each land cover around a station
extract_stations_clc_buffer <- function(corine.wal.simple.sf = NULL, radius.num = NULL, stations.sf = NULL) {
  
  # Make a buffer around stations
  # https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a
  # https://stackoverflow.com/questions/46704878/circle-around-a-geographic-point-with-st-buffer
  stations.buff.sf <- sf::st_buffer(x = stations.sf, dist = radius.num)
  
  # Cross-reference data to find the different land covers in the buffer
  # http://inspire.ngi.be/download-free/atomfeeds/AtomFeed-CLC2012-en.xml - CRS provided in the link
  class.buff.stations.sf <- sf::st_intersection(corine.wal.simple.sf, stations.buff.sf) 
  class.buff.stations.sf <- dplyr::mutate(class.buff.stations.sf, customID = base::paste0("poly_",base::seq_along(1:base::nrow(class.buff.stations.sf))))
  
  # Verification
  base::identical(base::nrow(class.buff.stations.sf), base::length(base::unique(class.buff.stations.sf$customID)))
  
  # Extract area of land covers in the buffer
  # https://gis.stackexchange.com/questions/229453/create-a-circle-of-defined-radius-around-a-point-and-then-find-the-overlapping-a
  class.buff.stations.sf.summary <- dplyr::group_by(class.buff.stations.sf, customID) %>% 
    dplyr::summarise() %>%
    dplyr::mutate(common_area = sf::st_area(.))
  
  # Make a column with percentage of occupation of each land cover
  class.buff <- sf::st_join(x = class.buff.stations.sf, y = class.buff.stations.sf.summary, join = sf::st_covered_by) %>%
    dplyr::select(sid, CLASS, common_area) %>%
    dplyr::mutate(rate_cover = base::as.numeric(common_area/(pi*radius.num^2) * 100))
  
  base::return(class.buff)
  
  base::library(mapview)
  mapview::mapview(class.buff)
  
}

# # Load AGROMET stations from API and project in EPSG:3812
# stations.sp <- build_agromet_stations_points.sp.fun()
# stations.sp <- sp::spTransform(stations.sp, CRSobj = "+proj=lcc +lat_1=49.83333333333334 +lat_2=51.16666666666666 +lat_0=50.797815 +lon_0=4.359215833333333 +x_0=649328 +y_0=665262 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
# stations.sf <- sf::st_as_sf(stations.sp)
# corine.wal.simple.sf <- get_clc_wal()
# test <- extract_stations_clc_buffer(corine.wal.simple.sf, 100, stations.sf)


#' It reshapes data
#' @param class.buffers.sf A simple feature corresponding to buffers associated to their land covers with proportions
#' @return a data frame with percentage of each land covers for each station
convert_stations_clc_buffer <- function(class.buffers.sf = NULL) {
  
  # Delete geometry column
  class.buffers.df <- base::data.frame(class.buffers.sf)
  
  # Reshape data with CLASS labels in columns names
  # https://stackoverflow.com/questions/39053451/using-spread-with-duplicate-identifiers-for-rows
  class.buffers.clean.df <- class.buffers.df %>%
    dplyr::select(sid, CLASS, rate_cover) %>%
    reshape2::dcast(sid ~ CLASS, fun = base::sum)
  
  # https://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame
  class.buffers.clean.df <- class.buffers.clean.df[,c(1,2,5,4,3,6)]
  
  base::return(class.buffers.clean.df)
}

# test2 <- convert_stations_clc_buffer(test)


