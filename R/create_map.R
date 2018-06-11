### Declaration of the function to do breaks for the legend
# breaks_legend <- function(spatial_data = NULL){
#   
#   # get values of summary and stock them in a list
#   breaks <- as.list(quantile(seq(min(spatial_data@data$response, na.rm = TRUE),
#                                  max(spatial_data@data$response, na.rm = TRUE),
#                                  length = 11), type = 5))
#   
#   
#   return(breaks)
# }

### Declaration of the function to create the map of spatialized temperature
# spatial_data.sp : file with spatialized data
# type.chr : static or interactive map ; "static" or "interactive"
# method.chr : method of spatialization
create_map_tsa <- function(
  spatial_data.sp = NULL,
  method.chr = NULL,
  type.chr = NULL){
  
  
  library(tmap)

  load("~/Documents/code/agromet-tests/data-raw/wallonie.3812.sp")
  file <- "./fig/craw.png"
  
  static <- tm_shape(spatial_data.sp, projection="3812") +            # Projection : Belgian Lambert
    tm_raster("response",                                             # spatialize temperature
              palette = "-RdYlBu",
              title = "Temperature (°C)",
              auto.palette.mapping=FALSE,
              breaks = c(quantile(spatial_data.sp$response, 0, na.rm = TRUE),
                         quantile(spatial_data.sp$response, 0.1, na.rm = TRUE),
                         quantile(spatial_data.sp$response, 0.2, na.rm = TRUE),
                         quantile(spatial_data.sp$response, 0.3, na.rm = TRUE),
                         quantile(spatial_data.sp$response, 0.4, na.rm = TRUE),
                         quantile(spatial_data.sp$response, 0.5, na.rm = TRUE),
                         quantile(spatial_data.sp$response, 0.6, na.rm = TRUE),
                         quantile(spatial_data.sp$response, 0.7, na.rm = TRUE),
                         quantile(spatial_data.sp$response, 0.8, na.rm = TRUE),
                         quantile(spatial_data.sp$response, 0.9, na.rm = TRUE),
                         quantile(spatial_data.sp$response, 1, na.rm = TRUE))) +
    tm_compass(position = c(0.9,0.15), color.light = "grey20") +      # north
    tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",  # scale bar 
                 color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.22,0.01),
                 just = NA) +
    tm_logo(file, height = 2, halign = "center", margin = 0.2,        # CRA-W logo
            position = c(-0.01,-0.04), just = NA) +
    tm_shape(wallonie.3812.sp) +                                      # outline of Wallonia
    tm_borders("grey20", lwd = 1.5) +
    tm_layout(legend.position = c(0.01,0.14),                         # parameters
              legend.height = 0.55,
              legend.text.size = 0.7,
              legend.bg.color = "white",
              legend.title.size = 0.9,
              inner.margins = c(0.03, 0.03, 0.07, 0.03),
              frame.lwd = 0,
              bg.color = "grey85",
              main.title = paste("Interpolated temperature with ", method.chr),
              title = "Resolution : 1 km²", 
              title.size = 0.6, 
              title.position = c(0.01, 0.96)) +
    tm_credits("© CRA-W", position = c(.87, 0))
  
  if(type.chr == "static") {
    return(static)
  }else{
    interactive <- tmap_leaflet(static)
    return(interactive)
  }
}


### Creation of the map but not in a function
# library(tmap)
# load("~/Documents/code/test/api_test/gridded.3812.sp")
# load("~/Documents/code/test/api_test/wallonie.3812.sp")
# file <- "./craw.png"
# map1 <- tm_shape(gridded.3812.sp, projection="3812") +
#   tm_raster("response",  
#             palette = "-RdYlBu",
#             title="Temperature (°C)",
#             auto.palette.mapping=FALSE,
#             breaks = breaks_legend(gridded.3812.sp)) +
#   tm_compass(position = c(0.9,0.15), color.light = "grey20") +
#   tm_scale_bar(breaks = NULL, width = NA, size = 0.8, text.color = "grey20",
#                color.dark = "grey20", color.light = "white", lwd = 1, position = c(0.22,0.01),
#                just = NA) +
#   tm_logo(file, height = 2, halign = "center", margin = 0.2,
#           position = c(-0.01,-0.04), just = NA) +
#   tm_shape(wallonie.3812.sp) +
#   tm_borders("grey20", lwd = 1.5) +
#   tm_layout(legend.position = c(0.01,0.14),
#             legend.height = 0.55,
#             legend.text.size = 0.7,
#             legend.bg.color = "white",
#             legend.title.size = 0.9,
#             inner.margins = c(0.03, 0.03, 0.07, 0.03),
#             frame.lwd = 0,
#             bg.color = "grey85",
#             main.title = "Interpolated temperature with <method>",
#             title = "Resolution : 1 km²", 
#             title.size = 0.6, 
#             title.position = c(0.01, 0.96)) +
#   tm_credits("© CRA-W", position = c(.87, 0))
# map1
# map <- tmap_leaflet(map1)
# map


load("~/Documents/code/agromet-tests/data-raw/gridded.3812.sp")
create_map_tsa(spatial_data.sp = gridded.3812.sp, type.chr = "static", method.chr = "<na>")







# recup land cover corine, rapport creation map, env travail
