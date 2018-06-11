# Load the recordset from the nearest station to Frasnes-lez-Anvaing : Esplechin 
records.df <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    sensors.chr= "hra",
    stations_ids.chr = "38",
    api_v.chr="v2",
    test.bool=FALSE,
    dfrom="2011-01-01",
    dto="2012-01-01"
  )
)

# Calculate the mean hra for each day
records.day.df <- records.df %>%
  mutate(day = as.POSIXlt(as.Date(mtime))$yday) %>%
  group_by(sid, poste, longitude, latitude, altitude, network_name, day) %>%
  summarise(hra_mean_day = mean(hra))
records.day.df
write.csv(records.day.df, file = "records_Esplechin_2010-2015.csv")

plot <- ggplot(data=records.day.df, aes(x=day, y=hra_mean_day)) + 
  geom_point(size = 0.1) + 
  geom_smooth()
plot <- ggplotly(plot)
plot

# Verification for mean function
#test <- subset(records.df, as.Date(mtime) == "2015-01-01")
#hra_test <- mean(test$hra)


records.sf <- st_as_sf(x = records.day.df, coords = c("longitude", "latitude") )
map <- build_leaflet_template.fun(records.sf) %>%
  addMarkers(map, lng = records.day.df$longitude[1], lat = records.day.df$latitude[1], 
             popup = "Esplechin", label = "Esplechin") %>%
  addMarkers(map, lng = 3.61536, lat = 50.66471, popup = "Frasnes-lez-Anvaing", 
             label = "Frasnes-lez-Anvaing") %>%
map


# for a year
records.day.df <- records.df %>%
  mutate(day = as.POSIXlt(as.Date(mtime))$yday) %>%
  group_by(sid, poste, longitude, latitude, altitude, network_name, day) %>%
  summarise(hra_mean_day = mean(hra))
records.day.df
write.csv(records.day.df, file = "records_Esplechin_2011.csv")

plot <- ggplot(data=records.day.df, aes(x=day, y=hra_mean_day)) + 
  geom_point(size = 0.1) + 
  geom_smooth()
plot <- ggplotly(plot)
plot



