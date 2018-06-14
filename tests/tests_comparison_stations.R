sid34 <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    sensors.chr= "hra, tsa, hct",
    stations_ids.chr = "34",
    api_v.chr="v2",
    test.bool=FALSE,
    dfrom="2011-05-01",
    dto="2011-07-01"
  )
)
sid36 <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    sensors.chr= "hra, tsa, hct",
    stations_ids.chr = "36",
    api_v.chr="v2",
    test.bool=FALSE,
    dfrom="2011-05-01",
    dto="2011-07-01"
  )
)

cor(sid34$tsa, sid36$tsa) # 0.995606 (1-3)   0.9894995  (5-7)     agri/road env 12km
cor(sid34$hra, sid36$hra) # 0.9365116 (1-3)  0.9706184  (5-7)

sid9 <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    sensors.chr= "hra, tsa, hct",
    stations_ids.chr = "9",
    api_v.chr="v2",
    test.bool=FALSE,
    dfrom="2011-05-01",
    dto="2011-07-01"
  )
)
sid10 <- prepare_agromet_API_data.fun(
  get_from_agromet_API.fun(
    user_token.chr = Sys.getenv("AGROMET_API_V1_KEY"),
    table_name.chr = "cleandata",
    sensors.chr= "hra, tsa, hct",
    stations_ids.chr = "10",
    api_v.chr="v2",
    test.bool=FALSE,
    dfrom="2011-05-01",
    dto="2011-07-01"
  )
)

cor(sid9$tsa,sid10$tsa) # 0.9818529 (1-3)  0.9678884 (5-7)   agri/forest env 20km
cor(sid9$hra,sid10$hra) # 0.8933561 (1-3)  0.940929  (5-7)



  