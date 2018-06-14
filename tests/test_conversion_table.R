### Conversion of a table

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



corine.wal.simple.sf <- get_clc_wal()
class.buff <- extract_stations_clc_buffer(corine.wal.simple.sf = corine.wal.simple.sf, radius.num = 100)
# Delete geometry column
class.buff.df <-data.frame(class.buff)

# Reshape data with CLASS labels in columns names
# https://stackoverflow.com/questions/39053451/using-spread-with-duplicate-identifiers-for-rows
class.stations.df <- class.buff.df %>%
  select(sid, CLASS, rate_cover) %>%
  dcast(sid ~ CLASS, fun = sum)
# reorder columns
class.stations.df<- class.stations.df[,c(1,2,5,4,3,6)]



