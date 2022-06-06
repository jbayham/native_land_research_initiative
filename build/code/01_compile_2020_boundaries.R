#This script compiles the 2020 census tribal boundaries and produces spatial and csv files

library(pacman)
p_load(tidyverse,sf)

#########################################
#Read in data - not using tigris because current version does not appear configured for 2020 data
aiannh <- st_read("01_data/census_2020/tl_2020_us_aiannh/tl_2020_us_aiannh.shp")
aitsn <- st_read("01_data/census_2020/tl_2020_us_aitsn/tl_2020_us_aitsn.shp")
anrc <- st_read("01_data/census_2020/tl_2020_02_anrc/tl_2020_02_anrc.shp")
tbg <- st_read("01_data/census_2020/tl_2020_us_tbg/tl_2020_us_tbg.shp")

m <- mapview::mapview(aiannh %>% filter(AIANNHCE=="0010")) +
  mapview::mapview(aitsn[1,],col.regions="red") +
  mapview::mapview(anrc[1,],col.regions="green") +
  mapview::mapview(tbg %>% filter(AIANNHCE=="0010"),col.regions="yellow")

m <- mapview::mapview(aiannh) +
  mapview::mapview(aitsn,col.regions="red") +
  mapview::mapview(anrc,col.regions="green") +
  mapview::mapview(tbg,col.regions="yellow")

mapview::mapshot(m,url="native_lands.html")

#Exporting lists of tribes and identifiers
aiannh %>%
  st_set_geometry(NULL) %>%
  select(AIANNHCE:COMPTYP) %>%
  arrange(AIANNHCE) %>%
  write_csv("01_data/cache/aiannh.csv")

aitsn %>%
  st_set_geometry(NULL) %>%
  select(AIANNHCE:NAMELSAD) %>%
  write_csv("01_data/cache/aitsn.csv")

anrc %>%
  st_set_geometry(NULL) %>%
  select(STATEFP:NAMELSAD) %>%
  write_csv("01_data/cache/anrc.csv")



