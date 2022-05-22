

library(pacman)
p_load(tidyverse,sf,data.table)

#tribe_shp <- readRDS("01_data/tribe_shapefiles.rds")
science_tribes <- read_csv("/RSTOR/bayham/projects/tribal_climate/01_data/clean/tribal_dispossession_block.csv",
                           col_types = "cccccccnnnnnnnnnnn")

summary(science_tribes)

#Load original block level file
science_geo <- readRDS("/RSTOR/bayham/projects/tribal_climate/01_data/cache/tribe_shapefiles_micro.rds")
#Calculate area based on shape at block level - reprojecting to meters so area is m^2
science_geo_area <- science_geo %>%
  st_transform(5070) %>%
  select(GEOID10) %>%
  mutate(gis_area = st_area(geometry))



science_area <- science_geo_area %>%
  #st_set_geometry(NULL) %>%
  mutate(gis_area = as.numeric(gis_area)) %>%
  inner_join(science_tribes) 
  
#Aggregating over all geometries - I think NAs are causing it to drop geometries
science_reservation <- science_area %>%
  group_by(tribe_census_id) %>%
  summarize(across(heatdays:tri,~weighted.mean(.,w=gis_area,na.rm=T))) %>%
  ungroup()

#Try with just drought bc there are no missing
science_reservation <- science_area %>%
  group_by(tribe_census_id) %>%
  summarize(across(drought,~weighted.mean(.,w=gis_area,na.rm=T))) %>%
  ungroup()

mapview::mapview(science_reservation,zcol="drought")
mapview::mapview(science_geo %>% filter(state=="06"))



glimpse(science_tribes)

rgdal::ogrListLayers("/RSTOR/bayham/projects/native_land_research_initiative/geo_compare/tlgdb_2018_a_us_aiarelated.gdb/")


tl <- st_read("/RSTOR/bayham/projects/native_land_research_initiative/geo_compare/tl_2018_us_aiannh/tl_2018_us_aiannh.shp")
cb <- st_read("/RSTOR/bayham/projects/native_land_research_initiative/geo_compare/cb_2018_us_aiannh_500k/cb_2018_us_aiannh_500k.shp")
tlgdb <- st_read(dsn="/RSTOR/bayham/projects/native_land_research_initiative/geo_compare/tlgdb_2018_a_us_aiarelated.gdb/",
                 layer = "AmericanIndian_AlaskaNative_NativeHawaiianArea")


m <- mapview::mapview(tl) +
  mapview::mapview(cb,col.regions="red") +
  mapview::mapview(tlgdb,col.regions="green") 

m
