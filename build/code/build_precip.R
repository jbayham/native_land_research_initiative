#This script generates the index for all gridmet products

library(pacman)
p_load(tidyverse,lubridate,data.table,sf,ncdf4,janitor,raster,exactextractr,furrr,conflicted)

conflict_prefer("select", "dplyr")


#################
#Read in the tribal boundaries file
aiannh <- st_read("01_data/census_2020/tl_2020_us_aiannh/tl_2020_us_aiannh.shp") %>%
  clean_names() %>%
  st_transform(4326)

#Alt method reading as raster then using exact_extract
prism <- raster("/RSTOR/bayham/projects/tribal_climate/01_data/PRISM_ppt_30yr_normal_4kmM2_annual_asc/PRISM_ppt_30yr_normal_4kmM2_annual_asc.asc")
crs(prism)<-CRS("+init=epsg:4326")
#mapview::mapview(prism)

tribal_precip <- aiannh %>%
  group_split(geoid) %>%
  map_dfr(~exact_extract(prism,.,include_xy=T,include_cell=T,force_df=T,
                         include_cols=c("geoid")))
  

nlri_precip <- tribal_precip %>%
  group_by(geoid) %>%
  summarize(precip=weighted.mean(value,coverage_fraction,na.rm=T)/25.4) %>%
  ungroup()

nlri_precip_geo <- inner_join(aiannh,final,by="geoid")

#mapview::mapview(nlri_precip_geo,zcol="precip")

#Join back with tribal data

st_write(nlri_precip_geo,"output/nlri_precip.shp")
st_write(nlri_precip_geo,"output/nlri_precip.gpkg")
