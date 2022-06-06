#This script generates the index for all gridmet products

library(pacman)
p_load(tidyverse,lubridate,data.table,sf,ncdf4,janitor,raster,exactextractr,furrr,conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")
#if(!dir.exists("build/cache/temperature")) dir.create("build/cache/temperature")


#################
#Read in the tribal boundaries file
aiannh <- st_read("build/inputs/census_2020/tl_2020_us_aiannh/tl_2020_us_aiannh.shp") %>%
  clean_names() %>%
  st_transform(5070)


#Alt method reading as raster then using exact_extract
whp_rast <- raster("build/inputs/RDS-2015-0046-2/Data/whp_2018_classified/whp2018_cls/w001001.adf")
#crs(whp_rast)<-CRS("+init=epsg:4326")
#mapview::mapview(whp_rast,zcol=)

plan(multisession(workers = 15))

whp_extracted <- aiannh %>%
  group_split(geoid) %>%
  future_map_dfr(~exact_extract(whp_rast,.,#summarize_df = TRUE,
                         fun=function(value,cov_frac){
                           value = ifelse(value>=6,NA,value)   #values 6 and 7 are nonburnable and water
                           weighted.mean(value,w=cov_frac,na.rm=T) #calculate coverage weighted
                         },
                         force_df=T,
                         append_cols=c("geoid")),.progress = T)
  

#Join back with tribal data
geo_whp <- inner_join(aiannh,whp_extracted,by="geoid") %>%
  rename(whp=result)
  

st_write(geo_whp,"output/nlri_whp.shp")
st_write(geo_whp,"output/nlri_whp.gpkg")

whp_extracted %>%
  dplyr::filter(!is.nan(result))
