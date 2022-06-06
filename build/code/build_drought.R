#This script generates the index for all gridmet products

library(pacman)
p_load(tidyverse,lubridate,data.table,sf,ncdf4,janitor,raster,exactextractr,furrr,conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("year", "lubridate")
#if(!dir.exists("build/cache/temperature")) dir.create("build/cache/temperature")

source("build/code/download_gridmet.R")
#################
#Download the data
gridmetr_download(variables="tmmx",years = c(1979:2021))

#################
#Read in the tribal boundaries file
aiannh <- st_read("build/inputs/census_2020/tl_2020_us_aiannh/tl_2020_us_aiannh.shp") %>%
  clean_names() %>%
  st_transform(4326)


#Open the connection to the netCDF file
nc <- nc_open("build/inputs/gridmet/pdsi.nc")


#Extract lat and lon vectors
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")

#Use the lat and lon vectors to create a grid represented as two vectors (note:
#lon must go first to match with netcdf data)
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat) %>%
  mutate(cell=row_number())

readin.proj=4326 #because it works with the lat and lons provided

#Alt method reading as raster then using exact_extract
gm_rast <- raster("build/inputs/gridmet/pdsi.nc")
crs(gm_rast)<-CRS("+init=epsg:4326")
#mapview::mapview(tmmx)

cell_index <- aiannh %>%
  group_split(geoid) %>%
  map_dfr(~exact_extract(gm_rast,.,include_xy=T,include_cell=T,force_df=T,
                         include_cols=c("geoid")))
  
#check coverage of tribal areas - confirmed that all conus geometries are included
# check_set <- aiannh$geoid[!(unique(aiannh$geoid) %in% unique(cell_index$geoid))]
# aiannh %>%
#   filter(geoid %in% check_set) %>%
#   mapview::mapview()


bridge <- inner_join(cell_index %>% 
             rename(lat=y,lon=x) %>%
             mutate(across(c(lat,lon),~round(.,5))),
           nc.coords %>%
             mutate(across(c(lat,lon),~round(.,5)))) %>%
  select(-value) %>%
  rename(weight=coverage_fraction)




###################################################



  #Construct the dataframe with all days and cells
  var.id=names(nc$var)
  date.vector <- as_date(nc$dim$day$vals,origin="1900-01-01")
  
  nc.data <- ncvar_get(nc = nc, varid = var.id)[,,]
  
  nc.data.df <- array(nc.data,dim=c(prod(dim(nc.data)[1:2]),dim(nc.data)[3])) %>%
    as_tibble(.name_repair = "universal") %>%
    rename_all(~str_c(date.vector))
  
  nc_lat <- ncvar_get(nc = nc, varid = "lat")
  nc_lon <- ncvar_get(nc = nc, varid = "lon")
  nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat) %>% 
    mutate(lon = round(lon,5),
           lat = round(lat,5),
           cell=row_number())
  
  nc.df <- bind_cols(nc.coords,nc.data.df)
  
  #Join gridmet data with bridge to tribe ids
  var.block <- inner_join(bridge,
                          nc.df,
                          by=c("lat","lon","cell")) %>%
    select(-c(lat,lon,cell)) %>%
    as.data.table() %>% 
    melt(., id.vars = c("weight","geoid"),
         variable.name = "date",
         value.name = "value") %>% 
    .[, keyby = .(geoid,date),
      .(mtemp = weighted.mean(value,weight, na.rm = T))] %>%
    .[, keyby = .(geoid,year=year(date)),
      .(pdsi = mean(mtemp, na.rm = T))]
    

  drought_bins=c(-100,-5,-4,-3,-2,-1,100)
  drought_labels=c("Exceptional","Extreme","Severe","Moderate","Abnormally Dry","Not Drought")

  drought_classified <- var.block[,`:=`(pdsi_bins=cut(pdsi,breaks=drought_bins,labels=drought_labels))] %>%
    .[,.N,by=.(geoid,pdsi_bins)] %>%
    pivot_wider(names_from = pdsi_bins,values_from = N,values_fill = 0)
  
############################################################
#Read in all individual years

write_csv(var.block,"output/nlri_drought_annual.csv")

#Join back with tribal data
geo_pdsi <- var.block[, keyby = .(geoid),
          .(pdsi = mean(pdsi, na.rm = T))] %>%
  inner_join(aiannh,.,by="geoid")
  

st_write(geo_pdsi,"output/nlri_drought.shp")
st_write(geo_pdsi,"output/nlri_drought.gpkg")

#Join back with tribal data
geo_pdsi_classified <- inner_join(aiannh,drought_classified,by="geoid")


st_write(geo_pdsi_classified,"output/nlri_drought_classified.shp")
st_write(geo_pdsi_classified,"output/nlri_drought_classified.gpkg")
