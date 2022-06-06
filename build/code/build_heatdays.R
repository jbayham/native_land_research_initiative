#This script generates the index for all gridmet products

library(pacman)
p_load(tidyverse,lubridate,data.table,sf,ncdf4,janitor,raster,exactextractr,furrr,conflicted)

conflict_prefer("select", "dplyr")

if(!dir.exists("build/cache/temperature")) dir.create("build/cache/temperature")

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
nc <- nc_open("build/inputs/gridmet/tmmx/tmmx_1979.nc")


#Extract lat and lon vectors
nc_lat <- ncvar_get(nc = nc, varid = "lat")
nc_lon <- ncvar_get(nc = nc, varid = "lon")

#Use the lat and lon vectors to create a grid represented as two vectors (note:
#lon must go first to match with netcdf data)
nc.coords <- expand.grid(lon=nc_lon,lat=nc_lat) %>%
  mutate(cell=row_number())

readin.proj=4326 #because it works with the lat and lons provided

#Alt method reading as raster then using exact_extract
tmmx <- raster("build/inputs/gridmet/tmmx/tmmx_1979.nc")
crs(tmmx)<-CRS("+init=epsg:4326")
#mapview::mapview(tmmx)

cell_index <- aiannh %>%
  group_split(geoid) %>%
  map_dfr(~exact_extract(tmmx,.,include_xy=T,include_cell=T,force_df=T,
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

file.list <- expand.grid("tmmx",1979:2020,stringsAsFactors = F) %>% 
  rename(var=Var1,year=Var2) %>%
  mutate(var=str_to_lower(var),
         file.name = str_c(var,"_",year,".nc")) %>% 
  arrange(var) 

allheat <- paste0("build/inputs/gridmet/tmmx/",file.list$file.name)
heat1 <- allheat[1]

plan(multisession(workers = 15))

future_map(allheat, 
           function(heat1){
  
  #Construct the dataframe with all days and cells
  nc <- nc_open(heat1)
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
    .[, keyby = .(geoid),
      .(heatdays = sum(mtemp > 310.928, na.rm = T))]   #310.928 ~ 100 F
    
    
  write_rds(var.block,paste0("build/cache/temperature/temp_",str_sub(heat1,-7,-4),".rds"))

},.progress = T)


############################################################
#Read in all individual years

heatdays <- dir("build/cache/temperature",full.names = F) %>%
  map_dfr(function(x){
    out <- read_rds(str_c("build/cache/temperature/",x)) %>%
      mutate(year=parse_number(x))
    })

write_csv(heatdays,"output/nlri_heatdays.csv")


#Join back with tribal data
nlri_heatdays <- inner_join(aiannh,heatdays,by="geoid")

st_write(nlri_heatdays,"output/nlri_heatdays.shp")
st_write(nlri_heatdays,"output/nlri_heatdays.gpkg")
