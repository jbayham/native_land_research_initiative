
library(tidyverse)
library(sf)
library(tigris)
library(ggbeeswarm)


# Time 1 Data -------------------------------------------------------------

# tribal lands data
dat_tribe <- read_csv("01_data/tribe_bg_full_integrated_combined_FINAL.csv") %>%
  mutate(fips2 = str_pad(fips, 5, pad = "0", side = "left") %>% as.character()) %>%
  filter(rt == "air_federal" | rt == "air_state" | rt == "otsa" | is.na(rt)) 

# count total number of unique tribes
unique(dat_tribe$tribe) %>% length()

# import USA counties
counties <- counties(cb = TRUE, class = "sf") %>%
  filter(STATEFP != "02" & # remove units which are not lower 48 states
           STATEFP != "60" &
           STATEFP != "66" &
           STATEFP != "69" &
           STATEFP != "72" &
           STATEFP != "78" &
           STATEFP != "15") 

# calculate area in m^2 of each county
counties$area_m2 <- st_area(counties)

unique(counties$STATEFP) %>% length() # should be 49 total (48 states plus Washington DC)

# Filter only T1 tribes list
dat_tribe_t1<- dat_tribe %>%
  filter(time == "time 1")

# total count of tribes at T1
unique(dat_tribe_t1$tribe) %>% length()

# Filter only counties (from shapefile) that had a tribe presence at T1
t1_shp <- right_join(counties, dat_tribe_t1, by = c("GEOID" = "fips2")) %>%
  mutate(dissolve = 1)
t1_shp_df <- as.data.frame(t1_shp)

# check number of unique fips in non-spatial and spatial data
unique(dat_tribe_t1$fips2) %>% length()
unique(t1_shp$GEOID) %>% length()

# Time 2 Data -------------------------------------------------------------

# This has to be done with a separate shapefile because different spatial units (CBG's)
# shapefile of T2 lands
tribe_shapefiles <- read_rds("01_data/tribe_shapefiles.rds")

# filter only T2 data
dat_tribe_t2 <- dat_tribe %>% 
  filter(time == "time 2") %>%
  mutate(geoid10 = as.character(geoid10)) %>%
  separate(col = census_tribe, into = c("UID", "census_tribe", remove = FALSE), sep = "_") %>%
  mutate(UID = str_extract(UID, "[[:digit:]]+")) # create identifier variable for join to shapefile

# pull out only unique UID's 
tribe_uid <- dplyr::select(dat_tribe_t2, tribe, UID, time, rt) %>% unique() # number of unique UID's in non-spatial data

# count of unique tribes with T2 data (223, includes 3 tribes which have no T1 data)
unique(tribe_uid$tribe) %>% length()

# clean tribe_shapefiles 
tribe_shapefiles2 <- tribe_shapefiles %>%
  filter(AIANNHCE != "0110") %>% # remove Alaska-based tribe (not included in this analysis)
  left_join(tribe_uid, by = c("UID" = "UID")) # join with uid data for categories

# convert to dataframe
tribe_shapefiles2_df <- as.data.frame(tribe_shapefiles2) %>% dplyr::select(-(geometry))

# NOTE: t1_shp and tribe_shapefiles2 are saved to dyad_data.rda for dyad analysis

# Save cleaned T2 shapefiles for OSF repository
tribe_shapefiles_OSF <- tribe_shapefiles2 %>% filter(!is.na(tribe))
st_write(tribe_shapefiles_OSF, "tribe_t2_shapefiles.gpkg")

# Calculate land area per tribe -------------------------------------------

t1_area <- t1_shp_df %>%
  group_by(tribe) %>%
  summarise(t1_area = sum(area_m2)) %>% # area is in m^2
  mutate(t1_area = as.vector(t1_area))

t2_area <- tribe_shapefiles2_df %>%
  group_by(tribe) %>%
  summarise(t2_area = sum(area)) %>% # sum all individual areas for each spatial unit for a given tribe
  mutate(t2_area = as.vector(t2_area)) # area is in m^2

# examine tribes which aren't included in our current dataset - these are not currently included in the final calculations
# as the t1_t2_area calculation below drops tribes missing both T1 and T2 area calculations
na_tribe <- filter(tribe_shapefiles2, is.na(tribe)) 

# Create joined dataframe that calculates change in land area for each tribe
t1_t2_area <- full_join(t1_area, t2_area) %>%
  mutate(land_change_m2 = (t2_area - t1_area), 
         land_change_km2 = land_change_m2*0.000001, # convert from m^2 to square km
         land_change_prop = round((t2_area / t1_area), digits = 3)) %>% # calculate contemporary lands in proportion to historic
  mutate(larger_t2 = ifelse(t2_area > t1_area, 1, 0)) %>% # check if any tribes had a larger land base at T2
  filter(!is.na(tribe)) # exclude area from tribes which are not in our dataset

# Total tribes in the dataset
unique(t1_t2_area$tribe) %>% length()
  
mean(t1_t2_area$land_change_prop, na.rm = TRUE) # average T2 proportion in relation to T1 lands
median(t1_t2_area$land_change_prop, na.rm = TRUE) # median T2 proportion in relation to T1 lands

mean(t1_t2_area$land_change_km2, na.rm = TRUE) # mean reduction of land in square km
median(t1_t2_area$land_change_km2, na.rm = TRUE) # median reduction of land in square km

filter(t1_t2_area, !is.na(t1_area) & is.na(t2_area)) %>% nrow() # count of tribes which had no landholdings at T2
filter(t1_t2_area, !is.na(t1_area) & is.na(t2_area)) %>% nrow() / filter(t1_t2_area, !is.na(t1_area)) %>% nrow() # proportion of tribes which had no landholdings at T2
filter(t1_t2_area, !is.na(t2_area)) %>% nrow() / unique(t1_t2_area$tribe) %>% length() #proportion of all tribes with T2 landholdings

filter(t1_t2_area, !is.na(t2_area)) %>% nrow() # count of tribes in the present day (regardless of prior landholdings)

total_t1 <- sum(t1_t2_area$t1_area, na.rm = TRUE)*0.000001 # total area across tribes at T1 (km) (co-extensive)
total_t2 <-sum(t1_t2_area$t2_area, na.rm = TRUE)*0.000001 # total area across tribes at T2 (km) (co-extensive)
1 - (total_t2/total_t1) # percent reduction of co-extensive lands

mean_t1 <- mean(t1_t2_area$t1_area, na.rm = TRUE)*0.000001 # mean area across tribes at T1 (km)
mean_t2 <- mean(t1_t2_area$t2_area, na.rm = TRUE)*0.000001 # mean area across tribes at T2 (km)

med_t1 <- median(t1_t2_area$t1_area, na.rm = TRUE)*0.000001 # median area across tribes at T1 (km)
med_t2 <- median(t1_t2_area$t2_area, na.rm = TRUE)*0.000001 # median area across tribes at T2 (km)

min_t1 <- min(t1_t2_area$t1_area, na.rm = TRUE)*0.000001 # min area per tribe across tribes at T1 (km)
min_t2 <- min(t1_t2_area$t2_area, na.rm = TRUE)*0.000001 # min area per tribe across tribes at T2 (km)

max_t1 <- max(t1_t2_area$t1_area, na.rm = TRUE)*0.000001 # max area across tribes at T1 (km)
max_t2 <- max(t1_t2_area$t2_area, na.rm = TRUE)*0.000001 # max area across tribes at T2 (km)

q_25_t1 <- quantile(t1_t2_area$t1_area, c(.25), na.rm = TRUE)*0.000001 # 25 quantile at T1 (km)
q_25_t2 <-quantile(t1_t2_area$t2_area, c(.25), na.rm = TRUE)*0.000001 # 25 quantile at T2 (km)

q_75_t1 <- quantile(t1_t2_area$t1_area, c(.75), na.rm = TRUE)*0.000001 # 75 quantile at T1 (km)
q_75_t2 <-quantile(t1_t2_area$t2_area, c(.75), na.rm = TRUE)*0.000001 # 75 quantile at T2 (km)


# Proportion of tribes with no recognized land base in T2 -----------------
count_t1 <- filter(t1_t2_area, !is.na(t1_area)) %>% dplyr::select(tribe) %>% unique  # count of tribes with documented land base at T1
count_t2 <- filter(t1_t2_area, !is.na(t1_area) & !is.na(t2_area)) # count of tribes with documented land base at T1 and T2
1 - (nrow(count_t2) / nrow(count_t1))

# Land Area Summary Statistics Table --------------------------------------

land_sum <- tibble(
  " " = c("Cumulative Sum", 
               "Minimum",
               "25th Percentile",
               "Mean",
               "Median",
               "75th Percentile",
               "Maximum"),
  "Historical (sq. km)" = c(total_t1, min_t1, 
                                       q_25_t1, mean_t1, 
                                       med_t1, q_75_t1, max_t1),
  "Present Day (sq. km)" = c(total_t2, min_t2, 
                                       q_25_t2, mean_t2, 
                                       med_t2, q_75_t2, max_t2)
)

land_area_kable <- kable(land_sum, format = "latex", booktabs = TRUE, digits = 0, align="lcc", linesep = "",
                        caption = "Change in Land Area per Tribe",
                        format.args = list(big.mark = ",")) %>%
  kable_styling(font_size = 9.5, latex_options = c('hold_position')) %>%
  footnote(general = c("Land area is calculated by tribe and in some cases includes coextensive", 
                       "areas where more than one tribe share a location.The minimum land", 
                       "area in the present day is rounded from .02.")) 

print(land_area_kable)

# Calculate proportion of total N Am area ---------------------------------

# calculate total land area of lower 48 states in North America
counties_dissolve <- counties %>% 
  summarize(land_area_m2 = sum(area_m2)) 

# calculate total land area of tribal land base at T1 (non-coextensive)
t1_area_dissolve <- t1_shp %>%
  as.data.frame() %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  summarise(t1_area_m2 = sum(area_m2))

# calculate total land area of tribal land base at T2 (non-coextensive)
t2_area_dissolve <- tribe_shapefiles2 %>%
  filter(!is.na(tribe)) %>% # exclude spatial units of tribes not included in our analysis
  distinct(GEOID, .keep_all = TRUE) %>%
  summarise(t2_area_m2 = sum(area))

# Proportion of land at T1 (of all lower 48) (non-coextensive)
t1_area_dissolve$t1_area_m2 / counties_dissolve$land_area_m2

# Total square km at T1 (unique, no overlapping lands) (non-coextensive)
t1_area_dissolve$t1_area_m2*0.000001 

# Proportion of land at T2 (of all lower 48)
t2_area_dissolve$t2_area_m2 / counties_dissolve$land_area_m2

# Total square km at T2 (unique, no overlapping lands) (non-coextensive)
t2_area_dissolve$t2_area_m2*0.000001

# Aggregate land reduction (unique, no overlapping lands)
(t1_area_dissolve$t1_area_m2 - t2_area_dissolve$t2_area_m2)*0.000001


# Test significant difference in land between T1 and T2 -------------------

# data subset 1: all tribes, lands value set to zero if missing at either T1 or T2
t1_t2_area_long <- t1_t2_area %>% 
  pivot_longer(cols = c("t1_area", "t2_area"), 
               names_to = "time", values_to = "land") 

# data subset 2: tribes with only T1 and T2 data
t1_t2_both_long <- t1_t2_area %>%
  filter(!is.na(t1_area) & !is.na(t2_area)) %>% 
  pivot_longer(cols = c("t1_area", "t2_area"), 
               names_to = "time", values_to = "land") 

# data subset 3: land values are zero for tribes that don't hand landholdings in the present-day
t1_t2_zero_t2 <- t1_t2_area %>%
  mutate(t2_area = if_else(is.na(t2_area), 0, t2_area)) %>%
  pivot_longer(cols = c("t1_area", "t2_area"), 
               names_to = "time", values_to = "land")

# data subset 4: land values are zero for tribes that did not have documented presence at T1
t1_t2_zero_t1 <- t1_t2_area %>%
  mutate(t1_area = if_else(is.na(t1_area), 0, t1_area)) %>%
  pivot_longer(cols = c("t1_area", "t2_area"), 
               names_to = "time", values_to = "land")

# OLS 1: all tribes (lands are 0 if missing at either T1 or T2)
f1 <- fixest::feols(land*0.000001 ~ time, data = t1_t2_area_long)
summary(f1, cluster = "tribe")

# OLS 2: tribes that exist in both historical and present-day
f2 <- fixest::feols(land*0.000001 ~ time, data = t1_t2_both_long)
summary(f2, cluster = "tribe")

# OLS 3: tribes that exist in historical period (setting values to zero for tribes that do not appear in present-day)
f3 <- fixest::feols(land*0.000001 ~ time, data = t1_t2_zero_t2)
summary(f3, cluster = "tribe")

# OLS 4: tribes that exist in the present-day (setting values to zero for tribes that do not appear in historical)
f4 <- fixest::feols(land*0.000001 ~ time, data = t1_t2_zero_t1 )
summary(f4, cluster = "tribe")
  
  
# Plots -------------------------------------------------------------------
# Prep Data -------------------
t1_t2_area$t1_area_km <- t1_t2_area$t1_area * 0.000001 #create new variable converting T1 area to KM
t1_t2_area$t2_area_km <- t1_t2_area$t2_area * 0.000001 #create new variable converting T2 area to KM
t1_t2_area_long <- pivot_longer(t1_t2_area, c(t1_area_km, t2_area_km), names_to = "time") # reshape to LONG
t1_t2_area_long$area_km <- t1_t2_area_long$value #rename main variable after reshape
t1_t2_area_long <- select(t1_t2_area_long, tribe, time, area_km) #select only columns I need
t1_t2_area_long <- mutate_at(t1_t2_area_long, c("area_km"), ~replace(., is.na(.), 0)) #impute 0s for NAs in area_km

# MAIN LOGGED PLOT (fig 1b) --------------

#function to drop trailing zeros from the tick marks
plain <- function(x,...) {
  format(x, ..., scientific = FALSE, trim = TRUE, drop0trailing=TRUE)
}

# PLOT
fig1_plot <- ggplot(subset(t1_t2_area_long), #omit outliers
                    aes(x=area_km, y=reorder(time, desc(time)))) +
  geom_quasirandom(groupOnX = F, varwidth=T, width=.35, size=2.5, color="#ffff00", alpha=.4) +
  scale_x_log10(breaks=c(.01, .1, 1, 10, 100, 1000, 10000, 100000, 1000000, 10000000), labels=plain) +
  geom_boxplot(color="lightgrey", alpha=0) +
  xlab("") +
  ylab("") +
  theme_dark() +
  theme(axis.title.x = element_text(family="Helvetica", margin = margin(t = 13, r = 0, b = 0, l = 0), size=13))
fig1_plot

#ggsave(fig1_plot, file="fig1.pdf",  width=8.32, height=9)
