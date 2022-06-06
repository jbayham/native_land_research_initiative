
library(tidyverse)
library(geosphere)

# Calculate Centroids -----------------------------------------------------

# Load data
load("03_analysis/dyad_data.rda")

### Calculate centroids for T1
# calculate centroids
t1_centroids <- t1_shp %>%
  st_centroid() %>%
  dplyr::select(tribe, time, geometry) %>%
  rename(tribe_t1 = tribe) #%>% as.data.frame()

# map centroids to visualize at T1
ggplot() +
  geom_sf(data = t1_centroids, fill="gray", alpha=.4, color="black")

### Calculate centroids for T2
# recast multipolygons as polygons
tribe_shapefiles3 <- tribe_shapefiles2 %>%
  st_cast('POLYGON')

# calculate centroids
t2_centroids <- tribe_shapefiles3 %>%
  st_centroid() %>%
  dplyr::select(tribe, time, geometry) %>%
  rename(tribe_t2 = tribe) #%>% as.data.frame()

# map centroids to visualize at T2
ggplot() +
  geom_sf(data = t2_centroids, fill="gray", alpha=.4, color="black")

# Calculate all pairwise combinations of points ---------------------------

tribe_t1_t2 <- dat_tribe %>%
  group_by(tribe, time) %>%
  summarise(observations = n()) %>%
  as.data.frame() %>%
  pivot_wider(names_from = time, values_from = observations) %>%
  rename(time1 = "time 1",
         time2 = "time 2")

# Filter only tribes with both T1 and T2 data
tribes_both_T <- filter(tribe_t1_t2, !is.na(time2) & !is.na(time1))$tribe

# Loop
all_tribe_dyads <- list() # create empty list to fill

for (i in seq_along(tribes_both_T)) { 
  
  # Filter centroids for single tribe
  tribe_t1 <- filter(t1_centroids, tribe_t1 == paste(tribes_both_T[i]))
  tribe_t2 <- filter(t2_centroids, tribe_t2 == paste(tribes_both_T[i]))
  
  # Calculate all possible dyads between T1 and T2
  tribe_pairwise <- expand.grid(tribe_t1$geometry, tribe_t2$geometry) %>% 
    as.data.frame() %>%
    tidyr::extract(Var1, into = c("Lon1", "Lat1"), "\\((.*),(.*)\\)", conv = T) %>%
    tidyr::extract(Var2, into = c('Lon2', 'Lat2'), '\\((.*),(.*)\\)', conv = T) %>%
    unique() %>%
    mutate(pair_dist_km = distHaversine(cbind(Lon1, Lat1), cbind(Lon2, Lat2)) / 1000) %>% # in kilometers
    mutate(tribe = paste(tribes_both_T[i]))
  
  # Save dataframe of all dyads for each tribe
  all_tribe_dyads[[i]] <- tribe_pairwise 
  
}

# Convert list loop output to dataframe
all_tribe_dyads_df <- do.call(rbind.data.frame, all_tribe_dyads)
nrow(all_tribe_dyads_df)

# Calculate summary statistics of dyads for each tribe
tribe_dyad_summary <- all_tribe_dyads_df %>%
  group_by(tribe) %>%
  summarise(mean_dist_km = mean(pair_dist_km),
            median_dist_km = median(pair_dist_km),
            min_dist_km = min(pair_dist_km),
            max_dist_km = max(pair_dist_km),
            quantile_25 = quantile(pair_dist_km, c(0.25)),
            quantile_75 = quantile(pair_dist_km, c(0.75)))

# Calculate summary statistics across all tribes
mean(tribe_dyad_summary$mean_dist_km) # average distance across all tribes
median(tribe_dyad_summary$median_dist_km) # median distance across all tribes
min(tribe_dyad_summary$min_dist_km) # minumum distance
max(tribe_dyad_summary$max_dist_km) # maximum distance
