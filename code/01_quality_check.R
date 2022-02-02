################################################################################
## QUALITY CHECKS ##############################################################
################################################################################

# install.packages("aqp")
# install.packages("sf")
# install.packages("mapview")

library(tidyverse)
library(aqp)
library(sf)
library(mapview)

rm(list = ls())
setwd("C:/Users/angel/git/NationalSoilDB/") # set your working directory




# Load site and horizon data ---------------------------------------------------
site <- read_csv("data_output/site.csv")
hor <-  read_csv("data_output/horizon.csv")

# Check locations ----------------------------------------------------- 
# https://epsg.io/6204
site %>% 
  st_as_sf(coords = c("x", "y"), crs = 6204) %>% # convert to spatial object
  mapview(zcol = "year", cex = 2, lwd = 0) # visualise in an interactive map
# errors related to yaml package
# https://community.rstudio.com/t/error-when-starting-rstudio-there-is-no-package-yaml/4070
 

# repeated locations
x <- site %>% 
  st_as_sf(coords = c("x", "y"), crs = 6204)  
# evaluate which sites are at zero distance
sp::zerodist(as_Spatial(x))
x[c(2530,3096),]

# identify the profiles
x <- x[sp::zerodist(as_Spatial(x)) %>% as.vector(),] %>% 
  st_drop_geometry()

# List of pid
x$pid[x$pid %>% order()]
unique(x$pid)

# Convert data into a Soil Profile Collection ----------------------------------
depths(hor) <- pid ~ top + bottom
site(hor) <- left_join(site(hor), site)
profiles <- hor

profiles

# aqp::coordinates(x) <- ~x+y
# aqp::proj4string(x) <- "+proj=tmerc +lat_0=0 +lon_0=21 +k=0.9999 +x_0=500000 +y_0=0 +ellps=bessel +towgs84=682,-203,480,0,0,0,0 +units=m +no_defs "

# plot first 20 profiles using pH as color
plotSPC(x = profiles[1:20], name = "hor_mk", color = "ph_h2o")

# check data integrity
# A valid profile is TRUE if all of the following criteria are false:
#    + depthLogic : boolean, errors related to depth logic
#    + sameDepth : boolean, errors related to same top/bottom depths
#    + missingDepth : boolean, NA in top / bottom depths
#    + overlapOrGap : boolean, gaps or overlap in adjacent horizons
aqp::checkHzDepthLogic(profiles)
# get only non valid profiles
aqp::checkHzDepthLogic(profiles ) %>% 
  filter(valid == FALSE) 
# visualize some of these profiles by the pid
subset(profiles, grepl("P0142", pid, ignore.case = TRUE))
subset(profiles, grepl("P0494", pid, ignore.case = TRUE))
subset(profiles, grepl("P3847", pid, ignore.case = TRUE))


# keep only valid profiles -----------------------------------------------------
clean_prof <- HzDepthLogicSubset(profiles)
metadata(clean_prof)$removed.profiles

# Save clean data --------------------------------------------------------------
# first, we save the soilProfileCollection object
saveRDS(clean_prof, file = "data_output/profiles.RData")
# now, split the SPC to have horizon data and site data 
write_csv(clean_prof@horizons, "data_output/clean_horizons.csv")
write_csv(clean_prof@site, "data_output/clean_site.csv")

# graphical inspections & descriptive statistics ===============================
s <- aqp::slab(clean_prof, 
               fm = ~ clay + sand + ph_h2o + humus,
               slab.structure = 0:100,
               slab.fun = function(x) quantile(x, c(0.01, 0.5, 0.99), na.rm = TRUE))

ggplot(s, aes(x = top, y = X50.)) +
  # plot median
  geom_line() +
  # plot 10th & 90th quantiles
  geom_ribbon(aes(ymin = X1., ymax = X99., x = top), alpha = 0.2) +
  # invert depths
  xlim(c(100, 0)) +
  # flip axis
  coord_flip() +
  facet_wrap(~ variable, scales = "free_x")

# Names
names(clean_prof@horizons)

# function to detect outliers
is_outlier <- function(x) {
  return(x < quantile(x, probs = 0.05, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE) |
           x > quantile(x, probs = 0.95, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))
}

# identify outliers for all soil properties
clean_prof@horizons %>%
  mutate_at(.vars = 9:34,.funs = function(y) ifelse(is_outlier(y), y, as.numeric(NA))) 

# create a table to review suspicious values (pay attention to the steps)

# 1. select horizon table from clean_prof
suspicious <- clean_prof@horizons %>% 
  # 2. keep only outliers for each soil property (columns 9 to 34)
  mutate_at(.vars = 9:34,.funs = function(y) ifelse(is_outlier(y), y, as.numeric(NA))) %>% 
  # 3. define key columns (profil id and horizon id)
  group_by(pid, hid) %>% 
  # 4. select soil properties (from:to)
  select(caco3:sand) %>% 
  # 5. pivot table from wide to long
  pivot_longer(cols = caco3:sand) %>% 
  # 6. remove missing values (NAs)
  na.omit()

suspicious

write_csv(suspicious, "data_output/suspicious.csv")
