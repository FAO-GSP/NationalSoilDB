library(aqp)
library(tidyverse)
library(readxl)
rm(list = ls())

# Pipe symbol %>% (in tidyverse package) --------------------------------------
# We use the pipe symbol %>%  to concatenate actions (functions)
# For instance, if apply 1 + 1 %>% + 3 the result is 5
1 + 1 %>% 
  + 3
# which means add 3 to the result of 1 + 1. 
# The pipe symbol can be written with the keyboard shortcut ctrl + shift + m

# Load data from an MS Excel file ----------------------------------------------
# site data
site <- read_excel("data_input/Profiles_data.xlsx", sheet = "Soil_profile_loc")
# horizon data
hor_des <- read_excel("data_input/Profiles_data.xlsx", sheet = "Hor_descr")
hor_chem <- read_excel("data_input/Profiles_data.xlsx", sheet = "Hor_chem")
hor_phys <- read_excel("data_input/Profiles_data.xlsx", sheet = "Hor_phys")
hor_mech <- read_excel("data_input/Profiles_data.xlsx", sheet = "Hor_mech")
# metadata (not available)

# Clean data and select useful variables =======================================
# Site -------------------------------------------------------------------------
site %>% View()
# Replace all "/" with NA
site <- site %>% 
  mutate_all(funs(na_if(., "/")))
#  How many NAs per column there are?
is.na(site) %>% colSums()
# Select (and rename) useful columns and filter out usefulness rows
site <- site %>% 
  select(pid = ProfID, x = X_coord, y = Y_coord, year = Year, kod_t) %>% # select and rename
  filter(!is.na(x)) # we remove profiles without X coordinate
site
summary(site)

# Q1. complete the following code to select only the column year

site %>% 
  select() %>% # complete this line
  ggplot(aes(year)) + geom_histogram()

# remove duplicated
site <- unique(site)


#
# Horizon description ----------------------------------------------------------
hor_des %>% View()
# Remove first row 
(hor_des <- hor_des[-1,])
# Replace all "/" with NA
hor_des <- hor_des %>% 
  mutate_all(funs(na_if(., "/")))
# How many NAs per column there are?
is.na(hor_des) %>% colSums()

# Select (and rename) useful columns and filter out usefulness rows
hor_des %>% filter(is.na(ProfID2))# check rows with missing values

hor_des <- hor_des %>% 
  select(pid = ProfID2, hid = HorID, hor_no = HorNO, top = DepthFrom, 
         bottom = DepthTo, hor_code = Code, hor_mk = Hor_MK, mak = MAKtext) %>% # select and rename
  filter(!is.na(pid)) # we remove profiles without X coordinate

hor_des
is.na(hor_des) %>% colSums()
# Change column type
hor_des <- hor_des %>% 
  mutate(hor_no = as.numeric(hor_no),
         top = as.numeric(top),
         bottom = as.numeric(bottom))

# Q2. Select only top horizons
hor_des %>% 
  filter()

# Horizon chemical analysis ----------------------------------------------------------
hor_chem

# Remove first row 
(hor_chem <- hor_chem[-1,])

# Replace all "/" with NA
hor_chem <- hor_chem %>% 
  mutate_all(funs(na_if(., "/")))%>% 
  mutate_all(funs(na_if(., "-")))
# How many NAs per column there are?
is.na(hor_chem) %>% colSums()

# Select (and rename) useful columns and filter out usefulness rows
hor_chem <- hor_chem %>% 
  select(hid = HorID, caco3 = CaCO3, humus = Humus, total_N = Total_N,
         ph_h2o = pH_H2O, ph_kcl = pH_nKCl, ap = Easily_available_P2O5,
         ak = Easily_available_K2O, Y1, sum_bases = S, cec = `T`, base_sat = `V %`) # select and rename
hor_chem

# Change column type
hor_chem %>% 
  mutate_at(.vars = 2:12,.funs = as.numeric) # NAs introduced by coercion 
# this means that the columns contain non numeric values
# check what the problem can be
# check content of humus
levels(as.factor(hor_chem$humus))
# comma instead of dot
hor_chem$humus <- str_replace(hor_chem$humus, pattern = ",", replacement = ".") 

# After solving the problems, we make the changes
hor_chem <- hor_chem %>% 
  mutate_at(.vars = 2:12,.funs = as.numeric) # warnings are not errors

hor_chem

# Horizon physical analysis ----------------------------------------------------------
hor_phys

# Remove first row 
(hor_phys <- hor_phys[-1,])

# Replace all "/" with NA
hor_phys <- hor_phys %>% 
  mutate_all(funs(na_if(., "/")))%>% 
  mutate_all(funs(na_if(., "-")))
# How many NAs per column there are?
is.na(hor_phys) %>% colSums()

# Select (and rename) useful columns and filter out usefulness rows
hor_phys <- hor_phys %>% 
  select(hid = HorID, bd = Bulk_density, Real_density, h_water = Higroscopic_water, 
         porosity = Porosity, wp = Wilting_point, fc = Field_capacity,
         ret_cap = Retention_capacity_H2O, air_cap = Air_capacity) # select and rename
hor_phys

# Change column type
hor_phys <- hor_phys %>% 
  mutate_at(.vars = 2:9,.funs = as.numeric) #

hor_phys

# Horizon mechanical analysis ----------------------------------------------------------
hor_mech

# Remove first row 
(hor_mech <- hor_mech[-1,])

# Replace all "/" with NA
hor_mech <- hor_mech %>% 
  mutate_all(funs(na_if(., "/")))%>% 
  mutate_all(funs(na_if(., "-")))
# How many NAs per column there are?
is.na(hor_mech) %>% colSums()

# Select (and rename) useful columns and filter out usefulness rows
hor_mech <- hor_mech %>% 
  select(hid = HorID, coarse_frag = Skeleton, clay = Clay, silt = Silt,
         Fine_sand, Coarse_sand, total = Total) # select and rename
hor_mech

# Change column type
hor_mech %>% 
  mutate_at(.vars = 2:7,.funs = as.numeric) #

levels(as.factor(hor_mech$clay))

# google it:
# https://www.google.com/search?q=select+stringr+r+not+number&oq=select+stringr+r+not+number&aqs=chrome..69i57j33i160.16960j0j7&sourceid=chrome&ie=UTF-8
# https://stackoverflow.com/questions/43195519/check-if-string-contains-only-numbers-or-only-characters-r
str_detect(hor_mech$clay, "^[:alpha:]+$")
hor_mech$clay[which(str_detect(hor_mech$clay, "^[:alpha:]+$"))]

# we convert it to numeric
hor_mech <- hor_mech %>% 
  mutate_at(.vars = 2:7,.funs = as.numeric) #

# sum sand fractions
hor_mech <- hor_mech %>% 
  mutate(sand = Fine_sand + Coarse_sand)

# Merge all horizon tables -----------------------------------------------------
# 
hor_des <- hor_des %>% unique()
hor_chem <- hor_chem %>% unique()
hor_phys <- hor_phys %>% unique()
hor_mech <- hor_mech %>% unique()

hor <- hor_des %>% 
  left_join(hor_chem) %>% 
  left_join(hor_phys) %>% 
  left_join(hor_mech) 

# Save tables ==================================================================

write_csv(site, "data_output/site.csv")
write_csv(hor, "data_output/horizon.csv")

################################################################################
## QUALITY CHECKS ##############################################################
################################################################################
library(aqp)
library(sf)
library(sp)
library(mapview)

# Load site and horizon data ---------------------------------------------------
site <- read_csv("data_output/site.csv")
hor <-  read_csv("data_output/horizon.csv")

# Check locations ----------------------------------------------------- 
# https://epsg.io/6204
site %>% 
  st_as_sf(coords = c("x", "y"), crs = 6204) %>% # convert to spatial object
  mapview() # visualise in an interactive map

# repeated locations
x <- site %>% 
  st_as_sf(coords = c("x", "y"), crs = 6204)  
# evaluate which sites are at zero distance
sp::zerodist(as_Spatial(x))

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
write_csv(clean_prof@horizons, "clean_horizons.csv")
write_csv(clean_prof@site, "clean_site.csv")

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
