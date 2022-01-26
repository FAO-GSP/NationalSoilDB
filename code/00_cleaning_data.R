# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("parzer")

library(tidyverse)
library(readxl)
rm(list = ls())

setwd("C:/Users/angel/git/NationalSoilDB/") # set your working directory


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
names(site)
site <- site %>% 
  select(pid = ProfID, x = X_coord, y = Y_coord, year = Year, kod_t) %>% # select and rename
  filter(!is.na(x)) # we remove profiles without X coordinate
site
summary(site)

# In case that you coordinate system is in degrees, minutes and seconds (42°34’18.7 N) 
# or similar format, parzer package can conver them to decimal degrees
# https://github.com/ropensci/parzer
library(parzer)
messy_coordinates <- 
  c("45N54.2356", "45.98739874", "40°34.75’N", "42°30’ 00.725 N") # your column of latitude
parse_lat(messy_coordinates)


# Check the distribution of the sample time

site %>% 
  select(year) %>% # complete this line
  ggplot(aes(x=year)) + geom_histogram(binwidth = 1)

# remove duplicated
site <- unique(site)


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
names(hor_phys)
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

