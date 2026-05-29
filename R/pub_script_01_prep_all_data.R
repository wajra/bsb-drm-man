# Script - pub_script_01_prep_all_data.R
# Task - Process NEUS Bottom Trawl Spring Survey and stock assessment data
# Author - Jeewantha Bandara (jeewantha.bandara@rutgers.edu)

# In this script, we will get data from Ocean Adapt (Pinsky et al. 2013)
# filter them to the Northeast US Spring Survey and Black Sea Bass
# and then create a table with numbers at length data for each length 
# (zero filled data) and haul that happened from 1989 to 2019.
# We will also summarize Weight-at-age data from the Black Sea Bass stock 
# assessment survey.
# Will also summarize Fishing pressure data for each year from the Black Sea 
# Bass stock asssessment survey.
# We will also download seasonal and annual bottom temperature data (GLORYS) 
# for the Northeast US via ecodata R package and format them for analysis.
# There is a separate section at the end of the file that writes all the data
# and figures to disk if the users chooses to do so.

# load packages
library(tidyverse)
library(here)
library(stringr)
library(lubridate)
library(ggthemes)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(units)
library(cowplot)
here <- here::here

#### SETUP THE SPATIAL DATA ####

# Get the rnaturalearth data for making the map. CRS is 4326 (Units in m)
world <- ne_countries(scale = "medium", returnclass = "sf")

# Hudson canyon
# The line itself are manual coordinates extracted from 
# https://marineregions.org/gazetteer.php?p=details&id=24909
canyon_line_df <- data.frame(lon=c(-73.85,-72.10),lat=c(40.48, 39.45))
# Convert to a spatial object
points_for_linestring <- data.matrix(canyon_line_df)
canyon_line_test <- sf::st_linestring(points_for_linestring, "XY")
canyon_line_obj <- sf::st_sfc(canyon_line_test, crs = 4326)

# Northeast US trawl survey spatial area data
# Data from Maureaud et al. (2020)
trawl_survey_areas <- sf::st_read(dsn=here("data",
                                           "TrawlSurveyMetadata-master",
                                           "data","metadata",
                                           "Metadata_18062020.shp"))

neus_area <- trawl_survey_areas %>% filter(Survey=="Northeast US")

# Split neus_area to north and south subunit using the Hudson canyon
neus_split <- neus_area %>%
  lwgeom::st_split(canyon_line_obj) %>%
  st_collection_extract("POLYGON") 

# Name the northern subunit and southern subunit
neus_north <- neus_split[2,]
neus_north$subunit <- "North"

neus_south <- neus_split[1,]
neus_south$subunit <- "South"

# Getting some basic statistics from both areas (km2. Calculated in m2)
print(sf::st_area(neus_north))
print(units::set_units(sf::st_area(neus_north),km^2))
neus_north_area <- round(units::drop_units(units::set_units(sf::st_area(neus_north),km^2)),2)

print(sf::st_area(neus_south))
print(units::set_units(sf::st_area(neus_south),km^2))
# Round the area to the nearest second decimal
neus_south_area <- round(units::drop_units(units::set_units(sf::st_area(neus_south),km^2)),2)

# The northern subunit is significant larger in area than the southern subunit
# 177773.4 km2 (North) vs. 59576.75 km2 (South)

# Make this into a dataframe to later save to the disk
subunit_info <- data.frame(subunit=c(1,2),
                           patch=c(1,2),
                           subunit_name = c("North", "South"),
                           area=c(neus_north_area, neus_south_area),
                           area_units = c("km^2"))

# Northeast US trawl survey raw data
# Data from OceanAdapt (2020 version) - Morley et al. (2018)
# https://zenodo.org/badge/latestdoi/29789533

# Get zero-inflated survey data
# Reference - https://stackoverflow.com/a/21505731 - For loading .rds files
dat_exploded <- readRDS(here("data","OceanAdapt-update2020","data_clean","dat_exploded.rds"))

# Get length data associated with hauls
load(here("data","OceanAdapt-update2020","data_raw","neus_Survdat.Rdata"))

# Get taxonomic data - To assign to hauls and filter species
load(here("data","OceanAdapt-update2020","data_raw", "neus_SVSPP.RData"))

# We will be selecting hauls only from the Spring survey following
# Miller et al. (2016) and the Black Sea Bass track assessment (2021)
spp_of_interest <- c("Centropristis striata")
reg_of_interest <- c("Northeast US Spring")

# First we filter for the Northeast US Spring
dat_exploded_neus <- dat_exploded %>% 
  filter(region==reg_of_interest) 

# Set the minimum and maximum years
# 2017 is missing, for some reason 
# early years have some issues; the newly filtered OA data starts here anyway
max_yr <- 2020
min_yr <- 1989 
# to explore more the changes in samplng over time, make annual maps 
# of haul locations, or a tile plot of year vs. lat band 
cutoff <- 1
forecast_yr_1 <- max_yr - cutoff

# Create a new table with haul data so that we can get bottom temperature and date data
# What we do here
# 1. Introduce a haulid where, haulid = Cruise + Station + Stratum
# 2. Select distinct/unique combinations of the rows and rename them to be more readable
hauldat <- survdat %>% 
  # create a haulid for joining
  mutate(haulid = paste(formatC(CRUISE6, width=6, flag=0),
                        formatC(STATION, width=3, flag=0),
                        formatC(STRATUM, width=4, flag=0), sep='-')) %>% 
  select(haulid, BOTTEMP, YEAR, EST_TOWDATE, LAT, LON) %>% 
  distinct() %>%
  rename("btemp"=BOTTEMP,
         "year"=YEAR,
         "date"=EST_TOWDATE,
         "lat"=LAT,
         "lon"=LON)

#### LENGTH DATA PROCESSING ####


# What we do here
# 1. Introduce a haulid where, haulid = Cruise + Station + Stratum
# 2. Join with taxonomic data to get assign species names
# 3. Filter for only the relevant species (In our case, black sea bass)
len_bsb_prep <- survdat %>% 
  # create a haulid for joining with dat.exploded
  mutate(haulid = paste(formatC(CRUISE6, width=6, flag=0),
                        formatC(STATION, width=3, flag=0),
                        formatC(STRATUM, width=4, flag=0), sep='-')) %>% 
  select(haulid, SVSPP, LENGTH, NUMLEN) %>% 
  left_join(spp, by="SVSPP") %>% # get species names from species codes
  mutate(spp = str_to_sentence(SCINAME)) %>% # change to format of sppOfInt
  select(spp, haulid, LENGTH, NUMLEN) %>%
  filter(!is.na(LENGTH),
         spp == spp_of_interest
         # LENGTH>X - Include this if any length based filtering is required
  ) %>% 
  rename("length"=LENGTH,
         "number_at_length"=NUMLEN)

# Then we need to expand this table into a table that also carries lengths
# for which the number of individuals were zero
# Then we join that with haul information (hauldat) using 'haulid'
# important to use haulids from dat_exploded_neus because it has been cleaned
len_bsb <- expand.grid(haulid=unique(dat_exploded_neus$haulid),
                       length=seq(min(len_bsb_prep$length),
                                  max(len_bsb_prep$length), 1)) %>% # get full factorial of every haul * length bin
  mutate(spp = spp_of_interest) %>% 
  # left_joining to use only the hauls in dat_exploded_neus
  left_join(len_bsb_prep, by=c('length','haulid','spp')) %>% 
  mutate(number_at_length = replace_na(number_at_length, 0)) %>%  # fill in absences with true zeroes
  left_join(hauldat)

# Get unique latitude and longitude combinations for hauls
# This is in order to preserve memory when running heavy spatial computations
unique_haul_locations <- unique(len_bsb[,c('lat','lon')])
unique_haul_sf <- st_as_sf(unique_haul_locations, coords = c("lon", "lat"), crs = 4326, remove=FALSE)
unique_haul_sf <- unique_haul_sf %>% mutate(subunit=ifelse(sf::st_intersects(unique_haul_sf,neus_north, sparse=FALSE)[,1],1,2))
unique_haul_sf <- unique_haul_sf %>% mutate(subunit=ifelse(sf::st_intersects(unique_haul_sf,sf::st_buffer(neus_south,1000), sparse=FALSE)[,1],2,1))
unique_haul_sf <- unique_haul_sf %>% mutate(subunit=ifelse(lat<39.45,2,subunit))
# NOTE: It must be noted that there are several hauls that are outside the 
# boundaries of the polygons from the trawl survey shapefiles.
# I tried fixing this by introducing a series of buffers from 1km to 3km to the souther subunit,
# with the idea of any haul that falls outside the buffer belongs to north subunit
# However, this buffer increases the size of the southern subunit by ~12%. Therefore
# I decided to simply assign any point that falls below 39.45 degrees N latitude
# to the southern subunit. This might cause some points that are in the southern subunit to still
# be assigned to northern subunit, but I don't believe it will be a significant number.

# Convert `unique_haul_sf` to a normal dataframe
unique_haul_df <- sf::st_drop_geometry(unique_haul_sf)
# Join unique_haul_df back to len_bsb
len_bsb <- left_join(len_bsb, unique_haul_df, by=c("lat","lon"))

#### SPLITTING INTO TRAINING AND TESTING DATA #### 
# Get the training length information (1989 to 2019)
bsb_train <- len_bsb %>% 
  filter(year >= min_yr,
         year < forecast_yr_1)

# Get the testing length information (2020)
# Not really relevant to us. Putting this here for completeness because the
# default DRM model does forecasting. We just turn off that 
# switch when we run the model
bsb_test <- len_bsb %>% 
  filter(year >= forecast_yr_1,
         year <= max_yr)

#### WAA AND F DATA PROCESSING ####

# Generating Weight at age and F at age data for black sea bass
# This data comes from the Black Sea Bass Operational Stock Assessment for the
# year 2021 - Source from NOAA and Mid-Atlantic Fisheries Council
north_stock <- dget(here("data","stock_assessment_data","NORTH.MT.2021.FINAL.RDAT"))
south_stock <- dget(here("data","stock_assessment_data","SOUTH.MT.2021.FINAL.RDAT"))

##### WAA DATA #####

# Get the weight at age data
waa_bsb_north <- north_stock$WAA.mats$WAA.jan1
# Convert to dataframe
waa_bsb_df_north <- data.frame(waa_bsb_north)
# Rename the columns
colnames(waa_bsb_df_north) <- colnames(waa_bsb_north)
waa_bsb_df_north$year <- rownames(waa_bsb_north)
waa_bsb_df_north$year <- as.integer(waa_bsb_df_north$year)
# Tidy the data
waa_bsb_df_pretty_north <- waa_bsb_df_north %>% 
  pivot_longer(
    cols = c(1,2,3,4,5,6,7,8), 
    names_to = "age", 
    values_to = "wt",
  )

# Get the weight at age data
waa_bsb_south <- south_stock$WAA.mats$WAA.jan1
# Convert to dataframe
waa_bsb_df_south <- data.frame(waa_bsb_south)
# Rename the columns
colnames(waa_bsb_df_south) <- colnames(waa_bsb_south)
waa_bsb_df_south$year <- rownames(waa_bsb_south)
waa_bsb_df_south$year <- as.integer(waa_bsb_df_south$year)
# Tidy the data
waa_bsb_df_pretty_south <- waa_bsb_df_south %>% 
  pivot_longer(
    cols = c(1,2,3,4,5,6,7,8), 
    names_to = "age", 
    values_to = "wt",
  )

# For weight at age, we are combining the two since they don't seem to present
# significant differences for age classes up to 6
# Getting average mean weight at age for all years. So this will be a 1x8 matrix
# Append the two dataframes
# WAA for the two subunits
# Name the subunits before appending them
waa_bsb_df_pretty_north <- waa_bsb_df_pretty_north %>% mutate(subunit=1)
waa_bsb_df_pretty_south <- waa_bsb_df_pretty_south %>% mutate(subunit=2)

waa_bsb_df_joined <- rbind(waa_bsb_df_pretty_north, waa_bsb_df_pretty_south)

waa_bsb_mean_df <- waa_bsb_df_joined %>% 
  group_by(age) %>% 
  summarize(wt=mean(wt))

##### FAA DATA #####

# FAA North subunit
# Get the F at age data
faa_bsb_north <- north_stock$F.age[,8]
# Get the selectivity at age
sel_north <- north_stock$fleet.sel.mats
fleet_1_sel_north <- sel_north$sel.m.fleet1
fleet_2_sel_north <- sel_north$sel.m.fleet2
# Create a list
fleet_sel_list_north <- list(fleet_1_sel_north, fleet_2_sel_north)
s_north <- Reduce('+',fleet_sel_list_north)/length(fleet_sel_list_north)
# Multiply selectivity by F at age 8 to get fishing pressure on each age class
faa_bsb_north <- s_north*faa_bsb_north
# Some years are missing (1972 to 1988). This is OK with us.
faa_bsb_df_north <- data.frame(faa_bsb_north)
# Rename the columns
colnames(faa_bsb_df_north) <- colnames(faa_bsb_north)
faa_bsb_df_north$year <- rownames(faa_bsb_north)
faa_bsb_df_north$year <- as.integer(faa_bsb_df_north$year)
# Tidy the data
faa_bsb_df_pretty_north <- faa_bsb_df_north %>% 
  pivot_longer(
    cols = c(1,2,3,4,5,6,7,8), 
    names_to = "age", 
    values_to = "f",
  )

# FAA South subunit
# Get the F at age data
faa_bsb_south <- south_stock$F.age[,8]
# Get the selectivity at age
sel_south <- south_stock$fleet.sel.mats
fleet_1_sel_south <- sel_south$sel.m.fleet1
fleet_2_sel_south <- sel_south$sel.m.fleet2
# Create a list
fleet_sel_list_south <- list(fleet_1_sel_south, fleet_2_sel_south)
s_south <- Reduce('+',fleet_sel_list_south)/length(fleet_sel_list_south)
# Multiply selectivity by F at age 8 to get fishing pressure on each age class
faa_bsb_south <- s_south*faa_bsb_south
# Some years are missing (1972 to 1988). This is OK with us.
faa_bsb_df_south <- data.frame(faa_bsb_south)
# Rename the columns
colnames(faa_bsb_df_south) <- colnames(faa_bsb_south)
faa_bsb_df_south$year <- rownames(faa_bsb_south)
faa_bsb_df_south$year <- as.integer(faa_bsb_df_south$year)
# Tidy the data
faa_bsb_df_pretty_south <- faa_bsb_df_south %>% 
  pivot_longer(
    cols = c(1,2,3,4,5,6,7,8), 
    names_to = "age", 
    values_to = "f",
  )

# FAA for the two subunits
# Add columns to denote subunit
faa_bsb_df_pretty_north <- faa_bsb_df_pretty_north %>% mutate(subunit=1) %>% relocate(subunit, .after=year)
faa_bsb_df_pretty_south <- faa_bsb_df_pretty_south %>% mutate(subunit=2) %>% relocate(subunit, .after=year)
# Append the two dataframes
faa_bsb_df_joined <- rbind(faa_bsb_df_pretty_north, faa_bsb_df_pretty_south)

# Some basic visualizations of the data (WAA and FAA)

# Visualizing the weight at age in the two subunits
ggplot(waa_bsb_df_joined , aes(x=year, y=wt, group=subunit)) +
  facet_wrap(~age) + 
  theme_fivethirtyeight() + 
  geom_line(aes(linetype=as.factor(subunit))) + 
  labs(x="Year", y="Weight", linetype="Subunit")

# Visualizing the mean WAA that will be used in the model (fixed in time)
# Visualizing the weight at age in the two subunits
ggplot(waa_bsb_mean_df , aes(x=age, y=wt, group=1)) + geom_point() + geom_line()

# Visualizing the full fishing pressure in the two subunits (At maximum age)
ggplot(faa_bsb_df_joined %>% filter(age=="8"), aes(x=year, y=f, group=subunit)) + 
  geom_line(aes(linetype=as.factor(subunit)))

#### ASSIGN SEASONAL AND ANNUAL TEMPERTURES TO SUBUNITS ####

# We download GLORYS data via the ecodata R package from NOAA
# GLORYS data is in 1/12° spatial resolution

# We will splitting the data by the latitude of the Hudson Canyon, NY - 39.5 °N
hudson_canyon_latitude <- 39.5

# Download GLORYS data and filter them for year and season
glorys_temps <- ecodata::bottom_temp_seasonal_gridded %>% 
  filter(Time>=1988 & Time<=2019)

# Assign subunits to the glorys data
glorys_temps <- glorys_temps %>% 
  mutate(subunit=ifelse(Latitude<hudson_canyon_latitude, 2, 1))

# Average them for each year - Winter
glorys_temps_winter_means <- glorys_temps %>% filter(Var=="winter") %>% 
  group_by(Time, subunit) %>% summarize(mean_winter_temp=mean(Value, na.rm=TRUE)) %>% 
  ungroup()


# Average them for each - Spring
glorys_temps_spring_means <- glorys_temps %>% filter(Var=="spring") %>% 
  group_by(Time, subunit) %>% summarize(mean_spring_temp=mean(Value, na.rm=TRUE)) %>% 
  ungroup()


# Average them for full year
glorys_temps_annual_means <- glorys_temps %>% group_by(Time, subunit) %>% 
  summarize(mean_annual_temp=mean(Value,na.rm=TRUE)) %>% ungroup()


# Rename time to year 
glorys_temps_spring_means <- glorys_temps_spring_means %>% 
  rename(year=Time) %>% 
  rename(sbt=mean_spring_temp) %>% 
  mutate(Source_Season="GLORYS-Spring")
glorys_temps_winter_means <- glorys_temps_winter_means %>% 
  rename(year=Time) %>% 
  rename(sbt=mean_winter_temp) %>% 
  mutate(Source_Season="GLORYS-Winter")
glorys_temps_annual_means <- glorys_temps_annual_means %>% 
  rename(year=Time) %>% 
  rename(sbt=mean_annual_temp) %>% 
  mutate(Source_Season="GLORYS-Annual")

# Bind all of them together
all_temps_bind <- rbind(glorys_temps_spring_means,glorys_temps_winter_means,
                        glorys_temps_annual_means)

# Rename the subunit column (1 to North, 2 to South)
# all_temps_bind <- all_temps_bind %>% mutate(subunit=ifelse(subunit==1, "North","South"))

# Plot all temperatures over time
ggplot(all_temps_bind, aes(x=year, y=sbt, group=Source_Season, color=Source_Season)) + 
  geom_line() + 
  theme_fivethirtyeight() + 
  facet_wrap(~subunit) + 
  labs(x="Year", y="Mean Temp (°C)", 
       color="Source and season",
       title="") + 
  theme(axis.title=element_text(),
        legend.text=element_text(size=10),
        plot.caption=element_text(size=8, family="Avenir Next Condensed"),
        plot.title=element_text(size=12, hjust=0.5))

#### SUBUNIT ABUNDANCES AND DENSITIES #### 
subunit_abundances <- bsb_train %>% 
  group_by(haulid) %>% 
  mutate(dens = sum(number_at_length)) %>% # get total no. fish in each haul, of any size
  group_by(year, subunit) %>% 
  summarise(mean_dens = mean(dens)) %>%  # get mean density (all sizes) / haul for the patch*year combo 
  ungroup() %>% 
  mutate(patch = as.integer(as.factor(subunit)))

abund_p_y_temp <- left_join(subunit_abundances, subunit_info %>% select(patch,area,area_units), c=("patch"))
abund_p_y <- abund_p_y_temp %>% mutate(abundance = mean_dens * area) %>% relocate(abundance, .after=mean_dens)


#### WRITE ALL DATA AND IMAGES TO DISK ####

##### WRITE DATA #####
# Flag to write data to disk
WRITE_DATA_TO_DISK = FALSE

if(WRITE_DATA_TO_DISK){
  # Write trawl survey processed training and testing data
  write_csv(bsb_train, here("processed-data","black_sea_bass_catch_at_length_spring_training.csv"))
  write_csv(bsb_test, here("processed-data","black_sea_bass_catch_at_length_spring_testing.csv"))
  # Write information about the two subunits (their area)
  write_csv(subunit_info, here("processed-data","subunit_information.csv"))
  # Write the abundances for each subunit (Mean density x subunit area)
  write_csv(abund_p_y, here("processed-data","subunit_abundances.csv"))
  # Write mean WAA to be used in DRM
  write_csv(waa_bsb_mean_df,
           here( "processed-data", "black_sea_bass_wt_at_age_mean.csv"))
  # Write F for both subunits properly formatted with mean selectivity from the two fleets applied
  write_csv(faa_bsb_df_joined,
            here("processed-data","black_sea_bass_F_by_age_all_subunits.csv"))
  # Write GLORYS temps
  write_csv(all_temps_bind, here("processed-data","all_temperatures_glorys.csv"))
}

print("Done")

##### WRITE IMAGES #####

# All these go to supplementary images

# Creating the following supplementary figures
# 1. WAA averaged and for each patch
# 2. F at maximum age through time for each patch
# 3. Selectivity for each age through time


# WAA
waa_bsb_df_joined <- waa_bsb_df_joined %>% mutate(subunit=replace(subunit,subunit==1,"North"))
waa_bsb_df_joined <- waa_bsb_df_joined %>% mutate(subunit=replace(subunit,subunit==2,"South"))

waa_plot_left <- ggplot(waa_bsb_df_joined , aes(x=year, y=wt, group=subunit)) +
  facet_wrap(~age, nrow=8) + 
  geom_line(aes(linetype=as.factor(subunit))) + 
  labs(x="Year", y="Weight", linetype="Subunit")

waa_plot_right <- ggplot(waa_bsb_mean_df , aes(x=age, y=wt, group=1)) + 
  geom_point() + 
  geom_line(linetype=3) + 
  labs(x="Age", y="Weight")

waa_plot <- cowplot::plot_grid(waa_plot_left, waa_plot_right, labels=c("A","B"), label_size = 8)

# FAA
# Visualizing the full fishing pressure in the two subunits (At maximum age)

faa_bsb_df_joined <- faa_bsb_df_joined %>% mutate(subunit=replace(subunit, subunit==1, "North"))
faa_bsb_df_joined <- faa_bsb_df_joined %>% mutate(subunit=replace(subunit, subunit==2, "South"))

ggplot(faa_bsb_df_joined %>% filter(age=="8"), aes(x=year, y=f, group=subunit)) + 
  geom_point(aes(color=subunit)) + 
  geom_line(aes(linetype=subunit)) + 
  labs(x="F", y="Year", linetype="Subunit")
  
# Selectivity

WRITE_IMAGES_TO_DISK = FALSE

if(WRITE_IMAGES_TO_DISK){
  # Write WAA plot
  ggsave(here("figures","supplementary-figures","waa.png"), waa_plot, height=10, width=8, units=c("in"), dpi=300)
}