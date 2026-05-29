# Script - pub_script_02_analyze_black_sea_bass_temp.R
# Task - Run models for black sea bass (Temporary version)
# Author -  Bandara (jeewantha.bandara@rutgers.edu)

# Load packages
# set.seed(42)
set.seed(50)
library(tidyverse)
library(tidybayes)
library(here)
library(magrittr)
library(rstan)
library(Matrix)
library(ggridges)
library(rstanarm)
library(geosphere)
library(ggridges)
library(purrr)
library(lme4)
library(lubridate)
library(bayesplot)

# Load functions
# source(here("R","functions","generate_length_at_age.R"))
funs <- list.files(here("R/functions"), pattern="\\.R$")
sapply(funs, function(x) source(file.path("R/functions",x)))

#### IF YOU WANT TO RUN CODE ON SERVER ####

# Setting rstan options. This must be uncommmented where running on a server
# rstan_options(javascript=FALSE, auto_write =TRUE)

#### READ IN DATA ####

# Reading in the following data
# 1. Training/Testing length data from NEUS spring trawl survey
# 2. Sea bottom temperatures for the subunits from GLORYS
# 3. Fixed WAA data
# 4. Time and patch specific FAA data
# 5. Patch information (For their area in km^2)

# Reading training data (All the data, since we aren't testing against anything)
dat <- read_csv(here("processed-data","black_sea_bass_catch_at_length_spring_training.csv"))

# Data for testing
# Spoiler: There is only one year for testing. Incorporate it later in the code
dat_test <- read_csv(here("processed-data","black_sea_bass_catch_at_length_spring_testing.csv"))

# Read in GLORYS temperatures and format them
glorys_temps <- read_csv(here("processed-data","all_temperatures_glorys.csv"))
glorys_temps$patch <- as.integer(as.factor(glorys_temps$subunit))
glorys_temps$year <- as.integer(as.factor(glorys_temps$year))
glorys_winter_temps <- glorys_temps %>% filter(Source_Season=="GLORYS-Winter")
glorys_annual_temps <- glorys_temps %>% filter(Source_Season=="GLORYS-Annual")

# Centering the winter and annual temperatures
# glorys_annual_temps <- glorys_annual_temps %>% mutate(sbt=sbt - mean(sbt,na.rm=TRUE))

# Read in WAA data
wt_at_age_df <- read_csv(here("processed-data","black_sea_bass_wt_at_age_mean.csv"))
wt_at_age <- wt_at_age_df$wt

# Read in FAA data
dat_f_age_df <- read_csv(here("processed-data","black_sea_bass_F_by_age_all_subunits.csv"))
# Introduce a year index column
dat_f_age_df$year_index <- as.integer(as.factor(dat_f_age_df$year))
# This should be processed later based on information derived from 

# Read in subunit information
subunit_info <- read_csv(here("processed-data","subunit_information.csv"))

# Constant/Fixed parameters from the stock assessment
# - and : set fixed parameters from stock assessment
# I'm setting the parameters for the species from the BSB
# stock assessment for the northern subunit (For now. In order to simplify the model)
loo = 60 # Length at terminal age/Infinity
k = 0.2 # Von-Bertalanffy growth coefficient
m = 0.4 # Natural mortality
age_at_maturity = 4 # It should be 3.5
t0=-.2
cv= 0.2 # guess. : I suppose this is coefficient of variance
min_age = 1
max_age = 8

#### LEGNTH AT AGE KEY ####
# Make a length at age key. Imports a function from `functions` folder
length_at_age_key <-
  generate_length_at_age_key(
    min_age = min_age,
    max_age = max_age,
    cv = cv,
    linf = loo,
    k = k,
    t0 = t0,
    time_step = 1,
    linf_buffer = 1.5
  )
# : Create a matrix from the length at age key

l_at_a_mat <- length_at_age_key %>% 
  select(age, length_bin, p_bin) %>% 
  # pivot_wider(names_from = length_bin, values_from = p_bin) %>% 
  spread(length_bin,p_bin) %>%
  ungroup() %>% 
  select(-age) %>% 
  as.matrix()


#### REGARDING MODEL TOGGLES ####

# A more verbose description of these toggles can be found in the R/README.md
# But in the interest of being short and sweet, please refer below

# Set `do_dirichlet` to 1 only if you want to evaluate length composition in the
# bottom trawl survey data using the dirichlet distribution

# Set `eval_l_comps` to 1  only if you want to evaluate length composition in the
# bottom trawl survey data using a multi-nomial distribution

# Set `T_dep_mortality` to 1 if you want to run the DRM with a link between
# sea bottom temperature and natural mortality

# Set `T_dep_recruitment` to 1 if you want to run the DRM with a link between
# sea bottom temperature and recruitment

# Set `T_dep_dispersal` to 1 if you want to run the DRM with a link between
# sea bottom temperature and dispersal between subunits

# Set `spawner_recruit_relationship` to 1 if you want to run a version of the DRM
# where recruitment is estimated using the classic 'Beverton-Holt' model.
# FAIR WARNING - As of this moment, we haven't been able to get this to work.
# The default version of the model has a raw mean recruitment value which is 
# estimated and is constant through time

# Set `run_forecast` to 0 for the default version of the model. We aren't concerned
# with forecasting the model in to the future at this point.

#### SET TOGGLES ####

do_dirichlet = 0 # This only needs to be turned on 
eval_l_comps = 0 # Evaluate length composition data? 0=no, 1=yes
T_dep_mortality = 0 # Turn on temperature dependent mortality
T_dep_recruitment = 1 # Turn on temperature dependent recruitment
T_dep_dispersal = 0 # Turn on temperature dependent dispersal
spawner_recruit_relationship = 0 # Turn on spawner recruit relationship (Beverton-Holt)
run_forecast=0 # Run a forecast for a certain number of years (Currently not being used)

#### PREP DATA FOR FITTING ####

# Set the number of patches (np)
use_patches <- dat %>%
  group_by(subunit) %>% 
  summarise(total = sum(number_at_length))

patches <- sort(unique(use_patches$subunit))
np = length(patches) 

##### SET UP TOTAL NUMBERS AT LENGTH ####
# Get the training lengths.
# This is summarized as the number of observations at a certain length for each year and patch
dat_train_lengths <- dat %>% 
  group_by(length, year, subunit) %>% 
  summarise(sum_num_at_length = sum(number_at_length)) %>% 
  filter(subunit %in% patches)%>% 
  ungroup() %>% 
  mutate(patch = as.integer(as.factor(subunit)))

# Get the testing lengths
# Output similar to training lengths
# NOTE: WE ARE COMMENTING THIS OUT
dat_test_lengths <- dat_test %>%
  group_by(length, year, subunit) %>% 
  summarise(sum_num_at_length = sum(number_at_length)) %>% 
  filter(subunit %in% patches)%>% 
  ungroup() %>% 
  mutate(patch = as.integer(as.factor(subunit)))

# Get the training densities
# For each 'haul id', get the mean density (mean number at a certain length) for
# each year and patch
dat_train_dens <- dat %>%
  filter(subunit %in% patches) %>% 
  group_by(haulid) %>% 
  mutate(dens = sum(number_at_length)) %>% # get total no. fish in each haul, of any size
  group_by(year, subunit) %>% 
  summarise(mean_dens = mean(dens)) %>%  # get mean density (all sizes) / haul for the patch*year combo 
  ungroup() %>% 
  mutate(patch = as.integer(as.factor(subunit)))


# Get the testing densities
dat_test_dens <- dat_test %>%
  filter(subunit %in% patches) %>% 
  group_by(haulid) %>% 
  mutate(dens = sum(number_at_length)) %>% # get total no. fish in each haul, of any size
  group_by(year, subunit) %>% 
  summarise(mean_dens = mean(dens)) %>%  # get mean density (all sizes) / haul for the patch*year combo 
  ungroup() %>% 
  mutate(patch = as.integer(as.factor(subunit)))


#### DURATION FOR TRAINING AND PROJECTIONS ####
# Get time dimensions (vector of years and number of years)
years <- sort(unique(dat_train_lengths$year)) 
years_proj <- sort(unique(dat_test_lengths$year))
ny <- length(years)
ny_proj <- length(years_proj)

#### PATCH AREA ####
# Patch area is already calculated from `pub_script_01`
subunit_info <- subunit_info %>% mutate(patch_area_km2=area)

#### SETUP F ####
# Introduce a `patch` column to dat_f_age_df
dat_f_age_df <- dat_f_age_df %>% mutate(patch=subunit, .after=subunit)

dat_f_age <- dat_f_age_df %>% 
  filter(year %in% years) 

dat_f_age_proj <- dat_f_age_df %>% 
  filter(year %in% years_proj) %>%
  bind_rows(dat_f_age %>% filter(year==max(year))) # need final year of training data to initialize projection

# Setting up `f_prep`: A stand-in F value if you want to project F beyond year 2019
# Mean of the last 5 years for each of the two patches
f_prep_df <- dat_f_age_df %>% group_by(subunit) %>% summarize(f_prep=mean(tail(f,5), na.rm=TRUE))

# NOTE: This stuff only matters if we are interesting in evaluating length composition of the stock. I have turned it off for this model!
# Make length to age conversions
# Function is from functions folder

# : Create length bins from the length at age key
lbins <- unique(length_at_age_key$length_bin)
# lbins <- sort(unique(dat_train_lengths$length))
# : Get the number of bins
n_lbins <- length(lbins) 

# Get the number of ages - Number of rows in the length at age matrix
n_ages <- nrow(l_at_a_mat)

# Now that years are defined above, convert them into indices in the datasets
# Be sure all these dataframes have exactly the same year range! 

dat_train_dens$year = as.integer(as.factor(dat_train_dens$year))
dat_test_dens$year = as.integer(as.factor(dat_test_dens$year))
dat_train_lengths$year = as.integer(as.factor(dat_train_lengths$year))
# dat_test_lengths$year = as.integer(as.factor(dat_test_lengths$year))

# -: make matrices/arrays from dfs
# : Create a matrix from number of patches x number of length bins x number of years
# : Then fill it up using the dat_train_length tibble
len <- array(0, dim = c(np, n_lbins, ny)) 
for(p in 1:np){
  for(l in 1:n_lbins){
    for(y in 1:ny){
      tmp <- dat_train_lengths %>% filter(patch==p, round(length)==lbins[l], year==y) 
      if (nrow(tmp) > 0){
        len[p,l,y] <- tmp$sum_num_at_length
      }
    }
  }
}

plot(len[1,,20])

# Create a matrix from number of patches and number of years. This is for density
# Fill it using the mean density in dat_train_dens
dens <- array(NA, dim=c(np, ny))
for(p in 1:np){
  for(y in 1:ny){
    tmp2 <- dat_train_dens %>% filter(patch==p, year==y) 
    #   left_join(patchdat, by = c("lat_floor","patch"))%>% 
    #   mutate(mean_dens = mean_dens * patch_area_km2)
    # dens[p,y] <- tmp2$mean_dens
    patch_area_temp <- subunit_info %>% filter(patch==p) %>% select(patch_area_km2) %>% pull()
    dens[p,y] <- tmp2$mean_dens*patch_area_temp
  }
}

# Setting GLORYS Sea Bottom temperature for training data by patches and year

# Set Annual Bottom temperature means
sbt <- array(NA, dim=c(np,ny))
for(p in 1:np){
  for(y in 1:ny){
    tmp3 <- glorys_annual_temps %>% filter(patch==p, year==y) 
    sbt[p,y] <- tmp3$sbt
  }
}

# Set Winter Sea Bottom temperatures
winter_sbt <- array(NA, dim=c(np,ny))
for(p in 1:np){
  for(y in 1:ny){
    tmp4 <- glorys_winter_temps %>% filter(patch==p, year==y) 
    winter_sbt[p,y] <- tmp4$sbt
  }
}


# : Setting Sea Bottom temperature for testing data by patches and year
sbt_proj <- array(NA, dim=c(np,ny_proj))
for(p in 1:np){
  for(y in 1:ny_proj){
    tmp6 <-  glorys_annual_temps %>% filter(patch==p, year==y) 
    sbt_proj[p,y] <- tmp6$sbt
  }
}

# Setting fishing mortality for training data by age and year
f <- array(NA, dim=c(np, n_ages, ny))
for(p in 1:np) {
  for (a in min_age:max_age) {
    for (y in 1:ny) {
      tmp4 <- dat_f_age_df %>% filter(patch == p, age == a, year_index == y)
      f[p, a, y] <- tmp4$f # add 1 because matrix indexing starts at 1 not 0
      print(paste0(p," ",a," ",y))
      print(tmp4)
    }
  }
}

# Setting fishing mortality for testing data by age and year
f_proj <- array(0.36, dim = c(np, n_ages, (ny_proj + 1)))
for (p in 1:np) {
  for (a in min_age:max_age) {
    for (y in 1:ny_proj+1) {
      tmp5 <- dat_f_age_proj %>% filter(patch == p, age == a, year_index ==ny+y)
      f_proj[p, a, y] <- tmp5$f # add 1 because matrix indexing starts at 1 not 0
      print(paste0(p," ",a," ",y))
      print(tmp5)
    }
  }
}

#### LAST CHECK BEFORE MODEL RUN #### 
a <- seq(min_age, max_age)

check <- a %*% l_at_a_mat

#### MODEL FITTING ####

# : Set the parameters for the stan model
stan_data <- list(
  np=np, # np = Number of patches
  n_ages=n_ages, # n_ages = Number of ages
  ny_train=ny, # ny_train = Number of years in training dataset
  ny_proj=ny_proj, # ny_proj = Number of years in testing dataset
  n_lbins=n_lbins, # n_lbins = Number of bins for length. 
  n_p_l_y = len, # n_p_l_y / len = Sum of number at length per length bin, per year, per patch. Refer line 340
  abund_p_y = dens, # abund_p_y = 
  sbt = sbt, # sbt = Temperature training dataset
  winter_sbt = winter_sbt, # winter_sbt = Annual winter sbt from GLORYS
  sbt_proj=sbt_proj, # sbt_proj = Temperature testing dataset
  m=m, # Natural mortality
  f=f, # Fishing mortality
  f_proj=f_proj, # Fishing mortality data for testing against.. 
  k=k, # Von-Bertalanffy growth coefficient
  loo=loo, # Maximum length
  t0=t0,
  cv=cv, # Coefficient of variance -> Standard deviation = Mean * cv
  length_50_sel_guess=20, # Educated guess
  n_lbins = n_lbins, # Number of length bins
  age_sel = 0,
  bin_mids=lbins+0.5, # also not sure if this is the right way to calculate the midpoints
  sel_100 = 3, # not sure if this should be 2 or 3. it's age 2, but it's the third age category because we start at 0, which I think Stan will classify as 3...?
  age_at_maturity = age_at_maturity,
  l_at_a_key = l_at_a_mat,
  wt_at_age = wt_at_age,
  do_dirichlet = do_dirichlet,
  eval_l_comps = eval_l_comps,
  T_dep_mortality = T_dep_mortality,
  T_dep_recruitment = T_dep_recruitment,
  T_dep_dispersal = T_dep_dispersal,
  spawner_recruit_relationship = spawner_recruit_relationship, 
  run_forecast=run_forecast
)


# Configure settings for stan model fitting
warmups <- 1000
total_iterations <- 3000
max_treedepth <-  10
n_chains <-  4
n_cores <- 1

stan_model_file <- "process_sdm_bsb.stan"

stan_model_fit <- stan(file = here::here("stan-src",stan_model_file), # check that it's the right model!
                       data = stan_data,
                       chains = n_chains,
                       warmup = warmups,
                       iter = total_iterations,
                       cores = n_cores,
                       refresh = 250,
                       control = list(max_treedepth = max_treedepth,
                                      adapt_delta = 0.95)
)


# Create a text file with model stats

SAVE_MODEL <- FALSE

if(SAVE_MODEL){
  # We will save both a text file detailing the model specifications and the 
  # model object itself
  stan_model_fit_text_df <- data.frame(model_spec=c("patch_no",
                                                    "begin_year",
                                                    "do_dirichlet",
                                                    "eval_l_comps",
                                                    "t_dep_mortality",
                                                    "t_dep_dispersal",
                                                    "t_dep_recruitment",
                                                    "spawn_rec_relationship",
                                                    "run_forecast",
                                                    "warmups",
                                                    "total_iterations",
                                                    "max_tree_depth",
                                                    "n_chains",
                                                    "stan_model_file"),
                                       vals=c(2,
                                              1989,
                                              do_dirichlet,
                                              eval_l_comps,
                                              T_dep_mortality,
                                              T_dep_dispersal,
                                              T_dep_recruitment,
                                              spawner_recruit_relationship,
                                              run_forecast,
                                              warmups,
                                              total_iterations,
                                              max_treedepth,
                                              n_chains,
                                              stan_model_file))
  
  # Date-Time information
  # Use lubridate to get date-time information
  curr_date_time <- Sys.time()
  dto <- lubridate::as_datetime(curr_date_time)
  write_csv(stan_model_fit_text_df, here("saved_models",paste("bsb_model_run_info_",year(dto),"_",month(dto),"_",day(dto),"_",hour(dto),"_",minute(dto),"_",round(second(dto),0),".csv",sep="")))
  write_rds(stan_model_fit,here("saved_models",paste("black_sea_bass_stan_fit_",year(dto),"_",month(dto),"_",day(dto),"_",hour(dto),"_",minute(dto),"_",round(second(dto),0),".rds",sep="")))
}

print("Done")
