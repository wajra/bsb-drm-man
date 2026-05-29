# Script - pub_script_03_visualize_model_results.R
# Task - Visualize model results, compare their performance against each other, 
# and plot the trace plots for the best performing model
# Author - Jeewantha Bandara (jeewantha.bandara@rutgers.edu)

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
library(ggthemes)
library(cowplot)
library(patchwork)
library(latex2exp)
library(PNWColors)
library(Metrics)
library(scales)

# Load functions
funs <- list.files(here("R/functions"), pattern="\\.R$")
sapply(funs, function(x) source(file.path("R/functions/",x)))

# Read in the subunits information and the recorded densities in the subunits
abund_p_y <- read_csv(here("processed-data","subunit_abundances.csv"))
subunit_info <- read_csv(here("processed-data","subunit_information.csv"))

# Read in the list of models
base_model <- readRDS(here("saved_models", "black_sea_bass_stan_fit_2025_9_22_14_27_8.rds"))
mortality_model <- readRDS(here("saved_models","black_sea_bass_stan_fit_2026_2_26_6_1_47.rds"))
temp_dep_rec_model <- readRDS(here("saved_models","black_sea_bass_stan_fit_2025_9_22_14_39_33.rds"))
temp_dep_disp_model <- readRDS(here("saved_models","black_sea_bass_stan_fit_2025_9_2_14_49_50.rds"))

# Extract the abundances
abund_p_y_hat_base_model <- tidybayes::spread_draws(base_model, dens_p_y_hat[patch,year])
abund_p_y_hat_mortality_model <- tidybayes::spread_draws(mortality_model, dens_p_y_hat[patch,year])
abund_p_y_hat_temp_dep_rec_model <- tidybayes::spread_draws(temp_dep_rec_model, dens_p_y_hat[patch,year])
abund_p_y_hat_temp_dep_disp_model <- tidybayes::spread_draws(temp_dep_disp_model, dens_p_y_hat[patch,year])


# Add a name column for both
abund_p_y_hat_base_model$model_name <- "Base model"
abund_p_y_hat_mortality_model$model_name <- "Temp. Dep. Mortality model"
abund_p_y_hat_temp_dep_rec_model$model_name <- "Temp. Dep. Recruitment model"
abund_p_y_hat_temp_dep_disp_model$model_name <- "Temp. Dep. Dispersal model"

# Bind the rows together
abund_py_bind <- rbind(abund_p_y_hat_base_model, 
                       abund_p_y_hat_mortality_model,
                       abund_p_y_hat_temp_dep_rec_model,
                       abund_p_y_hat_temp_dep_disp_model)

# Rename 'dens_p_y_hat' to abundance
abund_py_bind <- abund_py_bind %>% rename(abundance=dens_p_y_hat)

# Rename the subunits
abund_py_bind <- abund_py_bind %>% mutate(patch=replace(patch,patch==1,"North"))
abund_py_bind <- abund_py_bind %>% mutate(patch=replace(patch,patch==2,"South"))

# Rename the subunits for 'abund_p_y' as well
abund_p_y_reformat <- abund_p_y %>% mutate(year=as.integer(as.factor(year)))
abund_p_y_reformat <- abund_p_y_reformat %>% mutate(patch=replace(patch,patch==1,"North"))
abund_p_y_reformat <- abund_p_y_reformat %>% mutate(patch=replace(patch,patch==2,"South"))

#  Making a log scale for the y axis
logplus1 <- function(x) log(x+1)
unlogplus1 <- function(x) exp(x)-1
logplus1trans <- scales::trans_new(name='logplus1', transform=logplus1, inverse=unlogplus1)

# Base model on new log scale
plot_base_model_pub <- abund_py_bind %>% filter(model_name=="Base model") %>% 
  ggplot(aes(year+1988, abundance)) + 
  stat_lineribbon() + 
  geom_point(data = abund_p_y_reformat, aes(year+1988, abundance), color = "black") +
  facet_wrap(~patch) +
  labs(x="Year",y="Abundance") + 
  scale_y_continuous(breaks=c(0,1000,10000,100000,1000000,10000000),labels=scales::comma, trans=logplus1trans) + 
  scale_fill_brewer("CI levels", palette="Purples") +
  theme_fivethirtyeight() + 
  theme(axis.title=element_text(),
        legend.text=element_text(size=10),
        plot.caption=element_text(size=8, family="Avenir Next Condensed"),
        plot.title=element_text(size=12, hjust=0.5),
        panel.background = element_rect(fill = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white"), # Set plot background to white
        legend.background = element_rect(fill = "white"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(face="bold"),
        legend.position = "none",
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(size = 0.5, color="black"))

# Dispersal model
plot_dispersal_model_pub <- abund_py_bind %>% filter(model_name=="Temp. Dep. Dispersal model") %>% 
  ggplot(aes(year+1988, abundance)) + 
  stat_lineribbon() + 
  geom_point(data = abund_p_y_reformat, aes(year+1988, abundance), color = "black") +
  facet_wrap(~patch) +
  labs(x="Year",y="Abundance") + 
  scale_y_continuous(breaks=c(0,1000,10000,100000,1000000,10000000),limits=c(0,10000000),labels=scales::comma, trans=logplus1trans) + 
  scale_fill_brewer("CI levels", palette="Purples") +
  theme_fivethirtyeight() + 
  theme(axis.title=element_text(),
        legend.text=element_text(size=10),
        plot.caption=element_text(size=8, family="Avenir Next Condensed"),
        plot.title=element_text(size=12, hjust=0.5),
        panel.background = element_rect(fill = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white"), # Set plot background to white
        legend.background = element_rect(fill = "white"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(face="bold"),
        legend.position = "none",
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(size = 0.5, color="black"))

# Recruitment model
plot_recruitment_model_pub <- abund_py_bind %>% filter(model_name=="Temp. Dep. Recruitment model") %>% 
  ggplot(aes(year+1988, abundance)) + 
  stat_lineribbon() + 
  geom_point(data = abund_p_y_reformat, aes(year+1988, abundance), color = "black") +
  facet_wrap(~patch) +
  labs(x="Year",y="Abundance") + 
  scale_y_continuous(breaks=c(0,1000,10000,100000,1000000,10000000),limits=c(0,10000000),labels=scales::comma, trans=logplus1trans) + 
  scale_fill_brewer("CI levels", palette="Purples") +
  theme_fivethirtyeight() + 
  theme(axis.title=element_text(),
        legend.text=element_text(size=10),
        plot.caption=element_text(size=8, family="Avenir Next Condensed"),
        plot.title=element_text(size=12, hjust=0.5),
        panel.background = element_rect(fill = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white"), # Set plot background to white
        legend.background = element_rect(fill = "white"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(face="bold"),
        legend.position = "none",
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(size = 0.5, color="black"))

# Mortality model
plot_mortality_model_pub <- abund_py_bind %>% filter(model_name=="Temp. Dep. Mortality model") %>% 
  ggplot(aes(year+1988, abundance)) + 
  stat_lineribbon() + 
  geom_point(data = abund_p_y_reformat, aes(year+1988, abundance), color = "black") +
  facet_wrap(~patch) +
  labs(x="Year",y="Abundance") + 
  scale_y_continuous(breaks=c(0,1000,10000,100000,1000000,10000000),limits=c(0,10000000),labels=scales::comma, trans=logplus1trans) + 
  scale_fill_brewer("CI levels", palette="Purples") +
  theme_fivethirtyeight() + 
  theme(axis.title=element_text(),
        legend.text=element_text(size=10),
        plot.caption=element_text(size=8, family="Avenir Next Condensed"),
        plot.title=element_text(size=12, hjust=0.5),
        panel.background = element_rect(fill = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white"), # Set plot background to white
        legend.background = element_rect(fill = "white"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(face="bold"),
        legend.position = "none",
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(size = 0.5, color="black"))

# Gather all four plots
all_models_together_pub <- plot_grid(plot_base_model_pub,
                                 plot_dispersal_model_pub,
                                 plot_mortality_model_pub,
                                 plot_recruitment_model_pub,
                                 labels = c('(a)', '(b)', '(c)', '(d)'), ncol=1)

#### Visualize trace plots ####

##### sigma_r #####
sigma_r_draws_recruitment <- tidybayes::spread_draws(temp_dep_rec_model, sigma_r)

names(sigma_r_draws_recruitment) <- c("chain","iteration","draw","sigma_r")

# Refactor chains as a factor
sigma_r_draws_recruitment <- sigma_r_draws_recruitment %>% mutate(chain=as.factor(chain))

# Set a color pallete for the chain plots
chain_palette <- pnw_palette("Sunset2",4,type="discrete")

# Same plot but with visual improvements
sigma_r_trace_plot <- ggplot(sigma_r_draws_recruitment, aes(iteration,sigma_r,color=chain)) + geom_line(alpha=0.75) + 
  scale_color_manual(values = chain_palette) + 
  theme_classic() + labs(x="Iteration", y=TeX(r'($\sigma_r$)'), color="Chain")

all_sigma_r_draws <- temp_dep_rec_model|>
  extract(permuted = FALSE, inc_warmup = TRUE) |> 
  posterior::as_draws_df() |>
  spread_draws(sigma_r)


##### mean_recruits #####
rec_draws <- tidybayes::spread_draws(temp_dep_rec_model, mean_recruits)

names(rec_draws) <- c("chain","iteration","draw","mean_recruits")

rec_draws <- rec_draws %>% mutate(chain=as.factor(chain))

mean_rec_trace_plot <- ggplot(rec_draws, aes(iteration,mean_recruits,color=chain)) + geom_line(alpha=0.75) + 
  scale_color_manual(values=chain_palette) + 
  scale_y_continuous(trans=logplus1trans, breaks=c(1000,10000,100000,1000000,10000000,1e+08)) + 
  theme_classic() + labs(x="Iteration", y=TeX(r'($\mu$)'), color="Chain")

##### t_opt and t_width #####
t_opt_draws <- tidybayes::spread_draws(temp_dep_rec_model, Topt_rec)

names(t_opt_draws) <- c("chain","iteration", "draw", "t_opt")

t_opt_draws <- t_opt_draws %>% mutate(chain=as.factor(chain))

t_opt_trace_plot <- ggplot(t_opt_draws, aes(iteration,t_opt,color=chain)) + geom_line(alpha=0.75) + 
  scale_color_manual(values=chain_palette) + 
  theme_classic() + labs(x="Iteration", y=TeX(r'($T_{opt}(\circ C)$)'), color="Chain")

t_width_draws <- tidybayes::spread_draws(temp_dep_rec_model, width_rec)

names(t_width_draws) <- c("chain","iteration", "draw","t_width")

t_width_draws <- t_width_draws %>% mutate(chain=as.factor(chain))

t_width_trace_plot <- ggplot(t_width_draws, aes(iteration,t_width,color=chain)) + geom_line(alpha=0.75) + 
  scale_color_manual(values=chain_palette) + 
  theme_classic() + labs(x="Iteration", y=TeX(r'($T_{w}(\circ C)$)'), color="Chain")

##### Autoregresive (AR1) term for recruitment error #####
alpha_draws <- tidybayes::spread_draws(temp_dep_rec_model, alpha)

names(alpha_draws) <- c("chain","iteration", "draw","alpha")

alpha_draws <- alpha_draws %>% mutate(chain=as.factor(chain))

alpha_trace_plot <- ggplot(alpha_draws, aes(iteration,alpha,color=chain)) + geom_line(alpha=0.75) + 
  scale_color_manual(values=chain_palette) + 
  theme_classic() + labs(x="Iteration", y=TeX(r'($\alpha$)'), color="Chain")

##### sigma_obs #####
sigma_obs_draws <- tidybayes::spread_draws(temp_dep_rec_model, sigma_obs)

names(sigma_obs_draws) <- c("chain","iteration","draw","sigma_obs")

sigma_obs_draws <- sigma_obs_draws %>% mutate(chain=as.factor(chain))

sigma_obs_trace_plot <- ggplot(sigma_obs_draws, aes(iteration,sigma_obs,color=chain)) + geom_line(alpha=0.75) + 
  scale_color_manual(values=chain_palette) + 
  theme_classic() + labs(x="Iteration", y=TeX(r'($\sigma_{obs}$)'), color="Chain")

##### d #####
d_draws <- tidybayes::spread_draws(temp_dep_rec_model, d)
names(d_draws) <- c("chain","iteration", "draw", "d")
d_draws <- d_draws %>% mutate(chain=as.factor(chain))

d_trace_plot <- ggplot(d_draws, aes(iteration,d,color=chain)) + geom_line(alpha=0.25) + 
  scale_color_manual(values=chain_palette) + 
  theme_classic() + labs(x="Iteration", y=TeX(r'($d$)'), color="Chain")

temp_dep_rec_model %>% spread_draws(d) %>% summarize(mean_d=mean(d,na.rm=TRUE))


# Gather all these plots under a common legend with two columns 
all_rec_trace_plots <- plot_grid( sigma_obs_trace_plot + theme(legend.position="none"),
                   alpha_trace_plot + theme(legend.position="none"),
                   t_opt_trace_plot + theme(legend.position="none"),
                   t_width_trace_plot + theme(legend.position="none"),
                   mean_rec_trace_plot + theme(legend.position="none"),
                   sigma_r_trace_plot + theme(legend.position="none"),
                   ncol=2,
                   align = 'vh',
                   labels = c("(a)", "(b)", "(c)","(d)","(e)","(f)"),
                   hjust = -1
)

# Function to grab a common legend
get_legend_alt <- function(plot) {
  # return all legend candidates
  legends <- get_plot_component(plot, "guide-box", return_all = TRUE)
  # find non-zero legends
  nonzero <- vapply(legends, \(x) !inherits(x, "zeroGrob"), TRUE)
  idx <- which(nonzero)
  # return first non-zero legend if exists, and otherwise first element (which will be a zeroGrob) 
  if (length(idx) > 0) {
    return(legends[[idx[1]]])
  } else {
    return(legends[[1]])
  }
}


# Extract the legend from one of the plots
legend_trace_plot <- get_legend_alt(sigma_obs_trace_plot + theme(legend.position="bottom"))
# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
rec_plots_save <- plot_grid(all_rec_trace_plots, legend_trace_plot, ncol = 1, rel_heights = c(1, .2)) + 
  theme(plot.background = element_rect(fill = "white", colour = "white"))


#### Calculate SMAPE and RMSLE ####

# Get means for each model, patch, and year
abund_py_bind_means <- abund_py_bind %>% group_by(model_name, patch, year) %>% summarize(pred_abundance=mean(abundance, na.rm=TRUE))

# Looks right!

# Join the predicted and actual recorded abundances
abund_joins <- left_join(abund_py_bind_means, abund_p_y_reformat, by=c("year","patch"))
##### RMSLE #####
# Base model
model_rmsle <- abund_joins %>% group_by(model_name) %>% 
  summarize(rmsle_val=rmse(log(pred_abundance+1), log(abundance+1)))

###### RMSLE by each patch ######
model_rmsle_patch <- abund_joins %>% 
  group_by(model_name, patch) %>% 
  summarize(rmsle_val=mltools::rmsle(pred_abundance, abundance))

#### SMAPE ####
model_smape <- abund_joins %>% group_by(model_name) %>% 
  summarize(smape_val=Metrics::smape(pred_abundance, abundance))

###### SMAPE by each patch ######
model_smape_patch <- abund_joins %>% 
  group_by(model_name, patch) %>% 
  summarize(smape_val=Metrics::smape(pred_abundance, abundance))

#### TABLE OF MODEL POSTERIOR VALUES ####

base_model_posteriors <- base_model %>%
  spread_draws(sigma_r, sigma_obs, d, alpha, beta_obs, theta_d) %>%
  summarise_draws() %>% mutate(model_name="Base model") %>% relocate(model_name)

temp_dep_disp_model_posteriors <- temp_dep_disp_model %>%
  spread_draws(sigma_r, sigma_obs, d, alpha, beta_obs, theta_d) %>%
  summarise_draws() %>% mutate(model_name="Temperature dependent dipsersal model") %>% relocate(model_name)

temp_dep_rec_model_posteriors <- temp_dep_rec_model %>%
  spread_draws(sigma_r, sigma_obs, d, alpha, beta_obs, theta_d) %>%
  summarise_draws() %>% mutate(model_name="Temperature dependent recruitment model") %>% relocate(model_name)

mortality_model_posteriors <- mortality_model %>%
  spread_draws(sigma_r, sigma_obs, d, alpha, beta_obs, theta_d) %>%
  summarise_draws() %>% mutate(model_name="Temperature dependent mortality model") %>% relocate(model_name)

# Bind them together

all_models_posteriors_bind <- rbind(base_model_posteriors,
                                    temp_dep_disp_model_posteriors,
                                    mortality_model_posteriors,
                                    temp_dep_rec_model_posteriors)

#

# Model specific parameters
model_t_posteriors <- read_csv(here("supplementary-material", "model_t_posteriors.csv"))

#### Temperature response curves and functions ####
##### THE ACTUAL TEMPERATURES #####
# Get the temperature data
subunit_temps <- read_csv(here("processed-data","all_temperatures_glorys.csv"))
# Rename the Source_Season
subunit_temps <- subunit_temps %>% rename(season=Source_Season)
# Introduce a character column in place of the current subunit column
subunit_temps <- subunit_temps %>% mutate(subunit=as.character(subunit))
subunit_temps <- subunit_temps %>% mutate(subunit=replace(subunit,subunit=="1","North"))
subunit_temps <- subunit_temps %>% mutate(subunit=replace(subunit,subunit=="2","South"))

# Rename the actual variables to be more readable
subunit_temps <- subunit_temps %>% mutate(season=replace(season,season=="GLORYS-Annual","Annual mean"))
subunit_temps <- subunit_temps %>% mutate(season=replace(season,season=="GLORYS-Winter","Winter mean"))

winter_temps <- subunit_temps %>% filter(season=="Winter mean")
winter_temps <- winter_temps %>% mutate(recruit_response=0)

annual_temps <- subunit_temps %>% filter(season=="Annual mean")

##### Recruitment function #####

# Draw the response functions
T_list <- seq(8,13,0.01)

rec_t_opt <- tidybayes::spread_draws(temp_dep_rec_model, Topt_rec) %>% summarize(t_opt=mean(Topt_rec, na.rm=TRUE))
rec_t_width <- tidybayes::spread_draws(temp_dep_rec_model, width_rec) %>% summarize(t_width=mean(width_rec, na.rm=TRUE))

all_rec_t_opts <- tidybayes::spread_draws(temp_dep_rec_model, Topt_rec) %>% select(Topt_rec) %>% pull()
all_rec_t_widths <- tidybayes::spread_draws(temp_dep_rec_model, width_rec) %>% select(width_rec) %>% pull()

# We have 8000 iterations
rec_iters <- 8000
rec_T_response <- array(NA,c(rec_iters,length(T_list)))
rec_med_T = array(NA,length(T_list))  # median curve
rec_lower_T = array(NA,length(T_list)) # low 95% CI 
rec_upper_T = array(NA,length(T_list)) # upper 95% CI

for(i in 1:rec_iters)
{
  rec_T_response[i,] = T_dep(T_list, all_rec_t_opts[i], all_rec_t_widths[i])# dnorm(x=temp_vals,mean=t_opt[i],sd=width[i])
  rec_T_response[i,] = rec_T_response[i,] / max(rec_T_response[i,])
}

rec_med_T = apply(rec_T_response,2,median)
rec_lower_T = apply(rec_T_response,2,quantile,0.05)
rec_upper_T = apply(rec_T_response,2,quantile,0.95)

med_rec_temp_dataframe <- data.frame(iteration_ref=seq(1,501), temp=T_list, recruit_response = rec_med_T)
q5_rec_temp_dataframe <- data.frame(iteration_ref=seq(1,501), temp=T_list, q5 = rec_lower_T)
q95_rec_temp_dataframe <- data.frame(iteration_ref=seq(1,501), temp=T_list, q95 = rec_upper_T)

rec_temp_percentile_dataframe <- data.frame(iteration_ref=seq(1,501), temp=T_list, 
                                            q5=rec_lower_T, q95=rec_upper_T)

# Get the percentiles for the width and optimal temperature
rec_t_opt_stats <- temp_dep_rec_model %>%
  spread_draws(Topt_rec, width_rec) %>%
  summarise_draws()

rec_plot <- ggplot() + 
  geom_line(data=med_rec_temp_dataframe, aes(x=temp, y=recruit_response), color='red', linewidth=1.5) + 
  geom_ribbon(
    data = rec_temp_percentile_dataframe,
    aes(
      x = T_list,
      ymin = q5,
      ymax = q95
    ),
    fill = "#af7d95",
    alpha = 0.5) + 
  theme_classic() + 
  labs(x="Mean Winter Temperature (°C)", y="Recruitment response",
       title="") + 
  theme(axis.title=element_text(),
        legend.text=element_text(size=10),
        plot.caption=element_text(size=8, family="Avenir Next Condensed"),
        plot.title=element_text(size=12, hjust=0.5),
        panel.background = element_rect(fill = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white"), # Set plot background to white
        legend.background = element_rect(fill = "white"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(face="bold"))

##### Disperal function #####


T_list_disp <- seq(3,20,0.01)

all_disp_t_opts <- tidybayes::spread_draws(temp_dep_disp_model, Topt) %>% select(Topt) %>% pull()
all_disp_t_widths <- tidybayes::spread_draws(temp_dep_disp_model, width) %>% select(width) %>% pull()

# We have 8000 iterations
disp_iters <- 8000
disp_T_response <- array(NA,c(disp_iters,length(T_list_disp)))
disp_med_T = array(NA,length(T_list_disp))  # median curve
disp_lower_T = array(NA,length(T_list_disp)) # low 95% CI 
disp_upper_T = array(NA,length(T_list_disp)) # upper 95% CI

for(i in 1:disp_iters)
{
  disp_T_response[i,] = 1-T_dep(T_list_disp, all_disp_t_opts[i], all_disp_t_widths[i])# dnorm(x=temp_vals,mean=t_opt[i],sd=width[i])
  disp_T_response[i,] = disp_T_response[i,] / max(disp_T_response[i,])
}

disp_med_T = apply(disp_T_response,2,median)
disp_lower_T = apply(disp_T_response,2,quantile,0.05)
disp_upper_T = apply(disp_T_response,2,quantile,0.95)

med_disp_temp_dataframe <- data.frame(iteration_ref=seq(1,1701), temp=T_list_disp, dispersal_response = disp_med_T)
q5_disp_temp_dataframe <- data.frame(iteration_ref=seq(1,1701), temp=T_list_disp, q5 = disp_lower_T)
q95_disp_temp_dataframe <- data.frame(iteration_ref=seq(1,1701), temp=T_list_disp, q95 = disp_upper_T)

disp_temp_percentile_dataframe <- data.frame(iteration_ref=seq(1,1701), temp=T_list_disp, 
                                             q5=disp_lower_T, q95=disp_upper_T)


disp_plot <- ggplot() + 
  geom_line(data=med_disp_temp_dataframe, aes(x=temp, y=dispersal_response), color='blue', linewidth=1.5) + 
  geom_ribbon(
    data = disp_temp_percentile_dataframe,
    aes(
      x = T_list_disp,
      ymin = q5,
      ymax = q95
    ),
    fill = "#b3ccff",
    alpha = 0.5) + 
  theme_classic() + 
  labs(x="Mean Annual Temperature (°C)", y="Dispersal response",
       title="") + 
  theme(axis.title=element_text(),
        legend.text=element_text(size=10),
        plot.caption=element_text(size=8, family="Avenir Next Condensed"),
        plot.title=element_text(size=12, hjust=0.5),
        panel.background = element_rect(fill = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white"), # Set plot background to white
        legend.background = element_rect(fill = "white"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(face="bold"))


##### Mortality function #####

mort_beta_vals <- model_t_posteriors %>% filter(model_name=="Temperature dependent mortality model")
mean_beta_0 <- mort_beta_vals %>% filter(variable=="beta_0") %>% select(mean) %>% pull()
mean_beta_1 <- mort_beta_vals %>% filter(variable=="beta_1") %>% select(mean) %>% pull()
mean_beta_2 <- mort_beta_vals %>% filter(variable=="beta_2") %>% select(mean) %>% pull()
q5_beta_0 <- mort_beta_vals %>% filter(variable=="beta_0") %>% select(q5) %>% pull()
q5_beta_1 <- mort_beta_vals %>% filter(variable=="beta_1") %>% select(q5) %>% pull()
q5_beta_2 <- mort_beta_vals %>% filter(variable=="beta_2") %>% select(q5) %>% pull()
q95_beta_0 <- mort_beta_vals %>% filter(variable=="beta_0") %>% select(q95) %>% pull()
q95_beta_1 <- mort_beta_vals %>% filter(variable=="beta_1") %>% select(q95) %>% pull()
q95_beta_2 <- mort_beta_vals %>% filter(variable=="beta_2") %>% select(q95) %>% pull()

T_list_mort <- seq(5,16,0.01)

mort_temp_response <- T_dep_mortality_func(T_list_mort, mean_beta_0,mean_beta_1,mean_beta_2)
plot(T_list_mort, mort_temp_response)

q5_temp_mort_response <- T_dep_mortality_func(T_list_mort, q5_beta_0,q5_beta_1,q5_beta_2)
q95_temp_mort_response <- T_dep_mortality_func(T_list_mort, q95_beta_0,q95_beta_1,q95_beta_2)

q5_temp_mort_dataframe <- data.frame(iteration_ref=9999, temp=T_list_mort, q5 = q5_temp_mort_response)
q95_temp_mort_dataframe <- data.frame(iteration_ref=9999, temp=T_list_mort, q95 = q95_temp_mort_response)

percentile_mort_dataframe <- data.frame(iteration_ref=9999, temp=T_list_mort, 
                                                      q5=q5_temp_mort_response, q95=q95_temp_mort_response)

mort_temp_dataframe <- data.frame(iteration_ref=9999, temp=T_list_mort, mort_response = mort_temp_response)


# Plot for no log no exp
mort_plot <- ggplot() + 
  # geom_point(data=winter_temps, aes(x=sbt, y=recruit_response)) + 
  geom_line(data=mort_temp_dataframe, aes(x=temp, y=mort_response), color='orange', linewidth=1.5) + 
  geom_ribbon(
    data = percentile_mort_dataframe,
    aes(
      x = T_list_mort,
      ymin = q5,
      ymax = q95
    ),
    fill = "#ffe6cc",
    alpha = 0.5) + 
  theme_classic() + 
  labs(x="Mean Annual Temperature (°C)", y="Natural mortality (m)",
       title="") + 
  theme(axis.title=element_text(),
        legend.text=element_text(size=10),
        plot.caption=element_text(size=8, family="Avenir Next Condensed"),
        plot.title=element_text(size=12, hjust=0.5),
        panel.background = element_rect(fill = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white"), # Set plot background to white
        legend.background = element_rect(fill = "white"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(face="bold"))


# NOTE: This is the plot for publication!
response_curves_plot <- plot_grid(rec_plot,
                                  disp_plot,
                                  mort_plot,
                                    ncol=3,
                                    align = 'vh',
                                    labels = c("(a)", "(b)", "(c)"),
                                    hjust = 0
)


#### Custom boxplot for percentiles ####

#### sigma_r ####
# Base model
base_sigma_r <- posterior::extract_variable_matrix(tidybayes::spread_draws(base_model, sigma_r), "sigma_r")
base_sigma_r <- posterior::quantile2(base_sigma_r, probs=c(0.05,0.25,0.75,0.95))
base_sigma_r_df <- data.frame(as.list(base_sigma_r))
base_sigma_r_df <- base_sigma_r_df %>% mutate(model_name="Base",variable="sigma_r",.before=1)
t1 <- base_model %>% 
  tidybayes::spread_draws(sigma_r) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Base",variable="sigma_r",.before=1)
base_sigma_r_df <- left_join(base_sigma_r_df, t1, by=c("model_name","variable"))

# Temperature dependent dispersal
disp_sigma_r <- posterior::extract_variable_matrix(tidybayes::spread_draws(temp_dep_disp_model, sigma_r), "sigma_r")
disp_sigma_r <- posterior::quantile2(disp_sigma_r, probs=c(0.05,0.25,0.75,0.95))
disp_sigma_r_df <- data.frame(as.list(disp_sigma_r))
disp_sigma_r_df <- disp_sigma_r_df %>% mutate(model_name="Dispersal",variable="sigma_r",.before=1)
t2 <- temp_dep_disp_model %>% 
  tidybayes::spread_draws(sigma_r) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Dispersal",variable="sigma_r",.before=1)
disp_sigma_r_df <- left_join(disp_sigma_r_df, t2, by=c("model_name","variable"))

# Temperature depedent mortality
mort_sigma_r <- posterior::extract_variable_matrix(tidybayes::spread_draws(mortality_model, sigma_r), "sigma_r")
mort_sigma_r <- posterior::quantile2(mort_sigma_r, probs=c(0.05,0.25,0.75,0.95))
mort_sigma_r_df <- data.frame(as.list(mort_sigma_r))
mort_sigma_r_df <- mort_sigma_r_df %>% mutate(model_name="Mortality",variable="sigma_r",.before=1)
t3 <- mortality_model %>% 
  tidybayes::spread_draws(sigma_r) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Mortality",variable="sigma_r",.before=1)
mort_sigma_r_df <- left_join(mort_sigma_r_df, t3, by=c("model_name","variable"))

# Temperature dependent recruitment
rec_sigma_r <- posterior::extract_variable_matrix(tidybayes::spread_draws(temp_dep_rec_model, sigma_r), "sigma_r")
rec_sigma_r <- posterior::quantile2(rec_sigma_r, probs=c(0.05,0.25,0.75,0.95))
rec_sigma_r_df <- data.frame(as.list(rec_sigma_r))
rec_sigma_r_df <- rec_sigma_r_df %>% mutate(model_name="Recruitment",variable="sigma_r",.before=1)
t4 <- temp_dep_rec_model %>% 
  tidybayes::spread_draws(sigma_r) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Recruitment",variable="sigma_r",.before=1)
rec_sigma_r_df <- left_join(rec_sigma_r_df, t4, by=c("model_name","variable"))


# Bind these dataframes together
sigma_r_bind <- rbind(base_sigma_r_df,
                      disp_sigma_r_df,
                      mort_sigma_r_df,
                      rec_sigma_r_df)

ggplot(sigma_r_bind, aes(x = as.factor(model_name))) +
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = median, upper = q75, ymax = q95),
    stat = "identity",
    fill = "lightblue",
    color = "black"
  ) +
  labs(
    y=TeX(r'($\sigma_{r}$)'),
    x = "Model"
  ) +
  theme_minimal()

# Looks like it works

#### sigma_obs ####

##### Base model #####
base_sigma_obs <- posterior::extract_variable_matrix(tidybayes::spread_draws(base_model, sigma_obs), "sigma_obs")
base_sigma_obs <- posterior::quantile2(base_sigma_obs, probs=c(0.05,0.25,0.75,0.95))
base_sigma_obs_df <- data.frame(as.list(base_sigma_obs))
base_sigma_obs_df <- base_sigma_obs_df %>% mutate(model_name="Base",variable="sigma_obs",.before=1)
t1 <- base_model %>% 
  tidybayes::spread_draws(sigma_obs) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Base",variable="sigma_obs",.before=1)
base_sigma_obs_df <- left_join(base_sigma_obs_df, t1, by=c("model_name","variable"))

##### Temperature dependent dispersal #####
disp_sigma_obs <- posterior::extract_variable_matrix(tidybayes::spread_draws(temp_dep_disp_model, sigma_obs), "sigma_obs")
disp_sigma_obs <- posterior::quantile2(disp_sigma_obs, probs=c(0.05,0.25,0.75,0.95))
disp_sigma_obs_df <- data.frame(as.list(disp_sigma_obs))
disp_sigma_obs_df <- disp_sigma_obs_df %>% mutate(model_name="Dispersal",variable="sigma_obs",.before=1)
t2 <- temp_dep_disp_model %>% 
  tidybayes::spread_draws(sigma_obs) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Dispersal",variable="sigma_obs",.before=1)
disp_sigma_obs_df <- left_join(disp_sigma_obs_df, t2, by=c("model_name","variable"))

##### Temperature depedent mortality #####
mort_sigma_obs <- posterior::extract_variable_matrix(tidybayes::spread_draws(mortality_model, sigma_obs), "sigma_obs")
mort_sigma_obs <- posterior::quantile2(mort_sigma_obs, probs=c(0.05,0.25,0.75,0.95))
mort_sigma_obs_df <- data.frame(as.list(mort_sigma_obs))
mort_sigma_obs_df <- mort_sigma_obs_df %>% mutate(model_name="Mortality",variable="sigma_obs",.before=1)
t3 <- mortality_model %>% 
  tidybayes::spread_draws(sigma_obs) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Mortality",variable="sigma_obs",.before=1)
mort_sigma_obs_df <- left_join(mort_sigma_obs_df, t3, by=c("model_name","variable"))

##### Temperature dependent recruitment #####
rec_sigma_obs <- posterior::extract_variable_matrix(tidybayes::spread_draws(temp_dep_rec_model, sigma_obs), "sigma_obs")
rec_sigma_obs <- posterior::quantile2(rec_sigma_obs, probs=c(0.05,0.25,0.75,0.95))
rec_sigma_obs_df <- data.frame(as.list(rec_sigma_obs))
rec_sigma_obs_df <- rec_sigma_obs_df %>% mutate(model_name="Recruitment",variable="sigma_obs",.before=1)
t4 <- temp_dep_rec_model %>% 
  tidybayes::spread_draws(sigma_obs) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Recruitment",variable="sigma_obs",.before=1)
rec_sigma_obs_df <- left_join(rec_sigma_obs_df, t4, by=c("model_name","variable"))


##### Bind these dataframes together #####
sigma_obs_bind <- rbind(base_sigma_obs_df,
                      disp_sigma_obs_df,
                      mort_sigma_obs_df,
                      rec_sigma_obs_df)

#### d ####

##### Base model #####
base_d <- posterior::extract_variable_matrix(tidybayes::spread_draws(base_model, d), "d")
base_d <- posterior::quantile2(base_d, probs=c(0.05,0.25,0.75,0.95))
base_d_df <- data.frame(as.list(base_d))
base_d_df <- base_d_df %>% mutate(model_name="Base",variable="d",.before=1)
t1 <- base_model %>% 
  tidybayes::spread_draws(d) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Base",variable="d",.before=1)
base_d_df <- left_join(base_d_df, t1, by=c("model_name","variable"))

##### Temperature dependent dispersal #####
disp_d <- posterior::extract_variable_matrix(tidybayes::spread_draws(temp_dep_disp_model, d), "d")
disp_d <- posterior::quantile2(disp_d, probs=c(0.05,0.25,0.75,0.95))
disp_d_df <- data.frame(as.list(disp_d))
disp_d_df <- disp_d_df %>% mutate(model_name="Dispersal",variable="d",.before=1)
t2 <- temp_dep_disp_model %>% 
  tidybayes::spread_draws(d) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Dispersal",variable="d",.before=1)
disp_d_df <- left_join(disp_d_df, t2, by=c("model_name","variable"))

##### Temperature depedent mortality #####
mort_d <- posterior::extract_variable_matrix(tidybayes::spread_draws(mortality_model, d), "d")
mort_d <- posterior::quantile2(mort_d, probs=c(0.05,0.25,0.75,0.95))
mort_d_df <- data.frame(as.list(mort_d))
mort_d_df <- mort_d_df %>% mutate(model_name="Mortality",variable="d",.before=1)
t3 <- mortality_model %>% 
  tidybayes::spread_draws(d) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Mortality",variable="d",.before=1)
mort_d_df <- left_join(mort_d_df, t3, by=c("model_name","variable"))

##### Temperature dependent recruitment #####
rec_d <- posterior::extract_variable_matrix(tidybayes::spread_draws(temp_dep_rec_model, d), "d")
rec_d <- posterior::quantile2(rec_d, probs=c(0.05,0.25,0.75,0.95))
rec_d_df <- data.frame(as.list(rec_d))
rec_d_df <- rec_d_df %>% mutate(model_name="Recruitment",variable="d",.before=1)
t4 <- temp_dep_rec_model %>% 
  tidybayes::spread_draws(d) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Recruitment",variable="d",.before=1)
rec_d_df <- left_join(rec_d_df, t4, by=c("model_name","variable"))


##### Bind these dataframes together #####
d_bind <- rbind(base_d_df,
                        disp_d_df,
                        mort_d_df,
                        rec_d_df)

#### alpha ####

##### Base model #####
base_alpha <- posterior::extract_variable_matrix(tidybayes::spread_draws(base_model, alpha), "alpha")
base_alpha <- posterior::quantile2(base_alpha, probs=c(0.05,0.25,0.75,0.95))
base_alpha_df <- data.frame(as.list(base_alpha))
base_alpha_df <- base_alpha_df %>% mutate(model_name="Base",variable="alpha",.before=1)
t1 <- base_model %>% 
  tidybayes::spread_draws(alpha) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Base",variable="alpha",.before=1)
base_alpha_df <- left_join(base_alpha_df, t1, by=c("model_name","variable"))

##### Temperature dependent dispersal #####
disp_alpha <- posterior::extract_variable_matrix(tidybayes::spread_draws(temp_dep_disp_model, alpha), "alpha")
disp_alpha <- posterior::quantile2(disp_alpha, probs=c(0.05,0.25,0.75,0.95))
disp_alpha_df <- data.frame(as.list(disp_alpha))
disp_alpha_df <- disp_alpha_df %>% mutate(model_name="Dispersal",variable="alpha",.before=1)
t2 <- temp_dep_disp_model %>% 
  tidybayes::spread_draws(alpha) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Dispersal",variable="alpha",.before=1)
disp_alpha_df <- left_join(disp_alpha_df, t2, by=c("model_name","variable"))

##### Temperature depedent mortality #####
mort_alpha <- posterior::extract_variable_matrix(tidybayes::spread_draws(mortality_model, alpha), "alpha")
mort_alpha <- posterior::quantile2(mort_alpha, probs=c(0.05,0.25,0.75,0.95))
mort_alpha_df <- data.frame(as.list(mort_alpha))
mort_alpha_df <- mort_alpha_df %>% mutate(model_name="Mortality",variable="alpha",.before=1)
t3 <- mortality_model %>% 
  tidybayes::spread_draws(alpha) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Mortality",variable="alpha",.before=1)
mort_alpha_df <- left_join(mort_alpha_df, t3, by=c("model_name","variable"))

##### Temperature dependent recruitment #####
rec_alpha <- posterior::extract_variable_matrix(tidybayes::spread_draws(temp_dep_rec_model, alpha), "alpha")
rec_alpha <- posterior::quantile2(rec_alpha, probs=c(0.05,0.25,0.75,0.95))
rec_alpha_df <- data.frame(as.list(rec_alpha))
rec_alpha_df <- rec_alpha_df %>% mutate(model_name="Recruitment",variable="alpha",.before=1)
t4 <- temp_dep_rec_model %>% 
  tidybayes::spread_draws(alpha) %>% 
  summarise_draws("mean","median") %>% 
  mutate(model_name="Recruitment",variable="alpha",.before=1)
rec_alpha_df <- left_join(rec_alpha_df, t4, by=c("model_name","variable"))


##### Bind these dataframes together #####
alpha_bind <- rbind(base_alpha_df,
                        disp_alpha_df,
                        mort_alpha_df,
                        rec_alpha_df)

#### Box plots ####

##### sigma_r #####

sigma_r_boxplot <- ggplot(sigma_r_bind, aes(x = as.factor(model_name))) +
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = median, upper = q75, ymax = q95),
    stat = "identity",
    fill = "lightblue",
    color = "black"
  ) +
  labs(
    y=TeX(r'($\sigma_{r}$)'),
    x = ""
  ) +
  theme_minimal() + 
  theme(axis.text.x=(element_text(angle=45, vjust=1, hjust=1)))

##### sigma_obs #####

sigma_obs_boxplot <- ggplot(sigma_obs_bind, aes(x = as.factor(model_name))) +
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = median, upper = q75, ymax = q95),
    stat = "identity",
    fill = "lightblue",
    color = "black"
  ) +
  labs(
    y=TeX(r'($\sigma_{obs}$)'),
    x = ""
  ) +
  theme_minimal() + 
  theme(axis.text.x=(element_text(angle=45, vjust=1, hjust=1)))

##### d #####

d_boxplot <- ggplot(d_bind, aes(x = as.factor(model_name))) +
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = median, upper = q75, ymax = q95),
    stat = "identity",
    fill = "lightblue",
    color = "black"
  ) +
  labs(
    y=TeX(r'($d$)'),
    x = ""
  ) +
  theme_minimal() + 
  theme(axis.text.x=(element_text(angle=45, vjust=1, hjust=1)))

##### alpha #####

alpha_boxplot <- ggplot(alpha_bind, aes(x = as.factor(model_name))) +
  geom_boxplot(
    aes(ymin = q5, lower = q25, middle = median, upper = q75, ymax = q95),
    stat = "identity",
    fill = "lightblue",
    color = "black"
  ) +
  labs(
    y=TeX(r'($\alpha$)'),
    x = ""
  ) +
  theme_minimal() + 
  theme(axis.text.x=(element_text(angle=45, vjust=1, hjust=1)))

##### Gather all boxplots ######

all_boxplots <- plot_grid(sigma_r_boxplot,
                          sigma_obs_boxplot,
                          d_boxplot,
                          alpha_boxplot,
                          ncol=2,
                          align = 'vh',
                          labels = c("(a)", "(b)", "(c)","(d)"),
                          hjust = 0
)

all_boxplots <- all_boxplots + theme(plot.background = element_rect(fill = "white", colour = "white"))

#### SAVE ALL PLOTS ####
SAVE_PLOTS <- FALSE

#### GENERATED PLOTS ####
# 1. fig_03_model_abundance_fits.png
# 2. fig_04_temperature_response_curves.png
# 3. fig_05_boxplot_common_parameters.png
# 4. supp_fig_01_trace_plots.png

if(SAVE_PLOTS){
  ggsave(here("figures","fig_03_model_abundance_fits.png"),all_models_together_pub,width=8, height=10, units=c("in"), dpi=300)
  ggsave(here("figures","fig_04_temperature_response_curves.png"), response_curves_plot, width=8, height=5, dpi=600, units=c("in"))
  ggsave(here("figures","fig_05_boxplot_common_parameters.png"),all_boxplots, width=6, height=8, dpi=600, units=c("in"))
  ggsave(here("figures","supplementary-figures","supp_fig_01_trace_plots.png"),rec_plots_save, width=6, height=10, dpi=600, units=c("in"))
}

#### END OF FILE ####
