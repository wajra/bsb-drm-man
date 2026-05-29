# Script - pub_script_04_study_area_plots.R
# Task - Make map of the study area along with temperatures in the subunit
# Author - Jeewantha Bandara (jeewantha.bandara@rutgers.edu)

# load packages
library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
library(patchwork)
library(RColorBrewer)
library(ggspatial)

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

# Get the temperature trends for the north and south subunit
# Linear model 1 - North + Annual mean
lm_1 <- lm(sbt~year, subunit_temps %>% filter(season=="Annual mean" & subunit=="North"))
# Linear model 2 - North + Winter mean
lm_2 <- lm(sbt~year, subunit_temps %>% filter(season=="Winter mean" & subunit=="North"))
# Linear model 3 - South + Annual mean
lm_3 <- lm(sbt~year, subunit_temps %>% filter(season=="Annual mean" & subunit=="South"))
# Linear model 4 - South + Winter mean
lm_4 <- lm(sbt~year, subunit_temps %>% filter(season=="Winter mean" & subunit=="South"))

# Get the individual slopes for each trend
north_annual_mean_trend <- round(summary(lm_1)$coefficients["year","Estimate"],2)
north_winter_mean_trend <- round(summary(lm_2)$coefficients["year","Estimate"],2)
south_annual_mean_trend <- round(summary(lm_3)$coefficients["year","Estimate"],2)
south_winter_mean_trend <- round(summary(lm_4)$coefficients["year","Estimate"],2)

# Get the standard error for each trend
north_annual_se <- round(summary(lm_1)$coefficients["year","Std. Error"],2)
north_winter_se <- round(summary(lm_2)$coefficients["year","Std. Error"],2)
south_annual_se <- round(summary(lm_3)$coefficients["year","Std. Error"],2)
south_winter_se <- round(summary(lm_4)$coefficients["year","Std. Error"],2)

summary(lm_1)

# P-values for the linear models
north_annual_p_val <- summary(lm_1)$coefficients["year",4]
north_winter_p_val <- summary(lm_2)$coefficients["year",4]
south_annual_p_val <- summary(lm_3)$coefficients["year",4]
south_winter_p_val <- summary(lm_4)$coefficients["year",4]

# Mean temeperature during study time period
subunit_temps %>% group_by(subunit, season) %>% summarize(mean_temp=mean(sbt, na.rm=TRUE),
                                                          se_temp=sd(sbt,na.rm=TRUE))

# Create a little dataframe for this
trend_dataframe <- data.frame(subunit=c("North","North","South","South"),
                              season=c("Annual mean","Winter mean","Annual mean","Winter mean"),
                              trend=c(north_annual_mean_trend, north_winter_mean_trend, south_annual_mean_trend, south_winter_mean_trend),
                              se=c(north_annual_se, north_winter_se, south_annual_se, south_winter_se),
                              xloc=c(2014,2014,2013,2014),
                              yloc=c(10.25,6.5,14.1,7))

# Get the rnaturalearth data
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


# Map of the study area
study_area <- ggplot(data = world) +
  geom_sf(fill="antiquewhite") + geom_sf(data = neus_north, fill="gray", linetype="twodash", linewidth=0.5, color="black", alpha=0.25) + # Add an arrow here to show direction of motion
  geom_sf(data = neus_south, fill="gray", linetype="dashed", linewidth=0.5, color="black", alpha=0.25) + # Add an arrow here to show direction of motion
  geom_sf(data=canyon_line_obj, color="black", shape=6, linewidth=0.5) + 
  annotate("text",label="Atlantic Ocean", x=-72.5+2, y=32.5+2,
           color="black", 
           size=5 , fontface="bold.italic") + 
  annotate("text",label="North \nsubunit", x=-68, y=42.5,
           color="black",
           size=4 ) + 
  annotate("text",label="South subunit", x=-70.75, y=38,
           color="black", 
           size=4 ) + 
  annotate("text",label="Hudson canyon", x=-68.75, y=39.4,
           color="black", 
           size=4, fontface="italic" ) + 
  coord_sf(xlim = c(-81.08+1, -63.59+1), ylim = c(29.81+3,46.73+3), expand = FALSE) + 
  annotation_scale(location = "bl", bar_cols = c("grey60", "white"), width_hint = 0.25) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         style = north_arrow_nautical()) + 
  labs(x="Longitude", y="Latitude") + 
  theme(panel.grid.major = element_line(color = gray(.4), size = 0.1), 
        panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
        plot.caption=element_text(size=8, family="Avenir Next Condensed"),
        plot.title=element_text(size=12, hjust=0.5),
        legend.position="bottom",
        legend.key = element_rect(fill = "white"))
# theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1))


# Create a plot of the zoomed out study area 
study_area_zoom_out <- ggplotGrob(ggplot(data = world) +
                                        geom_sf(fill="antiquewhite") + 
                                        annotate("rect", xmin=c(-81.08+1), xmax=c(-63.59+1), ymin=c(29.81+3) , ymax=c(46.73+3), alpha=0.2, color="blue", fill="blue") + 
                                        coord_sf(xlim = c(-132, -50), ylim = c(19.7,60.1), expand = FALSE) + 
                                        labs(x="Longitude", y="Latitude") + 
                                        theme(panel.background = element_rect(fill = "aliceblue"),
                                              plot.caption=element_text(size=8, family="Avenir Next Condensed"),
                                              plot.title=element_text(size=12, hjust=0.5),
                                              legend.position="bottom",
                                              legend.key = element_rect(fill = "white"),
                                              axis.title.x=element_blank(),
                                              axis.text.x=element_blank(),
                                              axis.ticks.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              axis.text.y=element_blank(),
                                              axis.ticks.y=element_blank(),
                                              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                              panel.border = element_rect(colour = "black", fill=NA, size=0.25),
                                              plot.background = element_blank()))

# Add the ggplotGrob to the study area for the zoomed out effect
study_area_plot <- study_area +
  annotation_custom(grob = study_area_zoom_out, xmin = -81+1, xmax = -74+1,
                    ymin = 40+3, ymax = 49+3) 


# Plot with temperature trends
subunit_temp_plot <- ggplot(subunit_temps %>% filter(season %in% c("Winter mean","Annual mean")), aes(x=year, y=sbt)) + 
  geom_line(aes(color=season), linewidth=0.9, show.legend=FALSE) + 
  geom_text(data=trend_dataframe, aes(x=xloc, y=yloc, label=paste("+",trend,"°C/yr",sep=""), color=season), fontface="bold", show.legend = FALSE) + 
  theme_fivethirtyeight() + 
  # scale_linetype_manual(values = c("North" = "twodash", "South" = "dashed")) + 
  facet_wrap(~subunit, nrow=2) + 
  labs(x="Year", y="Temperature (°C)", 
       color="Season",
       title="") + 
  scale_y_continuous(name="Temperature (°C)", limits=c(5, 15)) + 
  theme(axis.title=element_text(),
        legend.text=element_text(size=10),
        plot.caption=element_text(size=8, family="Avenir Next Condensed"),
        plot.title=element_text(size=12, hjust=0.5),
        panel.background = element_rect(fill = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white"), # Set plot background to white
        legend.background = element_rect(fill = "white"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(face="bold"),
        axis.line = element_line(size = 0.5, colour = "black"),
        axis.ticks = element_line(size = 0.5, color="black"))


# Join the study area figure and temperature trends plots together
study_area_temp_trends_plot <- study_area_plot + subunit_temp_plot

study_area_temp_trends_plot <- study_area_temp_trends_plot + 
  plot_annotation(tag_levels = 'a', tag_prefix='(',tag_suffix = ')') & theme(plot.tag = element_text(face = 'bold'))

ggsave(here("figures","fig_01_study_area_temp_trends.png"),study_area_temp_trends_plot,width=8, height= 6, units=c("in"), dpi=300)
