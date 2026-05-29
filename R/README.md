# R code

Contains the .R files that are to be run sequentially to set up the models, it's 
variations, and derive and show results. You don't have to run most of these files 
(those required for cleaning and preparing the data).
Short descriptions of the .R files and their outputs are outlined below.

1. `pub_script_01_prep_all_data.R` - Filters out the species data from the Ocean 
Adapt dataset and formats trawl hauls nicely for later analysis, reads in 
weight-at-age and fishing pressure data from the black sea bass stock assessment 
(2021 MAFC), downloads seasonal and annual bottom temperatures from GLORYS via 
`ecodata` package and tidies them for later analysis.
2. `pub_script_02_analyze_black_sea_bass.R` - Format the black sea bass trawl 
survey data and run the stan models and save their outputs.
3. `pub_script_03_visualize_model_results.R` - Reads in all models and visualizes 
diagnostic trace plots, estimated temperature optima, and model estimated vs. recorded abundances.
4. `pub_script_04_study_area_plots.R` - Creates plots of the study area, map of 
management area, and change in temperature of study area.

`pub_script_01_prep_all_data.R` and `pub_script_02_analyze_black_sea_bass.R` is 
not required to be run on your computer unless you want to reproduce certain 
results or change certain parameters or switch on/off different model settings. 
All the data files that are produced from `pub_script_01_prep_all_data.R` and 
`pub_script_02_analyze_black_sea_bass.R` are included in this github repo.
