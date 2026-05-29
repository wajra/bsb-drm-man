# Data for analysis

This folder contains the data files required to run a dynamic range model (DRM) 
on black sea bass (_Centropristis striata_) in the Northeast US.

1. `OceanAdapt-update2020/` - Data files from the OceanAdapt repository that contains raw trawl survey information for NEUS
2. `TrawlSurveyMetadata-master/` -  Data files from Maureaud et al. (2020,2024). We specifically use trawl area shapefiles for the manuscript
3. `stock_assessment_data` - Stock assessment output files for black sea bass in the NEUS

Please note that only the relevant raw files from the OceanAdapt (`OceanAdapt-update2020/data_clean/dat_exploded.rds`,
`OceanAdapt-update2020/data_raw/neus_Survdat.Rdata`, `OceanAdapt-update2020/data_raw/neus_SVSPP.Rdata`) and 
TrawlSurveyMetadata-master (`TrawlSurveyMetadata-master/data/metadata/Metadata_18062020.shp`) have been 
kept in this repository the interest of preserving space. We have kept their native README files in place for clarity.

Sources for the complete datasets are provided below.

The sources for the data are as follows

1. [Northeast Fisheries Science Center Data Portal](https://apps-nefsc.fisheries.noaa.gov/saw/sasi.php)
2. [Black Sea Bass Operational Stock Assessment for year 2021](https://apps-nefsc.fisheries.noaa.gov/saw/sasi_files.php?year=2021&species_id=33&stock_id=6&review_type_id=2&info_type_id=-1&map_type_id=&filename=BSB_Operational_assessment_2021-iii.pdf)
3. Northeast Fisheries Science Center Trawl Survey data from [OceanAdapt](https://github.com/pinskylab/OceanAdapt) - Methods described in [Pinsky et al. 2013](https://www.science.org/doi/10.1126/science.1239352) and [Morley et al. 2018](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0196127)
4. Shapefiles for global bottom trawl surveys - Source from [Maureaud et al. (2020, 2024)](https://github.com/AquaAuma/FishGlob_data)

