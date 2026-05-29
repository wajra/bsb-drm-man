# TrawlSurveyMetadata
Codes and data files to reproduce figures from *Are we ready to track climate-driven shifts in marine species across international boundaries? A global survey of scientific bottom trawl data* published in Global Change Biology, [DOI: 10.1111/gcb.15404](https://doi.org/10.1111/gcb.15404)

Authors: Aurore Maureaud, Romain Frelat, Laurène Pécuchet, Nancy Shackell, Bastien Mérigot, Malin L. Pinsky, Kofi Amador, Sean C. Anderson, Alexander Arkhipkin, Arnaud Auber, Iça Barri, Rich Bell, Jonathan Belmaker, Esther Beukhof, Mohamed Lamine Camara, Renato Guevara-Carrasco, Junghwa Choi, Helle Torp Christensen, Jason Conner, Luis A. Cubillos, Hamet Diaw Diadhiou, Dori Edelist, Margrete Emblemsvåg, Billy Ernst, Tracey P. Fairweather, Heino O. Fock, Kevin D. Friedland, Camilo B. Garcia, Didier Gascuel, Henrik Gislason, Menachem Goren, Jérôme Guitton, Didier Jouffre, Tarek Hattab, Manuel Hidalgo, Johannes N. Kathena, Ian Knuckey, Saïkou Oumar Kidé, Mariano Koen-Alonso, Matt Koopman, Vladimir Kulik, Jacqueline Palacios León, Ya’arit Levitt-Barmats, Martin Lindegren, Marcos Llope, Félix Massiot-Granier, Hicham Masski, Matthew McLean, Beyah Meissa, Laurène Mérillet, Vesselina Mihneva, Francis K.E. Nunoo, Richard O'Driscoll, Cecilia A. O'Leary, Elitsa Petrova, Jorge E. Ramos,, Wahid Refes, Esther Román-Marcote, Helle Siegstad, Ignacio Sobrino, Jón Sólmundsson, Oren Sonin, Ingrid Spies, Petur Steingrund, Fabrice Stephenson, Nir Stern, Feriha Tserkova, Georges Tserpes, Evangelos Tzanatos, Itai van Rijn, Paul A.M. van Zwieten, Paraskevas Vasilakopoulos, Daniela V. Yepsen, Philippe Ziegler, James Thorson



The code is organized in 6 scripts:

- plotMap.r: create Figure 1 and supplementary 4: Additional maps.

- commercial.spp.range.r: create Figure 2a-c with coverage of stocks per FAO region

- transboundary.stock.r : create figure 2d-f with stock definition from the RAM legacy database

- VAST.arrowtooth.r : compute VAST and create figure 3, see Supplementary 7 for more methodological details.

- statBathyEEZ.r: compute statistics of coverage for Supplementary 3: Productive and fished areas covered by surveys

- plot.COG.r: use center of gravity for cod for insert in Figure 3c.

  

We try to keep the metadata information on bottom trawl survey updated in the repository `update`. If you see wrong or incomplete information, please report a new issue. 

The last update of the metadata (similar to table in Supplementary 1) is available here: [update/Metadata_last.csv](https://raw.githubusercontent.com/AquaAuma/TrawlSurveyMetadata/master/update/Metadata_last.csv)

The updated extent of the surveys can be visualize interactively: https://rfrelat.shinyapps.io/metabts/



[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

