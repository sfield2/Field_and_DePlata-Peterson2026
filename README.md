[![Last-updated](https://img.shields.io/badge/last%20update-February%202026-brightgreen.svg)](ithub.com/sfield2/Field_and_DePlata-Peterson2026)
[![License](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)


# Research compendium for Field and DePlata-Peterson (under revision)
This R-based research compendium supports an analysis of precipitation variability between 600-1350 C.E> in the northern US Southwest.

# Overview
The script used in this analysis "Field_and_DePlataPeterson_2026.qmd", which is a Quarto markdown document that includes code to perform all of the data scraping and image creation steps reported in the manuscript. 

Field_and_DePlataPeterson_2026.qmd performs the following steps:
1. Import `R` dependencies
2. Import Study Region Shapefiles
3. Import Site Data (NOTE: this data is not publicly accessible, but can be retrieved from sources cited in the markdown document)
4. Download Paleoclimate Data and Analyze for precipitation availability and variability
5. Integrate Paleoclimate and Settlement Data as Time Series
6. Analyze Relationship between Settlement and Paleoclimate
7. Import Dendrochronological Data (NOTE: this data is not publicly accessible, but can be retrieved from sources cited in the markdown document) and compare to Paleocliamte

# Directory Strucutre
1. `FieldandDean2026.qmd`: `R` and `Python` scripts to conduct analyses
2. Data/: Directory containing data used in study. NOTE: Non-publicly accessible data is not included.
4. Figures/: Directory containing low resolution figures used in the manuscript.
6. README.Rmd: This `README` file

# Citation
When using the code included in this research compenidum, please cite all of the following:

Field, S and M. DePlata-Peterson. 2026. Variability as a Driver of Re-settlement in the northern US Southwest. Journal of Archaeological Science, In Prep.

Field, S and M. DePlata-Peterson. 2026. Research Compendium for Variability as a Driver of Re-settlement in the northern US Southwest., Version 1.0.0. Zenodo. [DOI]

# Requirements
This analysis requires R (≥ 4.2.0) and the following packages:
`sf`  `terra`  `ggplot2`  `tidyterra`  `dplyr`  `tidyr` `gifski` `gganimate` `ggforce` `ggpubr` `scales` `lmtest` `tseries` `FedData`
