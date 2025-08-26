# Repository for submitted manuscript

## "`neonSoilFlux`: An R Package for Continuous Sensor-Based Estimation of Soil CO~2~ Fluxes"

## Directory structure:

-   `data/raw`: collected field data and `neonSoilFlux` generated data files
-   `data/derived`: derived data files (`.Rda`) from running all the scripts `01` - `04` in `R/process`
-   `figures`: output figures generated (along with other associated figure files for the manuscript).
-   `R/functions`: common functions used across all processing files
-   `R/process`: structured processing files to generate manuscript output. This directory has the convention that the structure for `.R` files:
    -   `00`-`04` files are processing files that generate derived data files for `data/derived`
    -   `f` denotes figure, `t` denotes table, `sf` supplementary figure
    -   the number corresponds to the figure or table number in the manuscript
    -   the text following corresponds to the name of the figure or table generated in `figures`

## Required R libraries (all on CRAN):

-   `tidyverse`
-   `lubridate` (if not included in `tidyverse` installation)
-   `broom`
-   `grid`
-   `gridExtra`
-   `gtable`
-   `gt`
-   `sf`
-   `jsonlite`
-   `lutz`
-   `neonSoilFlux`
-   `neonUtilities`

To reproduce manuscript figures and tables, source the following file: `source('00-start-here.R')`
