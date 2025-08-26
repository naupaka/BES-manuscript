# Main file for processing figure outputs

# Load up the associated libraries:
library(tidyverse)
library(lubridate)
library(broom)
library(grid)
library(gridExtra)
library(gtable)
library(gt)
library(jsonlite)
library(lutz)
library(neonSoilFlux)
library(neonUtilities)

# Source the associated R functions (to process data)
source('R/functions/extract_licor_automated_flux.R')
source('R/functions/extract_licor_flux.R')

# Source the following files in order:
source('R/process/01-process-neonSoilFlux.R')
source('R/process/02-process-field-licor.R')
source('R/process/03-field-neonSoilFlux-combine.R')
source('R/process/04-extract-diffusivity-field.R')
source('R/process/f4-flux-results.R')
source('R/process/f5-flux-results-year.R')
source('R/process/f6-r2-plot.R')
source('R/process/f7-diffusivity-plot.R')
source('R/process/sf1-gap-filled-stats.R')
source('R/process/sf2-uncertainty-stats.R')

