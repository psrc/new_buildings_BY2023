library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)
library(tidyverse)
library(DT)

base.ind.dir <- "/media/modelsrv8d/opusgit/urbansim_data/data/psrc_parcel/runs"
base.ind.dir <- "/Volumes/d$/opusgit/urbansim_data/data/psrc_parcel/runs"
             

#wrkdir <- '/home/shiny/apps/' # shiny path
wrkdir <- '/Users/hana/R/shinyserver/'
# wrkdir <- 'C:/Users/CLam/Desktop/'

data <- 'parcel-viewer/data'

parcel.main <- 'parcels2014.rds'
parcel.att <- 'parcels_for_viewer.rds'

parcels <- readRDS(file.path(wrkdir, data, parcel.main))
attr <- readRDS(file.path(wrkdir, data, parcel.att))

parcels.attr <- parcels %>% left_join(attr, by = "parcel_id")

rm(attr)
rm(parcels)
